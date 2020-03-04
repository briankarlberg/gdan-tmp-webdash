suppressMessages({
  library(foreach)
  library(magrittr)
})

message("loading prediction files...")
output_files <- list.files("/mnt/data/predictions", full.names = T)
if (length(output_files) == 0) {
  message("no prediction files were found!")
  predictions <- dplyr::tibble(
    prediction_id = character(),
    cancer_id = character(),
    model_id = character(),
    featureset_id = character(),
    sample_id = character(),
    `repeat` = numeric(),
    fold = numeric(),
    type = character(),
    actual_value = factor(),
    predicted_value = factor(),
    date = date()
  )
} else {
  predictions <- foreach(f = output_files, .combine = dplyr::bind_rows) %do% {
    data.table::fread(f) %>%
      dplyr::as_tibble() %>%
      tidyr::gather(-Sample_ID, -Repeat, -Fold, -Test, -Label, key = "prediction_id", value = "predicted_value") %>%
      dplyr::rename(sample_id = Sample_ID, `repeat` = Repeat, fold = Fold, actual_value = Label, type = Test) %>%
      tidyr::separate(prediction_id, sep = "\\|", into = c("model_id", "featureset_id", "date", "ptype"), remove = F) %>%
      tidyr::separate(actual_value, sep = ":", into = c("cancer_id", "extra"), remove = F) %>%
      dplyr::select(-ptype, -extra) %>%
      dplyr::mutate(type = ifelse(type == 1, "testing", "training"))
  } %>%
    dplyr::mutate(predicted_value = as.factor(predicted_value),
                  actual_value = as.factor(actual_value),
                  date = as.Date(date)) %>%
    tidyr::separate(model_id, "\\:", into = c("cancer_id", "model_id"))
}

message("loading feature set files...")
featureset_files <- list.files("/mnt/data/feature-sets", full.names = T)
if (length(featureset_files) == 0) {
  message("no feature set files were found!")
  featureSets <- dplyr::tibble(
    featureset_id = character(),
    cancer_id = character(),
    feature_id = character())
} else {
  featureSets <- foreach(f = featureset_files, .combine = dplyr::bind_rows) %do% {
    data.table::fread(f) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(Features = purrr::map(Features, jsonlite::fromJSON),
                    TCGA_Projects = purrr::map(TCGA_Projects, jsonlite::fromJSON)) %>%
      tidyr::unnest(TCGA_Projects) %>%
      tidyr::unnest(Features) %>%
      dplyr::rename(featureset_id = Feature_Set_ID,
                    cancer_id = TCGA_Projects,
                    feature_id = Features)
  }
}

message("getting cancer list...")
cancers <- predictions %>% dplyr::pull(cancer_id) %>% unique()

message("connecting to feature database...")
if (file.exists("/mnt/data/features.sqlite")) {
  feature_con <- DBI::dbConnect(RSQLite::SQLite(), "/mnt/data/features.sqlite")
} else {
  feature_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  tbl <- dplyr::tibble(feature_id = character(),
                       sample_id = character(),
                       subtype = character(),
                       value = numeric())
  feat_lst = dplyr::tibble(feature_id = character())
  for (c in cancers) {
    DBI::dbWriteTable(feature_con, c, tbl)
    DBI::dbWriteTable(feature_con, sprintf("%s_features", c), feat_lst)
  }
}

message("done")
