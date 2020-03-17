suppressMessages({
  library(foreach)
  library(magrittr)
  library(doParallel)
})

cleanLabels <- function(labels) {
  stringr::str_replace(labels, "_", ":") %>%
    stringr::str_split(., ":", simplify = F) %>%
    sapply(., function(x) {
      if (length(x) > 2) {
        x <- x[2:3]
      }
      paste(x, collapse = ":")
    })
}

cl <- makeCluster(4, outfile = "")
registerDoParallel(cl)

message("loading prediction files...")
output_files <- list.files("/mnt/data/predictions", recursive = T, full.names = T) %>% sort()
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
  predictions <- foreach(f = output_files, .combine = dplyr::bind_rows,
                         .multicombine = T, .maxcombine = 10,
                         .packages = c("magrittr")) %dopar% {
    message("loading prediction file: ", f)
    sep <- ifelse(endsWith(f, ".csv"), ",", "\t")
    tryCatch({
      suppressMessages({
        tmp <- readr::read_delim(f, comment = "#", delim = sep)
      })
      colnames(tmp) <- c("Sample_ID", "Repeat", "Fold", "Test", "Label",
                         colnames(tmp)[6:dim(tmp)[2]])
      tmp <- tmp %>%
        tidyr::gather(-Sample_ID, -Repeat, -Fold, -Test, -Label,
                      key = "prediction_id", value = "predicted_value") %>%
        dplyr::rename(sample_id = Sample_ID, `repeat` = Repeat,
                      fold = Fold, actual_value = Label, type = Test) %>%
        tidyr::separate(prediction_id, sep = "\\|",
                        into = c("model_id", "featureset_id", "date", "ptype"),
                        remove = F)

      ptype <- tmp %>% head(1) %>% dplyr::pull(ptype)
      if (ptype == "p") {
        tmp$predicted_value <- lapply(tmp$predicted_value, function(x) {
          jsonlite::parse_json(x)$classification %>% which.max() %>% names()
        }) %>% cleanLabels()
      } else {
        tmp$predicted_value <- cleanLabels(tmp$predicted_value)
      }

      tmp <- tmp %>%
        dplyr::mutate(actual_value = cleanLabels(actual_value),
                      type = ifelse(type == 1, "testing", "training")) %>%
        tidyr::separate(actual_value, sep = ":",
                        into = c("cancer_id", "extra"),
                        remove = F) %>%
        dplyr::select(-extra, -ptype)

      dtypes <- lapply(tmp, class)
      if (dtypes$`repeat` == "character") {
        tmp <- tmp %>% mutate(`repeat` = as.integer(stringr::str_replace(`repeat`, "R", "")))
      }
      if (dtypes$fold == "character") {
        tmp <- tmp %>% mutate(fold = as.integer(stringr::str_replace(fold, "F", "")))
      }
      try({
        tmp$date <- as.Date(parsedate::parse_iso_8601(tmp$date))
      })
      if (class(tmp$date) == "character") {
        tmp$date <- as.Date(NA)
      }
      tmp
    },
    error = function(e) {
      message(e, ": ", f)
      NULL
    })
  } %>%
    dplyr::mutate(predicted_value = as.factor(predicted_value),
                  actual_value = as.factor(actual_value))
}

message("loading feature set files...")
featureset_files <- list.files("/mnt/data/feature-sets", recursive = T, full.names = T) %>% sort()
if (length(featureset_files) == 0) {
  message("no feature set files were found!")
  featureSets <- dplyr::tibble(
    featureset_id = character(),
    cancer_id = character(),
    feature_id = character())
} else {
  featureSets <- foreach(f = featureset_files, .combine = dplyr::bind_rows,
                         .multicombine = T, .maxcombine = 10,
                         .packages = c("magrittr")) %dopar% {
    message("loading feature set file: ", f)
    sep <- ifelse(endsWith(f, ".csv"), ",", "\t")
    suppressMessages({
      tmp <- readr::read_delim(f, comment = "#", delim = sep)
    })
    if (!all(colnames(tmp) == c("Feature_Set_ID", "TCGA_Projects", "Features"))) {
      message("ERROR: incorrect header for: ", f)
      return(NULL)
    }
    tmp %>%
      tidyr::replace_na(list(TCGA_Projects = "[]", Features = "[]")) %>%
      dplyr::mutate(Features = purrr::map(Features, jsonlite::fromJSON),
                    TCGA_Projects = purrr::map(TCGA_Projects, jsonlite::fromJSON)) %>%
      tidyr::unnest(TCGA_Projects) %>%
      tidyr::unnest(Features) %>%
      dplyr::rename(featureset_id = Feature_Set_ID,
                    cancer_id = TCGA_Projects,
                    feature_id = Features) %>%
      dplyr::mutate(feature_id = as.character(feature_id),
                    cancer_id = as.character(cancer_id))
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

stopCluster(cl)

message("done")
