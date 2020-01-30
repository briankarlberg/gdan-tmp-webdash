library(foreach)
library(magrittr)

message("loading prediction files...")
output_files <- list.files("/mnt/data/predictions", full.names = T)
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

message("loading feature set files...")
feature_sets_file <- "/mnt/data/feature-sets/featuresets_struck.tsv"
featureSets <- data.table::fread(feature_sets_file) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(Features = purrr::map(Features, jsonlite::fromJSON),
         TCGA_Projects = purrr::map(TCGA_Projects, jsonlite::fromJSON)) %>%
  tidyr::unnest(TCGA_Projects) %>%
  tidyr::unnest(Features) %>%
  dplyr::rename(featureset_id = Feature_Set_ID,
                cancer_id = TCGA_Projects,
                feature_id = Features)

## feature_files <- list.files("/mnt/data/v7-matrices/", ".tsv", full.names = T)
## message("loading feature matrices...")
## feature_vals <- foreach(f = feature_files[1], .combine = dplyr::bind_rows) %do% {
##   message(sprintf("loading %s", f))
##   data.table::fread(f) %>%
##     dplyr::as_tibble() %>%
##     dplyr::rename(Sample_ID = 1) %>%
##     dplyr::select(-Labels) %>%
##     tidyr::gather(-Sample_ID, key = "feature_id", value = "value")
## } %>%
##   dplyr::rename(sample_id = Sample_ID)

## features <- feature_vals %>% dplyr::pull(feature_id) %>% unique()

message("connecting to feature database...")
feature_con <- DBI::dbConnect(RSQLite::SQLite(), "/mnt/data/features_by_cancer.sqlite")

message("getting cancer list...")
cancers <- predictions %>% dplyr::pull(cancer_id) %>% unique()

message("done")
