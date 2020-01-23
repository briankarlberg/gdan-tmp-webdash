cancers <- c("ACC", "BRCA")

acc_preds <- readr::read_csv("/Users/strucka/Projects/gdan-tmp-webdash/webdash/acc_predictions.csv") %>%
  dplyr::mutate(
    cancer_id = "ACC",
    model_id = stringr::str_replace(model_id,
                                    sprintf("Model:%s:", "ACC"),
                                    ""),
    predicted_value = as.factor(
      stringr::str_replace(predicted_value,
                           "Subtype:",
                           "")
    ),
    actual_value = as.factor(
      stringr::str_replace(actual_value,
                           "Subtype:",
                           "")
    )
  )

acc_feature_set <- readr::read_delim("/Users/strucka/Projects/gdan-tmp-webdash/fbed-tests/ACC.tsv",
                                     delim = "\t",
                                     col_names = c("featureset_id", "feature_id")) %>%
  mutate(feature_id = purrr::map(feature_id, jsonlite::fromJSON)) %>%
  tidyr::unnest(feature_id) %>%
  mutate(cancer_id = "ACC",
         model_id = NA)

features <- unique(acc_feature_set$feature_id)

acc_feature_vals <- data.table::fread("/Users/strucka/Projects/gdan-tmp-webdash/data/v7-matrices/ACC_v7_20191227.tsv") %>%
  as_tibble() %>%
  tidyr::gather(-ACC, -Labels, key = "feature_id", value = "value") %>%
  rename(sample_id = ACC) %>%
  mutate(subtype_id = as.factor(paste("ACC", Labels, sep=":"))) %>%
  select(-Labels, -subtype_id)

