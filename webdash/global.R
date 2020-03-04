##-------------------------
## Global variables
##-------------------------
DEFAULT_GRAPH <- ifelse(Sys.getenv("GDAN_TMP_GRAPH") == "", "gdan_tmp", Sys.getenv("GDAN_TMP_GRAPH"))
DEFAULT_GRIP_HOST <- ifelse(Sys.getenv("GRIP_HOST") == "", "localhost:8201", Sys.getenv("GRIP_HOST"))

message("GRIP HOST: ", DEFAULT_GRIP_HOST)
message("GRAPH: ", DEFAULT_GRAPH)

hclustfunc <- function(x, method = "complete", dmeth = "euclidean") {
  hclust(dist(x, method = dmeth), method = method)
}

# source("helpers.R")
# cancers <- getCancers()
# features <- getFeatures()
# featureSets <- getFeatureSets(NULL)
# predictions <- getPredictions(NULL)

source("load_data.R")
# creates variables:
# predictions
# featureSets
# feature_con
# cancers
#
suppressMessages({
  if (dim(predictions)[1] > 0) {
    model_summary <- dplyr::left_join(
      dplyr::left_join(
        predictions %>%
          dplyr::group_by(cancer_id, model_id, featureset_id, type) %>%
          dplyr::summarize(correct = table(as.numeric(predicted_value) == as.numeric(actual_value))["TRUE"],
                           total = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(tpr = round(correct / total, digits = 3)) %>%
          dplyr::select(cancer_id, model_id, featureset_id, type, tpr) %>%
          tidyr::spread(type, tpr),
        predictions %>%
          dplyr::select(cancer_id, model_id, featureset_id, date, prediction_id) %>%
          dplyr::distinct()
      ) %>%
        dplyr::rename(Project = cancer_id, Model = model_id, Features = featureset_id,
                      Date = date, TPR_Training = training, TPR_Testing = testing),
      featureSets %>%
        dplyr::group_by(featureset_id, cancer_id) %>%
        dplyr::summarize(N_Features = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::rename(Features = featureset_id, Project = cancer_id)
    ) %>%
      dplyr::group_by(Project) %>%
      dplyr::arrange(desc(TPR_Testing)) %>%
      dplyr::mutate(Model_Rank = dplyr::row_number()) %>%
      dplyr::arrange(Model_Rank, desc(TPR_Testing)) %>%
      dplyr::ungroup()
  } else {
    model_summary = dplyr::tibble(
      Project = character(),
      Model = character(),
      Features = character(),
      Date = date(),
      prediction_id = character(),
      TPR_Training = numeric(),
      TPR_Testing = numeric(),
      N_Features = numeric(),
      Model_Rank = numeric()
    )
  }
})

selected_models <- which(paste(model_summary$Project, model_summary$Model_Rank, sep = "|") %in% paste(cancers, 1,  sep = "|"))

message("selected models: ", paste(selected_models, collapse = ", "))
