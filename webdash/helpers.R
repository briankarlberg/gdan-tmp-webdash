library(dplyr)
library(stringr)
library(rlist)
library(gripql)

hclustfunc <- function(x, method = "complete", dmeth = "euclidean") {
  hclust(dist(x, method = dmeth), method = method)
}

getCancers <- function(graph_name = "gdan_tmp", grip_host = "localhost:8201") {
  gripql(grip_host) %>%
    graph(graph_name) %>%
    query() %>%
    V() %>%
    hasLabel("Cancer") %>%
    render("_gid") %>%
    execute() %>%
    unlist() %>%
    str_replace("Cancer:", "") %>%
    sort()
}

getFeatures <- function(graph_name = "gdan_tmp", grip_host = "localhost:8201") {
  gripql(grip_host) %>%
    graph(graph_name) %>%
    query() %>%
    V() %>%
    hasLabel("Feature") %>%
    render("_gid") %>%
    execute() %>%
    unlist() %>%
    str_replace("Feature:", "") %>%
    sort()
}

flatten_all <- function(results) {
  flatten_one <- function(result) {
    fres <- list.flatten(result) %>%
      list.subset(c("cancer_id", "model_id", "prediction_id",
                    "data.predicted_value", "data.actual_value",
                    "data.type", "data.repeat", "data.fold"))
    names(fres) <- str_replace(names(fres), "data.", "")
    fres %>% as_tibble()
  }
  res_df <- do.call(bind_rows, lapply(results, flatten_one))
  res_df
}

getPredictions <- function(cancer_id, graph_name = "gdan_tmp", grip_host = "localhost:8201") {
  results <- gripql(grip_host) %>%
    graph(graph_name) %>%
    query() %>%
    V(cancer_id) %>% hasLabel("Cancer") %>% as_("cancer") %>%
    out("models") %>% as_("model") %>%
    out("predictions") %>% as_("prediction") %>%
    render(list("cancer_id" = "$cancer._gid",
                "model_id" = "$model._gid",
                "prediction_id" = "$prediction._gid",
                "data" = "$prediction._data")) %>%
    execute()
  fres <- flatten_all(results)
  fres$sample_id <- sapply(fres$prediction_id,
                           function(x) { strsplit(x, ":")[[1]][7] },
                           USE.NAMES = F)
  fres$featureset_id <- sapply(fres$prediction_id,
                               function(x) { strsplit(x, ":")[[1]][2] },
                               USE.NAMES = F)
  fres %>%
    dplyr::mutate(
      cancer_id = str_replace(cancer_id,
                              "Cancer:",
                              ""),
      model_id = str_replace(model_id,
                             "Model:",
                             ""),
      prediction_id = str_replace(prediction_id,
                                  "Prediction:",
                                  ""),
      predicted_value = as.factor(
        str_replace(predicted_value,
                    "Subtype:",
                    "")
      ),
      actual_value = as.factor(
        str_replace(actual_value,
                    "Subtype:",
                    "")
      )
    )
}

getFeatureSets <- function(cancer_id, graph_name = "gdan_tmp", grip_host = "localhost:8201") {
  results <- gripql(grip_host) %>%
    graph(graph_name) %>%
    query() %>%
    V(cancer_id) %>% hasLabel("Cancer") %>% as_("c") %>%
    out("featuresets") %>% as_("fs") %>%
    out("models") %>% as_("m") %>%
    select("fs") %>%
    out("features") %>% as_("f") %>%
    render(list("cancer_id" = "$c._gid",
                "model_id" = "$m._gid",
                "featureset_id" = "$fs._gid",
                "feature_id" = "$f._gid")) %>%
    execute()

  fres <- do.call(bind_rows, lapply(results, as_tibble))
  fres %>%
    dplyr::mutate(
      cancer_id = str_replace(cancer_id,
                              "Cancer:",
                              ""),
      model_id = str_replace(model_id,
                             "Model:",
                             ""),
      featureset_id = str_replace(featureset_id,
                                  "FeatureSet:",
                                  ""),
      feature_id = str_replace(feature_id,
                               "Feature:",
                               "")
    )
}

within <- function(k, values) {
  if (length(values) == 0) {
    values <- list()
  }
  if (length(values) == 1) {
    values <- list(values)
  }
  list("condition" = list("key" = k, "value" = values, "condition" = "WITHIN"))
}

getFeatureVals <- function(features, graph_name = "gdan_tmp", grip_host = "localhost:8201") {
  selected_features <- paste("Feature", features, sep = ":")

  results <- gripql(grip_host) %>%
    graph(graph_name) %>%
    query() %>%
    E() %>%
    hasLabel("samples") %>%
    has(within("_from", selected_features)) %>%
    render(list("feature_id" = "$._from", "sample_id" = "$._to", "value" = "$._data.value")) %>%
    execute()

  fres <- do.call(bind_rows, lapply(results, as_tibble))
  fres %>%
    dplyr::mutate(
      sample_id = str_replace(sample_id,
                              "Sample:",
                              ""),
      feature_id = str_replace(feature_id,
                               "Feature:",
                               "")
    )
}
