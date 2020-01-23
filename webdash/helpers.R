require(gripql)
require(magrittr)

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
    stringr::str_replace("Cancer:", "") %>%
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
    stringr::str_replace("Feature:", "") %>%
    sort()
}

flatten_all <- function(results) {
  flatten_one <- function(result) {
    fres <- rlist::list.flatten(result) %>%
      rlist::list.subset(c("cancer_id", "model_id", "prediction_id",
                           "data.predicted_value", "data.actual_value",
                           "data.type", "data.repeat", "data.fold"))
    names(fres) <- stringr::str_replace(names(fres), "data.", "")
    fres %>% as_tibble()
  }
  res_df <- do.call(dplyr::bind_rows, lapply(results, flatten_one))
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
  fres %>%
    dplyr::mutate(
      cancer_id = stringr::str_replace(cancer_id,
                                       "Cancer:",
                                       ""),
      model_id = stringr::str_replace(model_id,
                                      "Model:",
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

  # TODO: flatten
  fres <- flatten_all(results)

  fres %>%
    dplyr::mutate(
      cancer_id = stringr::str_replace(cancer_id,
                                       "Cancer:",
                                       ""),
      model_id = stringr::str_replace(model_id,
                                      "Model:",
                                      ""),
      featureset_id = stringr::str_replace(featureset_id,
                                           "FeatureSet:",
                                           ""),
      feature_id = stringr::str_replace(feature_id,
                                        "Feature:",
                                        "")
    )
}

getFeatureVals <- function(features, graph_name = "gdan_tmp", grip_host = "localhost:8201") {
  results <- gripql(grip_host) %>%
    graph(graph_name) %>%
    query() %>%
    V(paste("Feature", features, sep = ":")) %>% as_("f") %>%
    outE("samples") %>% as_("val") %>%
    out() %>% as_("s") %>%
    out("subtype") %>% as_("type") %>%
    render(
      list("feature_id" = "$g._gid",
           "value" = "$val._data.value",
           "sample_id" = "$s._gid",
           "subtype_id" = "$type._gid")
    ) %>%
    execute()

  # TODO: flatten
  fres <- flatten_all(results)

  fres %>%
    dplyr::mutate(
      sample_id = stringr::str_replace(sample_id,
                                       "Sample:",
                                       ""),
      subtype_id = stringr::str_replace(subtype_id,
                                      "Subtype:",
                                      ""),
      feature_id = stringr::str_replace(feature_id,
                                        "Feature:",
                                        "")
    )
}
