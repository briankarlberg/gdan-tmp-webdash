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

getPredictions <- function(cancer_id, graph_name = "gdan_tmp", grip_host = "localhost:8201") {
  results <- gripql(grip_host) %>%
    graph(graph_name) %>%
    query() %>%
    V(cancer_id) %>%
    out("models") %>% as_("model") %>%
    out("predictions") %>% as_("prediction") %>%
    render(list("model_id" = "$model._gid",
                "prediction_id" = "$prediction._gid",
                "data" = "$prediction._data")) %>%
    execute()
  fres <- flatten_all(results)
  fres$sample_id <- sapply(fres$prediction_id,
                           function(x) { strsplit(x, ":")[[1]][7] },
                           USE.NAMES = F)
  fres %>%
    dplyr::mutate(
      model_id = stringr::str_replace(model_id,
                                      sprintf("Model:%s:", cancer_id),
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

flatten_all <- function(results) {
  flatten_one <- function(result) {
    fres <- rlist::list.flatten(result) %>%
      rlist::list.subset(c("model_id", "prediction_id",
                           "data.predicted_value", "data.actual_value",
                           "data.type", "data.repeat", "data.fold"))
    names(fres) <- stringr::str_replace(names(fres), "data.", "")
    fres %>% as_tibble()
  }
  res_df <- do.call(dplyr::bind_rows, lapply(results, flatten_one))
  res_df
}
