model_selector <- function(model) {
  method <- list(list(""))
  if (model == "km") {
    method <- tmk
  }
  return(method)
}

build_data_to_predict <- function(sepal_length, sepal_width, petal_length, petal_width) {
  data.frame(sepal_length=sepal_length, sepal_width=sepal_width, petal_length=petal_length, petal_width=petal_width)
}

cluster_predict <- function(model, sepal_length, sepal_width, petal_length, petal_width) {
  method <- model_selector(model)
  checker <- method[[1]][[1]]
  if (checker == "") {
    return("")
  }
  data_to_predict <- build_data_to_predict(sepal_length, sepal_width, petal_length, petal_width)
  closest_cluster <- function(x) {
    cluster.dist <- apply(method$centers, 1, function(y) sqrt(sum((x-y)^2)))
    return(which.min(cluster.dist)[1])
  }
  label <- method$centers
  label <- apply(data_to_predict, 1, closest_cluster)
  return(label)
}