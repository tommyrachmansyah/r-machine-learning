library("clue")

model_selector <- function(model) {
  method <- ""
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
  if (method == "") {
    return("")
  }
  data_to_predict <- build_data_to_predict(sepal_length, sepal_width, petal_length, petal_width)
  label <- method$cluster
  # label <- cl_predict(method, data_to_predict)
  return(label)
}