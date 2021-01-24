model_selector <- function(model) {
  method <- ""
  if (model == "lda") {
    method <- tmc[1]
  } else if (model == "cart") {
    method <- tmc[2]
  } else if (model == "knn") {
    method <- tmc[3]
  } else if (model == "svm") {
    method <- tmc[4]
  } else if (model == "rf") {
    method <- tmc[5]
  }
  return(method)
}

build_data_to_predict <- function(sepal_length, sepal_width, petal_length, petal_width) {
  data.frame(sepal_length=sepal_length, sepal_width=sepal_width, petal_length=petal_length, petal_width=petal_width)
}

species_predict <- function(model, sepal_length, sepal_width, petal_length, petal_width) {
  method <- model_selector(model)
  if (method == "") {
    return("")
  }
  data_to_predict <- build_data_to_predict(sepal_length, sepal_width, petal_length, petal_width)
  label <- predict(method, data_to_predict)
  return(label)
}