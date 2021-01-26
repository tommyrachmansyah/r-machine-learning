library("fastDummies")

get_data <- function(method) {
  filename <- "data/iris.csv"
  dataset <- read.csv(filename)
  # length and width in cm
  colnames(dataset) <- c("id", "sepal_length", "sepal_width", "petal_length", "petal_width", "species")
  if (method == "classification") {
    dataset <- dataset[-c(1)]
  } else if (method == "clustering") {
    dataset <- dataset[-c(1,6)]
  } else if (method == "regression") {
    dataset[] <- data.matrix(dataset)
    dataset <- dummy_cols(dataset, select_columns="species")
    dataset <- dataset[-c(1,6)]
  }
  return(dataset)
}