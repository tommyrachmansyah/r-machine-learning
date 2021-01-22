get_data <- function(method) {
  filename <- "iris.csv"
  dataset <- read.csv(filename)
  # length and width in cm
  colnames(dataset) <- c("id", "sepal_length", "sepal_width", "petal_length", "petal_width", "species")
  if (method == "classification" || method == "regression") {
    dataset <- dataset[-c(1)]
  } else if (method == "clustering") {
    dataset <- dataset[-c(1,6)]
  }
  return(dataset)
}