library("logger")

# classification
train_model_classification <- function () {
  source("modelling/classification/models.R")
  lda <- lda()
  cart <- cart()
  knn <- knn()
  svm <- svm()
  rf <- rf()
  log_info("Classification model has train and load!")
  return(list(lda, cart, knn, svm, rf))
}

# clustering
train_model_clustering <- function () {
  source("modelling/clustering/models.R")
  km <- km()
  log_info("Clustering model has train and load!")
  return(km)
}

# regression
train_model_regression <- function () {
  source("modelling/regression/models.R")
  lasso <- lasso()
  ridge <- ridge()
  log_info("Regression model has train and load!")
  return(list(lasso, ridge))
}