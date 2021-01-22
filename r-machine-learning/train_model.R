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