library("caret")

source("modelling/training_data.R")

classification_data_training <- get_data(method="classification")
train_control <- trainControl(method="cv", number=10)

lda <- function() {
  fit.lda <- train(species~., data=classification_data_training, method="lda", trControl=train_control)
  return(fit.lda)
}

cart <- function() {
  fit.cart <- train(species~., data=classification_data_training, method="rpart", trControl=train_control)
  return(fit.cart)
}

knn <- function() {
  fit.knn <- train(species~., data=classification_data_training, method="knn", trControl=train_control)
  return(fit.knn)
}

svm <- function() {
  fit.svm <- train(species~., data=classification_data_training, method="svmRadial", trControl=train_control)
  return(fit.svm)
}

rf <- function() {
  fit.rf <- train(species~., data=classification_data_training, method="rf", trControl=train_control)
  return(fit.rf)
}