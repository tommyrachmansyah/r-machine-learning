library("caret")

source("modelling/training_data.R")

regression_data_training <- get_data(method="regression")
train_control <- trainControl(method="cv", number=10)

lasso <- function() {
  fit.lasso.sl <- train(sepal_length~., data=regression_data_training, method="lasso", trControl=train_control)
  fit.lasso.sw <- train(sepal_width~., data=regression_data_training, method="lasso", trControl=train_control)
  fit.lasso.pl <- train(petal_length~., data=regression_data_training, method="lasso", trControl=train_control)
  fit.lasso.pw <- train(petal_width~., data=regression_data_training, method="lasso", trControl=train_control)
  return(list(fit.lasso.sl, fit.lasso.sw, fit.lasso.pl, fit.lasso.pw))
}

ridge <- function() {
  fit.ridge.sl <- train(sepal_length~., data=regression_data_training, method="ridge", trControl=train_control)
  fit.ridge.sw <- train(sepal_width~., data=regression_data_training, method="ridge", trControl=train_control)
  fit.ridge.pl <- train(petal_length~., data=regression_data_training, method="ridge", trControl=train_control)
  fit.ridge.pw <- train(petal_width~., data=regression_data_training, method="ridge", trControl=train_control)
  return(list(fit.ridge.sl, fit.ridge.sw, fit.ridge.pl, fit.ridge.pw))
}