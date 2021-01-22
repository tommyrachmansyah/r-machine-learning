#* @get /ping
middleware_resp <- function() {
  return("PONG")
}

#* @get /method/checker
method_checker <- function(method) {
  list.method <- c("lda", "cart", "knn", "svm", "rf")
  if (method %in% list.method) {
    checker <- 1
  } else {
    checker <- 0
  }
  if (checker == 1) {
    return(paste(as.character(method), "method is EXIST"))
  } else {
    return(paste(as.character(method), "method is NOT EXIST"))
  }
}

#* @get /predict/species
predict_species <- function(method="lda", sepal_length=5.0, sepal_width=3.0, petal_length=1.5, petal_width=0.5) {
  source("../modelling/classification/predicts.R")
  result <- species_predict(method, as.numeric(sepal_length), as.numeric(sepal_width), as.numeric(petal_length), as.numeric(petal_width))
  if (result == "") {
    return(list(message = "method not found, please input between lda, cart, knn, svm, or rf"))
  }
  return(list(species = unlist(result)))
}