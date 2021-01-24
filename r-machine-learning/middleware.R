#* @get /ping
middleware_resp <- function() {
  return("PONG")
}

#* @get /method/checker
method_checker <- function(method) {
  list.method.classification <- c("lda", "cart", "knn", "svm", "rf")
  list.method.clustering <- c("km")
  if (method %in% list.method.classification) {
    return(paste(as.character(method), "is CLASSIFICATION method, can be used on /predict/species API"))
  } else if (method %in% list.method.clustering) {
    return(paste(as.character(method), "is CLUSTERING method, can be used on /predict/cluster API"))
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

#* @get /predict/cluster
predict_cluster <- function(method="km", sepal_length=5.0, sepal_width=3.0, petal_length=1.5, petal_width=0.5) {
  source("../modelling/clustering/predicts.R")
  result <- cluster_predict(method, as.numeric(sepal_length), as.numeric(sepal_width), as.numeric(petal_length), as.numeric(petal_width))
  if (result == "") {
    return(list(message = "method not found, please input km"))
  } else {
    return(list(cluster = unlist(result)))
  }
}