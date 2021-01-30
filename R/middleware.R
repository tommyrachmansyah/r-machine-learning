#* @serializer unboxedJSON
#* @get /ping
middleware_resp <- function() {
  return("PONG")
}

#* @serializer unboxedJSON
#* @get /method/checker
method_checker <- function(method) {
  list.method.classification <- c("lda", "cart", "knn", "svm", "rf")
  list.method.clustering <- c("km")
  list.method.regression <- c("lasso", "ridge")
  if (method %in% list.method.classification) {
    return(list(message = paste(as.character(method), "is CLASSIFICATION method, can be used on /predict/species API")))
  } else if (method %in% list.method.clustering) {
    return(list(message = paste(as.character(method), "is CLUSTERING method, can be used on /predict/cluster API")))
  } else if (method %in% list.method.regression) {
    return(list(message = paste(as.character(method), "is REGRESSION method, can be used on /predict/{sepal_length||sepal_width||petal_length||petal_width} API")))
  } else {
    return(list(message = paste(as.character(method), "method is NOT EXIST")))
  }
}

#* @serializer unboxedJSON
#* @get /species
species <- function() {
  species_id <- c(1,2,3)
  species_name <- c("Iris-setosa", "Iris-versicolor", "Iris-virginica")
  df <- data.frame(species_id, species_name)
  return(list(species = df))
}

#* @serializer unboxedJSON
#* @get /predict/species
predict_species <- function(method="lda", sepal_length=5.0, sepal_width=3.0, petal_length=1.5, petal_width=0.5) {
  source("../modelling/classification/predicts.R")
  result <- species_predict(method, as.numeric(sepal_length), as.numeric(sepal_width), as.numeric(petal_length), as.numeric(petal_width))
  if (result == "") {
    return(list(message = "method not found, please input between lda, cart, knn, svm, or rf"))
  }
  return(list(species = unlist(result)))
}

#* @serializer unboxedJSON
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

#* @serializer unboxedJSON
#* @get /predict/sepal-length
predict_sepal_length <- function(method="lasso", sepal_width=3.0, petal_length=1.5, petal_width=0.5, species_id=1) {
  source("../modelling/regression/predicts.R")
  result <- sepal_length_predict(method, as.numeric(sepal_width), as.numeric(petal_length), as.numeric(petal_width), as.numeric(species_id))
  if (result == "") {
    return(list(message = "method not found, please input between lasso or ridge"))
  }
  return(list(sepal_length_cm = unlist(result)))
}

#* @serializer unboxedJSON
#* @get /predict/sepal-width
predict_sepal_width <- function(method="lasso", sepal_length=5.0, petal_length=1.5, petal_width=0.5, species_id=1) {
  source("../modelling/regression/predicts.R")
  result <- sepal_width_predict(method, as.numeric(sepal_length), as.numeric(petal_length), as.numeric(petal_width), as.numeric(species_id))
  if (result == "") {
    return(list(message = "method not found, please input between lasso or ridge"))
  }
  return(list(sepal_width_cm  = unlist(result)))
}

#* @serializer unboxedJSON
#* @get /predict/petal-length
predict_petal_length <- function(method="lasso", sepal_length=5.0, sepal_width=3.0, petal_width=0.5, species_id=1) {
  source("../modelling/regression/predicts.R")
  result <- petal_length_predict(method, as.numeric(sepal_length), as.numeric(sepal_width), as.numeric(petal_width), as.numeric(species_id))
  if (result == "") {
    return(list(message = "method not found, please input between lasso or ridge"))
  }
  return(list(petal_length_cm  = unlist(result)))
}

#* @serializer unboxedJSON
#* @get /predict/petal-width
predict_petal_width <- function(method="lasso", sepal_length=5.0, sepal_width=3.0, petal_length=1.5, species_id=1) {
  source("../modelling/regression/predicts.R")
  result <- petal_width_predict(method, as.numeric(sepal_length), as.numeric(sepal_width), as.numeric(petal_length), as.numeric(species_id))
  if (result == "") {
    return(list(message = "method not found, please input between lasso or ridge"))
  }
  return(list(petal_width_cm  = unlist(result)))
}