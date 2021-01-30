model_selector <- function(model, attribute) {
  method <- list("")
  if (model == "lasso") {
    method <- tmr[[1]]
  } else if (model == "ridge") {
    method <- tmr[[2]]
  } else {
    return(method)
  }
  if (attribute == "sl") {
    method <- method[[1]]
  } else if (attribute == "sw") {
    method <- method[[2]]
  } else if (attribute == "pl") {
    method <- method[[3]]
  } else if (attribute == "pw") {
    method <- method[[4]]
  }
  return(method)
}

build_data_to_predict <- function(sepal_length=NULL, sepal_width=NULL, petal_length=NULL, petal_width=NULL, species_id) {
  if (is.null(sepal_length)) {
    df <- data.frame(sepal_width=sepal_width, petal_length=petal_length, petal_width=petal_width)
  } else if (is.null(sepal_width)) {
    df <- data.frame(sepal_length=sepal_length, petal_length=petal_length, petal_width=petal_width)
  } else if (is.null(petal_length)) {
    df <- data.frame(sepal_length=sepal_length, sepal_width=sepal_width, petal_width=petal_width)
  } else if (is.null(petal_width)) {
    df <- data.frame(sepal_length=sepal_length, sepal_width=sepal_width, petal_length=petal_length)
  }
  if (species_id == 1) {
    df$species_1 <- 1
    df$species_2 <- 0
    df$species_3 <- 0
  } else if (species_id == 2) {
    df$species_1 <- 0
    df$species_2 <- 1
    df$species_3 <- 0
  } else if (species_id == 3) {
    df$species_1 <- 0
    df$species_2 <- 0
    df$species_3 <- 1
  } else {
    df$species_1 <- 0
    df$species_2 <- 0
    df$species_3 <- 0
  }
  return(df)
}

sepal_length_predict <- function(model, sepal_width, petal_length, petal_width, species_id) {
  method <- model_selector(model, attribute="sl")
  checker <- method[[1]]
  if (checker == "") {
    return("")
  }
  data_to_predict <- build_data_to_predict(sepal_width=sepal_width, petal_length=petal_length, petal_width=petal_width, species_id=species_id)
  value <- predict(method, data_to_predict)
  return(value)
}

sepal_width_predict <- function(model, sepal_length, petal_length, petal_width, species_id) {
  method <- model_selector(model, attribute="sw")
  checker <- method[[1]]
  if (checker == "") {
    return("")
  }
  data_to_predict <- build_data_to_predict(sepal_length=sepal_length, petal_length=petal_length, petal_width=petal_width, species_id=species_id)
  value <- predict(method, data_to_predict)
  return(value)
}

petal_length_predict <- function(model, sepal_length, sepal_width, petal_width, species_id) {
  method <- model_selector(model, attribute="pl")
  checker <- method[[1]]
  if (checker == "") {
    return("")
  }
  data_to_predict <- build_data_to_predict(sepal_length=sepal_length, sepal_width=sepal_width, petal_width=petal_width, species_id=species_id)
  value <- predict(method, data_to_predict)
  return(value)
}

petal_width_predict <- function(model, sepal_length, sepal_width, petal_length, species_id) {
  method <- model_selector(model, attribute="pw")
  checker <- method[[1]]
  if (checker == "") {
    return("")
  }
  data_to_predict <- build_data_to_predict(sepal_length=sepal_length, sepal_width=sepal_width, petal_length=petal_length, species_id=species_id)
  value <- predict(method, data_to_predict)
  return(value)
}