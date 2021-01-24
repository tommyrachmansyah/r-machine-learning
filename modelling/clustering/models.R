source("modelling/training_data.R")

clustering_data_training <- get_data(method="clustering")
number_of_cluster = 3

km <- function() {
  fit.km <- kmeans(clustering_data_training, number_of_cluster)
  return(fit.km)
}