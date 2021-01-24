library("plumber")
library("logger")
library("tictoc")

source("r-machine-learning/train_model.R")
source("settings.R")

tmc <<- train_model_classification()
tmk <<- train_model_clustering()
route <- "r-machine-learning/middleware.R"

convert_empty <- function(string) {
  if (string == "") {
    "-"
  } else {
    string
  }
}

addr <- '{convert_empty(req$REMOTE_ADDR)}'
agent <- '"{convert_empty(req$HTTP_USER_AGENT)}"'
http_host <- '{convert_empty(req$HTTP_HOST)}'
request_method <- '{convert_empty(req$REQUEST_METHOD)}'
path_info <- '{convert_empty(req$PATH_INFO)}'
res_status <- '{convert_empty(res$status)}'
running_time <- '{round(end$toc - end$tic, digits = getOption("digits", 5))}'

run_app <- function() {
  log_info("App run!")
  root <- plumb(route)
  root$registerHooks(
    list(
      preroute = function() {
        tic()
      },
      postroute = function(req, res) {
        end <- toc(quiet=TRUE)
        log_info(paste(addr, agent, http_host, request_method, path_info, res_status, running_time, sep=" "))
      }
    )
  )
  root$run(host=basic_host, port=basic_port)
}