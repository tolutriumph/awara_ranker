# run_app.R - from the awara_ranker folder:  source("run_app.R")
if (!file.exists(file.path("app", "app.R")))
  stop("Run this from the awara_ranker folder. Your R session is in: ", getwd(),
       "\nFix: setwd(\"<path to awara_ranker>\") then source(\"run_app.R\")")

pk <- c("shiny", "bslib", "DT", "plotly", "dplyr", "tidyr", "readr", "jsonlite", "httr2", "digest",
        "cluster", "survival", "ggplot2", "purrr", "tibble", "stringr")
miss <- pk[!vapply(pk, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")

shiny::runApp("app", launch.browser = TRUE)
