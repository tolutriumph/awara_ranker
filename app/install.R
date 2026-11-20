# install.R - run once on each laptop:  Rscript install.R
pk <- c("shiny", "bslib", "DT", "plotly", "dplyr", "tidyr", "readr", "jsonlite", "httr2", "digest",
        "cluster", "survival", "ggplot2", "purrr", "tibble", "stringr")
miss <- pk[!vapply(pk, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")
cat("All packages ready:", all(vapply(pk, requireNamespace, logical(1), quietly = TRUE)), "\n")
