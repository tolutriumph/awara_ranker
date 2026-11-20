#!/usr/bin/env Rscript
# sync_data.R - copy the model outputs (03_* scripts) and the seasonality input into the app's data/ folder.
# Usage (from the app folder): OUT_DIR=../outputs IN_DIR=../inputs Rscript sync_data.R
out_dir <- Sys.getenv("OUT_DIR", "../outputs"); in_dir <- Sys.getenv("IN_DIR", "../inputs")
need_out <- c("ranked_opportunities.csv", "opportunity_contributions.csv", "mc_criteria_draws.rds", "screening_37_states.csv",
              "launch_timing.csv", "segment_profiles.csv", "segment_share_by_state.csv", "choice_model_coefficients.csv",
              "segment_validation_confusion.csv", "assumptions.csv", "criteria_correlation.csv", "sensitivity.csv",
              "llm_input_top10.json", "state_channel_inputs.csv", "rank_stability.csv", "price_model.rds")
need_in <- c("seasonality_month.csv")
dir.create("data", showWarnings = FALSE)
miss <- c(need_out[!file.exists(file.path(out_dir, need_out))], need_in[!file.exists(file.path(in_dir, need_in))])
if (length(miss)) stop("Missing: ", paste(miss, collapse = ", "))
file.copy(file.path(out_dir, need_out), "data", overwrite = TRUE)
file.copy(file.path(in_dir, need_in), "data", overwrite = TRUE)
cat("Copied", length(need_out) + length(need_in), "files into data/\n")
