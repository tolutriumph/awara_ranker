#!/usr/bin/env Rscript
# 03_A_screening.R - Stage A: screening of all 37 states, REAL data only.
# Usage: IN_DIR=inputs OUT_DIR=outputs Rscript 03_A_screening.R
# Assumptions (also written to assumptions_stageA rows in assumptions.csv by 03_BCDEF_pipeline.R):
#  A1 affordability = mean of the 0-100 scores of wealth_index and wealth_upper_share (brief gives one
#     affordability weight 0.4 for two criteria; equal split assumed).
#  A2 weights 0.4/0.4/0.2 re-normalised over AVAILABLE criteria; soy NA states scored on market+affordability only.
#  A3 min-max over the 37 states; NA stays NA (min/max taken over non-NA values).
#  A4 annual rainfall not scored (suspect for Kaduna/Plateau).
suppressPackageStartupMessages({library(dplyr); library(readr); library(ggplot2)})
set.seed(42)
in_dir <- Sys.getenv("IN_DIR", "inputs"); out_dir <- Sys.getenv("OUT_DIR", "outputs")
mm <- function(x) { r <- range(x, na.rm = TRUE); 100 * (x - r[1]) / (r[2] - r[1]) }

m <- read_csv(file.path(in_dir, "master_states_final.csv"), show_col_types = FALSE)
stopifnot(nrow(m) == 37)
W <- c(market = 0.4, affordability = 0.4, soy = 0.2)

sc <- m %>%
  transmute(state, zone, population, wealth_index, wealth_upper_share, soy_per_100k_people, soy_status_detail,
            s_log_population = mm(log(population)),
            s_wealth_index = mm(wealth_index),
            s_wealth_upper_share = mm(wealth_upper_share),
            s_soy_per_100k = mm(soy_per_100k_people),
            s_market = s_log_population,
            s_affordability = (s_wealth_index + s_wealth_upper_share) / 2) %>%
  mutate(w_avail = W["market"] + W["affordability"] + if_else(is.na(s_soy_per_100k), 0, W["soy"]),
         contrib_market = W["market"] / w_avail * s_market,
         contrib_affordability = W["affordability"] / w_avail * s_affordability,
         contrib_soy = if_else(is.na(s_soy_per_100k), NA_real_, W["soy"] / w_avail * s_soy_per_100k),
         screening_score = contrib_market + contrib_affordability + coalesce(contrib_soy, 0),
         criteria_used = if_else(is.na(s_soy_per_100k), "market+affordability (soy NA, weights renormalised to 0.5/0.5)",
                                 "market+affordability+soy"),
         # score if soy were scored 0 (worst case) - shows how much the renormalisation helps NA states
         screening_score_soy_as_0 = 0.4 * s_market + 0.4 * s_affordability + 0.2 * coalesce(s_soy_per_100k, 0)) %>%
  arrange(desc(screening_score)) %>%
  mutate(rank = row_number(),
         data_basis = "real: population WorldPop/NPC (ref year undocumented); wealth DHS 2024 state aggregates; soy NASS 2022/23 (Yobe derived residual); soy NA never zero-filled; rainfall not scored")
write_csv(sc, file.path(out_dir, "screening_37_states.csv"), na = "")

cat("STAGE A top 10:\n"); print(sc %>% select(rank, state, screening_score, s_market, s_affordability, s_soy_per_100k) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>% head(10), n = 10)
cat("soy NA states:", sum(is.na(sc$s_soy_per_100k)), "| Kwara soy/100k =", round(m$soy_per_100k_people[m$state == "Kwara"]),
    "vs median reported", round(median(m$soy_per_100k_people, na.rm = TRUE)), "(linear min-max: Kwara compresses all others)\n")

p <- ggplot(sc, aes(x = reorder(state, screening_score), y = screening_score, fill = is.na(s_soy_per_100k))) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c(`FALSE` = "#2a6f97", `TRUE` = "#b5838d"),
                    labels = c("soy scored", "soy NA (renormalised)"), name = NULL) +
  labs(x = NULL, y = "Screening score (0-100)",
       title = "Stage A: 37-state screening (market 0.4, affordability 0.4, soy 0.2)",
       subtitle = "Data basis: REAL (WorldPop/NPC, DHS 2024, NASS 2022/23). Rainfall not scored.") +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(out_dir, "chart_screening_37_states.png"), p, width = 7, height = 8, dpi = 150)
