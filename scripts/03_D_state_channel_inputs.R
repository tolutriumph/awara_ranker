#!/usr/bin/env Rscript
# 03_D_state_channel_inputs.R - survey-INDEPENDENT parts of Stage D/E for the 10 pilot states:
#   state x channel table (shelf price, margin base/stress, soy, channel readiness) and launch_timing.csv.
# Usage: IN_DIR=inputs OUT_DIR=outputs Rscript 03_D_state_channel_inputs.R
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})
set.seed(42)
in_dir <- Sys.getenv("IN_DIR", "inputs"); out_dir <- Sys.getenv("OUT_DIR", "outputs")
rd <- function(f) read_csv(file.path(in_dir, f), show_col_types = FALSE)

m  <- rd("master_states_final.csv"); ps <- rd("pilot_states.csv"); ue <- rd("unit_economics_v1b.csv")
cl <- rd("channel_density_long.csv"); sea <- rd("seasonality_month.csv")

# Survey channel -> channel-density channel (brief). gym_health_store is a FLAGGED proxy.
chan_map <- tribble(~channel, ~density_channel, ~proxy_flag,
  "street_market", "street_market", FALSE,
  "supermarket", "supermarket", FALSE,
  "campus_shop", "campus_institution", FALSE,
  "delivery_app", "delivery_app", FALSE,
  "restaurant", "restaurant", FALSE,          # added by user decision (75 respondents prefer it; density exists)
  "gym_health_store", "supermarket", TRUE)
write_csv(chan_map, file.path(out_dir, "channel_mapping.csv"))

fsp <- ue %>% filter(format == "fried_seasoned_pack") %>%
  transmute(state, shelf_price_low, shelf_price_base, shelf_price_high,
            reported_total_unit_cost_low, reported_total_unit_cost_base, reported_total_unit_cost_high,
            total_unit_cost_base_recomputed = total_unit_cost_base,
            retail_margin_pct_base, producer_margin_pct_base, producer_margin_pct_stress,
            # check of the margin formula: net = price*(1-retail margin) - REPORTED total; spoilage not used
            margin_check_pct = 100 * (shelf_price_base * (1 - retail_margin_pct_base / 100) - reported_total_unit_cost_base) / shelf_price_base,
            margin_if_recomputed_cost_pct = 100 * (shelf_price_base * (1 - retail_margin_pct_base / 100) - total_unit_cost_base) / shelf_price_base)
stopifnot(setequal(fsp$state, ps$state), all(abs(fsp$margin_check_pct - fsp$producer_margin_pct_base) < 0.15))

sc <- ps %>% select(state) %>% crossing(chan_map) %>%
  left_join(cl %>% select(state, density_channel = channel, channel_density_value = value, channel_density_unit = unit),
            by = c("state", "density_channel")) %>%
  left_join(m %>% select(state, population, soy_per_100k_people, soy_status_detail), by = "state") %>%
  left_join(fsp, by = "state") %>%
  # channel readiness is in different units per channel (outlets/100k vs 0-1 index): scale WITHIN channel first
  group_by(density_channel) %>%
  mutate(channel_readiness_within = 100 * (channel_density_value - min(channel_density_value)) /
           (max(channel_density_value) - min(channel_density_value))) %>% ungroup() %>%
  mutate(data_basis = "shelf price/margin: synthetic (unit_economics_v1b, margin on REPORTED totals); channel: synthetic, collinear with wealth; soy: real NASS (NA in 5 pilot states); population: real")
stopifnot(!anyNA(sc$channel_density_value))
write_csv(sc, file.path(out_dir, "state_channel_inputs.csv"), na = "")
unmapped <- setdiff(unique(cl$channel), chan_map$density_channel)
cat("density channels with no survey channel mapped to them:", paste(unmapped, collapse = ", "), "\n")
cat("margin base (reported totals) vs if recomputed totals used, fried_seasoned_pack:\n")
print(fsp %>% transmute(state, shelf_price_base, margin_base = producer_margin_pct_base,
                        margin_recomputed = round(margin_if_recomputed_cost_pct, 1), margin_stress = producer_margin_pct_stress))

# Launch timing: top 3 months by seasonality_index per pilot state; ties at the 3rd place reported, not broken
lt <- sea %>% filter(scope == "state") %>%
  group_by(state) %>% arrange(desc(seasonality_index), match(month, month.abb), .by_group = TRUE) %>%
  mutate(third = sort(seasonality_index, decreasing = TRUE)[3]) %>%
  summarise(best_3_months = paste(month[1:3], collapse = "|"),
            best_3_index = paste(round(seasonality_index[1:3], 2), collapse = "|"),
            months_tied_at_3rd = paste(month[seasonality_index == first(third)], collapse = "|"),
            n_tied_at_3rd = sum(seasonality_index == first(third)),
            n_vendors = first(n_vendors), .groups = "drop") %>%
  mutate(tie_flag = n_tied_at_3rd > 1 & !(best_3_months == months_tied_at_3rd),
         note = "months where most vendors report peak sales; tie order = calendar order (arbitrary); n = 3-6 vendors, indicative only",
         data_basis = "synthetic (vendor peak_months, seasonality_month.csv)")
write_csv(lt, file.path(out_dir, "launch_timing.csv"))
print(lt %>% select(state, best_3_months, months_tied_at_3rd, n_vendors))
