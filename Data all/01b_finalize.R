#!/usr/bin/env Rscript
# =============================================================================
# 01b_finalize.R  -  Hack the Fork (Nigeria, fried awara 150 g): FINAL DATA PACKAGE
# No modelling, no scoring, no imputation. Every change is logged in data_dictionary.csv.
# Usage:  IN_DIR=<folder with inputs> OUT_DIR=<outputs folder> Rscript 01b_finalize.R
# set.seed(42) is set for reproducibility; no random numbers are drawn.
# Inputs that are not present are SKIPPED and reported, never recreated or invented.
# =============================================================================
suppressPackageStartupMessages(library(tidyverse))
set.seed(42)
in_dir  <- Sys.getenv("IN_DIR",  ".")
out_dir <- Sys.getenv("OUT_DIR", "outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

rd  <- function(f, ...) read_csv(file.path(in_dir, f), show_col_types = FALSE,
                                 guess_max = 1e6, progress = FALSE, ...)
has <- function(f) file.exists(file.path(in_dir, f))
manifest <- tibble(file = character(), rows = character(), what = character(),
                   basis = character(), source = character())
not_produced <- character()
out <- function(df, f, what, basis, source) {
  write_csv(df, file.path(out_dir, f), na = "")
  manifest <<- add_row(manifest, file = f, rows = as.character(nrow(df)),
                       what = what, basis = basis, source = source)
  invisible(df)
}
say <- function(...) cat(..., "\n", sep = "")

SRC_LIC <- "licence: check license on source page"

# ---------------------------------------------------------------- 1-4 MASTER
m0 <- rd("master_states_v2.csv")
stopifnot(nrow(m0) == 37, n_distinct(m0$state) == 37)
chan_cols <- c("channel_street_market_per100k", "channel_supermarket_per100k",
               "channel_restaurant_per100k", "channel_campus_institution_per100k",
               "channel_delivery_app_readiness_0_1")
m <- m0

# Task 1: drop urban columns, rename + overwrite channel note (channel values untouched)
m <- m %>%
  select(-urban_share, -urban_data_basis, -urban_source_note) %>%
  rename(channel_input_note = channel_urban_input_note) %>%
  mutate(channel_input_note = "wealth_index scaled 0-1 used as development driver (urban_share unavailable); channel values are collinear with wealth by construction")

# Task 2: wealth_upper_share (percent of state persons in national quintiles 4+5)
m <- m %>%
  mutate(wealth_upper_share = wealth_pct_q4_fourth + wealth_pct_q5_highest) %>%
  relocate(wealth_upper_share, .after = wealth_pct_q5_highest)

# Task 3: soy detail + Yobe residual
ne_zone <- unique(na.omit(m$soy_zone_output_tonnes[m$zone == "North East"]))
stopifnot(length(ne_zone) == 1)
ne_other <- m %>% filter(zone == "North East", state != "Yobe")
stopifnot(nrow(ne_other) == 5, !anyNA(ne_other$soy_output_tonnes))
yobe_resid <- ne_zone - sum(ne_other$soy_output_tonnes)
yobe_pop   <- m$population[m$state == "Yobe"]
yobe_ok    <- abs(yobe_resid - 809.1) < 0.5
say("YOBE: NE zone total ", round(ne_zone, 3), " - sum(5 states) ",
    round(sum(ne_other$soy_output_tonnes), 3), " = ", round(yobe_resid, 3),
    " t | matches ~809.1: ", yobe_ok)
stopifnot(is.na(m$soy_output_tonnes[m$state == "Yobe"]))

m <- m %>%
  mutate(
    soy_status_detail = case_when(
      state == "Yobe"                          ~ "derived_residual",
      zone  == "South West"                    ~ "zone_total_only",
      zone  %in% c("South East", "South South") ~ "not_reported",
      !is.na(soy_output_tonnes)                ~ "reported",
      TRUE                                     ~ NA_character_),
    soy_output_tonnes   = if_else(state == "Yobe", yobe_resid, soy_output_tonnes),
    soy_per_100k_people = if_else(state == "Yobe", yobe_resid / yobe_pop * 1e5, soy_per_100k_people),
    soy_source_note = if_else(state == "Yobe",
      paste0(soy_source_note, "; Yobe soy_output_tonnes DERIVED = North East zone total minus sum of the other five NE states (NASS state value suppressed (S)); harvested area and yield left NA"),
      soy_source_note)) %>%
  relocate(soy_status_detail, .after = soy_status)
stopifnot(!anyNA(m$soy_status_detail))
# zero is never filled: only Yobe changed in soy_output_tonnes
stopifnot(identical(is.na(m$soy_output_tonnes), is.na(m0$soy_output_tonnes) & m$state != "Yobe"))

# Task 4: climate flags (no climate values changed)
pairs <- list(c("Akwa Ibom", "Cross River"), c("Anambra", "Delta"), c("Ekiti", "Ondo"))
for (p in pairs) {
  x <- m %>% filter(state %in% p) %>% distinct(annual_rainfall_mm, rainy_season_months, mean_temp_c)
  if (nrow(x) != 1) warning("pair does not share an identical climate triple: ", paste(p, collapse = "/"))
}
note_txt <- "coarse NASA POWER grid, neighbouring centroids can share a cell"
pair_of <- setNames(rep(map_chr(pairs, ~ paste(.x, collapse = "/")), each = 2), unlist(pairs))
m <- m %>%
  mutate(
    climate_suspect = state %in% c("Kaduna", "Plateau"),
    climate_suspect_note = if_else(climate_suspect,
      "annual rainfall implausible (>1.75x zone median); being re-pulled by user", NA_character_),
    climate_shared_cell_flag = state %in% names(pair_of),
    climate_shared_cell_note = if_else(climate_shared_cell_flag,
      paste0(note_txt, " (pair: ", unname(pair_of[state]), ")"), NA_character_)) %>%
  relocate(climate_suspect, climate_suspect_note, climate_shared_cell_flag,
           climate_shared_cell_note, .after = climate_source_note)

# integrity: rows, channel values and climate values unchanged
stopifnot(nrow(m) == 37)
stopifnot(isTRUE(all.equal(m[chan_cols], m0[chan_cols])))
stopifnot(isTRUE(all.equal(m[c("annual_rainfall_mm", "rainy_season_months", "mean_temp_c")],
                           m0[c("annual_rainfall_mm", "rainy_season_months", "mean_temp_c")])))
out(m, "master_states_final.csv",
    "State-level master table (36 states + FCT): population, climate, wealth, soy, synthetic channel density",
    "mixed: real (population, climate, wealth, soy) + synthetic (channel_*)",
    paste("WorldPop/NPC (population); NASA POWER (climate); DHS Program Nigeria 2024 (wealth, state-level aggregates only); NASS 2022/23 (soy); channel_* synthetic;", SRC_LIC))

# ----------------------------------------------------------- CHANNEL DENSITY
cl <- rd("channel_density_long.csv")
stopifnot(nrow(cl) == 185, n_distinct(cl$state) == 37)
map_ch <- c(street_market = "channel_street_market_per100k", supermarket = "channel_supermarket_per100k",
            restaurant = "channel_restaurant_per100k", campus_institution = "channel_campus_institution_per100k",
            delivery_app = "channel_delivery_app_readiness_0_1")
cw <- cl %>% mutate(col = unname(map_ch[channel])) %>%
  select(state, col, value) %>% pivot_wider(names_from = col, values_from = value) %>%
  mutate(data_basis = "synthetic placeholder (real source: field verification/OSM)")
chk <- m0 %>% select(state, all_of(chan_cols)) %>%
  inner_join(cw %>% select(state, all_of(chan_cols)), by = "state", suffix = c("", ".w"))
stopifnot(nrow(chk) == 37, all(abs(chk[chan_cols] - chk[paste0(chan_cols, ".w")]) < 1e-9))
say("CHANNELS: wide rebuilt from long (channel_density_wide.csv was not supplied); matches master values")
out(cw, "channel_density_wide.csv", "Synthetic channel density per state, wide (rebuilt from long file)",
    "synthetic", "Synthetic placeholder generated in earlier chat; real source would be field verification/OSM")
out(cl, "channel_density_long.csv", "Synthetic channel density, long (state, channel, value, unit, data_basis, assumption_note)",
    "synthetic", "Synthetic placeholder generated in earlier chat; copied unchanged")

# ------------------------------------------------------------- 5 FOOTPRINT
raw_f  <- "impacts_2026-03-10.csv"
fp_src <- if (has(raw_f)) raw_f else "clean_hestia_soybean_impacts.csv"
say("FOOTPRINT source used: ", fp_src, if (!has(raw_f)) "  (raw impacts_2026-03-10.csv NOT supplied)" else "")
hz <- rd(fp_src) %>% filter(countryName == "World")
spec <- tribble(~product_requested, ~pattern,
  "soybean seed (whole)", "^Soybean, seed \\(whole\\)$",
  "beef cattle",          "beef cattle",
  "chicken meat",         "chicken.*meat|meat.*chicken")
fp <- map_dfr(seq_len(nrow(spec)), function(i)
  hz %>% filter(str_detect(productName, regex(spec$pattern[i], ignore_case = TRUE))) %>%
    mutate(product_requested = spec$product_requested[i])) %>%
  mutate(metric = case_when(
    termId == "gwp100" & termUnits == "kg CO2 eq"                  ~ "GWP100",
    str_detect(termId, regex("^landOccupation", TRUE))             ~ "land_occupation_area",
    str_detect(termId, regex("landOccupation", TRUE))              ~ "land_occupation_indicator_not_area",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(metric)) %>%
  transmute(product_requested, productName, countryName, metric, termId, termName, termUnits,
            value, min, max, functionalUnitQuantity, startDate, endDate, methodModelId,
            source_file = fp_src, data_basis = "real (HESTIA)",
            label = "global average, no Nigeria data; pitch criterion only, not per-state")
need <- expand_grid(product_requested = spec$product_requested, metric = c("GWP100", "land_occupation_area"))
fp_missing <- need %>% anti_join(fp %>% distinct(product_requested, metric), by = c("product_requested", "metric"))
say("FOOTPRINT found: ", paste(unique(paste(fp$product_requested, fp$metric, sep = ":")), collapse = " | "))
say("FOOTPRINT missing: ", paste(paste(fp_missing$product_requested, fp_missing$metric, sep = ":"), collapse = " | "))
out(fp, "footprint_standin.csv", "World-level HESTIA footprint stand-in rows (only what exists in supplied file; missing items listed in README)",
    "real (HESTIA global average; not Nigeria)", "HESTIA, CC BY 4.0 (hestia.earth)")

# ------------------------------------------- 6 SURVEY / CHOICE / HIDDEN TRUTH
id_scan <- function(df, nm) {
  by_name <- names(df)[str_detect(names(df), regex("name|phone|mobile|email|address|whatsapp|contact", TRUE))]
  by_val  <- names(df)[map_lgl(df, ~ is.character(.x) && any(str_detect(na.omit(.x), "@|^\\+?[0-9]{10,}$")))]
  ids     <- names(df)[str_detect(names(df), regex("(^|_)id$", TRUE))]
  say("ID SCAN ", nm, ": name-like cols=[", paste(by_name, collapse = ","), "] phone/email-like values in=[",
      paste(by_val, collapse = ","), "] id cols=[", paste(ids, collapse = ","), "]")
}
raw_sets <- tribble(~inf, ~outf, ~what, ~basis,
  "survey_responses.csv", "survey_responses_synthetic.csv", "Synthetic consumer survey, respondent level", "synthetic",
  "choice_tasks.csv",     "choice_tasks_synthetic.csv",     "Synthetic discrete-choice tasks, respondent x task", "synthetic",
  "hidden_truth.csv",     "validation_only_hidden_truth.csv", "Synthetic ground-truth segment labels; VALIDATION ONLY, never use as a model input", "synthetic")
for (i in seq_len(nrow(raw_sets))) {
  if (has(raw_sets$inf[i])) {
    d <- rd(raw_sets$inf[i]); id_scan(d, raw_sets$inf[i])
    out(d, raw_sets$outf[i], raw_sets$what[i], raw_sets$basis[i], "Synthetic, generated for this project")
  } else {
    not_produced <- c(not_produced, paste0(raw_sets$outf[i], " (input ", raw_sets$inf[i], " not supplied)"))
    say("NOT PRODUCED: ", raw_sets$outf[i], " - input ", raw_sets$inf[i], " not supplied")
  }
}

# ---------------------------------------------------------- 7 PILOT STATES
sv <- rd("clean_survey_responses.csv"); vd <- rd("clean_vendor_interviews.csv")
ps <- inner_join(sv %>% select(state, n_respondents), vd %>% select(state, n_vendors), by = "state") %>%
  mutate(data_basis = "synthetic") %>% arrange(state)
stopifnot(nrow(ps) == nrow(sv), nrow(ps) == nrow(vd))   # no state lost in the join
if (has("survey_responses.csv")) {                       # cross-check counts against raw if present
  rs <- rd("survey_responses.csv") %>% count(state, name = "n_raw")
  stopifnot(all(left_join(ps, rs, by = "state")$n_raw == ps$n_respondents))
}
out(ps, "pilot_states.csv", "Pilot states (survey + vendor data) with respondent and vendor counts",
    "synthetic", "Counts taken from clean_survey_responses.csv and clean_vendor_interviews.csv (synthetic)")
pilot_soy_gap <- m %>% filter(state %in% ps$state, !soy_status_detail %in% c("reported", "derived_residual")) %>% pull(state)
say("PILOT states: ", nrow(ps), " | pilot states without state-level soy: ", paste(pilot_soy_gap, collapse = ", "))

# ------------------------------------------------------ 8 UNIT ECONOMICS v1b
ue <- rd("clean_unit_economics.csv")
comps <- c("soybean", "coagulant", "cooking_oil", "spice", "labour", "fuel_energy", "packaging", "transport_to_retail")
for (lv in c("low", "base", "high")) {
  cc  <- paste0(comps, "_cost_", lv); stopifnot(all(cc %in% names(ue)))
  tot <- paste0("total_unit_cost_", lv)
  ue[[paste0("reported_total_unit_cost_", lv)]] <- ue[[tot]]
  ue[[tot]] <- round(rowSums(ue[cc]), 1)
  ue[[paste0("total_diff_", lv)]] <- round(ue[[paste0("reported_total_unit_cost_", lv)]] - ue[[tot]], 1)
}
if ("diff_vs_total_base" %in% names(ue)) {            # earlier helper column = reported - sum; replaced by total_diff_*
  stopifnot(all(abs(ue$diff_vs_total_base - ue$total_diff_base) < 0.06))
  ue$diff_vs_total_base <- NULL
}
ue <- ue %>% relocate(starts_with("reported_total_unit_cost_"), starts_with("total_diff_"), .after = total_unit_cost_high)
dif <- ue %>% filter(if_any(starts_with("total_diff_"), ~ abs(.x) > 0.05))
say("UNIT ECON: rows ", nrow(ue), " | rows where reported total != sum of components: ", nrow(dif))
say("  diff range (reported - recomputed) low: ", paste(range(ue$total_diff_low), collapse = " to "),
    " | base: ", paste(range(ue$total_diff_base), collapse = " to "),
    " | high: ", paste(range(ue$total_diff_high), collapse = " to "))
print(dif %>% group_by(format) %>% summarise(n = n(), base_diff_min = min(total_diff_base),
      base_diff_max = max(total_diff_base), .groups = "drop"))
out(ue, "unit_economics_v1b.csv", "Unit economics per 150 g pack by state x format; totals recomputed as sum of 8 listed components; originals kept as reported_total_*; total_diff_* = reported - recomputed",
    "synthetic", "Synthetic, generated for this project (clean_unit_economics.csv)")

# ------------------------------------------------------------ 9 SEASONALITY
# Built from clean_seasonality.csv (per state x month count of vendors naming the month as a peak month,
# already tokenised from vendor_interviews.peak_months, "|" separated). Raw vendor_interviews.csv not supplied,
# so counts are re-used and checked against clean_vendor_interviews.csv, not re-tokenised.
ss <- rd("clean_seasonality.csv") %>% mutate(month = factor(month, levels = month.abb))
stopifnot(!anyNA(ss$month), nrow(ss) == 120)
chk <- ss %>% distinct(state, n_vendors_state) %>% full_join(vd %>% select(state, n_vendors), by = "state")
stopifnot(nrow(chk) == 10, all(chk$n_vendors_state == chk$n_vendors))
stopifnot(all(abs(ss$n_vendors_peak / ss$n_vendors_state - ss$share_vendors_peak) < 1e-9))
st  <- ss %>% transmute(scope = "state", state, month, n_vendors_peak, n_vendors = n_vendors_state,
                        share_vendors_peak = n_vendors_peak / n_vendors_state)
nat <- ss %>% group_by(month) %>% summarise(n_vendors_peak = sum(n_vendors_peak), .groups = "drop") %>%
  mutate(scope = "national", state = "ALL", n_vendors = sum(vd$n_vendors),
         share_vendors_peak = n_vendors_peak / n_vendors)
sea <- bind_rows(nat, st) %>%
  group_by(scope, state) %>% mutate(seasonality_index = share_vendors_peak / mean(share_vendors_peak)) %>% ungroup() %>%
  mutate(small_n_flag = n_vendors <= 6,
         small_n_note = if_else(small_n_flag, "small n: 3-6 vendors in this state; indicative only",
                                "national = pooled 40 vendors across 10 pilot states, unweighted by state size"),
         data_basis = "synthetic (derived from synthetic vendor peak_months)",
         month = as.character(month)) %>%
  arrange(scope != "national", state, match(month, month.abb)) %>%
  select(scope, state, month, n_vendors_peak, n_vendors, share_vendors_peak, seasonality_index,
         small_n_flag, small_n_note, data_basis)
stopifnot(all(abs(tapply(sea$seasonality_index, paste(sea$scope, sea$state), mean) - 1) < 1e-9))
say("SEASONALITY: ", nrow(sea), " rows; national peak months: ",
    paste(sea %>% filter(scope == "national") %>% arrange(desc(seasonality_index)) %>% slice_head(n = 3) %>% pull(month), collapse = ","))
out(sea, "seasonality_month.csv", "Share of vendors naming each month as peak, and index normalised to mean 1.0 across 12 months; national + per pilot state",
    "synthetic", "Synthetic vendor peak_months (clean_seasonality.csv)")

# --------------------------------------------------------- DATA DICTIONARY
dd <- rd("data_dictionary.csv", col_types = cols(.default = col_character()))
POP_SRC <- "WorldPop/NPC, scaled to national total, reference year not stated in source file"
dd <- dd %>%
  filter(column != "urban_share") %>%
  mutate(
    source = if_else(column == "population", POP_SRC, source),
    meaning = case_when(
      column == "channel_urban_input_note" ~ "Which development driver fed the synthetic channels: wealth_index scaled 0-1 (urban_share unavailable); collinear with wealth by construction",
      column == "soy_output_tonnes"        ~ paste0(meaning, "; Yobe = DERIVED residual (see soy_status_detail)"),
      column == "soy_per_100k_people"      ~ paste0(meaning, "; Yobe computed from the derived residual"),
      column == "ASSUMPTION_7" ~ "Survey, choice and hidden-truth files are saved in full ONLY when the respondent-level inputs are supplied; otherwise not produced (see README). Master/pilot tables never hold individual records",
      column == "ASSUMPTION_8" ~ "Seasonality built from clean_seasonality.csv state x month counts (derived from vendor_interviews.peak_months); index = share / mean share across 12 months; synthetic; n = 3-6 vendors per state",
      column == "ASSUMPTION_9" ~ "Footprint stand-in = HESTIA World rows only (no Nigeria data); only products and metrics present in the supplied HESTIA file are saved, missing ones are listed in README, nothing invented",
      TRUE ~ meaning),
    column = if_else(column == "channel_urban_input_note", "channel_input_note", column))
new <- tribble(~column, ~meaning, ~unit, ~source, ~data_basis,
  "wealth_upper_share", "DERIVED wealth_pct_q4_fourth + wealth_pct_q5_highest", "percent", "table_2_6.csv (DHS 2024)", "real",
  "soy_status_detail", "reported / derived_residual (Yobe) / zone_total_only (South West) / not_reported (South East, South South); never zero-filled", "text", "NASS + this script", "real",
  "climate_suspect", "TRUE for Kaduna and Plateau: annual rainfall implausible, being re-pulled by user; values unchanged", "logical", "this script", "",
  "climate_suspect_note", "Reason for climate_suspect", "text", "this script", "",
  "climate_shared_cell_flag", "TRUE for Akwa Ibom/Cross River, Anambra/Delta, Ekiti/Ondo; values unchanged", "logical", "this script", "",
  "climate_shared_cell_note", "coarse NASA POWER grid, neighbouring centroids can share a cell (+ pair)", "text", "this script", "",
  "CHANGE_1", "Dropped urban_share, urban_data_basis, urban_source_note (urban share unavailable); renamed channel_urban_input_note -> channel_input_note with new text; channel values unchanged", "", "01b_finalize.R", "",
  "CHANGE_2", "Yobe soy_output_tonnes derived = NE zone total minus sum of other five NE states; soy_per_100k_people recomputed for Yobe only; harvested area and yield stay NA", "", "01b_finalize.R", "real (derived)",
  "CHANGE_3", "unit_economics_v1b: total_unit_cost_* recomputed as sum of 8 listed components; originals in reported_total_unit_cost_*; total_diff_* = reported - recomputed; earlier helper column diff_vs_total_base replaced by total_diff_base. producer_net_* / producer_margin_* / shelf_price_* NOT recomputed (still based on reported totals)", "", "01b_finalize.R", "synthetic",
  "unit_economics_v1b.total_diff_low|base|high", "Reported total minus sum of components (NGN per 150 g pack)", "NGN", "01b_finalize.R", "synthetic",
  "seasonality_month.share_vendors_peak", "Share of vendors naming the month as a peak month (a vendor can name several months, so shares sum to about 3 per scope)", "fraction 0-1", "clean_seasonality.csv", "synthetic",
  "seasonality_month.seasonality_index", "share_vendors_peak / mean share across the 12 months within scope; mean = 1.0", "index", "01b_finalize.R", "synthetic",
  "seasonality_month.small_n_flag", "TRUE where n_vendors <= 6 (all state rows)", "logical", "01b_finalize.R", "synthetic",
  "pilot_states.n_respondents / n_vendors", "Synthetic respondents and vendors per pilot state", "count", "clean_survey_responses.csv, clean_vendor_interviews.csv", "synthetic",
  "footprint_standin.*", "HESTIA World-level rows; metric = GWP100 (kg CO2 eq per functional unit) or land occupation; label column gives usage limit", "varies", "HESTIA (CC BY 4.0)", "real")
dd <- bind_rows(dd, new)
out(dd, "data_dictionary.csv", "Column dictionary and change log", "n/a", "this script")

# ------------------------------------------------------------------ README
manifest <- bind_rows(manifest,
  tibble(file = c("README_data.md", "01b_finalize.R"), rows = "n/a",
         what = c("This file", "Reproducible script that builds every output"), basis = "n/a", source = "this project"))
tab <- c("| File | Rows | What it is | Real / synthetic | Source and attribution |", "|---|---|---|---|---|",
         pmap_chr(manifest, function(file, rows, what, basis, source) paste0("| `", file, "` | ", rows, " | ", what, " | ", basis, " | ", source, " |")))
fp_missing_txt <- if (nrow(fp_missing)) paste0("- ", fp_missing$product_requested, " - ", fp_missing$metric, collapse = "\n") else "- none"
lim <- c(
  paste0("1. Soy missing in ", length(pilot_soy_gap), " of ", nrow(ps), " pilot states (", paste(pilot_soy_gap, collapse = ", "), "): no state-level NASS soy for the South (soy_status_detail = zone_total_only / not_reported). Left NA, never zero."),
  "2. Yobe soy is a DERIVED residual (NE zone total minus five other NE states); NASS suppressed the state value (S). Harvested area and yield not available.",
  "3. Climate suspect for Kaduna and Plateau (annual rainfall implausible; user is re-pulling). Pairs Akwa Ibom/Cross River, Anambra/Delta, Ekiti/Ondo share a coarse NASA POWER cell.",
  "4. Urban share missing: no state-level value in any supplied file; columns dropped.",
  "5. Channel density is synthetic and collinear with wealth by construction (wealth_index scaled 0-1 stands in for urban share).",
  "6. Vendor n is 3 to 6 per state (40 vendors in total); seasonality per state is indicative only.",
  "7. Population reference year is undocumented (WorldPop/NPC, scaled to national total).",
  "8. Unit economics: reported totals differ from the sum of the eight listed components; recomputed totals are in total_unit_cost_*, originals in reported_total_*. Producer net/margin/shelf price columns still rest on reported totals.",
  "9. Footprint is a global average with no Nigeria data; pitch criterion only, not per-state.",
  "10. Survey, vendor, choice, unit-economics and seasonality data are SYNTHETIC; hidden_truth is validation only.")
np <- if (length(not_produced)) paste0("- ", not_produced, collapse = "\n") else "- none"
writeLines(c(
  "# Data package: AI for Alternative-Protein Market Opportunity Identification (Nigeria)",
  "Product: packaged seasoned fried awara (soy tofu), 150 g pack. No modelling or scoring in this package.", "",
  "## Output files", tab, "",
  "## Not produced (input not supplied)", np, "",
  "## Footprint items missing (not in supplied HESTIA file; nothing invented)", fp_missing_txt, "",
  "Footprint label: global average, no Nigeria data; pitch criterion only, not per-state.", "",
  "## Known limitations", lim, "",
  "## Sources",
  paste0("- WorldPop/NPC (population): ", SRC_LIC),
  paste0("- NASA POWER (climate): ", SRC_LIC),
  paste0("- DHS Program Nigeria 2024 (wealth, state-level aggregates only): ", SRC_LIC),
  paste0("- NASS 2022/23 (soy output): ", SRC_LIC),
  "- HESTIA (footprint): CC BY 4.0, attribution required (hestia.earth)",
  "- Survey, choice, vendor, unit-economics, seasonality and channel data: synthetic, generated for this project; contain no real personal data"
), file.path(out_dir, "README_data.md"))

say("\nOUTPUTS in ", out_dir, ":")
print(sort(list.files(out_dir)))
