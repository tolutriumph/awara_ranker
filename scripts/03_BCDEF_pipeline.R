#!/usr/bin/env Rscript
# 03_BCDEF_pipeline.R - Stages B (segments), C (choice model), D (opportunity criteria), E (Monte Carlo),
# F (explainability + handoff). Requires 03_D_state_channel_inputs.R and 03_A_screening.R to have run first.
# Usage: IN_DIR=inputs OUT_DIR=outputs Rscript 03_BCDEF_pipeline.R
#
# INPUT CONTRACT (edit COLS below if your files use other names; the script stops if anything is missing):
#  survey_responses_synthetic.csv : one row per respondent; respondent_id, state, gender + the 12 clustering vars.
#      awara_frequency values: daily / weekly / monthly / rarely / never.  intent_to_try_packaged: 1-5 (top-2-box = 4-5).
#      max_price_150g_pack_ngn: numeric NGN.  preferred_channel: street_market / supermarket / campus_shop /
#      delivery_app / gym_health_store.
#  choice_tasks_synthetic.csv : WIDE as audited (task_id, respondent_id, task_number, option_a_price_ngn, option_a_format,
#      option_a_channel, option_a_shelf_life_days, option_b_*, chosen_option = A / B / neither; 3 tasks per respondent).
#      A LONG layout (respondent_id, task_id, option, chosen, price, format, channel, shelf_life) is also accepted.
#  validation_only_hidden_truth.csv : respondent_id, true_segment. Read ONLY after PAM is fitted (Stage B validation).
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(purrr)
  library(cluster); library(survival); library(jsonlite); library(ggplot2)})
set.seed(42)
in_dir <- Sys.getenv("IN_DIR", "inputs"); out_dir <- Sys.getenv("OUT_DIR", "outputs")
rd <- function(f, d = in_dir) read_csv(file.path(d, f), show_col_types = FALSE, guess_max = 1e5)
say <- function(...) cat(..., "\n", sep = "")
mm <- function(x) { r <- range(x, na.rm = TRUE); if (diff(r) == 0) return(rep(50, length(x))); 100 * (x - r[1]) / diff(r) }

pick <- function(a, b) if (file.exists(file.path(in_dir, a)) || !file.exists(file.path(in_dir, b))) a else b   # 01b name, else raw name
F_SURVEY <- pick("survey_responses_synthetic.csv", "survey_responses.csv"); F_CHOICE <- pick("choice_tasks_synthetic.csv", "choice_tasks.csv")
F_TRUTH <- pick("validation_only_hidden_truth.csv", "hidden_truth.csv")
COLS <- list(
  id = "respondent_id", state = "state", gender = "gender",
  clust = c("setting", "age_band", "income_band", "occupation", "awara_frequency", "meat_meals_per_week",
            "trust_packaged_food", "hygiene_concern", "intent_to_try_packaged", "max_price_150g_pack_ngn",
            "preferred_channel", "preferred_format"),
  ch = c(id = "respondent_id", task = "task_id", option = "option", chosen = "chosen", price = "price",
         format = "format", channel = "channel", shelf_life = "shelf_life"),
  truth = c(id = "respondent_id", seg = "true_segment"))
CAL_DISCOUNT <- 0.5; N_MC <- 2000; DIR_CONC <- 30
W0 <- c(market_demand = 0.30, affordability = 0.20, margin = 0.20, soy_supply = 0.15, channel_readiness = 0.15)
PPM <- c(daily = 20, weekly = 4, monthly = 1, rarely = 0.5, never = 0)
FREQ_LEV <- c("never", "rarely", "monthly", "weekly", "daily")
K_TOL <- 0.02   # interpretable-k rule: smallest k whose mean silhouette is within 0.02 of the best

# ------------------------------------------------------------------ input check
need <- c(F_SURVEY, F_CHOICE, F_TRUTH, "master_states_final.csv", "unit_economics_v1b.csv", "pilot_states.csv")
miss <- need[!file.exists(file.path(in_dir, need))]
prev <- c("state_channel_inputs.csv", "channel_mapping.csv", "launch_timing.csv", "screening_37_states.csv")
miss_prev <- prev[!file.exists(file.path(out_dir, prev))]
if (length(miss) || length(miss_prev))
  stop("STOPPED, nothing fitted. Missing inputs in ", in_dir, ": [", paste(miss, collapse = ", "),
       "]; missing outputs of 03_A / 03_D in ", out_dir, ": [", paste(miss_prev, collapse = ", "), "]", call. = FALSE)
sv <- rd(F_SURVEY); ct <- rd(F_CHOICE)
mc <- setdiff(c(COLS$id, COLS$state, COLS$gender, COLS$clust), names(sv)); if (length(mc)) stop("survey missing columns: ", paste(mc, collapse = ", "))
sv <- sv %>% rename(respondent_id = all_of(COLS$id), state = all_of(COLS$state))
# choice_tasks: WIDE layout as audited in 01_build_master (one row per task, option_a_* / option_b_*, chosen_option A/B/neither)
WIDE <- c("task_id", "respondent_id", "option_a_price_ngn", "option_a_format", "option_a_channel", "option_a_shelf_life_days",
          "option_b_price_ngn", "option_b_format", "option_b_channel", "option_b_shelf_life_days", "chosen_option")
if (all(WIDE %in% names(ct))) {
  stopifnot(all(ct$chosen_option %in% c("A", "B", "neither")))
  side <- function(o) ct %>% transmute(respondent_id, task = task_id, option = toupper(o),
    price = .data[[paste0("option_", o, "_price_ngn")]], format = .data[[paste0("option_", o, "_format")]],
    channel = .data[[paste0("option_", o, "_channel")]], shelf_life = .data[[paste0("option_", o, "_shelf_life_days")]],
    chosen = as.integer(chosen_option == toupper(o)))
  ct <- bind_rows(side("a"), side("b"), ct %>% transmute(respondent_id, task = task_id, option = "neither", price = NA_real_,
    format = NA_character_, channel = NA_character_, shelf_life = NA_real_, chosen = as.integer(chosen_option == "neither")))
  say("C: choice_tasks read in WIDE layout -> ", n_distinct(ct$task), " tasks, ", n_distinct(ct$respondent_id), " respondents")
} else {
  mc <- setdiff(unname(COLS$ch), names(ct)); if (length(mc)) stop("choice_tasks is neither the audited wide layout nor long layout; missing: ", paste(mc, collapse = ", "))
  ct <- ct %>% rename(!!!setNames(unname(COLS$ch), sub("^id$", "respondent_id", names(COLS$ch))))
}
stopifnot(!anyDuplicated(sv$respondent_id))
sci <- rd("state_channel_inputs.csv", out_dir); chan_map <- rd("channel_mapping.csv", out_dir)
m <- rd("master_states_final.csv"); ue <- rd("unit_economics_v1b.csv"); ps <- rd("pilot_states.csv")
assum <- tibble(name = character(), value = character(), range = character(), basis = character())
A <- function(name, value, range = "", basis = "") assum <<- add_row(assum, name = name, value = as.character(value), range = range, basis = basis)

# ================================================================== STAGE B
na_share <- sv %>% summarise(across(all_of(COLS$clust), ~ mean(is.na(.x)))) %>% pivot_longer(everything(), names_to = "column", values_to = "na_share")
say("B: n = ", nrow(sv), " respondents; NA share per clustering column: ", paste0(na_share$column, "=", round(100 * na_share$na_share, 1), "%", collapse = " "))
X <- sv %>% select(all_of(COLS$clust)) %>%
  mutate(across(where(is.character), as.factor),
         awara_frequency = if (all(na.omit(as.character(awara_frequency)) %in% FREQ_LEV)) factor(awara_frequency, levels = FREQ_LEV, ordered = TRUE) else awara_frequency,
         # bands are ordinal; their labels sort correctly ("18-24" < ... < "55+", "1_under_50k" < ... < "5_over_500k")
         age_band = factor(age_band, levels = sort(unique(na.omit(as.character(age_band)))), ordered = TRUE),
         income_band = factor(income_band, levels = sort(unique(na.omit(as.character(income_band)))), ordered = TRUE)) %>%
  as.data.frame()
A("gower_variable_types", "ordinal: awara_frequency, age_band, income_band; interval: 1-5 scales, meat meals, max price; nominal: rest", "", "Gower (daisy); ordinal treated by rank")
say("B: age_band levels ", paste(levels(X$age_band), collapse = "<"), " | income_band levels ", paste(levels(X$income_band), collapse = "<"))
D <- daisy(X, metric = "gower")          # NAs handled pairwise by daisy; no row dropped, nothing imputed
fits <- map(3:6, ~ pam(D, k = .x, diss = TRUE))
sil <- tibble(k = 3:6, mean_silhouette = map_dbl(fits, ~ .x$silinfo$avg.width))
k_best <- sil$k[which.max(sil$mean_silhouette)]
k_sel <- min(sil$k[sil$mean_silhouette >= max(sil$mean_silhouette) - K_TOL])
fit <- fits[[k_sel - 2]]
say("B: silhouette ", paste0("k", sil$k, "=", round(sil$mean_silhouette, 3), collapse = " "), " | best k=", k_best, " | selected k=", k_sel, " (smallest k within ", K_TOL, " of best)")
A("pam_k", k_sel, "3-6", paste0("silhouette; smallest k within ", K_TOL, " of best (best k=", k_best, ")"))
A("gower_na_handling", "pairwise (daisy)", "", "about 3% NA per column; no rows dropped, nothing imputed")
sv$segment_id <- fit$clustering

# automatic segment names from profile: dominant non-trivial features with the highest lift vs. whole sample
cat_cols <- names(X)[map_lgl(X, ~ is.factor(.x) || is.character(.x))]
lift <- map_dfr(cat_cols, function(v) sv %>% filter(!is.na(.data[[v]])) %>% count(segment_id, level = as.character(.data[[v]])) %>%
  group_by(segment_id) %>% mutate(seg_share = n / sum(n)) %>% ungroup() %>%
  left_join(sv %>% filter(!is.na(.data[[v]])) %>% count(level = as.character(.data[[v]])) %>% mutate(all_share = n / sum(n)) %>% select(level, all_share), by = "level") %>%
  mutate(var = v, lift = seg_share / all_share)) %>% filter(seg_share >= 0.4)
intent_num <- suppressWarnings(as.numeric(sv$intent_to_try_packaged))
seg_names <- lift %>% group_by(segment_id) %>% arrange(desc(lift)) %>% slice_head(n = 3) %>%
  summarise(label = paste(paste0(var, "=", level), collapse = "; "), .groups = "drop") %>%
  right_join(tibble(segment_id = sort(unique(sv$segment_id))), by = "segment_id") %>%
  left_join(sv %>% mutate(intent_num = intent_num) %>% group_by(segment_id) %>%
              summarise(t2b = mean(intent_num >= 4, na.rm = TRUE),
                        student = mean(occupation %in% "student") / mean(!is.na(occupation)),
                        eats_weekly_plus = mean(awara_frequency %in% c("daily", "weekly")) / mean(!is.na(awara_frequency)),
                        med_price = median(max_price_150g_pack_ngn, na.rm = TRUE), .groups = "drop"), by = "segment_id") %>%
  arrange(segment_id)
# plain-language names by fixed rules on the profile (applied in this order; segments not matched keep their feature label):
#  highest student share -> price-driven students; then highest daily+weekly awara share -> traditional awara eaters;
#  then highest median max price -> affluent convenience / health seekers
seg_names$plain <- NA_character_
rules <- list(c("student", "Price-driven students"), c("eats_weekly_plus", "Traditional awara eaters"), c("med_price", "Affluent convenience/health seekers"))
for (r in rules) { free <- is.na(seg_names$plain); if (!any(free)) break
  i <- which(free)[which.max(seg_names[[r[1]]][free])]; seg_names$plain[i] <- r[2] }
seg_names <- seg_names %>% mutate(profile_label = coalesce(label, "no dominant feature"),
  segment = paste0("S", segment_id, " ", coalesce(plain, profile_label), " (intent T2B ", round(100 * t2b), "%)"))
A("segment_naming", "rule-based on profile: max student share; then max daily+weekly awara share; then max median max price", "", "names are labels for readability, not validated personas")
sv <- sv %>% left_join(seg_names %>% select(segment_id, segment), by = "segment_id")

mix <- function(v) sv %>% filter(!is.na(.data[[v]])) %>% count(segment_id, lv = paste0(v, "_", .data[[v]])) %>%
  group_by(segment_id) %>% mutate(s = round(n / sum(n), 3)) %>% select(-n) %>% pivot_wider(names_from = lv, values_from = s, values_fill = 0)
prof <- sv %>% group_by(segment_id, segment) %>%
  summarise(n = n(), share = n() / nrow(sv), intent_top2box_share = mean(as.numeric(intent_to_try_packaged) >= 4, na.rm = TRUE),
            median_max_price_ngn = median(max_price_150g_pack_ngn, na.rm = TRUE), .groups = "drop") %>%
  left_join(mix("awara_frequency"), by = "segment_id") %>% left_join(mix("income_band"), by = "segment_id") %>%
  left_join(mix("preferred_channel"), by = "segment_id") %>%
  mutate(data_basis = "synthetic survey (n=1200); PAM on Gower, state/gender/id excluded; NA excluded per column in shares")
write_csv(prof, file.path(out_dir, "segment_profiles.csv"))
sbs <- sv %>% count(state, segment_id, segment) %>% group_by(state) %>% mutate(n_state = sum(n), segment_share_in_state = n / n_state) %>% ungroup() %>%
  mutate(data_basis = "synthetic survey; unweighted sample shares (assumed representative of state population)")
write_csv(sbs, file.path(out_dir, "segment_share_by_state.csv"))
A("segment_share_in_state", "unweighted survey sample share", "", "assumes survey sample in each state is representative; n per state 60-200")

# validation ONLY after fitting
ari <- function(a, b) { t <- table(a, b); n <- sum(t); s2 <- function(x) sum(choose(x, 2))
  sa <- s2(rowSums(t)); sb <- s2(colSums(t)); e <- sa * sb / choose(n, 2); (s2(t) - e) / ((sa + sb) / 2 - e) }
tr <- rd(F_TRUTH) %>% rename(respondent_id = all_of(unname(COLS$truth["id"])), true_segment = all_of(unname(COLS$truth["seg"])))
vt <- sv %>% select(respondent_id, segment_id) %>% inner_join(tr, by = "respondent_id")
ARI <- ari(vt$segment_id, vt$true_segment)
conf <- as.data.frame.matrix(table(found = vt$segment_id, true = vt$true_segment))
write_csv(conf %>% tibble::rownames_to_column("found_segment") %>% mutate(ARI = round(ARI, 3), data_basis = "validation only, synthetic hidden truth; NOT used in fitting"),
          file.path(out_dir, "segment_validation_confusion.csv"))
say("B: ARI vs hidden truth = ", round(ARI, 3), " (n matched ", nrow(vt), ")"); print(conf)

# ================================================================== STAGE C
ct <- ct %>% mutate(option = as.character(option), chosen = as.integer(chosen),
                    is_none = tolower(option) %in% c("neither", "none", "opt_out"),
                    price = readr::parse_number(as.character(price)), shelf_life = readr::parse_number(as.character(shelf_life)),
                    task_key = paste(respondent_id, task, sep = "_"))
chk <- ct %>% group_by(task_key) %>% summarise(s = sum(chosen), n = n(), .groups = "drop")
stopifnot(all(chk$s == 1))
fmt_lev <- sort(unique(na.omit(ct$format[!ct$is_none]))); ch_lev <- sort(unique(na.omit(ct$channel[!ct$is_none])))
fmt_ref <- if ("fried_seasoned_pack" %in% fmt_lev) "fried_seasoned_pack" else fmt_lev[1]
ch_ref <- if ("street_market" %in% ch_lev) "street_market" else ch_lev[1]
dm <- ct %>% mutate(price100 = if_else(is_none, 0, price / 100), shelf_life = if_else(is_none, 0, shelf_life), asc_none = as.integer(is_none))
for (l in setdiff(fmt_lev, fmt_ref)) dm[[paste0("fmt_", l)]] <- as.integer(!dm$is_none & dm$format == l)
for (l in setdiff(ch_lev, ch_ref)) dm[[paste0("ch_", l)]] <- as.integer(!dm$is_none & dm$channel == l)
terms <- c("price100", grep("^fmt_|^ch_", names(dm), value = TRUE), "shelf_life", "asc_none")
fml <- as.formula(paste("chosen ~", paste(terms, collapse = " + "), "+ strata(task_key)"))
fit_cl <- function(d, lab) {
  f <- tryCatch(clogit(fml, data = d, method = "approximate"), error = function(e) NULL)
  if (is.null(f)) return(tibble(model = lab, term = terms, estimate = NA_real_, se = NA_real_, rho2 = NA_real_, n_resp = n_distinct(d$respondent_id), n_tasks = n_distinct(d$task_key), status = "failed"))
  tibble(model = lab, term = names(coef(f)), estimate = unname(coef(f)), se = sqrt(diag(vcov(f))),
         rho2 = 1 - f$loglik[2] / f$loglik[1], n_resp = n_distinct(d$respondent_id), n_tasks = n_distinct(d$task_key),
         status = if (any(!is.finite(sqrt(diag(vcov(f)))) | abs(coef(f)) > 15)) "suspect (separation?)" else "ok")
}
dm <- dm %>% left_join(sv %>% select(respondent_id, segment_id), by = "respondent_id")
cf <- bind_rows(fit_cl(dm, "overall"), map_dfr(sort(unique(sv$segment_id)), ~ fit_cl(filter(dm, segment_id == .x), paste0("segment_", .x)))) %>%
  mutate(reference_levels = paste0("format=", fmt_ref, "; channel=", ch_ref), price_unit = "per 100 NGN",
         data_basis = "synthetic choice tasks; conditional logit (survival::clogit), 'neither' = opt-out constant asc_none")
write_csv(cf, file.path(out_dir, "choice_model_coefficients.csv"))
say("C: rho2 ", paste0(unique(cf$model), "=", round(tapply(cf$rho2, cf$model, first)[unique(cf$model)], 3), collapse = " "))
if (fmt_ref != "fried_seasoned_pack") warning("fried_seasoned_pack not in choice format levels; product utility uses reference format ", fmt_ref)
A("p_choose", "exp(U)/(exp(U)+exp(U_none)), format fried_seasoned_pack", "", "binary product vs opt-out; IGNORES competing products, so it overstates take-up")
A("choice_shelf_life", "shelf_life_days_base of fried_seasoned_pack in that state", "3-5 days", "unit_economics_v1b (synthetic)")

coef_vec <- function(seg_id, draw = FALSE) {
  d <- cf %>% filter(model == paste0("segment_", seg_id))
  if (!nrow(d) || any(is.na(d$estimate)) || any(d$status != "ok")) d <- cf %>% filter(model == "overall")
  b <- setNames(d$estimate, d$term); if (draw) b <- b + rnorm(length(b)) * d$se; b }
seg_fallback <- cf %>% filter(model != "overall") %>% group_by(model) %>% summarise(fallback = any(is.na(estimate)) || any(status != "ok"), .groups = "drop")
p_choose <- function(b, channel, price, shelf) {
  ch_term <- paste0("ch_", channel); bc <- if (channel == ch_ref) 0 else if (ch_term %in% names(b)) b[[ch_term]] else NA_real_
  bf <- if (fmt_ref == "fried_seasoned_pack") 0 else b[["fmt_fried_seasoned_pack"]] %||% NA_real_
  U <- b[["price100"]] * price / 100 + bf + bc + b[["shelf_life"]] * shelf
  1 / (1 + exp(b[["asc_none"]] - U)) }

# ================================================================== STAGE D
surv_ch <- sort(unique(na.omit(sv$preferred_channel)))
unmapped_survey <- setdiff(surv_ch, chan_map$channel); unmapped_choice <- setdiff(chan_map$channel, c(ch_lev))
say("D: survey channels not in mapping: [", paste(unmapped_survey, collapse = ", "), "]; mapped channels with no choice-model level: [", paste(unmapped_choice, collapse = ", "), "]")
seg_stats <- sv %>% group_by(segment_id, segment) %>%
  summarise(eater_share = mean(!is.na(awara_frequency) & awara_frequency != "never") / mean(!is.na(awara_frequency)),
            ppm_per_buyer = mean(PPM[as.character(awara_frequency[!is.na(awara_frequency) & awara_frequency != "never"])]), .groups = "drop")
# USER DECISION 2: split each segment's eaters by preferred channel so the same person is not counted in every channel.
# eater_channel_share = share of the segment (respondents with both answers) who eat awara AND name this channel as preferred.
seg_ch <- sv %>% filter(!is.na(awara_frequency), !is.na(preferred_channel)) %>%
  group_by(segment_id) %>% mutate(n_seg = n()) %>%
  group_by(segment_id, channel = preferred_channel) %>%
  summarise(eater_channel_share = sum(awara_frequency != "never") / first(n_seg), .groups = "drop") %>%
  complete(segment_id, channel = chan_map$channel, fill = list(eater_channel_share = 0))
A("addressable_channel_split", "population x segment share in state x share of segment that eats awara AND prefers this channel", "",
  "user decision: volumes are additive across channels (brief formula counted each eater in every channel)")
seg_intent <- sv %>% group_by(segment_id) %>% summarise(stated_intent_top2box = mean(as.numeric(intent_to_try_packaged) >= 4, na.rm = TRUE), .groups = "drop")
maxp <- sv %>% filter(!is.na(max_price_150g_pack_ngn)) %>% select(segment_id, max_price_150g_pack_ngn)
afford <- function(seg_id, price) mean(maxp$max_price_150g_pack_ngn[maxp$segment_id == seg_id] >= price)
shelf_days <- ue %>% filter(format == "fried_seasoned_pack") %>% select(state, shelf_life_days_base)

soy_rep <- m$soy_per_100k_people[m$soy_status_detail == "reported"]
SOY_LO <- min(soy_rep); SOY_HI <- unname(quantile(soy_rep, 0.25))
A("soy_missing_fill", paste0("Uniform(", round(SOY_LO, 1), ", ", round(SOY_HI, 1), ") t/100k; default run uses midpoint ", round((SOY_LO + SOY_HI) / 2, 1)),
  paste(round(c(SOY_LO, SOY_HI), 1), collapse = "-"), "Enugu, Lagos, Ogun, Oyo, Rivers: no state-level NASS soy; range = min to 25th pct of 'reported' states (Yobe derived excluded); ASSUMPTION, not data")
A("channel_readiness_scaling", "min-max WITHIN channel (pilot states), then min-max across opportunities", "", "channel units differ (outlets/100k vs 0-1 index); pooled min-max would make street_market dominate; DEVIATION from brief, user decision")
A("gym_health_store_density", "supermarket density", "", "flagged proxy (brief)")
A("calibration_discount", CAL_DISCOUNT, "0.3-0.7 (MC)", "brief default; stated-preference overstatement correction")
A("purchases_per_month", "daily=20, weekly=4, monthly=1, rarely=0.5", "", "brief; averaged among eaters in segment")
A("margin", "producer_margin_pct_base, fried_seasoned_pack", "", "rests on REPORTED unit-cost totals (3.3-4.7 pts below recomputed); synthetic")

opp0 <- crossing(ps %>% select(state), seg_stats, sci %>% distinct(channel) ) %>%
  left_join(sci %>% select(state, channel, proxy_flag, channel_readiness_within, channel_density_value, population, soy_per_100k_people, soy_status_detail,
                           shelf_price_low, shelf_price_base, shelf_price_high, reported_total_unit_cost_low, reported_total_unit_cost_base,
                           reported_total_unit_cost_high, retail_margin_pct_base, producer_margin_pct_base, producer_margin_pct_stress), by = c("state", "channel")) %>%
  left_join(sbs %>% select(state, segment_id, segment_share_in_state), by = c("state", "segment_id")) %>%
  left_join(shelf_days, by = "state") %>%
  left_join(seg_ch, by = c("segment_id", "channel")) %>% left_join(seg_intent, by = "segment_id") %>%
  mutate(eater_channel_share = coalesce(eater_channel_share, 0)) %>%
  mutate(segment_share_in_state = coalesce(segment_share_in_state, 0), soy_imputed = is.na(soy_per_100k_people),
         opportunity = paste(state, paste0("S", segment_id), channel, sep = " | "))

score_opps <- function(o, w, disc, price, margin, soy, coefs) {
  o$p_choose <- pmap_dbl(list(o$segment_id, o$channel, price, o$shelf_life_days_base), function(s, c, p, sl) p_choose(coefs[[as.character(s)]], c, p, sl))
  o %>% mutate(price_used = price, margin_used = margin, soy_used = soy,
               addressable_people = population * segment_share_in_state * eater_channel_share,
               p_buy = p_choose * disc,
               monthly_volume_packs = addressable_people * p_buy * ppm_per_buyer,
               monthly_revenue_ngn = monthly_volume_packs * price,
               raw_market_demand = log(pmax(monthly_volume_packs, 1)),
               raw_affordability = map2_dbl(segment_id, price, afford),
               raw_margin = margin, raw_soy_supply = soy, raw_channel_readiness = channel_readiness_within,
               s_market_demand = mm(raw_market_demand), s_affordability = mm(raw_affordability), s_margin = mm(raw_margin),
               s_soy_supply = mm(raw_soy_supply), s_channel_readiness = mm(raw_channel_readiness),
               c_market_demand = w[["market_demand"]] * s_market_demand, c_affordability = w[["affordability"]] * s_affordability,
               c_margin = w[["margin"]] * s_margin, c_soy_supply = w[["soy_supply"]] * s_soy_supply,
               c_channel_readiness = w[["channel_readiness"]] * s_channel_readiness,
               score = c_market_demand + c_affordability + c_margin + c_soy_supply + c_channel_readiness,
               rank = rank(-score, ties.method = "min"))
}
segs <- as.character(sort(unique(sv$segment_id)))
coefs0 <- setNames(map(segs, ~ coef_vec(.x)), segs)
soy0 <- coalesce(opp0$soy_per_100k_people, (SOY_LO + SOY_HI) / 2)
base <- score_opps(opp0, W0, CAL_DISCOUNT, opp0$shelf_price_base, opp0$producer_margin_pct_base, soy0, coefs0) %>% arrange(rank)
if (any(is.na(base$score))) warning(sum(is.na(base$score)), " opportunities have NA score (channel without choice coefficient?)")
crit <- c("market_demand", "affordability", "margin", "soy_supply", "channel_readiness")
cm <- cor(base %>% select(all_of(paste0("s_", crit))), use = "pairwise.complete.obs")
say("D: criteria correlation (0-100 scores, across ", nrow(base), " opportunities):"); print(round(cm, 2))
write_csv(as_tibble(round(cm, 3), rownames = "criterion") %>% mutate(note = "weights NOT changed because of collinearity (brief)"), file.path(out_dir, "criteria_correlation.csv"))

# ================================================================== STAGE E
rtri <- function(n, a, c, b) { u <- runif(n); out <- numeric(n); deg <- b - a <= 0
  Fc <- ifelse(deg, 0, (c - a) / (b - a)); lo <- u < Fc
  out[lo] <- (a + sqrt(u * (b - a) * (c - a)))[lo]; out[!lo] <- (b - sqrt((1 - u) * (b - a) * (b - c)))[!lo]; out[deg] <- a[deg]; out }
rdir <- function(w, conc) { g <- rgamma(length(w), shape = conc * w); setNames(g / sum(g), names(w)) }
st <- ps$state; idx <- match(opp0$state, st)
ue_s <- opp0 %>% distinct(state, shelf_price_low, shelf_price_base, shelf_price_high, reported_total_unit_cost_low,
                          reported_total_unit_cost_base, reported_total_unit_cost_high, retail_margin_pct_base, soy_per_100k_people) %>% arrange(match(state, st))
run_mc <- function(soy_weight_zero = FALSE) {
  set.seed(42)
  ranks <- matrix(NA_integer_, N_MC, nrow(opp0)); scores <- matrix(NA_real_, N_MC, nrow(opp0))
  S_draw <- array(NA_real_, c(N_MC, nrow(opp0), length(crit)), dimnames = list(NULL, opp0$opportunity, crit))  # for the app
  V_draw <- matrix(NA_real_, N_MC, nrow(opp0))
  for (i in seq_len(N_MC)) {
    w <- rdir(W0, DIR_CONC); if (soy_weight_zero) { w["soy_supply"] <- 0; w <- w / sum(w) }
    disc <- runif(1, 0.3, 0.7)
    P <- rtri(nrow(ue_s), ue_s$shelf_price_low, ue_s$shelf_price_base, ue_s$shelf_price_high)
    C <- rtri(nrow(ue_s), ue_s$reported_total_unit_cost_low, ue_s$reported_total_unit_cost_base, ue_s$reported_total_unit_cost_high)
    M <- 100 * (P * (1 - ue_s$retail_margin_pct_base / 100) - C) / P
    S <- ifelse(is.na(ue_s$soy_per_100k_people), runif(nrow(ue_s), SOY_LO, SOY_HI), ue_s$soy_per_100k_people)
    cfs <- setNames(map(segs, ~ coef_vec(.x, draw = TRUE)), segs)
    r <- score_opps(opp0, w, disc, P[idx], M[idx], S[idx], cfs)
    ranks[i, ] <- r$rank; scores[i, ] <- r$score
    for (k in seq_along(crit)) S_draw[i, , k] <- round(r[[paste0("s_", crit[k])]], 2)
    V_draw[i, ] <- r$monthly_volume_packs
  }
  list(ranks = ranks, scores = scores, S = S_draw, V = V_draw) }
say("E: Monte Carlo N=", N_MC, " ...")
mcA <- run_mc(FALSE); mcB <- run_mc(TRUE)
saveRDS(list(S = mcA$S, volume_p10 = apply(mcA$V, 2, quantile, 0.1), volume_p90 = apply(mcA$V, 2, quantile, 0.9),
             opportunity = opp0$opportunity, criteria = crit, default_weights = W0, n = N_MC,
             note = "criterion scores (0-100) per Monte Carlo draw; app recombines with user weights"),
        file.path(out_dir, "mc_criteria_draws.rds"), compress = "xz")
summ <- function(mcx, tag) tibble(opportunity = opp0$opportunity,
  !!paste0(tag, "median_score") := apply(mcx$scores, 2, median), !!paste0(tag, "p10") := apply(mcx$scores, 2, quantile, 0.1, na.rm = TRUE),
  !!paste0(tag, "p90") := apply(mcx$scores, 2, quantile, 0.9, na.rm = TRUE), !!paste0(tag, "median_rank") := apply(mcx$ranks, 2, median),
  !!paste0(tag, "P_rank1") := colMeans(mcx$ranks == 1), !!paste0(tag, "P_top3") := colMeans(mcx$ranks <= 3),
  !!paste0(tag, "P_top5") := colMeans(mcx$ranks <= 5), !!paste0(tag, "P_top10") := colMeans(mcx$ranks <= 10))
base_soy0 <- score_opps(opp0, { w <- W0; w["soy_supply"] <- 0; w / sum(w) }, CAL_DISCOUNT, opp0$shelf_price_base, opp0$producer_margin_pct_base, soy0, coefs0)
ranked <- base %>% select(opportunity, state, segment_id, segment, channel, proxy_flag, soy_imputed, default_rank = rank, default_score = score,
                          addressable_people, eater_channel_share, p_choose, p_buy, stated_intent_top2box,
                          monthly_volume_packs, monthly_revenue_ngn, raw_affordability, producer_margin_pct_base, producer_margin_pct_stress) %>%
  mutate(model_vs_intent = case_when(p_buy > stated_intent_top2box ~ "choice model more optimistic than stated intent",
                                     TRUE ~ "consistent: stated intent >= p_buy")) %>%
  left_join(summ(mcA, ""), by = "opportunity") %>% left_join(summ(mcB, "soy0_"), by = "opportunity") %>%
  left_join(base_soy0 %>% select(opportunity, soy0_default_rank = rank), by = "opportunity") %>%
  arrange(default_rank) %>%
  mutate(data_basis = "mixed: population/soy REAL; survey, choice, unit economics, channel SYNTHETIC; soy imputed (assumption) where soy_imputed=TRUE")
write_csv(ranked, file.path(out_dir, "ranked_opportunities.csv"))
top_ids <- head(ranked$opportunity, 15); jj <- match(top_ids, opp0$opportunity)
rs <- map_dfr(seq_along(jj), function(k) tibble(opportunity = top_ids[k], rank = 1:15, prob = map_dbl(1:15, ~ mean(mcA$ranks[, jj[k]] == .x)))) %>%
  mutate(data_basis = "Monte Carlo N=2000 over weights, discount, price, cost, choice coefficients, missing soy (synthetic inputs)")
write_csv(rs, file.path(out_dir, "rank_stability.csv"))

# sensitivity (deterministic): each weight and the discount x0.5 and x1.5; other weights rescaled to sum 1
scen <- c(map(names(W0), function(n) map(c(0.5, 1.5), function(f) { w <- W0; w[n] <- w[n] * f; o <- setdiff(names(w), n)
  w[o] <- w[o] * (1 - w[n]) / sum(W0[o]); list(name = paste0(n, " x", f), w = w, d = CAL_DISCOUNT) })) %>% unlist(recursive = FALSE),
  list(list(name = "discount x0.5", w = W0, d = CAL_DISCOUNT * 0.5), list(name = "discount x1.5", w = W0, d = CAL_DISCOUNT * 1.5)))
top10 <- head(base$opportunity, 10)
sens <- map_dfr(scen, function(s) score_opps(opp0, s$w, s$d, opp0$shelf_price_base, opp0$producer_margin_pct_base, soy0, coefs0) %>%
  filter(opportunity %in% top10) %>% transmute(scenario = s$name, opportunity, new_rank = rank)) %>%
  left_join(base %>% select(opportunity, default_rank = rank), by = "opportunity") %>% mutate(rank_change = new_rank - default_rank,
  note = "discount scales every p_buy equally, so log volume shifts by a constant and min-max removes it: ranks cannot change",
  data_basis = "deterministic re-score; synthetic + real inputs")
write_csv(sens, file.path(out_dir, "sensitivity.csv"))
sens_sum <- sens %>% group_by(scenario) %>% summarise(max_abs_change = max(abs(rank_change)), n_changed = sum(rank_change != 0), .groups = "drop") %>% arrange(desc(max_abs_change))
print(sens_sum)

# ================================================================== STAGE F
basis <- c(market_demand = "mixed: real population x synthetic survey shares x synthetic choice model",
           affordability = "synthetic survey max price vs synthetic shelf price", margin = "synthetic unit economics (reported totals)",
           soy_supply = "real NASS 2022/23 (imputed range for Enugu/Lagos/Ogun/Oyo/Rivers)", channel_readiness = "synthetic placeholder, collinear with wealth")
contrib <- map_dfr(crit, ~ base %>% transmute(opportunity, default_rank = rank, criterion = .x, raw_value = .data[[paste0("raw_", .x)]],
                   score_0_100 = .data[[paste0("s_", .x)]], weight = W0[[.x]], contribution = .data[[paste0("c_", .x)]], data_basis = basis[[.x]])) %>%
  arrange(default_rank, criterion)
write_csv(contrib, file.path(out_dir, "opportunity_contributions.csv"))
mean_c <- contrib %>% group_by(criterion) %>% summarise(mean_contrib = mean(contribution), .groups = "drop")
llm <- map(head(ranked$opportunity, 10), function(o) {
  r <- ranked %>% filter(opportunity == o); b <- base %>% filter(opportunity == o)
  dv <- contrib %>% filter(opportunity == o) %>% left_join(mean_c, by = "criterion") %>% mutate(vs_mean = contribution - mean_contrib)
  list(rank = r$default_rank, state = r$state, segment = r$segment, channel = r$channel, score = round(r$default_score, 2),
       score_p10 = round(r$p10, 2), score_p90 = round(r$p90, 2), P_top3 = round(r$P_top3, 3),
       # driver = contribution minus the mean contribution of that criterion across all opportunities
       top_positive_drivers = dv %>% filter(vs_mean > 0) %>% arrange(desc(vs_mean)) %>% head(3) %>% transmute(criterion, contribution_vs_mean = round(vs_mean, 2)),
       top_negative_drivers = dv %>% filter(vs_mean < 0) %>% arrange(vs_mean) %>% head(3) %>% transmute(criterion, contribution_vs_mean = round(vs_mean, 2)),
       addressable_people = round(b$addressable_people), p_choose_model = round(b$p_choose, 4), p_buy = round(b$p_buy, 4),
       stated_intent_top2box = round(b$stated_intent_top2box, 3), share_affording_shelf_price = round(b$raw_affordability, 3),
       shelf_price_ngn = b$price_used, monthly_volume_packs = round(b$monthly_volume_packs),
       volume_p10 = round(quantile(mcA$V[, match(o, opp0$opportunity)], 0.1)), volume_p90 = round(quantile(mcA$V[, match(o, opp0$opportunity)], 0.9)),
       monthly_revenue_ngn = round(b$monthly_revenue_ngn), margin_pct_base = b$producer_margin_pct_base, margin_pct_stress = b$producer_margin_pct_stress,
       channel_density_proxy = b$proxy_flag, soy_imputed = b$soy_imputed, data_basis = as.list(basis),
       caveats = c("p_buy ignores competing products", "survey, choice, unit economics and channel data are synthetic",
                   if (b$p_buy > b$stated_intent_top2box) "choice model is more optimistic than the segment's stated intent; the pilot should test this",
                   if (b$raw_affordability < 0.25) "fewer than 1 in 4 of this segment say they would pay the shelf price",
                   if (b$soy_imputed) "soy value is an assumed range, not data", if (b$proxy_flag) "channel density is a supermarket proxy",
                   if (b$producer_margin_pct_stress < 0) "margin negative under stress"))
})
write_json(list(generated = as.character(Sys.Date()), product = "fried_seasoned_pack 150 g", top10 = llm,
                key_assumptions = assum$name), file.path(out_dir, "llm_input_top10.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)

A("weights_default", paste(names(W0), W0, sep = "=", collapse = "; "), "Dirichlet conc 30 in MC", "brief")
A("price_cost_mc", "triangular(low, base, high) per state; cost = reported_total_unit_cost_*", "", "unit_economics_v1b (synthetic); retail margin held at base")
A("coef_mc", "independent Normal(estimate, SE) per coefficient", "", "covariances ignored; failed/suspect segment models fall back to overall")
A("stageA_affordability", "mean of wealth_index and wealth_upper_share scores", "", "brief gives one 0.4 weight for two criteria")
A("stageA_soy_NA", "weights renormalised over available criteria", "", "brief; rewards soy-NA states (see screening_score_soy_as_0)")
write_csv(assum, file.path(out_dir, "assumptions.csv"))

# charts
th <- theme_minimal(base_size = 9)
g1 <- ranked %>% head(20) %>% ggplot(aes(x = reorder(opportunity, -default_rank), y = median_score)) +
  geom_pointrange(aes(ymin = p10, ymax = p90, colour = soy_imputed)) + coord_flip() + th +
  labs(x = NULL, y = "Score (median, p10-p90)", title = "Top 20 opportunities, Monte Carlo N=2000",
       subtitle = "Data basis: real population/soy; synthetic survey, choice, unit economics, channels")
ggsave(file.path(out_dir, "chart_ranked_opportunities.png"), g1, width = 8, height = 6, dpi = 150)
w1 <- contrib %>% filter(default_rank == 1) %>% mutate(end = cumsum(contribution), start = end - contribution, id = row_number())
g2 <- ggplot(w1, aes(xmin = id - 0.4, xmax = id + 0.4, ymin = start, ymax = end)) + geom_rect(fill = "#2a6f97") +
  scale_x_continuous(breaks = w1$id, labels = w1$criterion) + th +
  labs(x = NULL, y = "Cumulative score", title = paste("Score build-up, rank 1:", w1$opportunity[1]), subtitle = "Data basis: mixed real + synthetic (see opportunity_contributions.csv)")
ggsave(file.path(out_dir, "chart_waterfall_rank1.png"), g2, width = 7, height = 4, dpi = 150)
g3 <- ggplot(rs, aes(x = rank, y = factor(opportunity, levels = rev(top_ids)), fill = prob)) + geom_tile() +
  scale_fill_gradient(low = "white", high = "#2a6f97") + th + labs(x = "Rank", y = NULL, fill = "P(rank)",
  title = "Rank stability, top 15 (MC N=2000)", subtitle = "Data basis: synthetic + real inputs, sampled uncertainty")
ggsave(file.path(out_dir, "chart_rank_stability.png"), g3, width = 8, height = 5, dpi = 150)
g4 <- ggplot(sens_sum, aes(x = reorder(scenario, max_abs_change), y = max_abs_change)) + geom_col(fill = "#b5838d") + coord_flip() + th +
  labs(x = NULL, y = "Max |rank change| among default top 10", title = "Sensitivity: weights and discount x0.5 / x1.5",
       subtitle = "Data basis: deterministic re-score, synthetic + real inputs")
ggsave(file.path(out_dir, "chart_sensitivity.png"), g4, width = 7, height = 4, dpi = 150)

say("\nTOP 5:"); print(ranked %>% head(5) %>% transmute(default_rank, opportunity, median_score = round(median_score, 1), P_top3 = round(P_top3, 2), P_top5 = round(P_top5, 2), soy0_default_rank))
say("segment fallback to overall model: ", paste(seg_fallback$model[seg_fallback$fallback], collapse = ", "))
