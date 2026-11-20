# R/price_model.R - re-score demand, affordability and margin at a chosen pack price (150 g pack).
# Uses data/price_model.rds (exported by scripts/03_BCDEF_pipeline.R); row i of every draw matrix pairs with draw i
# of mc_criteria_draws.rds. Scores stay on the STANDARD-PRICE scale: each draw's min/max at the standard shelf
# prices is reused, so a cheaper price shows up as higher affordability/demand scores and lower margin scores.
# Scores can therefore leave 0-100; the UI clamps what it displays. Soy supply and channel readiness do not depend on price.
# Definitions only: shiny::runApp() sources R/*.R BEFORE app.R, so nothing here may touch app objects at load time.
# app.R calls pm_load() once mc, S0m, OPP, CRIT and meta exist. If the file is missing, pm stays NULL and the price
# controls are hidden.

PRICE_CRIT <- c("market_demand", "affordability", "margin")
PM_MIN_DEN <- 0.05   # target-margin pricing: 1 - retail margin - target margin is capped at this
pm <- NULL; .pm_wtp <- NULL; .pm_sc <- NULL
pm_load <- function(path = "data/price_model.rds") {
  if (!file.exists(path)) return(invisible(NULL))
  x <- readRDS(path)
  stopifnot(identical(x$opp$opportunity, OPP), nrow(x$draws$P) == mc$n)
  # per-state unit cost and retail margin (fried 150 g pack), as in state_channel_inputs.csv
  sc <- read_csv("data/state_channel_inputs.csv", show_col_types = FALSE) %>%
    distinct(state, reported_total_unit_cost_base, retail_margin_pct_base)
  stopifnot(!anyDuplicated(sc$state), setequal(sc$state, unique(x$opp$state)))
  pm <<- x; .pm_sc <<- sc; .pm_wtp <<- lapply(split(x$wtp$max_price_ngn, x$wtp$segment_id), sort)
  invisible(x)
}

# Target-margin pricing: price_s = unit_cost_base_s / (1 - retail_margin_base_s - m), m = target producer margin.
# In the Monte Carlo draws m stays fixed and the price follows each draw's unit cost.
margin_pricing <- function(margin_pct) {
  sc <- .pm_sc; den <- 1 - sc$retail_margin_pct_base / 100 - margin_pct / 100
  capped <- den <= PM_MIN_DEN; den <- pmax(den, PM_MIN_DEN)
  price <- setNames(sc$reported_total_unit_cost_base / den, sc$state); rng <- pm$price_range_tested
  structure(list(key = paste0("margin:", margin_pct), margin_pct = margin_pct, den = setNames(den, sc$state), price = price,
                 capped = sc$state[capped], out_of_range = sc$state[price < rng[1] | price > rng[2]]), class = "pm_pricing")
}

# resolve a price argument into a point price per opportunity and a draw x opportunity price matrix (NULL = standard)
.pm_resolve <- function(price) {
  if (is.null(price)) return(NULL)
  o <- pm$opp; d <- pm$draws; si <- d$state_idx; n <- nrow(d$P)
  if (inherits(price, "pm_pricing")) {
    den <- unname(price$den[o$state])
    list(key = price$key, point = o$reported_total_unit_cost_base / den, draws = sweep(d$C[, si, drop = FALSE], 2, den, `/`))
  } else list(key = as.character(price), point = rep(price, nrow(o)), draws = matrix(price, n, nrow(o)))
}
# demand uses the price clamped to the range tested in the choice tasks (affordability and margin use the real price)
.pm_demand_price <- function(P) { r <- pm$price_range_tested; pmin(pmax(P, r[1]), r[2]) }
# share of the segment whose max price >= p (same as mean(maxp >= p) in the pipeline), for a vector/matrix of prices
pm_afford <- function(seg, p) {
  out <- p
  for (s in unique(seg)) { w <- .pm_wtp[[as.character(s)]]; j <- seg == s
    pj <- if (is.matrix(p)) p[, j, drop = FALSE] else p[j]
    out_j <- (length(w) - findInterval(pj, w, left.open = TRUE)) / length(w)
    if (is.matrix(p)) out[, j] <- out_j else out[j] <- out_j }
  out
}

# raw criterion values per Monte Carlo draw (n x 180). price = NULL -> each draw's own sampled shelf price (standard)
pm_raw_draws <- function(price = NULL) {
  o <- pm$opp; d <- pm$draws; si <- d$state_idx; n <- nrow(d$P); k <- nrow(o); seg <- as.character(o$segment_id)
  rp <- .pm_resolve(price)
  P <- if (is.null(rp)) d$P[, si, drop = FALSE] else rp$draws
  Pd <- if (is.null(rp)) P else .pm_demand_price(P)   # standard path untouched so it reproduces the pipeline exactly
  C <- d$C[, si, drop = FALSE]
  pc <- 1 / (1 + exp(d$asc_none[, seg, drop = FALSE] - (d$b_price[, seg, drop = FALSE] * Pd / 100 + d$u_nonprice)))
  vol <- sweep(pc * d$disc, 2, o$addressable_people, `*`); vol <- sweep(vol, 2, o$ppm_per_buyer, `*`)
  rm <- matrix(o$retail_margin_pct_base, n, k, byrow = TRUE)
  list(market_demand = log(pmax(vol, 1)), affordability = pm_afford(o$segment_id, P),
       margin = 100 * (P * (1 - rm / 100) - C) / P, volume = vol)
}

# raw point values (one per opportunity), as score_opps() at the default discount
pm_raw_point <- function(price = NULL) {
  o <- pm$opp; rp <- .pm_resolve(price); p <- if (is.null(rp)) o$shelf_price_base else rp$point
  pd <- if (is.null(rp)) p else .pm_demand_price(p)
  pc <- 1 / (1 + exp(o$asc_none_point - (o$b_price_point * pd / 100 + o$u_nonprice_point)))
  vol <- o$addressable_people * (pc * pm$calibration_discount) * o$ppm_per_buyer
  margin <- if (is.null(price)) o$producer_margin_pct_base   # pipeline point score uses the table value
            else 100 * (p * (1 - o$retail_margin_pct_base / 100) - o$reported_total_unit_cost_base) / p
  stress <- if (is.null(price)) o$producer_margin_pct_stress
            else 100 * (p * (1 - o$retail_margin_pct_high / 100) - o$reported_total_unit_cost_high) / p
  list(market_demand = log(pmax(vol, 1)), affordability = pm_afford(o$segment_id, p), margin = margin,
       stress = stress, volume = vol, p_buy = pc * pm$calibration_discount, price = p)
}

# min-max with fixed bounds; identical to the pipeline's mm() when lo/hi are the values' own range
.pm_scale <- function(x, lo, hi) { d <- hi - lo; out <- 100 * (x - lo) / d; out[!is.finite(out) & d == 0] <- 50; out }
.pm_std <- NULL
pm_standard <- function() {
  if (is.null(.pm_std)) {
    rd <- pm_raw_draws(NULL); rp <- pm_raw_point(NULL)
    .pm_std <<- list(raw = rd, point = rp,
      lo = sapply(PRICE_CRIT, function(k) apply(rd[[k]], 1, min, na.rm = TRUE)), hi = sapply(PRICE_CRIT, function(k) apply(rd[[k]], 1, max, na.rm = TRUE)),
      plo = sapply(PRICE_CRIT, function(k) min(rp[[k]], na.rm = TRUE)), phi = sapply(PRICE_CRIT, function(k) max(rp[[k]], na.rm = TRUE)))
  }
  .pm_std
}

# criteria at a fixed price: S (draw array like mc$S), S0 (point matrix like S0m) and display values. Cached per price.
.pm_cache <- new.env()
criteria_at_price <- function(price) {
  key <- .pm_resolve(price)$key; hit <- .pm_cache[[key]]; if (!is.null(hit)) return(hit)
  std <- pm_standard(); rd <- pm_raw_draws(price); rp <- pm_raw_point(price)
  S <- mc$S; S0 <- S0m
  for (k in PRICE_CRIT) {
    S[, , k] <- .pm_scale(rd[[k]], std$lo[, k], std$hi[, k])          # vector lo/hi recycles down rows (one per draw)
    S0[, k] <- .pm_scale(rp[[k]], std$plo[[k]], std$phi[[k]])
  }
  o <- pm$opp
  out <- list(price = price, S = S, S0 = S0,
    display = tibble(opportunity = o$opportunity, shelf_price_base = rp$price, raw_affordability = rp$affordability,
                     producer_margin_pct_base = round(rp$margin, 1), producer_margin_pct_stress = round(rp$stress, 1),
                     p_buy = rp$p_buy, monthly_volume_packs = rp$volume,
                     volume_p10 = apply(rd$volume, 2, quantile, 0.1), volume_p90 = apply(rd$volume, 2, quantile, 0.9)))
  assign(key, out, envir = .pm_cache); out
}

# meta with the price-dependent display columns replaced (price = NULL -> meta unchanged)
meta_at_price <- function(price = NULL) {
  if (is.null(price)) return(meta)
  dsp <- criteria_at_price(price)$display
  meta %>% select(-any_of(setdiff(names(dsp), "opportunity"))) %>% left_join(dsp, by = "opportunity")
}

# break-even shelf price per state at base cost and base retail margin
break_even_price <- function(st) {
  o <- pm$opp[match(st, pm$opp$state), ]; o$reported_total_unit_cost_base / (1 - o$retail_margin_pct_base / 100)
}

# self-test: recomputing at each draw's own sampled price must reproduce the pipeline's stored scores
price_model_selftest <- function() {
  std <- pm_standard(); rd <- std$raw; rp <- std$point; o <- pm$opp
  draws <- sapply(PRICE_CRIT, function(k) max(abs(.pm_scale(rd[[k]], std$lo[, k], std$hi[, k]) - mc$S[, , k]), na.rm = TRUE))
  point <- sapply(PRICE_CRIT, function(k) max(abs(.pm_scale(rp[[k]], std$plo[[k]], std$phi[[k]]) - S0m[, k]), na.rm = TRUE))
  m <- meta[match(o$opportunity, meta$opportunity), ]
  rk_p <- rk[match(o$opportunity, rk$opportunity), ]
  rel <- function(a, b) max(ifelse(b == 0, abs(a), abs(a / b - 1)))   # zero volumes (no addressable people) must stay exactly 0
  list(draw_scores_max_abs_diff = draws, point_scores_max_abs_diff = point,
       p_buy_max_rel_diff = rel(rp$p_buy, rk_p$p_buy),
       volume_max_rel_diff = rel(rp$volume, m$monthly_volume_packs),
       affordability_max_abs_diff = max(abs(rp$affordability - m$raw_affordability)),
       volume_p10_max_rel_diff = rel(apply(rd$volume, 2, quantile, 0.1), mc$volume_p10),
       volume_p90_max_rel_diff = rel(apply(rd$volume, 2, quantile, 0.9), mc$volume_p90),
       stress_formula_max_abs_diff = max(abs(100 * (o$shelf_price_base * (1 - o$retail_margin_pct_high / 100) - o$reported_total_unit_cost_high) /
                                             o$shelf_price_base - o$producer_margin_pct_stress)),
       base_margin_formula_max_abs_diff = max(abs(100 * (o$shelf_price_base * (1 - o$retail_margin_pct_base / 100) - o$reported_total_unit_cost_base) /
                                                  o$shelf_price_base - o$producer_margin_pct_base)))
}
