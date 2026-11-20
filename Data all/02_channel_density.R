# =============================================================================
# 02_channel_density.R | Hack the Fork, Nigeria awara project
# SYNTHETIC PLACEHOLDER channel density. Real source would be field verification
# or a local business listing (OSM coverage in Nigeria is unreliable).
# Every number produced here is illustrative, NOT data. set.seed(42).
# Run: Rscript 02_channel_density.R   (IN_DIR / OUT_DIR env vars override paths)
# =============================================================================
suppressPackageStartupMessages({ library(tidyverse) })
set.seed(42)
out_dir <- Sys.getenv("OUT_DIR", "/mnt/user-data/outputs")
in_dir  <- Sys.getenv("IN_DIR",  out_dir)
m <- read_csv(file.path(in_dir, "master_states.csv"), show_col_types = FALSE, progress = FALSE) %>% arrange(state)
stopifnot(all(c("state", "population", "urban_share", "wealth_index") %in% names(m)))

BASIS <- "synthetic placeholder (real source: field verification/OSM)"

# ---- inputs -----------------------------------------------------------------
# urban_share is NA in master_states.csv (no source supplied). If it is ever filled, it is used as is.
# Otherwise a STAND-IN is used: wealth_index min-max scaled to 0-1 across the 37 states (illustrative only).
w01 <- (m$wealth_index - min(m$wealth_index)) / (max(m$wealth_index) - min(m$wealth_index))
urban_missing <- all(is.na(m$urban_share))
u <- if (urban_missing) w01 else m$urban_share
w <- w01
urban_input <- if (urban_missing) "urban_share NA in master: STAND-IN = wealth_index scaled 0-1 (urban and wealth are therefore collinear)" else "urban_share from master"
cat("urban input:", urban_input, "\n")

uni_states   <- c("Lagos", "Oyo", "Kaduna", "FCT", "Enugu", "Rivers", "Plateau", "Kano")   # major university clusters
deliv_states <- c("Lagos", "FCT", "Rivers", "Oyo")
noise <- function(n) exp(rnorm(n, 0, 0.15))                      # lognormal, sd 0.15 (log scale)
clipv <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# ---- generation (illustrative assumptions; see assumption_note) ---------------
raw <- tibble(state = m$state)
raw$street_market      <- 150 + 300 * (0.85 * u + 0.15 * (1 - w))                 # mildly lower when wealthy
raw$supermarket        <- 3 + 27 * (u * w)^0.8
raw$restaurant         <- 30 + 170 * (0.5 * u + 0.5 * w)
raw$campus_institution <- (0.5 + 2.5 * (0.5 * u + 0.5 * w)) * ifelse(m$state %in% uni_states, 1.8, 1)
raw$delivery_app       <- (0.02 + 0.9 * (u * w)^1.2) * ifelse(m$state %in% deliv_states, 1.3, 1)
channels <- c("street_market", "supermarket", "restaurant", "campus_institution", "delivery_app")
lims <- list(street_market = c(150, 450), supermarket = c(3, 30), restaurant = c(30, 200), campus_institution = c(0.5, 6), delivery_app = c(0, 1))
unit <- c(street_market = "outlets per 100,000 people", supermarket = "outlets per 100,000 people", restaurant = "outlets per 100,000 people",
          campus_institution = "outlets per 100,000 people", delivery_app = "readiness index 0-1 (not an outlet count)")
note <- c(
  street_market = "Illustrative: 150 + 300*(0.85*urban + 0.15*(1-wealth01)), lognormal noise sd 0.15, clipped 150-450",
  supermarket = "Illustrative: 3 + 27*(urban*wealth01)^0.8, lognormal noise sd 0.15, clipped 3-30",
  restaurant = "Illustrative: 30 + 170*(0.5*urban + 0.5*wealth01), lognormal noise sd 0.15, clipped 30-200",
  campus_institution = "Illustrative: (0.5 + 2.5*(0.5*urban + 0.5*wealth01)), x1.8 for Lagos/Oyo/Kaduna/FCT/Enugu/Rivers/Plateau/Kano, noise sd 0.15, clipped 0.5-6",
  delivery_app = "Illustrative readiness: 0.02 + 0.9*(urban*wealth01)^1.2, x1.3 for Lagos/FCT/Rivers/Oyo, noise sd 0.15, clipped 0-1")

clipped_n <- integer(0)
for (ch in channels) {                                             # fixed channel order keeps the seed stream reproducible
  v <- raw[[ch]] * noise(nrow(raw)); lo <- lims[[ch]][1]; hi <- lims[[ch]][2]
  clipped_n[ch] <- sum(v < lo | v > hi)
  raw[[ch]] <- clipv(v, lo, hi)
}
raw <- raw %>% mutate(across(all_of(channels), ~ round(.x, ifelse(cur_column() %in% c("campus_institution", "delivery_app"), 3, 1))))

long <- raw %>% pivot_longer(all_of(channels), names_to = "channel", values_to = "value") %>%
  mutate(channel = factor(channel, levels = channels), unit = unit[as.character(channel)], data_basis = BASIS,
         assumption_note = paste0(note[as.character(channel)], "; ", urban_input)) %>% arrange(state, channel) %>% mutate(channel = as.character(channel))
wide <- raw %>% rename(channel_street_market_per100k = street_market, channel_supermarket_per100k = supermarket,
                       channel_restaurant_per100k = restaurant, channel_campus_institution_per100k = campus_institution,
                       channel_delivery_app_readiness_0_1 = delivery_app) %>% mutate(channel_basis = BASIS)
write_csv(long, file.path(out_dir, "channel_density_long.csv"), na = "")
write_csv(wide, file.path(out_dir, "channel_density_wide.csv"), na = "")
write_csv(m %>% left_join(wide, by = "state") %>% mutate(channel_urban_input_note = urban_input), file.path(out_dir, "master_states_v2.csv"), na = "")

# ---- checks -----------------------------------------------------------------
cat("\nCLIPPED values (hit a bound after noise):", paste0(names(clipped_n), "=", clipped_n, collapse = ", "), "\n")
for (ch in channels) {
  d <- raw %>% select(state, v = all_of(ch)) %>% arrange(desc(v))
  cat(sprintf("%-19s top3: %s | bottom3: %s\n", ch, paste0(head(d$state, 3), " ", head(d$v, 3), collapse = ", "),
              paste0(tail(d$state, 3), " ", tail(d$v, 3), collapse = ", ")))
}
rk <- function(ch, s) { v <- raw[[ch]]; rank(-v, ties.method = "min")[raw$state == s] }
for (ch in c("supermarket", "delivery_app")) cat("Rank", ch, "- Lagos:", rk(ch, "Lagos"), " FCT:", rk(ch, "FCT"), " (of", nrow(raw), ")\n")
cat("Both lead (top 2) on supermarket and delivery_app:",
    all(c(rk("supermarket", "Lagos"), rk("supermarket", "FCT"), rk("delivery_app", "Lagos"), rk("delivery_app", "FCT")) <= 2), "\n")
cat("Rank delivery_app Rivers:", rk("delivery_app", "Rivers"), " Oyo:", rk("delivery_app", "Oyo"), "\n")
cat("Corr(street_market, supermarket):", round(cor(raw$street_market, raw$supermarket), 2), "\n")

# ---- data dictionary (append, idempotent) ---------------------------------------
dd <- read_csv(file.path(out_dir, "data_dictionary.csv"), show_col_types = FALSE) %>%
  filter(!str_detect(column, "^channel_|^ASSUMPTION_CH"))
nr <- function(column, meaning, unit, source, basis = BASIS) tibble(column = column, meaning = meaning, unit = unit, source = source, data_basis = basis)
dd <- bind_rows(dd,
  nr("channel_street_market_per100k", "SYNTHETIC open markets and roadside vendors per 100k people", "outlets per 100,000 people", "02_channel_density.R"),
  nr("channel_supermarket_per100k", "SYNTHETIC supermarkets and grocery stores per 100k people", "outlets per 100,000 people", "02_channel_density.R"),
  nr("channel_restaurant_per100k", "SYNTHETIC restaurants, bukas, canteens per 100k people", "outlets per 100,000 people", "02_channel_density.R"),
  nr("channel_campus_institution_per100k", "SYNTHETIC universities, colleges, school canteens per 100k people", "outlets per 100,000 people", "02_channel_density.R"),
  nr("channel_delivery_app_readiness_0_1", "SYNTHETIC delivery app readiness index (not an outlet count)", "index 0-1", "02_channel_density.R"),
  nr("channel_basis", "Flag: all channel_* columns are synthetic placeholders", "text", "02_channel_density.R"),
  nr("channel_urban_input_note", "Which urban input fed the synthetic channels", "text", "02_channel_density.R"),
  nr("ASSUMPTION_CH1", "All channel values are illustrative and invented; real source would be field verification or a local business listing", NA, "02_channel_density.R"),
  nr("ASSUMPTION_CH2", "urban_share is NA in master_states.csv, so wealth_index min-max scaled 0-1 across states is used as the urban stand-in; urban and wealth effects are collinear until urban_share is supplied", NA, "02_channel_density.R"),
  nr("ASSUMPTION_CH3", "Functional forms, ranges, university-cluster boost (x1.8) and delivery boost (x1.3) are my illustrative choices from the brief; lognormal noise sd 0.15 then clip", NA, "02_channel_density.R"),
  nr("ASSUMPTION_CH4", "Channel values are independent of population size (per-100k rates); outlets per state = rate * population / 1e5 if needed", NA, "02_channel_density.R"))
write_csv(dd, file.path(out_dir, "data_dictionary.csv"), na = "")

# ---- heatmap -------------------------------------------------------------------
lab <- c(street_market = "Street market\n(per 100k)", supermarket = "Supermarket\n(per 100k)", restaurant = "Restaurant\n(per 100k)",
         campus_institution = "Campus/institution\n(per 100k)", delivery_app = "Delivery app\n(readiness 0-1)")
pl <- long %>% group_by(channel) %>% mutate(scaled = (value - min(value)) / (max(value) - min(value))) %>% ungroup() %>%
  mutate(channel = factor(channel, levels = channels, labels = lab[channels]),
         state = factor(state, levels = m$state[order(m$wealth_index)]),
         txt = ifelse(channel == lab["delivery_app"] | channel == lab["campus_institution"], sprintf("%.2f", value), sprintf("%.0f", value)))
g <- ggplot(pl, aes(channel, state, fill = scaled)) + geom_tile(colour = "white") + geom_text(aes(label = txt), size = 2.4) +
  scale_fill_gradient(low = "#f7fbff", high = "#2171b5", name = "Relative level\n(within channel)") +
  labs(title = "Synthetic placeholder: channel density by state and channel",
       subtitle = "Illustrative values, NOT data. Colour is scaled within each channel; numbers are raw values. States ordered by wealth_index (top = richest).",
       caption = paste0("Real source would be field verification or a local business listing. ", ifelse(urban_missing, "Urban input = wealth stand-in (urban_share missing).", "")),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) + theme(panel.grid = element_blank(), plot.title = element_text(face = "bold", colour = "#b30000"),
                                        plot.subtitle = element_text(size = 7.5), plot.caption = element_text(size = 7), axis.text.x = element_text(size = 8))
ggsave(file.path(out_dir, "channel_heatmap.png"), g, width = 8, height = 9, dpi = 150, bg = "white")
cat("done\n")
