# =============================================================================
# Awara Opportunity Ranker - Shiny dashboard (Hack the Fork Nigeria, Challenge 3)
# Product: packaged seasoned fried awara (soy tofu), 150 g. Opportunity = pilot state x segment x channel.
# Every number shown comes from data/ (built by the 03_* R scripts). The AI layer (Amazon Bedrock) only writes
# text from those numbers; the ranking never depends on it. Run:  shiny::runApp(".")
# =============================================================================
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT); library(plotly)
  library(dplyr); library(tidyr); library(readr); library(jsonlite)
})
source("R/bedrock.R")
source("R/start_page.R", local = TRUE)
set.seed(42)
for (loc in c("en_US.UTF-8", "C.UTF-8", "English_United States.utf8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break

# ---------------------------------------------------------------- data
rd <- function(f) read_csv(file.path("data", f), show_col_types = FALSE)
rk   <- rd("ranked_opportunities.csv")
ctb  <- rd("opportunity_contributions.csv")
mc   <- readRDS("data/mc_criteria_draws.rds")
scr  <- rd("screening_37_states.csv")
lt   <- rd("launch_timing.csv")
sea  <- rd("seasonality_month.csv")
prof <- rd("segment_profiles.csv")
sbs  <- rd("segment_share_by_state.csv")
cf   <- rd("choice_model_coefficients.csv")
conf <- rd("segment_validation_confusion.csv")
asm  <- rd("assumptions.csv")
sens <- rd("sensitivity.csv")
top10_facts <- fromJSON("data/llm_input_top10.json", simplifyVector = FALSE)
expl_file <- "data/explanations_top10.json"
expl <- if (file.exists(expl_file)) fromJSON(expl_file, simplifyVector = FALSE)$notes else list()
expl_by_opp <- setNames(expl, vapply(expl, function(x) x$opportunity, ""))

CRIT <- mc$criteria
CRIT_LAB <- c(market_demand = "Market demand", affordability = "Affordability", margin = "Margin",
              soy_supply = "Soy supply", channel_readiness = "Channel readiness")
W0 <- mc$default_weights
OPP <- mc$opportunity
CH_LAB <- c(street_market = "Street market", supermarket = "Supermarket", campus_shop = "Campus shop",
            delivery_app = "Delivery app", gym_health_store = "Gym / health store", restaurant = "Restaurant")
seg_short <- function(x) sub(" \\(intent.*$", "", x)
SEG_TINY <- c("Price-driven students" = "Students", "Traditional awara eaters" = "Traditional eaters",
              "Affluent convenience/health seekers" = "Affluent seekers")
seg_tiny <- function(x) { s <- sub("^S[0-9]+ ", "", seg_short(x)); out <- unname(SEG_TINY[s]); ifelse(is.na(out), s, out) }
COL <- list(primary = "#1e6b52", accent = "#c08a2e", muted = "#8a8f87", warn = "#b5523b", soft = "#e9f1ec")
CRIT_COL <- c(market_demand = "#1e6b52", affordability = "#5b9a7f", margin = "#c08a2e", soy_supply = "#8c6d46", channel_readiness = "#7d8f9a")

# point scores (0-100 per criterion) in the same opportunity order as the Monte Carlo draws
S0 <- ctb %>% select(opportunity, criterion, score_0_100) %>%
  pivot_wider(names_from = criterion, values_from = score_0_100) %>%
  slice(match(OPP, opportunity))
S0m <- as.matrix(S0[, CRIT]); rownames(S0m) <- OPP
RAW <- ctb %>% select(opportunity, criterion, raw_value) %>% pivot_wider(names_from = criterion, values_from = raw_value)
meta <- rk %>% select(opportunity, state, segment, channel, proxy_flag, soy_imputed, addressable_people, p_choose, p_buy,
                      stated_intent_top2box, raw_affordability, monthly_volume_packs, producer_margin_pct_base,
                      producer_margin_pct_stress, model_vs_intent) %>%
  slice(match(OPP, opportunity)) %>%
  mutate(segment_short = seg_short(segment), channel_lab = unname(CH_LAB[channel]),
         label = paste(state, seg_tiny(segment), channel_lab, sep = " · "),
         volume_p10 = mc$volume_p10, volume_p90 = mc$volume_p90)
shelf <- read_csv("data/state_channel_inputs.csv", show_col_types = FALSE) %>% distinct(state, shelf_price_base)
meta <- meta %>% left_join(shelf, by = "state")
source("R/price_model.R", local = TRUE)
pm_load()

# ---------------------------------------------------------------- scoring under any weights
rank_mat <- function(sc) { r <- t(apply(-sc, 1, rank, ties.method = "min")); dimnames(r) <- dimnames(sc); r }
# price = NULL: standard shelf prices (pipeline scores). A number (NGN): demand, affordability and margin re-scored at that
# 150 g pack price on the standard-price scale (R/price_model.R); scores can then leave 0-100.
score_with <- function(w, jitter = TRUE, conc = 30, n = mc$n, price = NULL) {
  w <- w / sum(w)
  cp <- if (is.null(price)) list(S = mc$S, S0 = S0m) else criteria_at_price(price)
  point <- as.vector(cp$S0 %*% w)
  set.seed(42)
  W <- if (jitter) { g <- sapply(w, function(a) rgamma(n, shape = conc * a)); g / rowSums(g) } else matrix(w, n, length(w), byrow = TRUE)
  W[is.na(W)] <- 0
  sc <- Reduce(`+`, lapply(seq_along(CRIT), function(k) cp$S[1:n, , k] * W[, k]))
  rkm <- rank_mat(sc)
  tibble(opportunity = OPP, score = point, rank = rank(-point, ties.method = "min"),
         p10 = apply(sc, 2, quantile, 0.10), p50 = apply(sc, 2, median), p90 = apply(sc, 2, quantile, 0.90),
         P1 = colMeans(rkm == 1), P3 = colMeans(rkm <= 3), P5 = colMeans(rkm <= 5), P10 = colMeans(rkm <= 10)) %>%
    left_join(meta_at_price(price), by = "opportunity") %>% arrange(rank) %>%
    { attr(., "rank_draws") <- rkm; attr(., "price") <- price; . }
}
contrib_with <- function(w, price = NULL) { w <- w / sum(w); sweep(if (is.null(price)) S0m else criteria_at_price(price)$S0, 2, w, `*`) }
base_default <- score_with(W0)
w_nosoy <- W0; w_nosoy["soy_supply"] <- 0
base_nosoy <- score_with(w_nosoy)

best_months <- function(st) { x <- lt$best_3_months[lt$state == st]; if (!length(x)) "n/a" else gsub("\\|", ", ", x) }
pct <- function(x, d = 0) paste0(formatC(100 * x, format = "f", digits = d), "%")
num <- function(x) formatC(round(x), format = "d", big.mark = ",")

drivers_of <- function(o, w, price = NULL) {
  cm <- contrib_with(w, price); avg <- colMeans(cm); d <- cm[o, ] - avg
  list(pos = names(sort(d[d > 0], decreasing = TRUE)), neg = names(sort(d[d < 0])), contrib = cm[o, ], avg = avg)
}

template_note <- function(row, w) {
  dv <- drivers_of(row$opportunity, w)
  pos <- if (length(dv$pos)) paste(tolower(CRIT_LAB[head(dv$pos, 2)]), collapse = " and ") else "no criterion above average"
  neg <- if (length(dv$neg)) tolower(CRIT_LAB[dv$neg[1]]) else NULL
  paste0(row$label, " ranks #", row$rank, " with a score of ", round(row$score, 1), " out of 100 (simulation range ",
         round(row$p10, 1), " to ", round(row$p90, 1), "; in the top 3 in ", pct(row$P3), " of 2,000 runs). ",
         "It is lifted by ", pos, if (!is.null(neg)) paste0(" and held back by ", neg), ". ",
         row$segment_short, " show ", pct(row$stated_intent_top2box), " stated intent to try, and ", pct(row$raw_affordability),
         " would pay the ₦", row$shelf_price_base, " shelf price. Producer margin is ", row$producer_margin_pct_base,
         "% at base cost and ", row$producer_margin_pct_stress, "% under stress. ",
         "Demand-side data are synthetic, so this is a hypothesis for a pilot, not a forecast.")
}

# ---------------------------------------------------------------- UI
theme <- bs_theme(version = 5, primary = COL$primary, secondary = COL$accent, success = COL$primary,
                  base_font = font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
                  heading_font = font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "sans-serif"),
                  "border-radius" = "0.6rem") |>
  bs_add_rules("
    .synthetic-note{font-size:.78rem;color:#6b7068}
    .tag{display:inline-block;font-size:.72rem;padding:.1rem .45rem;border-radius:1rem;margin-right:.25rem;font-weight:600}
    .tag-real{background:#e3f0e8;color:#1e6b52}.tag-syn{background:#f6ead6;color:#8a5a12}.tag-ai{background:#e7ecf6;color:#2c4f8a}
    .tag-tpl{background:#eee;color:#555}
    .decision{font-size:1.25rem;line-height:1.5}
    .weight-sum{font-size:.8rem;color:#6b7068}
    .bslib-value-box .value-box-value{font-size:1.5rem}
    .note-text{font-size:1rem;line-height:1.55}")

weight_inputs <- tagList(
  h6("Criterion weights"),
  lapply(CRIT, function(k) sliderInput(paste0("w_", k), CRIT_LAB[[k]], min = 0, max = 1, value = unname(W0[k]), step = 0.05, ticks = FALSE)),
  uiOutput("weight_sum"),
  div(class = "d-flex flex-wrap gap-1 mb-2",
      actionButton("preset_default", "Default", class = "btn-sm btn-outline-primary"),
      actionButton("preset_nosoy", "Ignore soy", class = "btn-sm btn-outline-primary"),
      actionButton("preset_demand", "Demand-led", class = "btn-sm btn-outline-primary"),
      actionButton("preset_profit", "Profit-led", class = "btn-sm btn-outline-primary")),
  checkboxInput("jitter", "Also vary weights in the simulation (±, concentration 30)", TRUE),
  hr(),
  h6("Filter the list"),
  selectInput("f_state", "State", c("All", sort(unique(meta$state))), "All"),
  selectInput("f_seg", "Segment", c("All", sort(unique(meta$segment_short))), "All"),
  selectInput("f_ch", "Channel", c("All", setNames(names(CH_LAB), CH_LAB)), "All"),
  div(class = "synthetic-note", "Weights are rescaled to sum to 100%. Ranks are recomputed live, with the 2,000 saved simulation runs.")
)

ui <- page_navbar(
  title = tags$span(tags$b("Awara Opportunity Ranker"), tags$small(class = "text-muted ms-2", "packaged fried awara · 150 g · Nigeria")),
  theme = theme, fillable = FALSE, id = "nav",
  sidebar = sidebar(id = "side", width = 300, weight_inputs, open = "desktop"),
  start_panel(),
  nav_panel("Decision",
    layout_columns(col_widths = c(12),
      card(card_header("Recommended pilot under the current weights"),
           div(class = "decision", uiOutput("decision_text")),
           div(class = "mt-2", HTML('<span class="tag tag-real">REAL</span>population, wealth (DHS 2024), soy (NASS 2022/23) &nbsp; <span class="tag tag-syn">SYNTHETIC</span>survey, choice tasks, vendor costs, channel density, seasonality')))),
    layout_columns(col_widths = c(3, 3, 3, 3),
      value_box("Top opportunity", textOutput("vb_top"), p(textOutput("vb_top_sub", inline = TRUE)), theme = value_box_theme(bg = COL$primary, fg = "#fff")),
      value_box("Chance it stays top 3", textOutput("vb_p3"), p("share of 2,000 simulation runs"), theme = "light"),
      value_box("Score range (p10–p90)", textOutput("vb_range"), p("out of 100"), theme = "light"),
      value_box("Best launch months", textOutput("vb_months"), p("vendor peak months (3–6 vendors, synthetic)"), theme = "light")),
    layout_columns(col_widths = c(7, 5),
      card(card_header("Top 10 with uncertainty"), plotlyOutput("p_top10", height = 380),
           div(class = "synthetic-note", "Dot = score under your weights; bar = 10th–90th percentile across 2,000 runs (weights, calibration discount, price, cost, choice coefficients, missing soy).")),
      card(card_header("How the decision changes"), uiOutput("decision_changes"))
    )),
  nav_panel("Ranking",
    card(fill = FALSE, card_header(div(class = "d-flex justify-content-between align-items-center", span("All 180 opportunities (10 states × 3 segments × 6 channels)"),
                         downloadButton("dl_rank", "Download CSV", class = "btn-sm"))),
         DTOutput("t_rank", fill = FALSE),
         div(class = "synthetic-note mt-2", "P(buy) = choice-model probability × calibration discount 0.5, ignoring competing products. Volume = relative monthly packs, not a forecast."))),
  nav_panel("Why this rank",
    layout_columns(col_widths = c(12), card(selectInput("sel_opp", "Opportunity", choices = NULL, width = "100%"))),
    layout_columns(col_widths = c(6, 6),
      card(card_header("What drives the score (current weights)"), plotlyOutput("p_contrib", height = 320),
           div(class = "synthetic-note", "Bar = this opportunity's weighted criterion score; tick = average across all 180 opportunities.")),
      card(card_header("Key facts"), tableOutput("t_facts"))),
    layout_columns(col_widths = c(6, 6),
      card(card_header(uiOutput("note_header")), div(class = "note-text", uiOutput("note_text"))),
      card(card_header(HTML('Ask the ranker <span class="tag tag-ai">Amazon Bedrock</span>')),
           textAreaInput("ask_q", NULL, placeholder = "e.g. Why does Kaduna beat FCT? What happens if we ignore soy supply?", width = "100%", rows = 3),
           div(class = "d-flex gap-2 align-items-center", actionButton("ask_go", "Ask", class = "btn-primary btn-sm"), uiOutput("ask_status")),
           div(class = "note-text mt-2", uiOutput("ask_answer"))))),
  nav_panel("Stability",
    layout_columns(col_widths = c(7, 5),
      card(card_header("Rank stability: chance of each rank (top 15, current weights)"), plotlyOutput("p_heat", height = 460)),
      card(card_header("Sensitivity: each weight ×0.5 / ×1.5 (default weights)"), plotlyOutput("p_sens", height = 460),
           div(class = "synthetic-note", "Largest rank change among the default top 10. The calibration discount barely moves ranks: it scales every opportunity's demand almost equally.")))),
  nav_panel("States",
    layout_columns(col_widths = c(6, 6),
      card(card_header("Stage A: all 37 states on real data"),
           radioButtons("soy_mode", NULL, c("Soy missing: reweight the rest (brief)" = "renorm", "Soy missing: count as 0" = "zero"), inline = TRUE),
           plotlyOutput("p_screen", height = 640),
           div(class = "synthetic-note", "Market = log population; affordability = DHS wealth index and upper-quintile share; soy = NASS output per 100k. 17 states (South) have no state-level soy.")),
      card(card_header("Seasonality in the 10 pilot states"), plotlyOutput("p_season", height = 420), tableOutput("t_months"),
           div(class = "synthetic-note", "Share of vendors naming a month as peak, indexed to 1.0. 3–6 synthetic vendors per state: indicative only.")))),
  nav_panel("Segments",
    layout_columns(col_widths = c(12), card(card_header("Customer segments found by clustering (Gower distance + PAM, k = 3 by silhouette)"), tableOutput("t_prof"))),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Segment mix by state"), plotlyOutput("p_sbs", height = 360)),
      card(card_header("Validation only: found vs planted segments"), uiOutput("ari_text"), tableOutput("t_conf")))),
  nav_panel("Assumptions",
    layout_columns(col_widths = c(7, 5),
      card(card_header("Every assumption in the model"), DTOutput("t_asm")),
      card(card_header("What we can and cannot claim"),
           h6("Safe to say"), tags$ul(
             tags$li("The pipeline ranks opportunities with visible criteria and adjustable weights."),
             tags$li("Population, wealth and soy output are real, state-level Nigerian data."),
             tags$li("Uncertainty is quantified: score ranges and rank stability, not single numbers."),
             tags$li("The method recovers segments planted in the synthetic survey (ARI 0.61)."),
             tags$li("The result is a set of hypotheses for a pilot; real survey data plugs into the same pipeline.")),
           h6("Do not say"), tags$ul(
             tags$li("That a segment exists, or is a given size, in Nigeria."),
             tags$li("That volume or revenue figures are forecasts."),
             tags$li("That channel counts or vendor costs are real."),
             tags$li("That a state is the best place to launch in reality."))))),
  nav_spacer(),
  nav_item(tags$span(class = "synthetic-note", textOutput("ai_mode", inline = TRUE)))
)

# ---------------------------------------------------------------- server
server <- function(input, output, session) {
  start_server(input, output, session)
  if (interactive()) session$onSessionEnded(shiny::stopApp)   # closing the browser tab stops the app (local runs only)
  set_weights <- function(w) for (k in CRIT) updateSliderInput(session, paste0("w_", k), value = unname(w[k]))
  observeEvent(input$preset_default, set_weights(W0))
  observeEvent(input$preset_nosoy, set_weights(w_nosoy))
  observeEvent(input$preset_demand, set_weights(c(market_demand = 0.45, affordability = 0.25, margin = 0.10, soy_supply = 0.10, channel_readiness = 0.10)))
  observeEvent(input$preset_profit, set_weights(c(market_demand = 0.20, affordability = 0.15, margin = 0.40, soy_supply = 0.15, channel_readiness = 0.10)))

  w_raw <- reactive(sapply(CRIT, function(k) input[[paste0("w_", k)]] %||% W0[[k]]))
  w <- debounce(reactive({ x <- w_raw(); if (sum(x) <= 0) W0 else x / sum(x) }), 350)
  is_default <- reactive(isTRUE(all.equal(unname(w()), unname(W0 / sum(W0)), tolerance = 1e-6)) && isTRUE(input$jitter))
  output$weight_sum <- renderUI({
    x <- w_raw(); if (sum(x) <= 0) return(div(class = "text-danger small", "All weights are 0: using defaults."))
    div(class = "weight-sum", paste0("Effective: ", paste0(CRIT_LAB[CRIT], " ", round(100 * x / sum(x)), "%", collapse = " · ")))
  })
  output$ai_mode <- renderText(if (identical(Sys.getenv("BEDROCK_MOCK"), "1")) "AI: MOCK mode (testing)" else
    if (bedrock_available()) paste("AI: Bedrock ·", bedrock_config()$model) else "AI: offline (template notes)")

  res <- reactive(score_with(w(), jitter = isTRUE(input$jitter)))
  filtered <- reactive({
    r <- res()
    if (input$f_state != "All") r <- r %>% filter(state == input$f_state)
    if (input$f_seg != "All") r <- r %>% filter(segment_short == input$f_seg)
    if (input$f_ch != "All") r <- r %>% filter(channel == input$f_ch)
    r
  })
  top <- reactive(res()[1, ])

  # Decision tab
  output$vb_top <- renderText(paste0(top()$state, ", ", CH_LAB[[top()$channel]]))
  output$vb_top_sub <- renderText(top()$segment_short)
  output$vb_p3 <- renderText(pct(top()$P3))
  output$vb_range <- renderText(paste0(round(top()$p10), " to ", round(top()$p90)))
  output$vb_months <- renderText(best_months(top()$state))
  output$decision_text <- renderUI({
    t <- top(); dv <- drivers_of(t$opportunity, w())
    HTML(paste0("Pilot with <b>", t$segment_short, "</b> in <b>", t$state, "</b> through <b>", tolower(CH_LAB[[t$channel]]),
                "</b>, launching around <b>", best_months(t$state), "</b>. It wins mainly on <b>",
                paste(tolower(CRIT_LAB[head(dv$pos, 2)]), collapse = "</b> and <b>"), "</b>, but it is in the top 3 in only <b>",
                pct(t$P3), "</b> of simulation runs, so treat it as the lead hypothesis, not a certainty."))
  })
  output$p_top10 <- renderPlotly({
    d <- res() %>% head(10) %>% mutate(lab = factor(label, levels = rev(label)))
    plot_ly(d, y = ~lab, x = ~score, type = "scatter", mode = "markers",
            marker = list(color = COL$primary, size = 10),
            error_x = list(type = "data", symmetric = FALSE, array = ~pmax(p90 - score, 0), arrayminus = ~pmax(score - p10, 0), color = COL$muted),
            text = ~paste0("#", rank, " ", label, "<br>score ", round(score, 1), " (", round(p10, 1), "–", round(p90, 1), ")<br>P(top 3) ", pct(P3)),
            hoverinfo = "text") |>
      layout(xaxis = list(title = "Score (0–100)"), yaxis = list(title = ""), margin = list(l = 10)) |> config(displayModeBar = FALSE)
  })
  output$decision_changes <- renderUI({
    row <- function(lbl, r) tags$tr(tags$td(lbl), tags$td(tags$b(r$label[1])), tags$td(pct(r$P3[1])))
    tagList(
      tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th("Scenario"), tags$th("#1 opportunity"), tags$th("P(top 3)"))),
        tags$tbody(row("Your current weights", res()), row("Default weights", base_default), row("Ignore soy supply", base_nosoy))),
      p(class = "small", "The soy weight decides between the north-central/north-west soy belt and the richer cities: with soy counted, Kaduna leads; ",
        "set soy to 0 and FCT takes the top spot. That is the business choice a founder makes: source near soy, or sell where incomes are highest."),
      p(class = "synthetic-note", "Southern states (Lagos, Ogun, Oyo, Enugu, Rivers) have no state-level soy data; the model samples a low assumed range for them, which keeps them down the list."))
  })

  # Ranking tab
  output$t_rank <- renderDT({
    d <- filtered() %>% transmute(Rank = rank, State = state, Segment = seg_tiny(segment), Channel = channel_lab,
      Score = round(score, 1), `p10–p90` = paste0(round(p10), "–", round(p90)), `P(top 3)` = round(P3, 3), `P(top 10)` = round(P10, 3),
      `Stated intent` = round(stated_intent_top2box, 2), `P(buy)` = round(p_buy, 3), `Can afford` = round(raw_affordability, 2),
      `Rel. volume` = round(monthly_volume_packs), `Soy imputed` = ifelse(soy_imputed, "yes", ""), `Proxy channel` = ifelse(proxy_flag, "yes", ""))
    datatable(d, rownames = FALSE, options = list(pageLength = 15, dom = "ftip", scrollX = TRUE), selection = "single") |>
      formatPercentage(c("P(top 3)", "P(top 10)", "Stated intent", "Can afford"), 0) |> formatPercentage("P(buy)", 1) |>
      formatRound("Rel. volume", 0) |> formatStyle("Rank", fontWeight = "600")
  })
  output$dl_rank <- downloadHandler(filename = function() "awara_ranking_current_weights.csv",
    content = function(f) write_csv(res() %>% select(rank, opportunity, state, segment_short, channel, score, p10, p50, p90, P1, P3, P5, P10,
                                                     stated_intent_top2box, p_buy, raw_affordability, monthly_volume_packs) %>%
                                      mutate(weights = paste(names(w()), round(w(), 3), sep = "=", collapse = ";"),
                                             data_basis = "real population/wealth/soy + synthetic survey/choice/vendor/channel"), f))
  observeEvent(input$t_rank_rows_selected, {
    o <- filtered()$opportunity[input$t_rank_rows_selected]; updateSelectInput(session, "sel_opp", selected = o); nav_select("nav", "Why this rank")
  })

  # Why tab
  observe({
    r <- res(); cur <- isolate(input$sel_opp)
    ch <- setNames(r$opportunity, paste0("#", r$rank, "  ", r$label))
    updateSelectInput(session, "sel_opp", choices = ch, selected = if (!is.null(cur) && cur %in% r$opportunity) cur else r$opportunity[1])
  })
  sel <- reactive({ req(input$sel_opp); res() %>% filter(opportunity == input$sel_opp) })
  output$p_contrib <- renderPlotly({
    s <- sel(); dv <- drivers_of(s$opportunity, w())
    d <- tibble(k = CRIT, lab = unname(CRIT_LAB[CRIT]), v = unname(dv$contrib[CRIT]), a = unname(dv$avg[CRIT])) %>% mutate(lab = factor(lab, levels = rev(lab)))
    plot_ly(d) |>
      add_bars(y = ~lab, x = ~v, orientation = "h", marker = list(color = unname(CRIT_COL[d$k])), name = "this opportunity",
               text = ~paste0(lab, ": ", round(v, 1), " pts (avg ", round(a, 1), ")"), hoverinfo = "text", textposition = "none") |>
      add_markers(y = ~lab, x = ~a, marker = list(symbol = "line-ns-open", size = 22, color = "#333", line = list(width = 2)), name = "average", hoverinfo = "skip") |>
      layout(xaxis = list(title = "Points contributed to the 0–100 score"), yaxis = list(title = ""), showlegend = FALSE, margin = list(l = 10)) |>
      config(displayModeBar = FALSE)
  })
  output$t_facts <- renderTable({
    s <- sel(); rw <- RAW %>% filter(opportunity == s$opportunity)
    tibble(Item = c("Rank (current weights)", "Score, p10–p90", "P(rank 1) / P(top 3) / P(top 10)", "Addressable eaters using this channel",
                    "Choice-model P(choose) → P(buy) after 0.5 discount", "Stated intent (top-2-box)", "Share who would pay the shelf price",
                    "Producer margin: base / stress", "Relative monthly packs (p10–p90)", "Soy per 100k people", "Channel density basis"),
           Value = c(paste0("#", s$rank), paste0(round(s$score, 1), ", ", round(s$p10, 1), "–", round(s$p90, 1)),
                     paste(pct(s$P1), pct(s$P3), pct(s$P10), sep = " / "), num(s$addressable_people),
                     paste0(pct(s$p_choose, 1), " → ", pct(s$p_buy, 1)), pct(s$stated_intent_top2box),
                     paste0(pct(s$raw_affordability), " at ₦", s$shelf_price_base),
                     paste0(s$producer_margin_pct_base, "% / ", s$producer_margin_pct_stress, "%"),
                     paste0(num(s$monthly_volume_packs), " (", num(s$volume_p10), "–", num(s$volume_p90), ")"),
                     paste0(num(rw$soy_supply), " t per 100k people", if (s$soy_imputed) " (assumed midpoint, no state data)" else " (NASS)"),
                     if (s$proxy_flag) "supermarket density (proxy)" else "own channel (synthetic)"))
  }, striped = TRUE, spacing = "s", width = "100%")
  cached_note <- reactive({ s <- sel(); key <- paste(s$state, s$segment, s$channel, sep = " | "); if (is_default()) expl_by_opp[[key]] else NULL })
  output$note_header <- renderUI({
    n <- cached_note()
    if (!is.null(n)) HTML(paste0('Decision note <span class="tag tag-ai">AI-written · ', n$model, '</span>')) else
      HTML('Decision note <span class="tag tag-tpl">template, no AI</span>')
  })
  output$note_text <- renderUI({
    n <- cached_note(); s <- sel()
    if (!is.null(n)) tagList(p(n$text), div(class = "synthetic-note", paste("Generated", n$generated_at, "from the structured facts only, at default weights.")))
    else tagList(p(template_note(s, w())), div(class = "synthetic-note",
      if (!is_default()) "AI notes are written for the default weights; this template follows your current weights." else
        "Built from the numbers by a fixed sentence template. Run 04_bedrock_explanations.R to add AI-written notes for the top 10."))
  })

  # Ask the ranker
  ask_cache <- reactiveVal(list()); last_call <- reactiveVal(as.POSIXct(0)); ans <- reactiveVal(NULL)
  facts_now <- reactive({
    r <- res(); s <- sel()
    list(product = "packaged seasoned fried awara, 150 g", weights = as.list(round(w(), 3)),
         current_top10 = lapply(seq_len(min(10, nrow(r))), function(i) { x <- r[i, ]; dv <- drivers_of(x$opportunity, w())
           list(rank = x$rank, opportunity = x$label, score = round(x$score, 1), p10 = round(x$p10, 1), p90 = round(x$p90, 1),
                P_top3 = round(x$P3, 3), best_months = best_months(x$state), drivers_above_avg = unname(CRIT_LAB[dv$pos]),
                drivers_below_avg = unname(CRIT_LAB[dv$neg]), stated_intent = round(x$stated_intent_top2box, 2), p_buy = round(x$p_buy, 3),
                can_afford = round(x$raw_affordability, 2), margin_base = x$producer_margin_pct_base, margin_stress = x$producer_margin_pct_stress) }),
         scope = paste("Ranks packaged seasoned fried awara, 150 g pack, only. Opportunities exist for 10 pilot states:",
                       paste(sort(unique(meta$state)), collapse = ", "), ". All 37 states are only screened on real data (population, wealth, soy)."),
         best_by_state = lapply(split(r, r$state), function(x) lapply(seq_len(min(3, nrow(x))), function(i) { y <- x[i, ]
           list(national_rank = y$rank, opportunity = y$label, score = round(y$score, 1), p10 = round(y$p10, 1), p90 = round(y$p90, 1),
                P_top3 = round(y$P3, 3), P_top10 = round(y$P10, 3), best_months = best_months(y$state), stated_intent = round(y$stated_intent_top2box, 2),
                can_afford = round(y$raw_affordability, 2), shelf_price_ngn = y$shelf_price_base, margin_base = y$producer_margin_pct_base,
                margin_stress = y$producer_margin_pct_stress) })),
         screening_37_states = lapply(seq_len(nrow(scr)), function(i) list(rank = scr$rank[i], state = scr$state[i], score = round(scr$screening_score[i], 1),
                                                                             soy_data = if (is.na(scr$s_soy_per_100k[i])) "missing" else "reported")),
         selected_opportunity = list(label = s$label, rank = s$rank, score = round(s$score, 1), P_top3 = round(s$P3, 3)),
         top1_if_soy_ignored = base_nosoy$label[1], top1_default = base_default$label[1],
         key_assumptions = asm$name, data_basis = "population, wealth, soy real; survey, choice, vendor, channel, seasonality synthetic")
  })
  observeEvent(input$ask_go, {
    q <- trimws(input$ask_q); if (!nzchar(q)) return()
    if (nchar(q) > 400) { ans(list(ok = FALSE, text = "Please keep questions under 400 characters.")); return() }
    if (!bedrock_available() && !identical(Sys.getenv("BEDROCK_MOCK"), "1")) {
      ans(list(ok = FALSE, text = "The AI layer is offline (no AWS credentials on this machine). Every number in the dashboard still works.")); return() }
    if (as.numeric(difftime(Sys.time(), last_call(), units = "secs")) < 1.2) { ans(list(ok = FALSE, text = "One question per second, please (AWS rate rule).")); return() }
    fj <- toJSON(facts_now(), auto_unbox = TRUE, digits = NA)
    key <- digest::digest(paste(q, fj), algo = "sha256", serialize = FALSE)
    hit <- ask_cache()[[key]]
    if (!is.null(hit)) { ans(c(hit, cached = TRUE)); return() }
    last_call(Sys.time())
    out <- withProgress(message = "Asking Bedrock…", bedrock_converse(ASK_SYSTEM, paste0("FACTS (JSON):\n", fj, "\n\nQUESTION:\n", q), max_tokens = 400))
    if (out$ok) { cc <- ask_cache(); cc[[key]] <- out; ask_cache(cc) }
    ans(out)
  })
  output$ask_status <- renderUI(if (bedrock_available() || identical(Sys.getenv("BEDROCK_MOCK"), "1")) span(class = "synthetic-note", "Answers use only this dashboard's numbers.") else
    span(class = "synthetic-note", "Offline: add AWS credentials to enable."))
  output$ask_answer <- renderUI({
    a <- ans(); if (is.null(a)) return(NULL)
    if (!isTRUE(a$ok)) return(div(class = "text-muted", a$text %||% a$error))
    tagList(p(a$text), div(class = "synthetic-note", paste0("Model: ", a$model, if (isTRUE(a$cached)) " · cached" else "", " · grounded on the facts shown in this dashboard")))
  })

  # Stability
  output$p_heat <- renderPlotly({
    r <- res(); rkm <- attr(r, "rank_draws"); ids <- head(r$opportunity, 15); labs <- head(r$label, 15)
    z <- t(sapply(ids, function(o) sapply(1:15, function(k) mean(rkm[, o] == k))))
    plot_ly(x = 1:15, y = labs, z = z, type = "heatmap",
            colorscale = list(c(0, "#ffffff"), c(1, COL$primary)), zmin = 0, zmax = max(z),
            hovertemplate = "%{y}<br>rank %{x}: %{z:.0%}<extra></extra>", colorbar = list(title = "P(rank)", tickformat = ".0%")) |>
      layout(xaxis = list(title = "Rank", dtick = 1), yaxis = list(title = "", autorange = "reversed"), margin = list(l = 10)) |> config(displayModeBar = FALSE)
  })
  output$p_sens <- renderPlotly({
    d <- sens %>% group_by(scenario) %>% summarise(m = max(abs(rank_change)), .groups = "drop") %>% arrange(m) %>%
      mutate(scenario = paste0(c(CRIT_LAB, discount = "Calibration discount")[sub(" x.*$", "", scenario)], " \u00d7", sub("^.* x", "", scenario))) %>%
      mutate(scenario = factor(scenario, levels = scenario))
    plot_ly(d, x = ~m, y = ~scenario, type = "bar", orientation = "h", marker = list(color = COL$accent),
            hovertemplate = "%{y}: max rank change %{x}<extra></extra>") |>
      layout(xaxis = list(title = "Max |rank change| in default top 10"), yaxis = list(title = ""), margin = list(l = 10)) |> config(displayModeBar = FALSE)
  })

  # States
  output$p_screen <- renderPlotly({
    d <- scr %>% mutate(s = if (input$soy_mode == "renorm") screening_score else screening_score_soy_as_0,
                        grp = ifelse(is.na(s_soy_per_100k), "soy missing", "soy reported"),
                        pilot = state %in% meta$state) %>% arrange(s) %>% mutate(state = factor(state, levels = state))
    plot_ly(d, x = ~s, y = ~state, color = ~grp, colors = c("soy missing" = "#d9b98a", "soy reported" = COL$primary), type = "bar", orientation = "h",
            text = ~paste0(state, if_else(pilot, " (pilot)", ""), "<br>score ", round(s, 1), "<br>market ", round(s_market), " · afford ", round(s_affordability),
                           " · soy ", ifelse(is.na(s_soy_per_100k), "NA", round(s_soy_per_100k))), hoverinfo = "text", textposition = "none") |>
      layout(xaxis = list(title = "Screening score (0–100)"), yaxis = list(title = "", tickfont = list(size = 10)),
             legend = list(orientation = "h", y = -0.08), margin = list(l = 10)) |> config(displayModeBar = FALSE)
  })
  output$p_season <- renderPlotly({
    d <- sea %>% filter(scope == "state") %>% mutate(month = factor(month, levels = month.abb))
    m <- d %>% select(state, month, seasonality_index) %>% pivot_wider(names_from = month, values_from = seasonality_index)
    plot_ly(x = month.abb, y = m$state, z = as.matrix(m[, month.abb]), type = "heatmap",
            colorscale = list(c(0, "#ffffff"), c(1, COL$accent)), hovertemplate = "%{y} %{x}: index %{z:.1f}<extra></extra>",
            colorbar = list(title = "index")) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "", autorange = "reversed")) |> config(displayModeBar = FALSE)
  })
  output$t_months <- renderTable(lt %>% transmute(State = state, `Best 3 months` = gsub("\\|", ", ", best_3_months),
                                                  `Tied at 3rd` = ifelse(tie_flag, gsub("\\|", ", ", months_tied_at_3rd), ""), Vendors = as.integer(n_vendors)),
                                 striped = TRUE, spacing = "xs", width = "100%")

  # Segments
  output$t_prof <- renderTable({
    ch_cols <- grep("^preferred_channel_", names(prof), value = TRUE)
    prof %>% rowwise() %>% mutate(top_ch = sub("preferred_channel_", "", ch_cols[which.max(c_across(all_of(ch_cols)))]),
                                  top_ch_share = max(c_across(all_of(ch_cols)))) %>% ungroup() %>%
      transmute(Segment = seg_short(segment), Respondents = as.integer(n), Share = pct(share), `Intent to try (top-2-box)` = pct(intent_top2box_share),
                `Median max price` = paste0("₦", median_max_price_ngn), `Never eat awara` = pct(awara_frequency_never),
                `Eat weekly or daily` = pct(awara_frequency_weekly + awara_frequency_daily),
                `Top channel` = paste0(CH_LAB[top_ch], " (", pct(top_ch_share), ")"))
  }, striped = TRUE, spacing = "s", width = "100%")
  output$p_sbs <- renderPlotly({
    d <- sbs %>% mutate(seg = seg_short(segment))
    plot_ly(d, x = ~segment_share_in_state, y = ~state, color = ~seg, colors = c(COL$accent, "#8c6d46", COL$primary), type = "bar", orientation = "h",
            hovertemplate = "%{y}: %{x:.0%}<extra>%{fullData.name}</extra>") |>
      layout(barmode = "stack", xaxis = list(title = "", tickformat = ".0%"), yaxis = list(title = ""),
             legend = list(orientation = "h", y = -0.12)) |> config(displayModeBar = FALSE)
  })
  output$ari_text <- renderUI(p(HTML(paste0("Adjusted Rand index <b>", conf$ARI[1], "</b> (1 = perfect). The method separated students and traditional eaters cleanly ",
    "and merged two planted groups (health-curious and premium professionals). The hidden labels were read only after fitting."))))
  output$t_conf <- renderTable({
    x <- conf %>% select(-ARI, -data_basis) %>% mutate(found_segment = paste0("S", found_segment)) %>% rename(`Found / planted` = found_segment)
    names(x) <- sub("health_fitness_curious", "Health-curious", sub("premium_professional", "Premium prof.", sub("price_driven_student", "Students", sub("traditional_awara_eater", "Traditional", names(x)))))
    x %>% mutate(across(where(is.numeric), as.integer))
  }, striped = TRUE, spacing = "xs", width = "100%")

  # Assumptions
  output$t_asm <- renderDT(datatable(asm, rownames = FALSE, options = list(pageLength = 20, dom = "tp", scrollX = TRUE)))
}

shinyApp(ui, server)