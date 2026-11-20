#!/usr/bin/env Rscript
# 04_bedrock_explanations.R - writes a plain-English note for each top-10 opportunity using Amazon Bedrock,
# from the structured facts in data/llm_input_top10.json ONLY. Results are cached in data/explanations_top10.json
# so the app (and the demo) never depends on a live AI call.
#
# Run at the event, from the app folder, after putting credentials in .Renviron (git-ignored):
#   Rscript 04_bedrock_explanations.R            # real calls (about 1 request per second, per AWS rules)
#   DRY_RUN=1 Rscript 04_bedrock_explanations.R  # writes the prompts to data/bedrock_prompts_preview.txt, no calls
#   FORCE=1  Rscript 04_bedrock_explanations.R   # re-generate even if a cached note exists for the same facts
suppressPackageStartupMessages({ library(jsonlite) })
source("R/bedrock.R")
facts_file <- "data/llm_input_top10.json"; out_file <- "data/explanations_top10.json"
stopifnot(file.exists(facts_file))
f <- fromJSON(facts_file, simplifyVector = FALSE)
dry <- identical(Sys.getenv("DRY_RUN"), "1"); force <- identical(Sys.getenv("FORCE"), "1")
cfg <- bedrock_config()
old <- if (file.exists(out_file)) fromJSON(out_file, simplifyVector = FALSE)$notes else list()
old_by_key <- setNames(old, vapply(old, function(x) paste(x$opportunity, x$facts_hash, x$model, sep = "|"), ""))

if (!dry && !bedrock_available() && !identical(Sys.getenv("BEDROCK_MOCK"), "1"))
  stop("No AWS credentials found. Put AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY (/ AWS_SESSION_TOKEN) in .Renviron, or use DRY_RUN=1.")
cat("Model:", cfg$model, "| region:", cfg$region, "| items:", length(f$top10), if (dry) "| DRY RUN" else "", "\n")

notes <- list(); preview <- character()
for (it in f$top10) {
  opp <- paste(it$state, it$segment, it$channel, sep = " | ")
  user_text <- paste0("Facts for one opportunity (JSON):\n", toJSON(it, auto_unbox = TRUE, pretty = TRUE, digits = NA),
                      "\n\nWrite the decision note for this opportunity.")
  h <- digest::digest(user_text, algo = "sha256", serialize = FALSE)
  key <- paste(opp, h, cfg$model, sep = "|")
  if (dry) { preview <- c(preview, paste0("=== rank ", it$rank, ": ", opp, " ===\n[SYSTEM]\n", EXPLAIN_SYSTEM, "\n[USER]\n", user_text, "\n")); next }
  if (!force && !is.null(old_by_key[[key]])) { cat("cached  #", it$rank, opp, "\n"); notes[[length(notes) + 1]] <- old_by_key[[key]]; next }
  res <- bedrock_converse(EXPLAIN_SYSTEM, user_text, max_tokens = 500, temperature = 0.2, cfg = cfg)
  if (!res$ok) { Sys.sleep(2); res <- bedrock_converse(EXPLAIN_SYSTEM, user_text, max_tokens = 500, temperature = 0.2, cfg = cfg) }  # one retry
  cat(if (res$ok) "written " else "FAILED  ", "#", it$rank, " ", opp, if (!res$ok) paste(" ->", res$error) else "", "\n", sep = "")
  if (res$ok) notes[[length(notes) + 1]] <- list(rank = it$rank, opportunity = opp, text = res$text, model = res$model,
                                                 generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), facts_hash = h,
                                                 basis = "AI-written from llm_input_top10.json only (default weights)")
  Sys.sleep(1.1)   # ~1 request per second per person (team AWS rule)
}
if (dry) { writeLines(preview, "data/bedrock_prompts_preview.txt"); cat("Prompts written to data/bedrock_prompts_preview.txt\n") } else {
  write_json(list(generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), notes = notes), out_file, auto_unbox = TRUE, pretty = TRUE)
  cat("Saved", length(notes), "notes to", out_file, "\n")
}
install.package("httr2")