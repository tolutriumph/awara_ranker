# Awara Opportunity Ranker (Hack the Fork Nigeria, Challenge 3)

Ranks where packaged seasoned fried awara (soy tofu, 150 g) has the best chance in Nigeria.
An opportunity is one **pilot state × customer segment × sales channel** (10 × 3 × 6 = 180).
All numbers are computed in R; Amazon Bedrock only writes text from those numbers.

## Folders

| Folder | What it holds |
|---|---|
| `scripts/` | The model: `03_A_screening.R` (37 states, real data), `03_D_state_channel_inputs.R`, `03_BCDEF_pipeline.R` (segments, choice model, scoring, Monte Carlo, handoff) |
| `outputs/` | Everything the model produced (CSV, JSON, PNG, `mc_criteria_draws.rds`) |
| `app/` | The Shiny dashboard, the Bedrock client and the explanation script |

Raw data is never committed. `inputs/`, `Hack the fork/` and the data files in `Data all/` are git-ignored, because `Hack the fork/hidden_truth.csv` holds the validation labels. Only the prep scripts and `README_data.md` in `Data all/` are tracked.
The input folder (`IN_DIR`, default `inputs/`) must hold: `master_states_final.csv`, `unit_economics_v1b.csv`, `pilot_states.csv`, `seasonality_month.csv`, `channel_density_long.csv`, `survey_responses.csv`, `choice_tasks.csv`, `hidden_truth.csv`.

These come from two folders. `Hack the fork/` (the challenge download) has `survey_responses.csv`, `choice_tasks.csv` and `hidden_truth.csv`, and the pipeline reads those raw names directly. `Data all/` (the data-prep step) has the other five. Note that `unit_economics.csv` in the download is not the cleaned `unit_economics_v1b.csv`. Build `inputs/` once (PowerShell, from the project root):

```powershell
New-Item -ItemType Directory -Force inputs
Copy-Item "Hack the fork\survey_responses.csv","Hack the fork\choice_tasks.csv","Hack the fork\hidden_truth.csv" inputs\
Copy-Item "Data all\master_states_final.csv","Data all\unit_economics_v1b.csv","Data all\pilot_states.csv","Data all\seasonality_month.csv","Data all\channel_density_long.csv" inputs\
```

## Run it (laptop, no AWS needed)

```bash
cd app
Rscript install.R                     # once
R -e 'shiny::runApp(".")'             # opens the dashboard
```

The app already contains the model outputs in `app/data/`. Without AWS credentials it shows template notes and the Ask box says "offline"; every number still works.

## Re-run the model (about 1 minute)

```bash
cd scripts
IN_DIR=../inputs OUT_DIR=../outputs Rscript 03_A_screening.R
IN_DIR=../inputs OUT_DIR=../outputs Rscript 03_D_state_channel_inputs.R
IN_DIR=../inputs OUT_DIR=../outputs Rscript 03_BCDEF_pipeline.R
cd ../app && OUT_DIR=../outputs IN_DIR=../inputs Rscript sync_data.R
```

On Windows PowerShell (re-running only the pipeline, which also writes `price_model.rds` for the Start page's margin-based pricing):

```powershell
cd scripts
$env:IN_DIR = "../inputs"; $env:OUT_DIR = "../outputs"
Rscript 03_BCDEF_pipeline.R
cd ../app; Rscript sync_data.R
```

Do not edit a script while `Rscript` is running it: R reads the file as it goes and will pick up the half-edited version.
Real field data later: replace `survey_responses.csv`, `choice_tasks.csv` (and vendor/unit-economics files), re-run the same commands, and the app re-ranks.

## At the event: switch on the AI layer (Amazon Bedrock)

1. Copy `app/.Renviron.example` to `app/.Renviron`, fill in the team's AWS values (region `us-east-1`), restart R. Never commit `.Renviron`.
2. In the Bedrock console (us-east-1), make sure the team has access to **Claude Haiku 4.5** (model ID `us.anthropic.claude-haiku-4-5-20251001-v1:0`; Nova Lite `us.amazon.nova-lite-v1:0` is the cheap alternative, set via `BEDROCK_MODEL_ID`).
3. Preview the prompts without calling AWS: `cd app && DRY_RUN=1 Rscript 04_bedrock_explanations.R` (writes `data/bedrock_prompts_preview.txt`).
4. Write and cache the top-10 notes: `Rscript 04_bedrock_explanations.R` (about 15 seconds, one request per second). They are saved to `data/explanations_top10.json` and shown in the "Why this rank" tab with an "AI-written" label.
5. Run the app; the "Ask the ranker" box now answers questions using only the dashboard's numbers.

If AWS fails during the demo, the app keeps working with cached notes or template notes.
For offline UI testing only: `BEDROCK_MOCK=1` returns a clearly marked fake answer.

## Decisions taken (20 Nov 2026)

1. Segments: k = 3 kept (chosen by silhouette before validation). Hidden labels have 4; switching after seeing them would be fitting on validation data. ARI 0.613.
2. Addressable people split by the segment's preferred channel, so volumes add up across channels (the brief's formula counted each eater in all channels).
3. Stated intent shown next to the choice-model purchase probability; caveat added where the model is more optimistic.
4. Restaurant channel added (75 respondents prefer it; synthetic restaurant density exists).

Also kept and listed in `outputs/assumptions.csv`: soy missing for Enugu, Lagos, Ogun, Oyo, Rivers sampled from a low assumed range; channel readiness scaled within each channel; margins from reported cost totals (3.3 to 4.7 points below recomputed totals); gym/health store uses supermarket density as a proxy.

## Headline result (default weights)

All top-10 opportunities target segment S3 (affluent convenience/health seekers: 71% stated intent, 58 to 82% can afford the shelf price). #1 is Kaduna · restaurant (score 72.4, range 59 to 79, top 3 in 35% of 2,000 runs). The soy weight decides #1: with soy weight 0, FCT · delivery app leads (top 3 in 44% of runs).

## What we can and cannot claim

Safe: transparent, adjustable criteria; real state-level population, wealth and soy data; quantified uncertainty; the method recovers planted segments; results are hypotheses for a pilot.
Do not claim: that segments exist in Nigeria at these sizes; that volumes or revenue are forecasts (they total about 101M packs/month across all 180 opportunities, which is a relative index, not a market size); that channel counts or vendor costs are real; that a state is the best launch site in reality.

## Data sources and credits

WorldPop/NPC (population), DHS Program Nigeria 2024 (wealth, state aggregates), NASS 2022/23 (soy), NASA POWER (climate, not scored), HESTIA (footprint, CC BY 4.0, pitch only). Survey, choice, vendor, unit economics, channel density and seasonality are synthetic, generated for this project. Check each source's licence on its own page.
