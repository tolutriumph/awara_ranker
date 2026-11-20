# Data package: AI for Alternative-Protein Market Opportunity Identification (Nigeria)
Product: packaged seasoned fried awara (soy tofu), 150 g pack. No modelling or scoring in this package.

## Output files
| File | Rows | What it is | Real / synthetic | Source and attribution |
|---|---|---|---|---|
| `master_states_final.csv` | 37 | State-level master table (36 states + FCT): population, climate, wealth, soy, synthetic channel density | mixed: real (population, climate, wealth, soy) + synthetic (channel_*) | WorldPop/NPC (population); NASA POWER (climate); DHS Program Nigeria 2024 (wealth, state-level aggregates only); NASS 2022/23 (soy); channel_* synthetic; licence: check license on source page |
| `channel_density_wide.csv` | 37 | Synthetic channel density per state, wide (rebuilt from long file) | synthetic | Synthetic placeholder generated in earlier chat; real source would be field verification/OSM |
| `channel_density_long.csv` | 185 | Synthetic channel density, long (state, channel, value, unit, data_basis, assumption_note) | synthetic | Synthetic placeholder generated in earlier chat; copied unchanged |
| `footprint_standin.csv` | 3 | World-level HESTIA footprint stand-in rows (only what exists in supplied file; missing items listed in README) | real (HESTIA global average; not Nigeria) | HESTIA, CC BY 4.0 (hestia.earth) |
| `pilot_states.csv` | 10 | Pilot states (survey + vendor data) with respondent and vendor counts | synthetic | Counts taken from clean_survey_responses.csv and clean_vendor_interviews.csv (synthetic) |
| `unit_economics_v1b.csv` | 30 | Unit economics per 150 g pack by state x format; totals recomputed as sum of 8 listed components; originals kept as reported_total_*; total_diff_* = reported - recomputed | synthetic | Synthetic, generated for this project (clean_unit_economics.csv) |
| `seasonality_month.csv` | 132 | Share of vendors naming each month as peak, and index normalised to mean 1.0 across 12 months; national + per pilot state | synthetic | Synthetic vendor peak_months (clean_seasonality.csv) |
| `data_dictionary.csv` | 54 | Column dictionary and change log | n/a | this script |
| `README_data.md` | n/a | This file | n/a | this project |
| `01b_finalize.R` | n/a | Reproducible script that builds every output | n/a | this project |

## Not produced (input not supplied)
- survey_responses_synthetic.csv (input survey_responses.csv not supplied)
- choice_tasks_synthetic.csv (input choice_tasks.csv not supplied)
- validation_only_hidden_truth.csv (input hidden_truth.csv not supplied)

## Footprint items missing (not in supplied HESTIA file; nothing invented)
- soybean seed (whole) - land_occupation_area
- beef cattle - GWP100
- beef cattle - land_occupation_area
- chicken meat - GWP100
- chicken meat - land_occupation_area

Footprint label: global average, no Nigeria data; pitch criterion only, not per-state.

## Known limitations
1. Soy missing in 5 of 10 pilot states (Enugu, Lagos, Ogun, Oyo, Rivers): no state-level NASS soy for the South (soy_status_detail = zone_total_only / not_reported). Left NA, never zero.
2. Yobe soy is a DERIVED residual (NE zone total minus five other NE states); NASS suppressed the state value (S). Harvested area and yield not available.
3. Climate suspect for Kaduna and Plateau (annual rainfall implausible; user is re-pulling). Pairs Akwa Ibom/Cross River, Anambra/Delta, Ekiti/Ondo share a coarse NASA POWER cell.
4. Urban share missing: no state-level value in any supplied file; columns dropped.
5. Channel density is synthetic and collinear with wealth by construction (wealth_index scaled 0-1 stands in for urban share).
6. Vendor n is 3 to 6 per state (40 vendors in total); seasonality per state is indicative only.
7. Population reference year is undocumented (WorldPop/NPC, scaled to national total).
8. Unit economics: reported totals differ from the sum of the eight listed components; recomputed totals are in total_unit_cost_*, originals in reported_total_*. Producer net/margin/shelf price columns still rest on reported totals.
9. Footprint is a global average with no Nigeria data; pitch criterion only, not per-state.
10. Survey, vendor, choice, unit-economics and seasonality data are SYNTHETIC; hidden_truth is validation only.

## Sources
- WorldPop/NPC (population): licence: check license on source page
- NASA POWER (climate): licence: check license on source page
- DHS Program Nigeria 2024 (wealth, state-level aggregates only): licence: check license on source page
- NASS 2022/23 (soy output): licence: check license on source page
- HESTIA (footprint): CC BY 4.0, attribution required (hestia.earth)
- Survey, choice, vendor, unit-economics, seasonality and channel data: synthetic, generated for this project; contain no real personal data
