# CLAUDE.md

## Project Overview

Do local income tax changes cause residential sorting? Uses LEHD LODES Origin-Destination data, aggregated from the **block** level to PA county subdivisions (cousubs), to measure whether workers relocate residence outside taxing municipalities. **PA-only** (Ohio dropped from the paper). The PA Earned Income Tax operates piecewise: each worker's bill = max(home_stack, work_nonresident_rate), with home_stack = muni_resident_rate + SD_rate. Act 32 of 2008 (effective 2012) made the EIT binding via consolidated employer withholding — the natural experiment.

## Headline Findings (current state, post block-level rebuild)

**Q2 muni rate decomposition, full sample TWFE:**
- share_live_in: muni rate −0.012\*\*; SD rate **null** (was +0.147\*\* under the old tract-aggregated panel — that effect was an artifact of cousub-level job misattribution).
- log(Resident workers): muni rate −0.201\*\*\*; SD rate null.
- log(Total Jobs): muni rate −0.128\*\*\*; SD rate null.

**Estimator triangulation (resident-side, log Resident workers):**
- Q2 TWFE pooled: −0.201\*\*\* (likely inflated by negative weighting under heterogeneous staggered effects).
- Single-event TWFE: −0.162\*\*\*.
- DCDH ATE: −0.049 (CI spans 0; underpowered without bootstrap).
- Stacked binary: ~0 (binary post collapses dose).
- **Stacked continuous (recommended headline): −0.091\*\*** (≈9% drop in resident workers per pp; 7% on total jobs).

**Act 32 structural break:** muni × Pre is null on share_live_in (−0.008); muni × Post is significant (−0.013\*\*); log(Resident) effect roughly *doubles* post-Act 32 (−0.139\*\*\* → −0.209\*\*\*). SD coefficient null in both periods.

**Q3 clean groups (Inc-only / Dec-only / NC, flip-floppers dropped):** muni × Inc = −0.016\*\* on share_live_in, −0.221\*\*\* on log(Resident). Dec-only is too thin (15 munis) to identify; SA pre-trends underpowered, ATT not credibly estimated.

**Nonresident-side parallel build:** point estimates negative on log(Nonres workers) and log(Total jobs) (~−0.13 to −0.30 per pp) but no estimator crosses 5% — smaller treatment group, smaller dose variation. Direction consistent with resident side.

**Q3 unified decomposition** (replaces per-block estimator stack and the piecewise pair-flow model): single design splits each muni-rate event by direction (Inc/Dec) × binding state (post-event home_stack vs muni_nonres). Five estimators run on each block. Predicted-null placebo cell is Inc × non-binding (rate hike doesn't move bill). See `q3_unified_decomposition.R`.

**Asymmetry**: Inc-only effects identified; Dec-only not identified (15 munis, underpowered). Subsumed within the unified decomposition's Inc/Dec rows.

**Mechanism**: rate increases push residents out (residential margin); total jobs fall via local-services-demand spillover (not via direct nonresident taxation, since 74% of resident-rate hikes leave the nonres rate unchanged).

## Run Order

```bash
# 1. Scrape PA tax rates and municipal finances
python3 scrape_pa_dced_eit.py
python3 scrape_pa_dced_afr.py

# 2. Reparse DCED XLS files to recover School District ID/name
Rscript reparse_dced.R

# 3. Download LODES at block level (PA only); ~4 GB, ~60 min
Rscript download_lodes.R

# 4. Build all intermediate datasets (block→cousub crosswalk + cousub OD panel)
Rscript build_data.R

# 5. Slide-model bundle: Q2 + Act 32 + Q3 clean groups + ES figures
Rscript run_slide_models.R

# 6. Q3 robustness (resident side)
Rscript q3_robustness.R          # single-event TWFE + DCDH on inc + NC
Rscript q3_stacked.R             # stacked DiD binary + continuous
Rscript make_dcdh_es_figure.R    # builds DCDH event-study from saved coefs

# 7. Q3 nonresident-side parallel
Rscript q3_nonres.R              # 4 estimators against muni_nonres_eit

# 8. Residence-side panel
Rscript build_residence_panel.R  # aggregate LODES OD by home cousub
Rscript q3_residence.R           # 4 estimators against home muni resident_eit

# 9. Q3 unified decomposition (replaces piecewise pair-flow model)
Rscript q3_unified_decomposition.R  # 5 estimators × 3 blocks × Inc/Dec × bind/no-bind

# 10. SD heterogeneity diagnostic (confirms SD attraction was an artifact)
Rscript sd_heterogeneity.R

# Legacy / reference
# Rscript local_income_tax_migration.R   # 4400-line master, mostly superseded
# Rscript companion_outcomes.R           # earlier decomposition
# Rscript pretreatment_balance.R         # 11-outcome balance + SA pre-trends
# Rscript robustness.R                   # Philly + Act 32 + fiscal robustness
# Rscript run_paper_models.R             # earlier paper-ready SA models
# Rscript run_dcdh.R                     # earlier DCDH (slow, kept for reference)
```

## Scripts

| Script | Purpose |
|--------|---------|
| `scrape_pa_dced_eit.py` | PA EIT rates → `data/pa_eit_rate_panel.csv` |
| `scrape_pa_dced_afr.py` | PA municipal finances → `data/pa_afr_panel.csv` |
| `reparse_dced.R` | Reparse PA DCED XLS files keeping School District ID/name (the original parser dropped these) |
| `download_lodes.R` | PA LODES at block level (`agg_geo = "block"`) → `data/lodes_block/*.rds`. ~4 GB. Legacy OH tract files remain in `data/lodes/`. |
| `build_data.R` | Rate panels, block→cousub crosswalk (`pa_block_xwalk.rds`), neighbors (80km), cousub-level OD panels via `build_city_od_block()` |
| `run_slide_models.R` | Slide-relevant models: Q2 TWFE total stack + decomposition, Act 32 break, Q3 clean groups (Inc/Dec/NC), Sun-Abraham event-study figures |
| `q3_robustness.R` | Single-event TWFE on inc-only with exactly one increase + DCDH continuous on inc + NC; rate-events-by-year and events-per-muni figures |
| `q3_stacked.R` | Stacked DiD binary + continuous Δτ on single-event Inc + NC; ±5-yr window, NC-only controls |
| `q3_nonres.R` | Same four estimators as resident-side, but treatment = `muni_nonres_eit`, outcomes = `log_live_outside`, `share_nonres`, `log_total_jobs` |
| `q3_unified_decomposition.R` | Replaces the piecewise pair-flow model. Decomposes each muni-rate event by direction (Inc/Dec) × binding (post home_stack vs muni_nonres). Runs 5 estimators on Blocks A/B/C with cell × Δτ interactions. |
| `make_dcdh_es_figure.R` | Renders DCDH event-study figure from saved per-period effects/placebos |
| `build_residence_panel.R` | Residence-side aggregation by home cousub: residents_employed, residents_work_in_own, residents_work_outside, share_work_outside |
| `sd_heterogeneity.R` | Diagnostic: confirms SD attraction effect was an artifact of tract-centroid job misattribution |
| `pilot_block_lodes.R` | One-year pilot used to validate the block migration (kept for reference) |
| `paper/main.tex` | LaTeX manuscript (PA-only) |
| `paper/slides.tex` | 56-page slide deck (advisor presentation) |
| `references/local_income_taxes.bib` | BibTeX (current; LODES technical citations included) |

## Sample: 2002–2019

Trimmed at load time (`max_year <- 2019`) to exclude COVID-contaminated LODES 2020–2021 and ARPA-distorted fiscal data 2020–2023. Estimation samples typically further restrict to `total_jobs > 50` and require non-missing fiscal controls.

## Data

| File | Contents |
|------|----------|
| `data/lodes_block/` | PA block-level LODES (current pipeline): pa_od_S000_main.rds, pa_od_SE03_main.rds, pa_rac_*.rds |
| `data/lodes/` | Legacy tract-level LODES (OH + earlier PA tract files) |
| `data/pa_block_xwalk.rds` | Block GEOID → cousub crosswalk (TIGER 2020 blocks via centroid `st_within`). Recovers all 2,561 PA cousubs vs 1,479 under tract-centroid join. |
| `data/pa_eit_rate_panel.csv` | DCED rates with School District ID/name preserved (rebuilt by `reparse_dced.R`) |
| `data/pa_afr_panel.csv` | PA municipal finances (56K rows, 71 cols, 2002–2023) |
| `data/pa_rate_panel.rds` | PA muni × year rate panel with cousub_fips, resident_eit, sd_eit, total_eit, muni_nonres_eit (post-2012 backfill applied), event_type |
| `data/pa_panel.rds` | Workplace-side cousub × year panel: total_jobs, live_and_work, live_outside, share_live_in + rates |
| `data/pa_residence_panel.rds` | Residence-side cousub × year panel: residents_employed, residents_work_in_own, residents_work_outside + home rates |
| `data/pa_pair_panel.rds` | Cousub-pair × year panel for piecewise model: flow, home_stack, tau_nr_w, work_binds, transition |
| `data/pa_fiscal_controls.rds` | log police/streets/debt/state aid + revenue shares |
| `data/pa_neighbors.rds` | Cousub pairs within 80km centroid distance |
| `data/pa_cousub.rds` | PA cousub geometry (TIGER 2020) |
| `data/crime_data/ucr_offenses_known_monthly/` | FBI UCR annual RDS (1960–present; legacy) |
| `figures/` | Publication-ready PNGs (300 DPI) |
| `paper/main.tex`, `paper/slides.tex` | Manuscript and slides |
| `references/local_income_taxes.bib` | BibTeX (active) |

## Key Joins

- **LODES → cousub**: block-level LODES merged via `pa_block_xwalk.rds`. Blocks nest within cousubs in the 2020 Census hierarchy, so the assignment is exact.
- **PA**: DCED `municipality_id` → Census `COUSUBFP` via municipality name + county matching (~99.6% match).
- **UCR crime**: `fips_place_code` → `cousub_fips` (93.5% match).
- **BPS permits**: `fips_mcd_code` → `cousub_fips` (93.3% match).
- **Muni → SD rate**: each (muni, year) cell gets `max(sd_eit_rate)` across the SDs reported by DCED. Only 2.4% of munis straddle multiple SDs (61 of 2,505), so the max-aggregation is correct for 97.6% of munis. Diagnosed in `sd_heterogeneity.R`; the prior +0.147\*\* SD attraction effect was traced to tract-centroid job misattribution, not multi-SD measurement.

## Slide Deck Structure (paper/slides.tex)

Resident-side Q3:
- Q2 Answer (TWFE total stack + decomposition)
- Act 32 Answer (pre/post-2012 split)
- Q3 Sample (Inc-only / Dec-only / NC)
- Q3 Answer + Q3 Full Results
- Binding-relevance slide (84% of resident-rate increases bind)
- (1) Single-event TWFE — table + ES (Sun-Abraham)
- (2) DCDH — table + ES
- (3) Stacked binary — table + ES
- (4) Stacked continuous — table + ES (recommended headline)
- (5) Piecewise binding model — setup, transition distribution, decomposition, ES
- Estimator triangulation (resident)

Nonresident-side Q3 (parallel):
- Motivation + group composition
- (1)–(4) same four estimators with table + ES on `log_live_outside`, `share_nonres`, `log_total_jobs`
- Nonres triangulation

Each slide has the estimating equation written out.

## Significance Convention

`* p < .1, ** p < .05, *** p < .01` (set via `setFixest_etable`).

## Dependencies

**R**: `sf`, `dplyr`, `tidyr`, `stringr`, `data.table`, `fixest`, `ggplot2`, `lehdr`, `tigris`, `readxl`, `janitor`, `tidycensus`, `beepr`, `DIDmultiplegtDYN`

**Python** (scrapers only): `requests`, `pandas`, `xlrd`

## GitHub

https://github.com/MatthewHockert/local-income-tax-migration
