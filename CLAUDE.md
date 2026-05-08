# CLAUDE.md

## Active model (current paper)

The headline empirical framework is a **unified continuous stacked
difference-in-differences design** estimated at three units of observation
(municipality, school district, census block) with two simultaneous
rate-setter treatments (muni resident rate $\tau^M$ and SD resident rate
$\tau^{SD}$). The piecewise PA EIT formula
$T = \max(\tau^M + \tau^{SD},\ \tau^{nr})$ enters via cell decomposition
(Inc-bind / Inc-becomes / Inc-nobind / Dec-bind / Dec-becomes-non /
Dec-nobind), where the work-side nonresident rate $\tau^{nr}$ is used to
classify whether each rate-change event moves the worker's actual bill.
The Inc-nobind cell is the within-design placebo.

**Pre-trend tests** are run as separate diagnostics:
- *Realized-dose Sun-Abraham continuous ES* — each unit's eventual cumulative
  $\Delta\tau$ at $+5$ as a constant treatment-magnitude tag interacted
  with relative-event-time indicators. Tests strong parallel trends
  across dose magnitudes (Callaway, Goodman-Bacon & Sant'Anna 2024).
- *Binary Sun-Abraham IW ES* (`fixest::sunab()`) — tests average parallel
  trends. Cousub and SD only; block-level IW is unstable due to thin
  cohort × event-time cells.

**Read `paper/guiding_principles.md` and `paper/outline.md` before making
changes to the regression specs.** Those documents are the source of truth
for the design choices.

## Control variable convention

| Estimating unit | Fiscal controls |
|---|---|
| Cousub | Muni AFR shares: `share_police`, `share_streets`, `share_debt_svc`, `share_eit`, `share_re`, `share_intergov` |
| School district | Census F-33 SD shares: `share_state_rev_sd`, `share_local_rev_sd`, `share_federal_rev_sd`, `share_property_tax_sd`, `share_instructional_sd`, `share_support_sd`, `share_capital_sd`, `share_debt_int_sd` plus `mean_muni_rate` (within-SD aggregate of muni rates) |
| Block | Both muni AFR shares AND F-33 SD shares (each block exposed to both governments) |

Each share is `<numerator> / <denominator>` (expenditure / total_exp or
revenue / total_rev). Time-varying within unit × year. The previous
muni-only / log-based control set has been retired; legacy scripts
(`run_slide_models.R`, `q3_robustness.R`, `q3_stacked.R`,
`pretreatment_balance.R`, `robustness.R`) still use it but are not in the
active pipeline.

## Fixed effects and clustering

Per the rule: **fixed effects follow the stacked estimating unit;
clustering follows the real policy jurisdiction that generates the tax
variation.** Stacking creates repeated copies of NC controls; FE must be
stack-interacted to keep within-cohort identification clean, but clusters
must be at the underlying unit so within-muni / within-SD residual
correlation is captured.

| Estimating unit | FE | Cluster |
|---|---|---|
| Cousub | `muni^stack + year^stack` | `~muni` |
| School district | `sd_id^stack + year^stack` | `~sd_id` |
| Block | `block_geoid^stack + year^stack` | `~muni + sd_id` |

## Project Overview

Do local income tax changes cause residential sorting? Uses LEHD LODES
Origin-Destination data at the **block** level for PA, 2002-2019. The
PA Earned Income Tax operates piecewise: each worker's bill is
$\max(\tau^M + \tau^{SD}, \tau^{nr})$. Act 32 of 2008 (effective 2012)
made the EIT binding via consolidated employer withholding — the
natural experiment for cell decomposition. PA-only (Ohio dropped from
the paper).

## Headline Findings

**Inc-bind cell across the three estimating units (per pp):**

| Outcome | Cousub | SD | Block |
|---|---|---|---|
| log_LW / log_home_count | **−0.401\*\*\*** | +0.066\* (sign reversed) | **−0.165\*\*\*** |
| share_live_in / home_in_muni_share | **−0.045\*\*\*** | +0.003 (null) | **−0.067\*\*** |
| log_total_residents | **−0.100\*\*\*** | +0.147\*\*\* (reversed) | n/a |

**Realized-dose ES pre-trend on Inc-bind:**
- Cousub log_LW: pre = +0.149 (|t|=2.66) ⚠ FAILS
- SD log_LW: pre = +0.061 (|t|=1.74) marginal (and post is wrong sign)
- Block log_home_count: **pre = −0.027 (|t|=0.51) ✅ CLEAN**

**Block-level identification has the cleanest pre-trends** and is the
defensible per-pp causal estimate. Cousub headline is overstated by
selection (high-eventual-dose munis on declining trajectories); credible
block-level estimate is roughly 40% the size of the cousub headline.
SD-level identification doesn't recover the causal direction reliably.

**Industry decomposition (cousub Inc-bind, log jobs by sector):**
- Total: −0.258\*\*\*; Consumer-facing: −0.297\*\*; Tradeable: +0.236 (null);
  Health/Edu: −0.253\*; Public: −0.706\*; share_tradeable: +0.106\*.
- Pattern matches local-services-demand-spillover prediction. SD-level
  industry pattern direction-mixed (consistent with weak SD-level
  identification overall).

**Pre-/post-Act-32 split:**
- Cousub log_LW Inc-bind: pre = −0.339, post = **−0.534** (effect
  strengthens after enforcement made EIT binding).
- Block log_home_count Inc-bind: pre = −0.173, post = −0.176 (similar;
  block-level effect predates Act 32, contradicting the binding-rule
  story at finer geography).

## Active pipeline run order

Master script: `Rscript run_active.R`. Stages are gated by logical flags
(`RUN_DATA_BUILD`, `RUN_MAIN_MUNI`, `RUN_MAIN_SD`, `RUN_MAIN_BLOCK`,
`RUN_HET_ACT32`, `RUN_HET_INDUSTRY`, `RUN_HET_MUNI_TYPE`,
`RUN_ROB_PRETREND`).

Sequence (manual, if not using `run_active.R`):

```bash
# 1. Scrape rates / muni AFR / SD F-33 (one-time)
python3 scrape_pa_dced_eit.py
python3 scrape_pa_dced_afr.py
python3 scrape_nces_sd_finance.py    # Census F-33; FY 2002-2019
Rscript reparse_dced.R                # recover SD ID/name on DCED rates
Rscript build_sd_controls.R           # F-33 -> SD share controls

# 2. LODES at block level (one-time, ~4 GB, ~60 min)
Rscript download_lodes.R

# 3. Build cousub + block panels
Rscript build_data.R                  # cousub-level rate / OD / fiscal panels
Rscript build_block_panel.R           # block panel with rates + share controls

# 4. Main results (continuous stacked DiD per level)
Rscript q3_unified_full_table.R       # cousub headline (muni + SD cells)
Rscript q3_unified_sd_level.R         # SD-level model
Rscript q3_unified_block_joint.R      # block-level joint TWFE

# 5. Heterogeneity
Rscript q3_unified_act32_3level.R     # pre/post Act 32 at each level
Rscript q3_unified_industry.R         # cousub industry decomposition
Rscript q3_unified_industry_sd.R      # SD-level industry (aggregated)
Rscript q3_unified_heterogeneity.R    # city / borough / township

# 6. Robustness (pre-trend ES + sample / dose-magnitude / alt-control)
Rscript q3_unified_headline_es.R      # cousub realized-dose ES
Rscript q3_unified_sd_es.R            # SD realized-dose ES
Rscript q3_unified_block_es_full.R    # block realized-dose ES
Rscript q3_unified_sa_binary_es.R     # binary SA-IW ES (cousub + SD)
Rscript q3_unified_tercile_3level.R   # dose-magnitude tercile
Rscript q3_unified_altctrl_3level.R   # alt-control (Cengiz clean-in-window)

# 7. Build paper tables and figures
Rscript build_paper_tables.R          # all LaTeX tables -> paper/tables/
Rscript build_industry_figure.R       # industry bar chart
```

## Active scripts

| Script | Purpose |
|--------|---------|
| `scrape_pa_dced_eit.py` | DCED EIT rates → `data/pa_eit_rate_panel.csv` |
| `scrape_pa_dced_afr.py` | DCED muni AFR → `data/pa_afr_panel.csv` |
| `scrape_nces_sd_finance.py` | Census F-33 SD finances → `data/pa_sd_finance_raw.csv` |
| `build_sd_controls.R` | F-33 → SD share controls + NCES↔DCED ID crosswalk (500/500 SDs match) |
| `reparse_dced.R` | Reparse DCED XLS keeping SD ID/name |
| `download_lodes.R` | PA block-level LODES |
| `build_data.R` | Cousub panels + block→cousub crosswalk |
| `build_block_panel.R` | Block × year panel inheriting rates/shares from cousub |
| `q3_unified_full_table.R` | Cousub headline: muni + SD cell × dose, with diagnostics |
| `q3_unified_sd_level.R` | SD-level analog with F-33 controls |
| `q3_unified_block_joint.R` | Block-level joint TWFE (muni + SD cells) |
| `q3_unified_headline_es.R` | Cousub realized-dose ES |
| `q3_unified_sd_es.R` | SD-level realized-dose ES |
| `q3_unified_block_es_full.R` | Block-level realized-dose ES (heavy — stacked panel ~30M rows) |
| `q3_unified_sa_binary_es.R` | Binary Sun-Abraham IW ES, all 3 levels |
| `q3_unified_act32_3level.R` | Pre/post Act 32 split, all 3 levels |
| `q3_unified_industry.R` | Cousub industry decomposition |
| `q3_unified_industry_sd.R` | SD-level industry (aggregated from cousub) |
| `q3_unified_heterogeneity.R` | City / borough / township |
| `q3_unified_tercile_3level.R` | Dose-magnitude tercile, all 3 levels |
| `q3_unified_altctrl_3level.R` | Alt-control (Cengiz clean-in-window), all 3 levels |
| `build_paper_tables.R` | LaTeX tables for paper |
| `build_industry_figure.R` | Industry bar chart |
| `run_active.R` | Master pipeline (calls above scripts; gated flags) |
| `paper/main.tex` | LaTeX manuscript |
| `paper/guiding_principles.md` | Design principles document |
| `paper/outline.md` / `paper/outline.tex` | Detailed paper outline |

## Legacy scripts (kept for reference, not in active pipeline)

`run_slide_models.R`, `q3_robustness.R`, `q3_stacked.R`,
`q3_residence.R`, `q3_nonres.R`, `pretreatment_balance.R`, `robustness.R`,
`q3_unified.R` (older 5-estimator version), `q3_unified_act32.R`
(cousub-only Act 32), `q3_unified_block_level.R` (older block stack),
`q3_unified_block_es.R` (muni-only block ES), `q3_unified_block_es_sd.R`
(SD-only block ES), `q3_unified_compare.R`, `q3_unified_es_realized.R`
(cousub muni-only realized-dose), `make_dcdh_es_figure.R`,
`q3_unified_robustness_passes.R`, `sd_heterogeneity.R`,
`q3_unified_decomposition.R`, `pilot_block_lodes.R`,
`build_residence_panel.R`, `build_industry_panel.R`,
`build_neighbors_rings.R`, `build_highway_panel.R`,
`q3_unified_industry.R` (kept for cousub-level industry — used by
active pipeline through `build_industry_table.R`).

To run a legacy script intentionally, source it directly: `Rscript <name>.R`.

## Sample: 2002–2019

Trimmed at load time (`max_year <- 2019`) to exclude COVID-contaminated
LODES 2020–2021 and ARPA-distorted fiscal data 2020–2023. Estimation
samples typically further restrict to non-missing fiscal controls and,
at the cousub level, `total_jobs > 50`.

## Data files

| File | Contents |
|------|----------|
| `data/lodes_block/` | PA block-level LODES (~4 GB) |
| `data/pa_block_xwalk.rds` | Block GEOID → cousub crosswalk (TIGER 2020) |
| `data/pa_eit_rate_panel.csv` | DCED rates with SD ID/name |
| `data/pa_afr_panel.csv` | PA muni AFR (56K rows, 2002–2023) |
| `data/pa_sd_finance_raw.csv` | Census F-33 SD finance, raw (10,790 SD-years, 146 cols) |
| `data/pa_sd_controls.rds` | F-33 SD share controls, matched to DCED sd_id |
| `data/pa_sd_id_xwalk.rds` | NCES `LEAID` → DCED `sd_id` crosswalk (500/500) |
| `data/pa_rate_panel.rds` | Muni × year rate panel |
| `data/pa_panel.rds` | Cousub × year workplace+residence panel |
| `data/pa_block_panel.rds` | Block × year panel (~5M rows) |
| `data/pa_residence_panel.rds` | Residence-side cousub panel |
| `data/pa_fiscal_controls.rds` | Legacy log-based controls (superseded) |
| `data/pa_industry_panel.rds` | Cousub × year × industry-bucket job counts |
| `data/pa_neighbors.rds` | Cousub-pair distances |
| `data/pa_cousub.rds` | PA cousub geometry (TIGER 2020) |
| `data/q3_unified/` | All current results CSVs (headline / ES / Act 32 / tercile / altctrl / industry / SA-IW) |
| `data/literature_comparison.csv` | 20-paper lit-comparison spreadsheet |
| `figures/` | Publication-ready PNGs |
| `paper/main.tex` | Manuscript |
| `paper/tables/` | Auto-built LaTeX table fragments |
| `references/local_income_taxes.bib` | BibTeX |

## Key joins

- **LODES → cousub**: block-level LODES merged via `pa_block_xwalk.rds`. Blocks nest in cousubs (Census 2020 hierarchy), exact assignment.
- **DCED muni → Census cousub**: `municipality_id` → `COUSUBFP` via name + county matching, ~99.6% match.
- **NCES SD → DCED SD**: `LEAID` → `sd_id` via name normalization with abbreviation-expansion + fuzzy fallback (Jaro-Winkler) + 2 manual overrides for completely-renamed SDs (KNOCH ↔ SOUTH BUTLER CO; RIVER VALLEY ↔ BLAIRSVILLE SALTSBURG). 500/500 DCED SDs matched.
- **Muni → SD rate (cousub-level model)**: each (muni, year) gets `max(sd_eit_rate)` across DCED-reported SDs. 97.6% of munis report to a single SD; max-aggregation is exact for those, conservative for the 2.4% multi-SD munis.
- **Muni → SD ID (block-level model)**: primary SD per muni = modal `sd_id` across years from raw DCED panel. Each block inherits its muni's primary SD.

## Significance convention

`* p < .1, ** p < .05, *** p < .01` (set via `setFixest_etable`).

## Dependencies

**R**: `sf`, `dplyr`, `tidyr`, `stringr`, `data.table`, `fixest`, `ggplot2`, `lehdr`, `tigris`, `readxl`, `janitor`, `tidycensus`, `beepr`, `stringdist` (for SD name fuzzy matching), `xlrd` (Python).

**Python** (scrapers only): `requests`, `pandas`, `xlrd`.

## GitHub

https://github.com/MatthewHockert/local-income-tax-migration
