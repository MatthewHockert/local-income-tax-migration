# Do Local Income Taxes Cause Residential Sorting?

Code for the paper in `manuscript/muni_tax_sorting.tex`. It estimates how Pennsylvania municipal earned income tax (EIT) increases change where workers live relative to where they work, 2002 to 2019, with a continuous stacked difference-in-differences at the municipality level.

Matthew Hockert, matthew.hockert@gmail.com

## How to run it

Open `local_income_tax_migration.Rproj` (or `cd` into the repo) and run the steps in order. Every R script finds the repo root with `here::here()`, so it runs from anywhere inside the repo.

| Step | Command | Needs | Time |
|---|---|---|---|
| 1. Get data | the scripts in `scripts/1_get_data/` | internet | about 1 hour, once |
| 2. Build data | the scripts in `scripts/2_build_data/`, in number order | `data/raw/` | not timed |
| 3 and 4. Models, figures, tables | `Rscript run_all.R` | `data/derived/` | about 45 min (dCDH is 40 of it) |
| 5. Paper | see below | `figures/`, `tables/` | 1 min |

Steps 1 and 2 can be skipped when `data/raw/` and `data/derived/` come from the data deposit. `run_all.R` runs steps 3 and 4 only and writes a log per script to `logs/`.

```bash
cd manuscript && pdflatex muni_tax_sorting && biber muni_tax_sorting && pdflatex muni_tax_sorting && pdflatex muni_tax_sorting
```

## Scripts

| Script | What it does | Writes |
|---|---|---|
| `1_get_data/scrape_pa_dced_eit.py` | DCED municipal tax register, one `.xls` per year (EIT and property millage) | `data/raw/pa_dced_raw/`, `data/raw/pa_eit_rate_panel.csv` |
| `1_get_data/scrape_pa_dced_afr.py` | DCED municipal Annual Financial Reports (fiscal controls) | `data/raw/pa_afr_raw/`, `data/raw/pa_afr_panel.csv` |
| `1_get_data/scrape_nces_sd_finance.py` | Census F-33 school-district finances | `data/raw/pa_sd_afr_raw/`, `data/raw/pa_sd_finance_raw.csv` |
| `1_get_data/download_lodes.R` | LEHD LODES origin-destination flows, PA blocks | `data/raw/lodes_block/` |
| `1_get_data/download_property.py` | county tax register, PDE school-district millage, PA Revenue CLR factors | `data/raw/pa_county_raw/`, `pde_retax_raw/`, `clr_raw/` |
| `2_build_data/1_build_panels.R` | re-parses the DCED register to add SD ids, then builds the rate panel, SD controls and crosswalk, municipality and block panels | `data/raw/pa_eit_rate_panel.csv`, `data/derived/*.rds`, `sd_id_xwalk.csv` |
| `2_build_data/2_build_property_rates.py` | muni, SD and county millage times the CLR, as effective rates | `data/derived/pa_property_burden_panel.csv` |
| `2_build_data/3_repair_property_rates.R` | fixes rates where millage and CLR sit on different assessment bases | `data/derived/pa_property_burden_panel_repaired.csv` |
| `3_models/1_main_models.R` | headline binding decomposition, pooled specification, EIT revenue, flexible dose | `data/results/` |
| `3_models/2_event_studies.R` | realized-dose event studies, headline and pooled | `data/results/` |
| `3_models/3_robustness.R` | dCDH, Act 32 split, Act 47 exclusion, pre-trend battery, fake timing | `data/results/`, `figures/` |
| `4_output/1_figures.R` | every paper figure, summary tables | `figures/`, `tables/` |
| `4_output/2_tables.R` | every paper table, in-text number checks | `tables/` |
| `helpers/config.R` | all paths | |
| `helpers/helpers.R` | builds the estimation panel, cells and stacks, shared by every model script | |

Each model and output script is split into sections switched on by `RUN_*` flags at the top. Set a flag to `FALSE` to skip a section. Sections marked `PARKED` are off because they are not in the paper. `scripts/parked/bacon_cont_decomp.R` is a parked standalone diagnostic.

`1_main_models.R` stops if the headline drifts. A good run prints

```
[validation] headline Inc-bind matches spec: log_LW=-0.537 log_IC=-0.345 log_TJ=-0.423 share=-0.058
```

## Where each exhibit comes from

| Exhibit | Estimated in | Written by |
|---|---|---|
| Summary statistics, cell counts, PA map, rate changes by year, Act 32 CDF | | `1_figures.R` (`RUN_SUMMARY_STATS`) |
| Statute timeline | | `1_figures.R` (`RUN_LAW_TIMELINE`) |
| Pooled per-pp table | `1_main_models.R` (`RUN_POOLED`) | `2_tables.R` |
| Binding decomposition (headline) | `1_main_models.R` (`RUN_HEADLINE`) | `2_tables.R` |
| Realized-dose event studies, all `es_*` tables | `2_event_studies.R` | `2_tables.R` |
| feols event study by cell | `1_figures.R` (`RUN_FEOLS_ES_BYCELL`) | `1_figures.R` |
| dCDH table and figure | `3_robustness.R` (`RUN_DCDH_POOLED`, `RUN_DCDH_BYCELL`) | `2_tables.R`, `3_robustness.R` |
| Act 32, Act 47, pre-trend battery, fake timing (headline and pooled) | `3_robustness.R` | `2_tables.R` |
| Appendix A flexible-dose table | `1_main_models.R` (`RUN_FLEX_DOSE`) | `2_tables.R` (`RUN_INTEXT_NUMBERS`) |
| Appendix A dose-response figures (`att_linear_*`, `att_flex_*`) | | `1_figures.R` (`RUN_SPLINE_DOSE_RESPONSE`) |

## Data

`data/` is not in git. It holds `raw/` (downloads), `derived/` (built panels) and `results/` (model output CSVs). One exception to raw being download-only: `1_build_panels.R` rewrites `raw/pa_eit_rate_panel.csv` with SD ids added. All sources are public.

| Source | Used for |
|---|---|
| PA DCED Municipal Statistics tax register | muni and SD EIT rates, muni and SD property millage |
| PA DCED county tax register | county property millage |
| PA DCED Annual Financial Reports | municipal fiscal controls |
| PDE Real Estate Tax Rates (Wayback Machine before 2016-17) | fills school-district millage gaps in the DCED register |
| PA Department of Revenue common level ratios | converts millage to rates on market value |
| Census F-33 / NCES | school-district finances, SD crosswalk |
| LEHD LODES v8 | live-and-work, in-commuter and job counts |
| Census TIGER 2020 (via `tigris`) | block to municipality crosswalk, map |

Judgment calls a replicator should know about:

- School-district crosswalk (NCES to DCED): name matching with two hand-coded renames, KNOCH = SOUTH BUTLER CO and RIVER VALLEY = BLAIRSVILLE SALTSBURG. 500 of 500 districts match.
- PDE district names to `sd_id`: exact match on normalized names, the same two renames, and a fuzzy match that resolves 5 names. 14 PDE rows stay unmatched.
- PDE school year 2010-11 is treated as calendar 2010. Checked against the DCED register, this lines up 90.5% of district-years within 1 mill.
- DCED municipality to Census county subdivision: name and county match, about 99.6%.
- The binding cells (Inc-bind, Inc-becomes, Inc-nobind) are a deterministic function of the rate panel.

## Software

R 4.5 with `data.table`, `fixest`, `dplyr`, `tidyr`, `stringr`, `ggplot2`, `scales`, `patchwork`, `ggrepel`, `sf`, `tigris`, `lehdr`, `readxl`, `janitor`, `stringdist`, `DIDmultiplegtDYN`, `here`.

Python 3.13 with `pandas`, `xlrd` 2.x, `openpyxl` and `requests`. The author runs `~/packaging-test/pkg-test/bin/python`. The property build also needs `pdftotext` (poppler).

LaTeX with `biber`. `manuscript/references.bib` is exported from Zotero, so edit citations there, not in the file.
