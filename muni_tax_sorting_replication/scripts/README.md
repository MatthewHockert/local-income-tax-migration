# Script map

Every analysis script, grouped into stages: an **optional** scrape (00), then
01–05. Each stage has a **runner** (`NN_*.R`) that runs its scripts in order as
isolated subprocesses and logs OK/FAIL to `logs/`. Run a whole stage, or any
single script, from the package root:

```bash
Rscript scripts/00_scrape.R            # OPTIONAL: download LODES — the only step needing internet
Rscript scripts/03_robustness.R        # run every robustness model
Rscript scripts/q3_unified_dcdh_robustness.R   # or just one script
Rscript scripts/run_all.R              # stages 02 -> 05 (from the vendored panels)
```

**Network:** only `00_scrape.R` needs the internet (it scrapes LODES from Census).
If you already have the data (deposit, or a prior `00` run), skip it — 01–05 run
entirely from local files.

`run_all.R` runs **main models → robustness → figures → tables**. It does *not*
run `01_data_prep.R` (that regenerates the panels from raw inputs + network; the
package ships the built panels). To compile the PDF, see `../run_repro.sh`.

## Stage 00 — scrape  (`00_scrape.R`) — *OPTIONAL, needs internet*
The only step that hits the network. **Skip it** if `data/lodes_block/` is already
present — everything below runs from local files.
| Script | What it does |
|---|---|
| `download_lodes.R` | PA block LODES OD from the Census **LEHD** server (~4 GB, ~60 min) |

## Stage 01 — data prep  (`01_data_prep.R`) — *builds panels from local files*
No network needed once `00_scrape.R` (or the data deposit) and the vendored
`data/pa_cousub.rds` are in place. `build_data.R` falls back to TIGER geometry via
`tigris` (cached) only for PA boundaries not already on disk.
| Script | What it does |
|---|---|
| `reparse_dced.R` | DCED EIT rate panel, keeping SD id/name |
| `build_sd_controls.R` | Census F-33 → SD share controls + NCES↔DCED crosswalk |
| `build_data.R` | cousub rate / OD / fiscal panels + block→cousub crosswalk (PA-only) |
| `build_block_panel.R` | block × year panel inheriting rates/shares |

## Stage 02 — main models  (`02_main_models.R`)
| Script | What it does | Key output |
|---|---|---|
| `q3_unified_full_table.R` | **HEADLINE** binding decomposition (Inc-bind etc.), all outcomes. Sourced by most robustness scripts. | `data/q3_unified/headline_full_table.csv` |
| `q3_unified_pooled.R` | pooled-Increase per-pp benchmark | `pooled_results.csv` |
| `q3_unified_headline_es.R` | realized-dose event study (main pre-trend diagnostic) | `headline_es_*.csv` |
| `q3_unified_pooled_es.R` | pooled-Increase event study | `es_pooled_*.csv` |
| `q3_unified_eit_revenue.R` | EIT revenue response (results section) | revenue CSVs |

## Stage 03 — robustness  (`03_robustness.R`)
The robustness checks the **paper** uses. (Exploratory checks not in the paper —
levels/property-tax/contdid/workbook — live in the main project repo, not this package.)
| Script | What it does | Key output |
|---|---|---|
| `q3_unified_pretrend_battery.R` | multi-test pre-trend battery (Wald / leads / slope / HonestDiD) | `pretrend_battery.csv` |
| `q3_unified_act32_3level.R` | pre/post Act 32 enforcement split → `act32_cousub.tex` | `act32_split_cousub.csv` |
| `q3_unified_act47_robustness.R` | Act 47 distressed-municipality exclusion → `act47_robustness.tex` | `act47_*.csv` |
| `q3_unified_dcdh_robustness.R` | de Chaisemartin–D'Haultfœuille (non-negative weights) → §rob-acr claim | `dcdh_robustness.csv` |
| `q3_unified_curvature_robustness.R` | ACRT/ATT curvature → `fig:curvature_att` | `curvature_*.csv`, `figures/curvature_att_curve.png` |

## Stage 04 — figures  (`04_figures.R`) — run after 02 + 03
| Script | What it does |
|---|---|
| `build_summary_stats.R` | summary tables + rate ECDF/CDF + descriptive figures (also writes `.tex`) |
| `build_uea_aggregate_figures.R` | pooled ES + dose-trajectory aggregate figures |
| `build_dose_trajectory_inc_becomes.R` | Inc-becomes dose-trajectory figure |
| `build_appendix_es_figures.R` | unified ribbon-style ES figures (paper appendix set) |

## Stage 05 — tables/output  (`05_tables.R`) — run after 02 + 03
| Script | What it does |
|---|---|
| `build_paper_tables.R` | all LaTeX table fragments → `tables/` |
| `build_intext_numbers.R` | reconcile the headline **in-text prose numbers** → value + source, with validation gates → `intext_numbers.csv` |
| `build_results_workbook.R` | every result CSV (tables + ES + **in-text numbers**) → `results/all_results.xlsx` |

## Infrastructure (not a stage; sourced by the above)
| File | Role |
|---|---|
| `config.R` | resolves package `ROOT` / `DATA_DIR` / `SCRIPT_DIR` from a script's own location |
| `paper_helpers.R` | shared formatting/helper functions |
| `_run_stage.R` | `run_stage()` used by the `NN_*.R` runners |
