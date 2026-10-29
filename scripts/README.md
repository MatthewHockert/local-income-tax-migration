# Script map

Seven self-contained stage scripts. Each contains its code **inline** in
flag-gated sections — open the script and set the `RUN_<SECTION> <- TRUE/FALSE`
flags at the top to run a subset. Shared machinery (paths, the panel/cell/stack
build, the pre-trend battery) lives in `_helpers.R`, sourced by every stage.

```bash
Rscript scripts/00_scrape.R        # OPTIONAL: download LODES — the only step needing internet
Rscript scripts/04_robustness.R    # one stage (dCDH + Act 32 + Act 47 + curvature)
Rscript scripts/run_all.R          # stages 02 -> 06 (from the vendored panels)
```

`run_all.R` runs **main models → event studies → robustness → figures →
tables**, each stage as an isolated subprocess with a log in `logs/`. It does
*not* run `01_data_prep.R` (that regenerates the panels from raw inputs; the
package ships the built panels). To compile the PDF, see `../run_repro.sh`.

| File | Sections (flags) | Reads | Writes |
|---|---|---|---|
| `00_scrape.R` | LODES download (optional, internet) | Census LEHD | `data/raw/lodes_block/` |
| `01_data_prep.R` | DCED reparse, SD controls, cousub panels, block panel | `data/raw/` | `data/derived/` |
| `02_main_models.R` | headline decomposition (validation gate), pooled benchmark, EIT revenue | `data/derived/` via `build_headline_env()` | `data/results/` |
| `03_event_studies.R` | headline realized-dose ES, pooled ES, pre-trend battery (`pretrend_bucket()`) | derived | results |
| `04_robustness.R` | dCDH pooled, dCDH by cell (slow), Act 32 split, Act 47 exclusion, curvature | derived | results + figures |
| `05_figures.R` | summary stats, aggregate figs, dose trajectory, appendix ES, descriptive scatters, spline dose-response, feols ES twin, law timeline | derived + results | `figures/` + summary `.tex` |
| `06_tables.R` | paper tables, in-text-number gate (off until prose rewrite), results workbook | results | `tables/` + `results/all_results.xlsx` |

Support files: `_helpers.R` (paths, `build_headline_env()`, `add_realized_dose()`,
`pretrend_bucket()`), `paper_helpers.R` (labels, theme, table formatter,
`wald_pretrend()`), `config.R` (ROOT + `RAW_DIR`/`DERIVED_DIR`/`RESULTS_DIR`),
`_run_stage.R` (subprocess runner, used by `00_scrape.R`), `download_lodes.R`,
and the standalone Python scrapers (`scrape_pa_dced_eit.py`,
`scrape_pa_dced_afr.py`, `scrape_nces_sd_finance.py`, `build_law_timeline.py`).

**Data layout:** `data/raw/` scraped inputs (never written by analysis) →
`data/derived/` built panels (stage 01) → `data/results/` model CSVs (stages
02–04). Archived outputs of dropped analyses: `data/results/_archive/`,
`figures/_archive/`.

**Retired scripts:** the pre-consolidation single-purpose scripts are in
`_retired/` (see its README — stale paths and, for older ones, stale controls).
The consolidation was verified byte-identical on all 152 result CSVs and 14
table fragments before retirement.
