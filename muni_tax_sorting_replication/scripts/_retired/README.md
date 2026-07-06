# Retired scripts

Superseded 2026-07-02 when the pipeline was consolidated into seven
flag-gated stage scripts (`00_scrape.R` ... `06_tables.R` + `_helpers.R`).
Kept for provenance and future replication, not run by the active pipeline.

Three things to know before running anything in here:

1. **Paths are stale.** These scripts predate the `data/raw/` /
   `data/derived/` / `data/results/` split — they read `data/<file>` and
   write `data/q3_unified/`. Point them at the new locations (or source
   `config.R` and use `RAW_DIR` / `DERIVED_DIR` / `RESULTS_DIR`).
2. **Controls may be stale.** Scripts retired before 2026-07 use the old
   control set (`share_eit`, `share_re`) rather than the property effective
   rates. The active control set lives in `_helpers.R::build_headline_env()`.
3. **Their outputs are archived, not deleted** — result CSVs in
   `data/results/_archive/`, figures in `figures/_archive/`.

Contents by group:

- `q3_unified_full_table.R`, `q3_unified_pooled.R`, `q3_unified_eit_revenue.R`,
  `q3_unified_headline_es.R`, `q3_unified_pooled_es.R`,
  `q3_unified_pretrend_battery.R`, `q3_unified_dcdh_robustness.R`,
  `q3_unified_dcdh_bycell.R`, `q3_unified_act32_3level.R`,
  `q3_unified_act47_robustness.R`, `q3_unified_curvature_robustness.R`
  — consolidated verbatim into stages 02-04 (verified byte-identical outputs).
- `build_summary_stats.R`, `build_uea_aggregate_figures.R`,
  `build_dose_trajectory_inc_becomes.R`, `build_appendix_es_figures.R`,
  `build_extra_figures.R`, `build_acr_acrt_att_spline.R`,
  `build_feols_es_bycell.R`, `build_law_timeline.R`
  — consolidated into stage 05.
- `build_paper_tables.R`, `build_intext_numbers.R`, `build_results_workbook.R`
  — consolidated into stage 06.
- `build_data.R`, `build_sd_controls.R`, `build_block_panel.R`,
  `reparse_dced.R` — consolidated into stage 01.
- Old stage runners: `02_main_models` predecessors `03_robustness.R`,
  `04_figures.R`, `05_tables.R`.
- Dropped-from-paper analyses (re-runnable if the block/SD units or binary
  Sun-Abraham ES are ever revived): the block/SD-unit and SA-binary builders
  consolidated sections no longer call.
