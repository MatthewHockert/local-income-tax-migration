# Conventions

Notes on how the sample is built and how the data sources are joined. The README covers how to run things. Numbers here were checked against the code and data on 2026-10-30.

## Retired and legacy scripts

- The October 2026 trim removed every script whose results neither paper reports. That covers the SD-as-unit and block-as-unit estimators and their event studies (`q3_unified_sd_level.R`, `q3_unified_block_joint.R`, `q3_unified_sd_es.R`, `q3_unified_block_es_full.R`), the industry decomposition, muni-type heterogeneity, the SA-binary, tercile and alternative-control robustness passes, and the figure and table builders that only fed them.
- They are not in this branch. The last commit before the trim is `main` on GitHub (9ed9ba4, tagged `pre-trim` locally), so `git checkout pre-trim -- <file>` brings any of them back. A copy of the old repo root, including `_retired/` and `REPO_INVENTORY.md` (which classifies every old script), sits in the git-ignored `_archive_pre_trim/` folder.
- Sections that are not in the paper but may come back stay in their script with the flag set to `FALSE` and a `PARKED` note. `scripts/parked/bacon_cont_decomp.R` is a parked standalone diagnostic.

## Sample

- Pennsylvania only, 2002 to 2019. `build_headline_env()` in `helpers.R` trims at load (`max_year = 2019`) to keep out the COVID years of LODES (2020 to 2021) and the ARPA years of the fiscal data (2020 to 2023).
- There is no minimum-jobs filter. Older versions kept only towns with `total_jobs > 50`. The current code does not.
- Models run on complete cases of the outcome, dose and controls. `share_debt_svc` and `share_intergov` come from AFR columns that start in 2006, so the headline sample is effectively 2006 to 2019.
- A treated town gets a binding cell only if its nonresident rate is known the year before and the year of its first rate change. Without it the cell is missing and the town drops out of the cell regressions.
- Rates above 5 pp (10 pp for the total) in the DCED register are Local Services Tax dollar amounts in the rate column. `1_build_panels.R` drops those lines.
- Before 2012 many towns did not report their nonresident rate. A missing pre-2012 value takes the town's earliest post-2012 rate, and `nonres_imputed` flags it.

## Joins

- LODES to municipality. LODES version 8 reports every year, 2002 to 2021, on 2020 Census blocks, so block boundaries never change across years. In every year 100% of workers have both their home and work block in the 2020 block crosswalk (`pa_block_xwalk.rds`). Each block goes to the municipality its center falls in, which is exact because 2020 blocks nest in county subdivisions. 42 of the 336,985 blocks have a center outside every municipality (water and edge slivers). They hold 594 resident and 205 workplace worker-years out of 94 million in 2002 to 2019, and are dropped. Switching to LODES7 (2010 blocks) would need a block crosswalk.
- DCED municipality to Census county subdivision. `municipality_id` to `COUSUBFP` by name and county, after normalizing suffixes and spellings. 2,496 of 2,505 match (99.6%). Four are matched by hand (Bethel Park, Monroeville, Murrysville, Oil City). The nine that stay unmatched no longer exist on the 2020 map. Where several DCED towns map to one subdivision, `build_headline_env()` averages their resident and nonresident rates and keeps the highest SD rate.
- NCES school district to DCED school district. `NCESID` to `sd_id` in three steps. 487 match exactly once both names are cleaned (school-district words dropped, abbreviations expanded). 11 more match by Jaro-Winkler distance, a string-similarity score from 0 (identical) to 1 (nothing shared) that weights matching first letters, so it catches abbreviations like "NEW KENSNGTN-ARNLD" against "NEW KENSINGTON ARNOLD". A match needs a distance under 0.15, and all 11 are the same district (the farthest is Sayre Area at 0.129). The last 2, Knoch and River Valley, were renamed or merged and are paired by hand. 500 of 500 match.
- Municipality to SD rate. The SD rate for a town-year is `max(sd_eit_rate)` across its school districts. 98.0% of town-years in 2002 to 2019 have one district, so this is exact for them and takes the higher rate for the rest (57 of 2,489 towns are split at some point).

## Reporting

- Significance stars: `* p<.1, ** p<.05, *** p<.01`, set by `setFixest_etable()` at the top of each model script.
