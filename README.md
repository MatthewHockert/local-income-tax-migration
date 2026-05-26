# Local Taxes and Residential Sorting in Pennsylvania

Do changes in local taxes cause residential sorting? This repository
estimates the effect of municipal **Earned Income Tax (EIT)** rate changes —
and, in the companion paper, **property-tax** changes — on where workers live
relative to where they work, and on local consumer-services employment, in
Pennsylvania, 2002–2019.

The Pennsylvania EIT is the largest source of local-income-tax variation in
the United States. Its piecewise formula

```
T = max( tau^M + tau^SD ,  tau^nr )
```

(home-side stack = municipal resident rate + school-district resident rate,
versus the work-side nonresident rate) generates predictable cross-sectional
variation in whether a statutory rate change actually moves a worker's bill.
**Act 32 of 2008** (effective January 1, 2012) is the state-wide enforcement
break that anchors identification: pre-2012 rate changes were poorly enforced,
post-2012 changes bind through consolidated employer withholding.

## The two papers

This repo builds **two** manuscripts. Both are self-contained and both
replicate from the pipeline below.

| File | Scope |
|------|-------|
| `paper/UEA_draft.tex` | Succinct EIT-only paper (the conference draft). |
| `paper/muni_tax_sorting.tex` | Two-leg paper: the EIT result **plus** a property-tax leg and an EIT-vs-property dollar-basis comparison. |

> `paper/main.tex` is a **retired** earlier manuscript, superseded by
> `muni_tax_sorting.tex`. It is kept in the tree for reference but is not part
> of the active pipeline, and the scripts that only fed it have been retired
> (see [`_retired/`](#retired-and-legacy-scripts)).

## Empirical framework

The headline model is a **continuous stacked difference-in-differences**
design estimated at the **municipality (Census county-subdivision, "cousub")**
level, with two simultaneous rate-setter treatments — the municipal resident
rate `tau^M` and the school-district resident rate `tau^SD`. Each rate-change
event is classified by direction (Inc/Dec) and binding-state transition
(bind / becomes / nobind). The **Inc-nobind** cell is the within-design
placebo: hikes whose post-event home stack stays below the work-side
nonresident rate should produce no behavioral response, and largely don't.

```
Y_it = alpha_{i x s} + gamma_{t x s}
     + sum_{c != NC} beta^M_c  1{cell^M_i  = c} * dTau^M_it
     + sum_{c != NC} beta^SD_c 1{cell^SD_i = c} * dTau^SD_it
     + X_it' phi + eps_it
```

with stack-by-unit and stack-by-year fixed effects, clustered at the
municipality. Pre-trend tests are run **separately** as realized-dose
Sun–Abraham continuous event studies (strong parallel trends across dose
magnitudes), and robustness via Rambachan–Roth honest-DiD sensitivity bounds.

The fully unified design is motivated at three units of observation
(municipality / school district / census block) in
`paper/guiding_principles.md`; **the SD-as-unit and block-as-unit estimators
are no longer reported** in either paper (municipality has the cleanest
pre-trend), so their estimation scripts have been retired from the active
pipeline. The LODES data are still block-level and aggregated to the cousub.

## Headline findings

- **Municipality (cousub) is the clean-headline geography.** A 1 pp municipal
  resident-rate increase in the **binding** cell reduces `log(live_and_work)`
  by **−0.573** log points per pp, and this outcome has a **clean pre-trend**
  (realized-dose ES pre-coefficient ≈ −0.07, joint Wald p ≈ 0.33 — the only
  level that does not reject parallel trends).
- In typical-event terms, a representative binding increase lowers the count
  of workers who both live and work in their own municipality by **≈ 13%**,
  with the **Inc-nobind placebo null** — workers respond to the *bundle* of
  local taxes and amenities, not to statutory rates alone.
- Municipal and school-district rates have **contrasting** estimated effects
  (municipal increases → negative sorting; SD increases → positive), estimated
  jointly in the same regression.
- (`muni_tax_sorting.tex` only) A **property-tax** realized-dose event study
  on the same outcomes shows the property channel operates on residence
  **scale** rather than commuting composition, and a dollar-basis comparison
  puts the EIT and property responses on a common per-\$1,000-of-tax footing.

## Requirements

**R 4.5+:**

```r
install.packages(c("sf", "dplyr", "tidyr", "stringr", "data.table",
  "fixest", "ggplot2", "lehdr", "tigris", "readxl", "janitor",
  "tidycensus", "stringdist", "beepr"))
```

`fixest` provides the TWFE / Sun–Abraham machinery; honest-DiD bounds use the
`HonestDiD` package (`remotes::install_github("asheshrambachan/HonestDiD")`).

**Python 3.10+:** `pip install requests pandas xlrd` (scrapers and the
property-tax assembly). The ACS step uses the Census API — set
`CENSUS_API_KEY` in your environment (`tidycensus::census_api_key()` for the R
side).

## Replication — EIT (income-tax) leg

The master script `run_active.R` runs the EIT pipeline end to end with
stage-gating flags (`RUN_DATA_BUILD`, `RUN_DESCRIPTIVES`, `RUN_MAIN_MUNI`,
`RUN_HET_ACT32`, `RUN_ROB_PRETREND`, `RUN_ROB_HONESTDID`, `RUN_PAPER_BUILD`).
`RUN_DATA_BUILD` is `FALSE` by default (the ~4 GB LODES + panel build is a
one-time step); set it `TRUE` for a cold build. To run the steps by hand:

```bash
# 1. Scrape rates + finances (one-time)
python3 scrape_pa_dced_eit.py        # -> data/pa_eit_rate_panel.csv
python3 scrape_pa_dced_afr.py        # -> data/pa_afr_panel.csv (muni fiscal shares)
python3 scrape_nces_sd_finance.py    # Census F-33 SD finances, FY 2002-2019
Rscript reparse_dced.R               # recover SD ID/name on DCED rates
Rscript build_sd_controls.R          # F-33 -> SD share controls + ID xwalk (500/500)

# 2. LODES at block level (one-time, ~4 GB, ~60 min)
Rscript download_lodes.R

# 3. Build cousub + block panels
Rscript build_data.R                 # cousub panels + block->cousub xwalk
Rscript build_block_panel.R          # block x year panel (feeds honest-DiD block bounds)

# 4-7. Everything else, in order, via the master script:
Rscript run_active.R
```

`run_active.R` then runs, in stage order:

```
build_summary_stats.R     [1b] summary tables + rate ECDF + Act-32 CDF + maps
q3_unified_full_table.R   [2]  cousub headline (stacked; muni + SD cells)
q3_unified_pooled.R       [2]  pooled Inc/Dec benchmark (cousub)
q3_unified_pooled_es.R    [2]  pooled realized-dose ES (cousub)
q3_unified_act32_3level.R [3]  pre/post-Act-32 split (cousub)
q3_unified_headline_es.R  [4]  cousub realized-dose event study (pre-trend test)
q3_unified_act47_robustness.R [4] Act 47 distressed-muni robustness table
q3_unified_honestdid.R    [4]  Rambachan-Roth sensitivity (cousub; SD self-skips)
build_paper_tables.R      [5]  -> paper/tables/*.tex
build_appendix_es_figures.R   [5] -> figures/es_realized_*.png
build_uea_aggregate_figures.R [5] -> pooled_es_muni_only + dose-trajectory figs
build_dose_trajectory_inc_becomes.R [5] -> support_dose_trajectory_inc_becomes.png
```

`run_active.R` reproduces every EIT-leg paper table and figure in one command.

## Replication — property-tax leg (`muni_tax_sorting.tex` only)

The property-tax effective rate is `(muni_mills + sd_mills + county_mills)/1000
* CLR`, where CLR is the county common-level (assessed/market) ratio. Treatment
is the **policy-only** discretionary millage change (CLR drift stripped,
revenue-neutral reassessment rolled to zero). Run in dependency order:

```bash
# Raw property sources
python3 scrape_pa_dced_county_tax.py    # county RE millage -> data/multitax/pa_county_tax_panel.csv
python3 download_pde_retax.py           # PDE RE-tax files (Wayback + live) -> data/pde_retax_raw/
python3 parse_clr_history.py            # PA Revenue CLR PDF -> pa_clr_panel.csv (+ reassessment years)

# Assemble the millage panels (DCED register + PDE backfill)
python3 multitax_coverage_report.py     # coverage diagnostics (COVERAGE_REPORT.md)
python3 multitax_build_muni_panel.py    # -> pa_muni_multitax_panel.csv
python3 multitax_build_sd_panel.py      # -> pa_sd_millage_panel.csv (register; gaps flagged)
python3 build_pde_sd_millage.py         # -> pde_sd_millage.csv (PDE-extracted)
python3 multitax_validate_pde.py        # picks the school-year->calendar mapping (B_start)
python3 multitax_gapfill_sd.py          # -> pa_sd_millage_panel_filled.csv (~99% coverage)

# Effective rate + supporting panels
python3 multitax_effective_rate.py      # -> pa_property_burden_panel.csv (the analysis panel)
python3 multitax_reassessment_inventory.py  # county reassessment timing (for the policy-only filter)
python3 multitax_acs_dollar_base.py     # ACS median income/home -> pa_acs_dollar_base.csv (per-$ basis)

# Estimation (requires the EIT leg's q3_unified_full_table.R outputs first)
Rscript q3_proptax_es.R       # property realized-dose ES -> figures/proptax_es_scale_vs_comp.png
Rscript q3_proptax_compare.R  # EIT-vs-property dollar-basis table -> proptax_eit_comparison.csv
Rscript q3_eit_es_decomp.R    # parallel EIT ES (scale/composition asymmetry framing)
```

`multitax_millage_profile.py` and `multitax_sales_tax.py` are reference/
diagnostic only (entanglement profile; local sales-tax constants) and are not
required for the property-tax estimates.

## Compile the papers

Both use `biblatex`/`biber`:

```bash
cd paper
for p in UEA_draft muni_tax_sorting; do
  pdflatex -interaction=nonstopmode $p && biber $p && pdflatex $p && pdflatex $p
done
```

Current output: `UEA_draft.pdf` (42 pp), `muni_tax_sorting.pdf` (52 pp).

## Where each paper number comes from

| Result | Script | Artifact |
|--------|--------|----------|
| Cousub headline cell × dose table | `q3_unified_full_table.R` → `build_paper_tables.R` | `paper/tables/headline_muni_combined.tex`, `app_full_muni_cell_results.tex` |
| Pooled Inc/Dec benchmark | `q3_unified_pooled.R` | `paper/tables/pooled_per_pp_cousub.tex` (← `pooled_results.csv`) |
| Pre/post-Act-32 split | `q3_unified_act32_3level.R` | `paper/tables/act32_cousub.tex` (← `act32_split_cousub.csv`) |
| Realized-dose ES (pre-trend test) | `q3_unified_headline_es.R`, `q3_unified_pooled_es.R` | `paper/tables/es_realized_cousub.tex`, `es_pooled_cousub_summary.tex`; `figures/es_realized_*.png` |
| Honest-DiD bounds | `q3_unified_honestdid.R` | `data/q3_unified/honestdid_sensitivity.csv` (cited in prose) |
| Act 47 robustness | `q3_unified_act47_robustness.R` | `paper/tables/act47_robustness.tex` |
| Summary stats / cell counts / maps | `build_summary_stats.R` | `paper/tables/summary_{muni,sd,block}.tex`, `cell_counts_summary.tex`; `figures/pa_map.png`, `rate_changes_by_year.png` |
| Property-tax ES (fig) | `q3_proptax_es.R` | `figures/proptax_es_scale_vs_comp.png` |
| EIT-vs-property \$ comparison | `q3_proptax_compare.R` | `data/q3_unified/proptax_eit_comparison.csv` (cited in prose) |

## External data sources

All public. Steps above download them.

| Source | Provider | Coverage | Script |
|--------|----------|----------|--------|
| EIT rates (resident + nonresident + SD) | PA DCED MunStats | 2,500+ munis, 2000–2026 | `scrape_pa_dced_eit.py` |
| Municipal fiscal shares (AFR) | PA DCED MunStats | 2,500+ munis, 2002–2023 | `scrape_pa_dced_afr.py` |
| F-33 school-district finances | U.S. Census Bureau | 500 PA SDs, FY 2002–2019 | `scrape_nces_sd_finance.py` |
| LODES origin–destination | Census LEHD | block-level, 2002–2021 (v8) | `download_lodes.R` (`lehdr`) |
| TIGER/Line geometry | Census Bureau | cousubs / SDs / blocks (2020) | `tigris` |
| County real-estate millage | PA DCED CountyTaxSummary | 67 counties, 2002–2019 | `scrape_pa_dced_county_tax.py` |
| School-district RE millage | PA Dept. of Education | school-years 2002–03…2019–20 | `download_pde_retax.py` → `build_pde_sd_millage.py` |
| Common Level Ratio (CLR) | PA Dept. of Revenue | county-year + reassessment dates | `parse_clr_history.py` |
| ACS median income / home value | Census ACS 5-yr (B19013, B25077) | cousub, 2009–2019 | `multitax_acs_dollar_base.py` |

The `data/` directory is **not** tracked (LODES alone is several GB); the steps
above regenerate every derived file from public sources.

## Retired and legacy scripts

- **`_retired/`** holds scripts removed from the active pipeline during the
  2026 cleanup because **neither paper reports their results**: the SD-as-unit
  and block-as-unit estimators and their event studies
  (`q3_unified_sd_level.R`, `q3_unified_block_joint.R`, `q3_unified_sd_es.R`,
  `q3_unified_block_es_full.R`), the industry decomposition, muni-type
  heterogeneity, and the SA-binary / tercile / alt-control robustness passes,
  plus the figure/table builders that only fed them. They are moved (not
  deleted); git history and `git mv` preserve provenance. Run one directly
  with `Rscript _retired/<name>.R` if needed.
- Other exploratory scripts from earlier paper structures remain in the
  repository root but are **not** in the active pipeline (`run_active.R` does
  not source them). `REPO_INVENTORY.md` classifies every script.

## Sample, joins, conventions

- **Sample:** 2002–2019, PA only. Trimmed at load (`max_year <- 2019`) to
  exclude COVID-contaminated LODES (2020–21) and ARPA-distorted fiscal data
  (2020–23). Cousub estimation typically restricts to `total_jobs > 50` and
  non-missing fiscal controls.
- **LODES → cousub:** block-level LODES merged via `pa_block_xwalk.rds`; 2020
  Census blocks nest exactly in cousubs.
- **DCED muni → Census cousub:** `municipality_id` → `COUSUBFP` by name +
  county, ~99.6% match.
- **NCES SD → DCED SD:** `LEAID` → `sd_id` by normalized name + Jaro–Winkler
  fallback + 2 manual overrides; 500/500 matched.
- **Muni → SD rate (cousub model):** `max(sd_eit_rate)` per muni-year; exact
  for the 97.6% single-SD munis, conservative for the rest.
- **Significance:** `* p<.1, ** p<.05, *** p<.01` (`setFixest_etable`).

## Contact

Matthew Hockert — matthew.hockert@gmail.com
