# Local Income Tax and Residential Sorting

Do local income tax changes cause residential sorting? This project
estimates the effect of municipal Earned Income Tax (EIT) rate
changes on residential location and consumer-services employment in
Pennsylvania, 2002–2019. The Pennsylvania EIT is the largest source
of local-income-tax variation in the United States. Its piecewise
formula

$$T = \max\bigl(\tau^{M} + \tau^{SD},\ \tau^{nr}\bigr)$$

(home-side stack vs. work-side nonresident rate) generates predictable
cross-sectional variation in whether a statutory rate change moves
the worker's actual bill. Act 32 of 2008 (effective January 1, 2012)
provides the state-wide enforcement break that anchors causal
identification: pre-2012 rate changes were poorly enforced, post-2012
rate changes are binding through consolidated employer withholding.

## Empirical framework

The headline model is a **unified continuous stacked
difference-in-differences design** estimated at three units of
observation — municipality, school district, and census block — with
two simultaneous rate-setter treatments (muni resident rate $\tau^M$
and SD resident rate $\tau^{SD}$). Each rate-change event is
classified by direction (Inc/Dec) and binding-state transition
(bind / becomes / nobind), yielding six non-NC cells per side. The
Inc-nobind cell is the within-design placebo: rate hikes whose
post-event home stack stays below the work-side nonresident rate
should produce no behavioral response, and largely don't. The
estimating equation is

$$Y_{it} = \alpha_{i \times s} + \gamma_{t \times s}
+ \sum_{c \neq NC} \beta^{M}_c \mathbf{1}\{\text{cell}^M_i = c\}\,\Delta\tau^M_{it}
+ \sum_{c \neq NC} \beta^{SD}_c \mathbf{1}\{\text{cell}^{SD}_i = c\}\,\Delta\tau^{SD}_{it}
+ X_{it}'\phi + \varepsilon_{it},$$

with stack-by-unit and stack-by-year fixed effects. Pre-trend tests
are run separately as realized-dose Sun–Abraham continuous event
studies (testing strong parallel trends across dose magnitudes) and
binary Sun–Abraham IW event studies (testing average parallel trends).

For the design's full motivation and per-level treatment-aggregation
rules, see `paper/guiding_principles.md`. For the paper's section
structure, see `paper/outline.md`.

## Headline finding

A 1 percentage-point muni resident-rate increase in the binding cell
reduces:

- **Cousub-level** `log(live_and_work)` by **40 percent** (per pp);
  pre-trend on this outcome is positive (+0.149, $|t|=2.66$),
  so the magnitude is partly inflated by selection.
- **Block-level** `log(home_count)` by **30 percent** (per pp), with a
  near-zero pre-trend ($\beta_{\text{pre}}=-0.027$, $|t|<1$). This
  is the cleaner per-pp causal estimate.
- **`share_live_in`** (workers living and working in the same muni)
  drops 4–7 percentage points across cousub and block specs.

Industry decomposition shows consumer-facing services
(retail / food / arts / personal services) contract 30 percent in
the binding cell while tradeable goods/services (manufacturing /
wholesale / finance / professional services) are insulated —
consistent with local-services-demand spillover from residents
leaving rather than direct nonresident-tax incidence.

The school district rate, estimated jointly in the same regression,
does not produce a clean sorting response through the binding
channel.

## Replication

### Requirements

**R 4.5+** with the following packages:

| Package | Version | Purpose |
|---------|---------|---------|
| `sf` | 1.0+ | Spatial operations |
| `dplyr` / `tidyr` / `stringr` | 1.1+ / 1.3+ / 1.6+ | Tidyverse |
| `data.table` | 1.17+ | Fast data wrangling |
| `fixest` | 0.13+ | TWFE, Sun–Abraham, event studies, `sunab()` |
| `ggplot2` | 4.0+ | Visualization |
| `lehdr` | 1.1+ | LODES downloads via Census API |
| `tigris` | 2.2+ | Census TIGER/Line shapefiles |
| `readxl` / `janitor` | 1.4+ / 2.2+ | Excel reading + name cleaning |
| `tidycensus` | 1.7+ | ACS data |
| `stringdist` | 0.9+ | Fuzzy SD name matching |
| `beepr` | 2.0+ | Completion alerts |

**Python 3.10+** with `requests`, `pandas`, `xlrd` for the scrapers.

```r
install.packages(c("sf", "dplyr", "tidyr", "stringr", "data.table",
  "fixest", "ggplot2", "lehdr", "tigris", "readxl", "janitor",
  "tidycensus", "stringdist", "beepr"))
```

```bash
pip install requests pandas xlrd
```

### Active pipeline

The master script `run_active.R` runs the active pipeline end to end
with stage-gating flags. To run individual stages:

```bash
# 1. Scrape: PA EIT rates / muni AFR / Census F-33 SD finances
python3 scrape_pa_dced_eit.py        # → data/pa_eit_rate_panel.csv
python3 scrape_pa_dced_afr.py         # → data/pa_afr_panel.csv
python3 scrape_nces_sd_finance.py     # Census F-33; FY 2002–2019
Rscript reparse_dced.R                # recover SD ID/name on DCED rates
Rscript build_sd_controls.R           # F-33 → SD share controls; 500/500 SDs match

# 2. LODES at block level (one-time, ~4 GB, ~60 min)
Rscript download_lodes.R

# 3. Build cousub + block panels
Rscript build_data.R
Rscript build_block_panel.R

# 4. Main results (continuous stacked DiD per estimating unit)
Rscript q3_unified_full_table.R       # cousub headline
Rscript q3_unified_sd_level.R         # SD-level
Rscript q3_unified_block_joint.R      # block-level joint TWFE

# 5. Heterogeneity
Rscript q3_unified_act32_3level.R
Rscript q3_unified_industry.R
Rscript q3_unified_industry_sd.R
Rscript q3_unified_heterogeneity.R    # city / borough / township

# 6. Robustness (pre-trend ES + sample / dose-magnitude / alt-control)
Rscript q3_unified_headline_es.R      # cousub realized-dose ES
Rscript q3_unified_sd_es.R            # SD realized-dose ES
Rscript q3_unified_block_es_full.R    # block realized-dose ES (heavy)
Rscript q3_unified_sa_binary_es.R     # binary SA-IW ES
Rscript q3_unified_tercile_3level.R   # dose-magnitude tercile
Rscript q3_unified_altctrl_3level.R   # Cengiz clean-in-window

# 7. Build paper tables and figures
Rscript build_paper_tables.R          # → paper/tables/*.tex
Rscript build_industry_figure.R       # → figures/industry_inc_bind.png
```

### Compile the paper

```bash
cd paper && pdflatex main.tex && bibtex main && pdflatex main.tex && pdflatex main.tex
```

### External data sources

All data are publicly available. Steps 1–2 above download them
automatically.

| Source | Provider | Coverage | Access |
|--------|----------|----------|--------|
| **EIT rates** | PA DCED MunStats | 2,500+ munis (resident + nonresident + SD), 2000–2026 | `scrape_pa_dced_eit.py` |
| **Municipal AFR** | PA DCED MunStats | 2,500+ munis, 2002–2023 | `scrape_pa_dced_afr.py` |
| **F-33 SD finances** | U.S. Census Bureau | 500 PA SDs, FY 2002–2019, revenue + expenditure detail | `scrape_nces_sd_finance.py` |
| **LODES OD** | Census LEHD | Block-level, 2002–2021, LODES v8 | Via `lehdr` (`agg_geo="block"`) |
| **TIGER/Line** | Census Bureau | Cousubs, school districts, blocks | Via `tigris` (2020 vintage) |

The `data/` directory is not tracked in this repo (~1.5 GB+ of LODES
alone). Steps 1–3 regenerate all derived files from public sources.

## Project structure

```
├── scrape_pa_dced_eit.py        # PA DCED EIT rates
├── scrape_pa_dced_afr.py        # PA DCED muni AFR
├── scrape_nces_sd_finance.py    # Census F-33 SD finances
├── reparse_dced.R               # Recover SD ID/name on DCED rates
├── download_lodes.R             # LODES at block level
├── build_sd_controls.R          # F-33 → SD share controls + ID xwalk
├── build_data.R                 # Cousub panels + block→cousub xwalk
├── build_block_panel.R          # Block × year panel
├── q3_unified_full_table.R      # Cousub continuous stacked DiD
├── q3_unified_sd_level.R        # SD-level
├── q3_unified_block_joint.R     # Block-level joint TWFE
├── q3_unified_headline_es.R     # Cousub realized-dose ES
├── q3_unified_sd_es.R           # SD realized-dose ES
├── q3_unified_block_es_full.R   # Block realized-dose ES (heavy)
├── q3_unified_sa_binary_es.R    # Binary SA-IW ES (3 levels)
├── q3_unified_act32_3level.R    # Pre/post Act 32 (3 levels)
├── q3_unified_industry.R        # Cousub industry decomposition
├── q3_unified_industry_sd.R     # SD-level industry (aggregated)
├── q3_unified_tercile_3level.R  # Dose-magnitude tercile (3 levels)
├── q3_unified_altctrl_3level.R  # Cengiz clean-in-window (3 levels)
├── q3_unified_heterogeneity.R   # City / borough / township
├── build_paper_tables.R         # All paper LaTeX tables
├── build_industry_figure.R      # Industry bar chart
├── run_active.R                 # Master pipeline (stage-gated)
├── paper/
│   ├── main.tex                 # Manuscript
│   ├── guiding_principles.md    # Design principles
│   ├── outline.md / outline.tex # Detailed paper outline
│   └── tables/                  # Auto-built LaTeX fragments
├── references/
│   └── local_income_taxes.bib
├── figures/                     # Output PNGs (300 DPI)
└── data/                        # Not tracked (regenerable)
```

Legacy scripts from earlier paper structures (single-event TWFE,
DCDH, stacked binary, piecewise pair-flow, Q3 clean-groups, ring
spillover) remain in the repository for reference but are not in the
active pipeline. Run them directly with `Rscript <name>.R` if needed.

## Sample

2002–2019. Trimmed at load (`max_year <- 2019`) to exclude
COVID-contaminated LODES (2020–2021) and ARPA-distorted fiscal data
(2020–2023). Pennsylvania only.

## Key joins

- **LODES → cousub:** block-level LODES merged via `pa_block_xwalk.rds`. 2020 Census blocks nest in cousubs, so the assignment is exact.
- **DCED muni → Census cousub:** `municipality_id` → `COUSUBFP` via name + county matching, ~99.6% match rate.
- **NCES SD → DCED SD:** `LEAID` → `sd_id` via name normalization with abbreviation expansion + Jaro–Winkler fuzzy fallback + 2 manual overrides for completely-renamed SDs (KNOCH ↔ SOUTH BUTLER CO; RIVER VALLEY ↔ BLAIRSVILLE SALTSBURG). 500/500 DCED SDs matched.
- **Muni → SD rate (cousub model):** `max(sd_eit_rate)` across DCED-reported SDs per muni-year. 97.6% of munis report to one SD; max-aggregation is exact for those, conservative for the 2.4% multi-SD munis.

## Significance convention

`* p < .1, ** p < .05, *** p < .01` (set via `setFixest_etable`).

## Contact

Matthew Hockert — matthew.hockert@gmail.com
