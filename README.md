# Local Income Tax and Residential Sorting

Do local income tax changes cause residential sorting? This project estimates the effect of municipal Earned Income Tax (EIT) rate changes on the share of workers who live in the city where they work, using LEHD Origin-Destination Employment Statistics (LODES) for Pennsylvania, 2002--2019. The Pennsylvania Earned Income Tax is the largest source of local-income-tax variation in the United States, and Act 32 of 2008 (effective January 1, 2012) provides a state-wide enforcement break that anchors causal identification.

## Replication Instructions

### Requirements

**R 4.5+** with the following packages:

| Package | Version | Purpose |
|---------|---------|---------|
| `sf` | 1.0+ | Spatial operations |
| `dplyr` | 1.1+ | Data manipulation |
| `tidyr` | 1.3+ | Reshaping |
| `stringr` | 1.6+ | String matching |
| `data.table` | 1.17+ | Fast data wrangling |
| `fixest` | 0.13+ | TWFE, Sun-Abraham, event studies |
| `ggplot2` | 4.0+ | Visualization |
| `lehdr` | 1.1+ | LODES downloads via Census API |
| `tigris` | 2.2+ | Census TIGER/Line shapefiles |
| `readxl` | 1.4+ | Excel file reading |
| `janitor` | 2.2+ | Column name cleaning |
| `DIDmultiplegtDYN` | 2.2+ | de Chaisemartin & D'Haultfoeuille estimator |
| `tidycensus` | 1.7+ | ACS data |
| `beepr` | 2.0+ | Completion alerts |

**Python 3.10+** with `requests`, `pandas`, `xlrd` (scrapers only).

Install all R packages:

```r
install.packages(c("sf", "dplyr", "tidyr", "stringr", "data.table", "fixest",
  "ggplot2", "lehdr", "tigris", "readxl", "janitor", "tidycensus", "beepr",
  "DIDmultiplegtDYN"))
```

Install Python packages:

```bash
pip install requests pandas xlrd
```

### Run Order

Execute scripts in this order from the project root:

```bash
# Step 1: Scrape Pennsylvania tax rates and municipal finances from DCED
python3 scrape_pa_dced_eit.py     # → data/pa_eit_rate_panel.csv (~15 min)
python3 scrape_pa_dced_afr.py     # → data/pa_afr_panel.csv (~10 min)

# Step 2: Download LODES origin-destination and residence data from Census
Rscript download_lodes.R          # → data/lodes/*.rds (~30 min, ~1.5 GB)

# Step 3: Build all intermediate datasets (rate panels, crosswalks, analysis panels,
#         fiscal controls including expenditure logs and revenue shares)
Rscript build_data.R              # → data/*.rds (~5 min)

# Step 4: Main analysis (source interactively in RStudio, or run in batch)
Rscript local_income_tax_migration.R

# Step 5: Supplementary analyses (independent of each other, any order)
Rscript companion_outcomes.R      # Outcome family on shared sample (controlled)
Rscript pretreatment_balance.R    # Balance table + pre-trend tests
Rscript robustness.R              # Philly exclusion, Act 32 break, early-data check
Rscript run_paper_models.R        # Publication-ready Sun-Abraham + DCDH event studies
Rscript run_dcdh.R                # DCDH continuous treatment estimator
```

### External Data Sources

All data are publicly available. Steps 1--2 above download them automatically.

| Source | Provider | Coverage | Access |
|--------|----------|----------|--------|
| **EIT rates** | PA DCED MunStats | 2,500+ municipalities (resident + nonresident muni rates, school district rates), 2000--2026 | [MunStats tax reports](https://apps.dced.pa.gov/munstats-public/ReportInformation2.aspx?report=taxes_Dyn_Excel) (scraped by `scrape_pa_dced_eit.py`) |
| **Municipal finances (AFR)** | PA DCED MunStats | 2,500+ municipalities, 2002--2023 (revenue, expenditures by function, debt, intergov) | [Statewide AFR](https://apps.dced.pa.gov/munstats-public/ReportInformation2.aspx?report=StatewideMuniAfr) (scraped by `scrape_pa_dced_afr.py`) |
| **LODES OD/RAC** | Census Bureau LEHD | Tract-level, 2002--2021, LODES v8 (2020 Census geography throughout) | Via `lehdr` R package |
| **TIGER/Line shapefiles** | Census Bureau | County subdivisions, places, tracts, school districts | Via `tigris` R package (2020 vintage) |
| **UCR crime** | FBI Uniform Crime Reports | Agency-level offenses known, 1960--present | Jacob Kaplan's concatenated files (RDS format) |
| **Building permits (BPS)** | Census Bureau | Place/MCD-level, annual | [Building Permits Survey](https://www.census.gov/construction/bps/) |
| **IRS SOI migration** | IRS Statistics of Income | County-to-county flows, annual | [County migration data](https://www.irs.gov/statistics/soi-tax-stats-migration-data) |

**Note on crime data:** The UCR offense files (`data/crime_data/ucr_offenses_known_monthly/`) are Jacob Kaplan's cleaned concatenation of raw FBI UCR data. These are annual RDS files and must be obtained separately from [openicpsr.org](https://www.openicpsr.org/openicpsr/project/100707/version/V18/view) or [Jacob Kaplan's data page](https://jacobdkaplan.com/data.html).

**Note on building permits:** BPS fixed-width files are read from a local path. Download "Place" level data from the Census BPS website for the Northeast region, years 2002--2019.

### Data Not Included in the Repository

The `data/` directory is not tracked due to file size (~1.5 GB for LODES alone). Running Steps 1--3 regenerates all derived files from public sources. Manual downloads required:

1. UCR crime RDS files
2. BPS permit text files

## Project Structure

```
├── scrape_pa_dced_eit.py        # Scrape PA EIT rates (resident + nonresident + SD)
├── scrape_pa_dced_afr.py        # Scrape PA municipal finances
├── download_lodes.R             # Download and cache LODES data
├── build_data.R                 # Build rate panels (with nonresident rates +
│                                #   backfill), crosswalks, analysis panels,
│                                #   fiscal controls (expenditures + revenue shares)
├── local_income_tax_migration.R # Main analysis: Q2 + Act 32 + Q3 clean groups
├── companion_outcomes.R         # Outcome family on harmonized samples
├── pretreatment_balance.R       # Balance table + pre-trends
├── robustness.R                 # Philly exclusion, Act 32 break, early-data drop
├── run_paper_models.R           # Sun-Abraham event studies (paper-ready)
├── run_dcdh.R                   # DCDH continuous treatment + placebo
├── paper/
│   └── main.tex                 # Manuscript
├── references/
│   ├── local_income_taxes.bib   # BibTeX file (current)
│   └── lit_review.bib           # Older bib file (deprecated)
├── figures/                     # Output PNGs (300 DPI)
└── data/                        # Not tracked
    ├── pa_eit_rate_panel.csv    # Scraped DCED rates
    ├── pa_afr_panel.csv         # Scraped DCED finances
    ├── pa_rate_panel.rds        # PA muni-year panel: muni res, muni nonres, SD,
    │                            #   total stack; nonres backfilled pre-2012
    ├── pa_panel.rds             # Analysis-ready muni-year panel (work-side)
    ├── pa_fiscal_controls.rds   # log spending (police, streets, debt, state aid)
    │                            #   + revenue shares (EIT, real estate, intergov)
    ├── pa_neighbors.rds         # Muni pairs within 80 km centroid
    ├── pa_cousub.rds            # PA cousub geometry (TIGER 2020)
    ├── lodes/                   # Cached LODES RDS files
    └── crime_data/, irs_soi/    # External data
```

## Specifications and Identification

The paper estimates three closely related models. The unit of observation is muni × year (cousub_fips × year); identification uses muni and year fixed effects.

### Q2 — Headline (per-pp continuous rate effect)

Two specs run side-by-side:

```
share_live_in ~ total_eit + controls | work_city + year                    # (A) total stack
share_live_in ~ resident_eit + sd_eit + controls | work_city + year        # (B) decomposition
```

Identification: within-muni rate variation, net of year shocks. Treatment intensity = continuous rate level (per pp). Decomposition splits the home-side rate into muni resident + SD components.

### Act 32 break

Same outcome with rate × pre-2012 / rate × post-2012 interactions to test the enforcement break:

```
share_live_in ~ resident_eit:pre + resident_eit:post + sd_eit:pre + sd_eit:post + controls | work_city + year
```

### Q3 — Clean groups (asymmetry)

Triple-interaction with muni-event groups (inc-only / dec-only / never-changed); flip-floppers dropped:

```
share_live_in ~ resident_eit:group + sd_eit + controls | work_city + year
```

Where `group ∈ {nc, inc, dec}`. The muni rate enters interacted with group (so `inc` and `dec` get separate slopes; `nc` is dropped due to no within-variation). The SD rate enters as a single coefficient, controlling for muni rate.

### Controls

| Type | Variables |
|------|-----------|
| Expenditures (logs) | police, streets, debt, state aid |
| Revenue shares | EIT / total revenue, real estate tax / total revenue, intergov / total revenue |

Comm-dev spending is **not** included as a control — it responds strongly to rate increases (Q7 robustness in earlier code) and would constitute a "bad control" by absorbing downstream causal channels. Likewise fire, sewer, parks, gen admin are not in the headline spec because including them halves the sample without methodological gain.

### Estimators

- **TWFE** with city + year FE (the headline tables)
- **Sun-Abraham** event study (`sunab(cohort, year)` in fixest) for staggered-DiD-robust dynamic effects
- **DCDH** continuous-treatment estimator (`did_multiplegt_dyn`) for ACR with placebo pre-trend test

## Sample

2002--2019. Trimmed at load time (`max_year <- 2019`) to exclude COVID-contaminated LODES vintages (2020--2021) and ARPA-distorted fiscal data. Pennsylvania municipalities (cousubs) only — Ohio and other states are not in the current paper.

## Key Joins

- **Pennsylvania:** DCED `municipality_id` → Census `COUSUBFP` via municipality name + county matching in `build_data.R` (~99.6% match rate after manual fixes for 9 dissolved/merged munis).
- **UCR crime:** `fips_place_code` → `cousub_fips` (93.5% match).
- **BPS permits:** `fips_mcd_code` → `cousub_fips` (93.3% match).
- **School district rates:** Currently assigned to munis by max SD rate among PSD codes per (muni, year). About 27% of munis span multiple school districts; tract-level SD assignment is queued as paper-level robustness.

## Significance Convention

All tables use: `* p < .1, ** p < .05, *** p < .01` (set via `setFixest_etable`).

## Contact

Matthew Hockert -- matthew.hockert@gmail.com
