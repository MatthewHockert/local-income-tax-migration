# Local Income Tax and Residential Sorting

Do local income tax changes cause residential sorting? This project estimates the effect of municipal earned income tax (EIT) rate changes on the share of workers who live in the city where they work, using LEHD Origin-Destination Employment Statistics (LODES) for Ohio and Pennsylvania, 2002--2019.

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
| `HonestDiD` | 0.2+ | Sensitivity analysis for pre-trends |
| `tidycensus` | 1.7+ | ACS data (Section 18) |
| `beepr` | 2.0+ | Completion alerts |

**Python 3.10+** with `requests`, `pandas`, `xlrd` (scrapers only).

Install all R packages:

```r
install.packages(c("sf", "dplyr", "tidyr", "stringr", "data.table", "fixest",
  "ggplot2", "lehdr", "tigris", "readxl", "janitor", "tidycensus", "beepr",
  "DIDmultiplegtDYN", "HonestDiD"))
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

# Step 3: Build all intermediate datasets (rate panels, crosswalks, analysis panels)
Rscript build_data.R              # → data/*.rds (~5 min)

# Step 4: Main analysis (source interactively in RStudio, or run in batch)
Rscript local_income_tax_migration.R  # Sections 1-21

# Step 5: Supplementary analyses (independent of each other, any order)
Rscript companion_outcomes.R      # Decomposition: resident/nonresident/total
Rscript pretreatment_balance.R    # Balance table + pre-trend tests
Rscript robustness.R              # Philly exclusion, fiscal controls, Act 32 Stage 2
Rscript run_paper_models.R        # Publication-ready Sun-Abraham models
Rscript run_dcdh.R                # DCDH continuous treatment estimator + ACRT
```

### External Data Sources

All data are publicly available. Steps 1--2 above download them automatically.

| Source | Provider | Coverage | Access |
|--------|----------|----------|--------|
| **EIT rates** | PA DCED MunStats | 2,500+ municipalities, 2000--2026 | [MunStats tax reports](https://apps.dced.pa.gov/munstats-public/ReportInformation2.aspx?report=taxes_Dyn_Excel) (scraped by `scrape_pa_dced_eit.py`) |
| **Municipal finances (AFR)** | PA DCED MunStats | 2,500+ municipalities, 2002--2023 | [Statewide AFR](https://apps.dced.pa.gov/munstats-public/ReportInformation2.aspx?report=StatewideMuniAfr) (scraped by `scrape_pa_dced_afr.py`) |
| **Ohio income tax rates** | OH Dept of Taxation | All taxing municipalities, 1970s--present | [The Finder boundary data](https://thefinder.tax.ohio.gov/streamlinesalestaxweb/download/BoundaryData/Muni/OHMuniRateTable.csv) (downloaded by `build_data.R`) |
| **LODES OD/RAC** | Census Bureau LEHD | Tract-level, 2002--2021 | Via `lehdr` R package (LODES version 8) |
| **TIGER/Line shapefiles** | Census Bureau | County subdivisions, places, tracts, school districts | Via `tigris` R package (2020 vintage) |
| **UCR crime** | FBI Uniform Crime Reports | Agency-level offenses known, 1960--present | Jacob Kaplan's concatenated files (RDS format, stored in `data/crime_data/`) |
| **Building permits (BPS)** | Census Bureau | Place/MCD-level, annual | [Building Permits Survey](https://www.census.gov/construction/bps/) (fixed-width text files) |
| **IRS SOI migration** | IRS Statistics of Income | County-to-county flows, annual | [County migration data](https://www.irs.gov/statistics/soi-tax-stats-migration-data) (CSV inflow/outflow files in `data/irs_soi/`) |
| **Tax Foundation cross-section** | Tax Foundation | Local income tax rates by state | Excel file in `data/tax_foundation_local_income_tax_rates.xlsx` |

**Note on crime data:** The UCR offense files (`data/crime_data/ucr_offenses_known_monthly/`) are Jacob Kaplan's cleaned concatenation of raw FBI UCR data. These are annual RDS files and must be obtained separately from [openicpsr.org](https://www.openicpsr.org/openicpsr/project/100707/version/V18/view) or [Jacob Kaplan's data page](https://jacobdkaplan.com/data.html). Place them in the directory shown above.

**Note on building permits:** The BPS fixed-width text files are read from a local path (`housing-data-data/data/bps/Place/Northeast Region/`). Download the "Place" level data from the Census BPS website for the Northeast region, years 2002--2019.

### Data Not Included in the Repository

The `data/` directory is not tracked in version control due to file size (~1.5 GB for LODES alone). Running Steps 1--3 above regenerates all data files from public sources. The only files requiring manual download are:

1. **UCR crime RDS files** -- see note above
2. **BPS permit text files** -- see note above
3. **Tax Foundation Excel file** -- included in repo (`data/tax_foundation_local_income_tax_rates.xlsx`)

## Project Structure

```
├── scrape_pa_dced_eit.py        # Scrape PA EIT rates from DCED
├── scrape_pa_dced_afr.py        # Scrape PA municipal finances from DCED
├── download_lodes.R             # Download and cache LODES OD + RAC data
├── build_data.R                 # Build rate panels, crosswalks, analysis panels
├── local_income_tax_migration.R # Main analysis (Sections 1-21)
├── companion_outcomes.R         # Resident/nonresident/total job decomposition
├── pretreatment_balance.R       # Balance table + pre-trends (11 observables)
├── robustness.R                 # Philly exclusion, fiscal controls, Act 32 Stage 2
├── run_paper_models.R           # Publication-ready Sun-Abraham event studies
├── run_dcdh.R                   # DCDH continuous treatment + ACRT placebo
├── paper/
│   └── main.tex                 # Manuscript with filled tables and figures
├── references/
│   └── lit_review.bib           # BibTeX (27 papers by relevance tier)
├── figures/                     # Output PNGs (300 DPI)
└── data/                        # Not tracked; regenerated by Steps 1-3
    ├── pa_eit_rate_panel.csv    # PA EIT rates (scraped)
    ├── pa_afr_panel.csv         # PA municipal finances (scraped)
    ├── oh_muni_rate_table.csv   # OH tax rates (downloaded)
    ├── pa_rate_panel.rds        # PA rate panel with COUSUBFP crosswalk
    ├── oh_rate_panel.rds        # OH rate panel with PLACEFP
    ├── pa_panel.rds             # PA analysis-ready city-year panel
    ├── oh_panel.rds             # OH analysis-ready city-year panel
    ├── pa_city_pair_od.rds      # City-pair OD flows (Stage 2)
    ├── pa_resident_panel.rds    # Resident workers + neighbor rates (Stage 2)
    ├── pa_tract_panel.rds       # Tract-level within-city panel (Stage 2)
    ├── pa_cousub.rds            # PA municipality boundaries (TIGER)
    ├── oh_places.rds            # OH place boundaries (TIGER)
    ├── pa_neighbors.rds         # PA city pairs within 80 km
    ├── oh_neighbors.rds         # OH city pairs within 80 km
    ├── lodes/                   # Cached LODES RDS files (~1.5 GB)
    ├── crime_data/              # UCR offense files (manual download)
    ├── irs_soi/                 # IRS county migration CSVs
    └── pa_afr_raw/              # Raw DCED XLS files (scraped)
```

## Main Script Sections

The master analysis script (`local_income_tax_migration.R`) is organized into 21 numbered sections:

| Section | Content |
|---------|---------|
| 1 | Load data, merge fiscal controls |
| 2 | Maps: OH tax rates, PA rate changes, Allegheny County SD/tract map |
| 3 | Rate change distributions |
| 4 | Descriptive trends, normalized event plots, distance gradient |
| 5 | Scatter plots and fiscal outcome scatters |
| 6 | OH estimation: TWFE, event study, stacked DiD |
| 7 | PA estimation: TWFE + fiscal controls, event study |
| 8 | Pooled OH + PA |
| 9 | Robustness: asymmetry, Sun-Abraham, rate dynamics |
| 10 | Dose-response (treated-only variation) |
| 11 | City-pair OD flows |
| 12 | Rate-year scatter plots |
| 13 | Fiscal impact models (AFR: revenue, police, debt, population) |
| 14 | Heterogeneity by earnings and age |
| 15 | Federal worker placebo |
| 16 | Sample sensitivity + full continuous dose-response |
| 17 | Publication-ready Sun-Abraham models |
| 18 | ACS place-level outcomes |
| 19 | IRS SOI county migration |
| 20 | PA institutional robustness: Act 32 structural break, SD rates |
| 21 | Tract-level: relocation, destination choice, SD decomposition, boundary gradient |

## Key Identifiers and Joins

- **Ohio:** `muni_fips` = Census `PLACEFP` (5-digit place FIPS). Direct join.
- **Pennsylvania:** DCED `municipality_id` is crosswalked to Census `COUSUBFP` (5-digit county subdivision FIPS) via municipality name + county matching in `build_data.R` (97% match rate).
- **Tracts to municipalities:** Census tract centroids spatially joined to municipality boundaries (`st_join` with 2020 TIGER geometries).
- **UCR crime:** `fips_place_code` matched to `cousub_fips` (93.5% match for PA).
- **BPS permits:** `fips_mcd_code` matched to `cousub_fips` (93.3% match for PA).

## Sample

2002--2019. The sample is trimmed at load time (`max_year <- 2019`) to exclude COVID-contaminated LODES vintages (2020--2021) and ARPA-distorted fiscal data (2020--2023).

## Significance Convention

All tables use: `* p < .1, ** p < .05, *** p < .01` (set via `setFixest_etable`).

## Contact

Matthew Hockert -- matthew.hockert@gmail.com
