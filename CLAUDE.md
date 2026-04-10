# CLAUDE.md

## Project Overview

Do local income tax changes cause residential sorting? Uses LODES Origin-Destination data to measure whether workers relocate residence outside taxing municipalities. Two-state design: Ohio (binary tax/no-tax) and Pennsylvania (universal EIT with rate variation).

## Key Finding

Higher local income tax rates reduce the locally-resident share of city workers. The behavioral response is real but too small to offset the fiscal gains from higher rates. Rate increases push people out (-0.018* per pp); rate decreases do not bring them back.

## Run Order

```bash
# 1. Scrape PA tax rates and municipal finances
python3 scrape_pa_dced_eit.py
python3 scrape_pa_dced_afr.py

# 2. Download and cache LODES OD + RAC data
Rscript download_lodes.R

# 3. Build all intermediate datasets
Rscript build_data.R

# 4. Main analysis (Sections 1-16)
# Source in RStudio: local_income_tax_migration.R

# 5. Additional analyses
Rscript companion_outcomes.R   # Decomposition: resident/nonresident/total
Rscript pretreatment_balance.R # Balance table + pre-trend tests
Rscript robustness.R           # Philadelphia exclusion + fiscal controls
```

## Scripts

| Script | Purpose |
|--------|---------|
| `scrape_pa_dced_eit.py` | PA EIT rates → `data/pa_eit_rate_panel.csv` |
| `scrape_pa_dced_afr.py` | PA municipal finances → `data/pa_afr_panel.csv` |
| `download_lodes.R` | Cache LODES to `data/lodes/*.rds` |
| `build_data.R` | Rate panels, crosswalks, neighbors (80km), city OD panels |
| `local_income_tax_migration.R` | Master analysis: Sections 1-16 |
| `companion_outcomes.R` | Decomposition: resident/nonresident/total jobs |
| `pretreatment_balance.R` | Balance table + pre-trend tests on fiscal observables |
| `robustness.R` | Philadelphia exclusion + fiscal controls (4-way table) |
| `paper/main.tex` | LaTeX scaffold with filled tables and figures |

## Main Script Sections

| Section | Content |
|---------|---------|
| 1 | Load data (trimmed to 2019) + merge fiscal controls |
| 2 | Maps (OH rates, PA rate changes) |
| 3 | Rate change distributions + adoption plots |
| 4 | Treated vs control OD trends + normalized event plots + distance gradient |
| 4b-4d | Tract-level distance, case studies (Circleville, Gahanna) |
| 5 | Scatter plots + fiscal outcome scatters |
| 6 (OH) | OH OD estimation: TWFE, event study, stacked, out-of-state, earnings |
| 7 (PA) | PA OD estimation: TWFE + controls, event study + controls |
| 8 | Pooled OH + PA |
| 9 | Robustness: asymmetry, decreases, Sun-Abraham, cleaned stacked, rate dynamics |
| 10 | Dose-response identification (treated-only, key result) |
| 11 | City-pair OD flows (distance gradient) |
| 12 | Rate x year scatter plots |
| 13 | Fiscal impact (AFR: revenue, police, debt, population, property values, asymmetry) |
| 14 | Heterogeneity: earnings (SE01/SE03) and age (SA01-SA03) |
| 15 | Federal worker placebo (JT04) |
| 16 | Sample sensitivity + full continuous dose-response (both directions) |

## Sample: 2002-2019

Trimmed at load time (`max_year <- 2019`) to exclude COVID-contaminated LODES 2020-2021 and ARPA-distorted fiscal data 2020-2023.

## Key Results

- PA dose-response (treated only): rate x post = **-0.008** (SE 0.003)**
- PA split dose: increases **-0.018*** per pp; decreases +0.001 (null, asymmetric)
- OH cleaned stacked DiD (no-tax controls): **-0.011*** at t+5**
- Sun-Abraham: clean pre-trends, decline -0.3 to -0.9 pp over 5-8 years
- Companion outcomes: resident workers -15.3%*, total jobs -11.7%*; cannot reject equality (p=0.09)
- Fiscal: EIT revenue +67%**, total tax +30%**, police +5%; population null; property values null
- Heterogeneity: low/all/high earners and all age groups respond identically
- Robust to fiscal controls, Philadelphia exclusion, sample restrictions

## Data

| File | Contents |
|------|----------|
| `data/lodes/` | 15+ cached RDS: OD main/aux, RAC, federal JT04, earnings, age segments |
| `data/oh_rate_panel.rds` | OH municipality x year rate panel |
| `data/pa_rate_panel.rds` | PA municipality x year rate panel with COUSUBFP crosswalk |
| `data/oh_panel.rds` / `pa_panel.rds` | Analysis-ready: city OD + tax rates |
| `data/pa_afr_panel.csv` | PA municipal finances (56K rows, 71 cols, 2002-2023) |
| `data/pa_fiscal_panel.rds` | Fiscal panel merged with rates |
| `data/oh_neighbors.rds` / `pa_neighbors.rds` | City-to-city pairs (80km radius) |
| `figures/` | Publication-ready PNGs (300 DPI) |
| `paper/main.tex` | LaTeX with filled tables, equations, figure refs |

## Key Joins

- OH: `muni_fips` = Census `PLACEFP` (direct FIPS join)
- PA: DCED `municipality_id` → Census `COUSUBFP` via name + county matching (97% match)

## Dependencies

**R**: `sf`, `dplyr`, `tidyr`, `stringr`, `data.table`, `fixest`, `ggplot2`, `lehdr`, `tigris`, `readxl`, `janitor`, `tidycensus`, `beepr`

**Python** (scrapers only): `requests`, `pandas`, `xlrd`

## GitHub

https://github.com/MatthewHockert/local-income-tax-migration
