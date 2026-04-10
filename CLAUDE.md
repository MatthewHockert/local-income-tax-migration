# CLAUDE.md

## Project Overview

Do local income tax changes cause residential sorting at municipal borders? Uses LODES Origin-Destination data to measure whether workers relocate their residence outside the taxing municipality while keeping their workplace inside it.

## Key Question

When a municipality raises its local earned income tax, does the share of workers who live in their work city decline?

## Key Finding

PA TWFE: a 1 pp EIT increase reduces the share of workers living in the municipality by 1.15 pp (p < 0.05). OH event study shows clean pre-trends and gradual -0.33 pp decline by year 5 (marginally significant).

## Run Order

```bash
# 1. Scrape PA DCED municipal tax rates (2000-2026)
python3 scrape_pa_dced_eit.py

# 2. Download and cache LODES OD + RAC data
Rscript download_lodes.R

# 3. Build all intermediate datasets (rate panels, crosswalks, OD panels)
Rscript build_data.R

# 4. Run analysis (loads pre-built RDS files, makes plots, runs models)
# Source in RStudio or: Rscript local_income_tax_migration.R
```

## Scripts

| Script | Purpose |
|--------|---------|
| `scrape_pa_dced_eit.py` | Scrapes PA DCED MunStats for EIT rates, saves `data/pa_eit_rate_panel.csv` |
| `download_lodes.R` | Downloads LODES OD/RAC via `lehdr`, caches as RDS in `data/lodes/` |
| `build_data.R` | Builds rate panels, TIGER crosswalks, neighbor pairs, city-level OD panels. Saves all to `data/*.rds` |
| `local_income_tax_migration.R` | Master analysis: loads data, makes maps/scatters, runs TWFE/event study/stacked DiD |
| `archive/` | One-off exploration scripts from initial development |

## Data Sources

| Source | Raw File | What It Provides |
|--------|----------|-----------------|
| OH Dept of Taxation "The Finder" | `data/oh_muni_rate_table.csv` | Municipal income tax rates with effective dates, 685 municipalities |
| PA DCED MunStats | `data/pa_eit_rate_panel.csv` | PA municipal EIT rates, 2,505 municipalities, 2000-2026 |
| Tax Foundation (2021) | `data/tax_foundation_local_income_tax_rates.xlsx` | Resident + nonresident rates, 5,060 jurisdictions across 16 states |
| LODES OD (LODES8) | `data/lodes/*.rds` | Origin-destination flows (home tract → work tract), 2002-2021 |
| TIGER/LINE | Via `tigris` | Municipal boundaries (places for OH, county subdivisions for PA) |

## Intermediate Data (built by `build_data.R`)

| File | Contents |
|------|----------|
| `oh_rate_panel.rds` | OH municipality × year rate panel with change flags |
| `pa_rate_panel.rds` | PA municipality × year rate panel with COUSUBFP crosswalk |
| `oh_tract_xwalk.rds` / `pa_tract_xwalk.rds` | Tract → municipality assignment |
| `oh_neighbors.rds` / `pa_neighbors.rds` | City-to-city neighbor pairs (20km OH, 15km PA) |
| `oh_panel.rds` / `pa_panel.rds` | Analysis-ready: city OD outcomes merged with tax rates |
| `oh_panel_high.rds` / `pa_panel_high.rds` | Same for high earners (SE03 segment) |

## Design

- **Treatment**: Municipality raises its income tax rate
- **Outcome**: `share_live_in` = workers who live AND work in city / total workers in city
- **Identification**: TWFE (city + year FE), stacked DiD with neighbor controls
- **LODES alignment**: Reference date = April 1. Tax change Jan 1 of year Y → LODES Y-1 is pre, Y+1 is post.

## Key Joins

- OH: `muni_fips` = Census `PLACEFP` (direct FIPS join)
- PA: DCED `municipality_id` → Census `COUSUBFP` via name + county matching (97% match rate)

## R Dependencies

`sf`, `dplyr`, `tidyr`, `stringr`, `data.table`, `fixest`, `ggplot2`, `lehdr`, `tigris`, `readxl`, `janitor`, `beepr`

## Python Dependencies (scraper only)

`requests`, `pandas`, `xlrd`
