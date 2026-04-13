# CLAUDE.md

## Project Overview

Do local income tax changes cause residential sorting? Uses LODES Origin-Destination data to measure whether workers relocate residence outside taxing municipalities. Two-state design: Ohio (binary tax/no-tax) and Pennsylvania (universal EIT with rate variation). Act 32 (2012) enforcement consolidation provides a natural experiment: sorting only appears when the tax becomes binding through employer withholding.

## Key Finding

Higher local income tax rates reduce the locally-resident share of city workers. The behavioral response is real but too small to offset the fiscal gains from higher rates. Rate increases push people out (-0.018** per pp post-Act 32); rate decreases do not bring them back. Cities raise rates to fund programs (e.g., police), not because of fiscal distress — pre-treatment budgets are flat. The sorting is cross-boundary (people leave the city) not within-city (people don't just reshuffle neighborhoods). School district EIT has the opposite effect: higher SD rates attract residents, fully offsetting municipal rate repulsion in high-SD areas.

## Run Order

```bash
# 1. Scrape PA tax rates and municipal finances
python3 scrape_pa_dced_eit.py
python3 scrape_pa_dced_afr.py

# 2. Download and cache LODES OD + RAC data
Rscript download_lodes.R

# 3. Build all intermediate datasets
Rscript build_data.R

# 4. Main analysis (Sections 1-21)
# Source in RStudio: local_income_tax_migration.R

# 5. Additional analyses
Rscript companion_outcomes.R   # Decomposition: resident/nonresident/total
Rscript pretreatment_balance.R # Balance + pre-trends (11 observables incl crime, permits)
Rscript robustness.R           # Philly exclusion, fiscal controls, Act 32 Stage 2
Rscript run_paper_models.R     # Paper-ready Sun-Abraham models
Rscript run_dcdh.R             # DCDH continuous treatment estimator
```

## Scripts

| Script | Purpose |
|--------|---------|
| `scrape_pa_dced_eit.py` | PA EIT rates → `data/pa_eit_rate_panel.csv` |
| `scrape_pa_dced_afr.py` | PA municipal finances → `data/pa_afr_panel.csv` |
| `download_lodes.R` | Cache LODES to `data/lodes/*.rds` |
| `build_data.R` | Rate panels, crosswalks, neighbors (80km), city OD panels |
| `local_income_tax_migration.R` | Master analysis: Sections 1-21 |
| `companion_outcomes.R` | Decomposition: resident/nonresident/total jobs |
| `pretreatment_balance.R` | Balance table + pre-trend tests (fiscal + crime + permits + jobs) |
| `robustness.R` | Philadelphia exclusion, fiscal controls, Act 32 structural break (all outcomes) |
| `run_paper_models.R` | Paper-ready Sun-Abraham models: inc/dec vs NC, OH controls, dose-response |
| `run_dcdh.R` | de Chaisemartin & D'Haultfoeuille (2024) continuous treatment + ACRT |
| `paper/main.tex` | LaTeX with filled tables, figures, and robustness section |
| `references/lit_review.bib` | BibTeX literature review (27 papers, tiered by relevance) |

## Main Script Sections

| Section | Content |
|---------|---------|
| 1 | Load data (trimmed to 2019) + merge fiscal controls + save `pa_fiscal_controls.rds` |
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
| 17 | Paper-ready models: Sun-Abraham inc/dec splits, OH two control groups, dose-response |
| 18 | ACS place-level outcomes (population, income growth — null) |
| 19 | IRS SOI county migration (null at county level — LODES detects what IRS can't) |
| 20 | PA institutional robustness: Act 32 structural break, total EIT, SD rates |
| 21 | Tract-level OD: complete relocation (Q4), destination choice (Q4), SD decomp (Q5), boundary gradient (Q6) |

## Sample: 2002-2019

Trimmed at load time (`max_year <- 2019`) to exclude COVID-contaminated LODES 2020-2021 and ARPA-distorted fiscal data 2020-2023.

## Key Results

**Stage 1: Municipal-level sorting**
- PA Sun-Abraham (inc vs NC): ATT = **-0.005** (SE 0.003), clean pre-trends, effect at t+8 significant
- PA DCDH continuous (inc vs NC): ATE = **-0.008** (SE 0.007), placebo p = 0.90, Effect_8 = **-0.011***
- OH Sun-Abraham (inc vs no-tax): ATT = **-0.008*** (SE 0.003), clean pre-trends
- OH Sun-Abraham (inc vs never-changed): null — stable-rate taxed cities are poor controls
- PA split dose: increases **-0.018** per pp; decreases +0.001 (null, asymmetric)
- Asymmetry confirmed across all estimators: rate decreases do not reverse sorting

**Act 32 structural break (enforcement mechanism)**
- Pre-Act 32 (2002-2011): β = -0.002 (precise zero) — no sorting when tax not enforced
- Post-Act 32 (2012-2019): β = -0.018*** — sorting only when employer withholding is binding
- Fiscal confirmation: same rate generates 5.1%** more EIT revenue post-Act 32
- Pattern holds across all Stage 2 outcomes (resident workers, commuter pairs, SD decomposition)

**Stage 2: Tract-level decomposition**
- Workers leave the city (cross-boundary sorting), not reshuffle within it
- Neighbor spillovers: cities gain resident workers when neighbors raise rates (Channel 2)
- Home rate irrelevant for commuters (consistent with credit/max system — Prediction 2)
- SD rates attract residents (+0.019*), completely offset muni rate repulsion in high-SD areas
- Boundary gradient null — sorting is city-wide relocation, not cross-boundary arbitrage

**Companion outcomes and fiscal**
- Companion outcomes: resident workers -15.3%**, total jobs -11.7%**; cannot reject equality (p=0.09)
- Fiscal: EIT revenue +67%***, total tax +30%***, police +5%; population null; property values null
- Fiscal controls (police, streets, debt, state aid) do not change estimates or SEs
- Crime declines gradually post-treatment (significant at t+7, t+8): rate increases fund police, reduce crime

**Pre-treatment balance (11 observables)**
- Clean pre-trends: revenue, EIT revenue, RE tax, streets, population, debt, jobs, crime, permits
- Exceptions: police (lower spending pre-treatment, addressed with controls), market value (limited coverage)

**Null results (confirming mechanism)**
- ACS outcomes: population and income growth null (effect too small for aggregate outcomes)
- IRS SOI: county-level migration null (sorting is sub-county, LODES detects what IRS can't)

## Data

| File | Contents |
|------|----------|
| `data/lodes/` | 15+ cached RDS: OD main/aux, RAC, federal JT04, earnings, age segments |
| `data/oh_rate_panel.rds` | OH municipality x year rate panel |
| `data/pa_rate_panel.rds` | PA municipality x year rate panel with COUSUBFP crosswalk |
| `data/oh_panel.rds` / `pa_panel.rds` | Analysis-ready: city OD + tax rates |
| `data/pa_afr_panel.csv` | PA municipal finances (56K rows, 71 cols, 2002-2023) |
| `data/pa_fiscal_panel.rds` | Fiscal panel merged with rates |
| `data/pa_fiscal_controls.rds` | Log police, streets, debt, state aid for Stage 2 merges |
| `data/pa_city_pair_od.rds` | City-pair panel with work/home rates + fiscal controls |
| `data/pa_resident_panel.rds` | Resident worker panel with own + neighbor rates + fiscal controls |
| `data/pa_tract_panel.rds` | Tract panel with boundary classification |
| `data/oh_neighbors.rds` / `pa_neighbors.rds` | City-to-city pairs (80km radius) |
| `data/crime_data/ucr_offenses_known_monthly/` | FBI UCR annual RDS (1960-present) |
| `figures/` | Publication-ready PNGs (300 DPI) |
| `paper/main.tex` | LaTeX with filled tables, equations, figure refs |
| `references/lit_review.bib` | 27 papers organized by relevance tier |

## Key Joins

- OH: `muni_fips` = Census `PLACEFP` (direct FIPS join)
- PA: DCED `municipality_id` → Census `COUSUBFP` via name + county matching (97% match)
- UCR crime: `fips_place_code` → `cousub_fips` (93.5% match for PA)
- BPS permits: `fips_mcd_code` → `cousub_fips` (93.3% match for PA)

## Significance Convention

All scripts use: `* p < .1, ** p < .05, *** p < .01` (set via `setFixest_etable`).

## Dependencies

**R**: `sf`, `dplyr`, `tidyr`, `stringr`, `data.table`, `fixest`, `ggplot2`, `lehdr`, `tigris`, `readxl`, `janitor`, `tidycensus`, `beepr`

**Python** (scrapers only): `requests`, `pandas`, `xlrd`

## GitHub

https://github.com/MatthewHockert/local-income-tax-migration
