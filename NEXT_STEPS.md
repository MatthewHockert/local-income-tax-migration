# Next Steps: Local Income Tax Migration Paper

## Priority 1: Additional Outcome Data

### ACS Place-Level Estimates (additional outcomes, not just validation)
- Population, median household income, median housing value
- Source: `tidycensus` R package, ACS 5-year at place/cousub level
- Run same Sun-Abraham/stacked DiD on each outcome
- Tests: (1) total population declines, (2) housing values soften
- `tidycensus` already installed; no API key needed for small queries

### IRS SOI Migration Data
- County-to-county inflows/outflows by AGI bin, 2011-2021
- Source: https://www.irs.gov/statistics/soi-tax-stats-migration-data (free CSVs)
- Expected: likely null at county level (within-county sorting invisible)
- That's a selling point — LODES detects sorting IRS can't see

### PA DCED Annual Financial Reports (DONE)
- Scraped via `scrape_pa_dced_afr.py`: `data/pa_afr_panel.csv` (56K rows, 2002-2023)
- EIT revenue, RE tax, total revenue, expenditure, debt, per capita measures
- **Key result**: 1pp EIT increase → +71%*** EIT revenue, +33%*** total tax revenue
- Rate increases MORE than offset base erosion from residential sorting
- Sun-Abraham: EIT revenue jumps 38% at t+1, persists at 43-45% through t+10

## Priority 2: Analysis Extensions

### City-Pair OD Flows (where do movers go?)
- For each treated city, track workers flowing to each specific neighbor
- Reshape OD at city-pair level, not just city aggregate
- Most direct test of Tiebout sorting

### Multiple Outcome Panel (main results section)
1. LODES OD `share_live_in` — residential sorting conditional on job (DONE)
2. ACS population — total residents including non-workers
3. IRS SOI county flows — actual movers
4. ACS median housing value — price capitalization
Shows tax increase ripples: sorting → population → migration → housing

### Heterogeneity
- City size (big metro cores vs small towns)
- Border status (state border = out-of-state alternative available)
- Industry composition (manufacturing vs service)
- Earnings: SE03 was null — likely power issue, not a puzzle

### Robustness
- Donut test: drop treatment year, compare t-1 to t+2
- Different control group radii (20km, 40km, 80km)
- Drop cities that change rate multiple times
- Federal worker placebo: ATTEMPTED but sample too small/noisy (pre-trends fail).
  Report honestly: "inconclusive due to power, not a refutation"

### Welfare Calculation
- Revenue: rate × tax base × N workers
- Erosion: estimated outflow × average income over 10 years
- At what rate does outmigration cost exceed revenue gain?
- Fiscal externality: do neighbors free-ride?

## Priority 3: Writing

### Main specification choice
- **OH headline**: Cleaned stacked DiD with no-tax controls (-0.011*** at t+5)
- **PA headline**: Dose-response among treated cities (rate × post = -0.009**)
- **Both states**: Sun-Abraham event studies showing clean pre-trends + gradual decline
- Robustness table: TWFE, FD, asymmetry, decrease mirror test

### Key identification argument
- Among treated-only cities: binary post = null (fiscal stress washes out)
- Rate level × post = -0.009** (tax channel survives)
- If fiscal stress alone drove the result, rate magnitude shouldn't matter — but it does
- PA EIT and property tax are uncorrelated (r = -0.038): not proxying for broad tax hikes
- PA municipalities raise EIT because they've hit property tax millage caps (substitutes, not complements)
- Federal worker placebo inconclusive due to tiny sample (N=836 OH, 1638 PA) with failed pre-trends

### Key figures
1. OH + PA maps showing treatment geography
2. Normalized event-time plots (treated vs control)
3. Sun-Abraham event studies (both states, all workers)
4. Decomposition: numerator (live_and_work) vs denominator (total_jobs)
5. Dose-response: rate level × post among treated cities

### Key tables
1. Summary statistics: rate panel, OD outcomes, city characteristics
2. Main results: stacked DiD + Sun-Abraham + dose-response
3. Robustness: TWFE, FD, asymmetry, decrease mirror, federal placebo (with caveats)
4. Additional outcomes: ACS population, housing values, IRS SOI
5. Welfare: revenue vs erosion tradeoff

### Paper structure
1. Introduction: Tiebout hypothesis + why local income taxes under-studied
2. Institutional background: OH and PA municipal tax systems, why EIT not property tax
3. Data: LODES OD, rate panels, construction of share_live_in
4. Empirical strategy: stacked DiD + Sun-Abraham + dose-response identification
5. Results: event studies, dose-response, asymmetry (increases vs decreases)
6. Mechanisms: decomposition, distance, housing prices, where movers go
7. Policy: revenue vs erosion tradeoff, fiscal externalities

## Why OH and PA
- Only states with enough municipal-level income tax variation
- OH: 685 municipalities with rates + hundreds of zero-tax townships (clean controls)
- PA: 2,505 municipalities with mandatory EIT, rate variation within universal coverage
- KY/IN = county level (too coarse), MD = 24 counties (too few), MI = 24 cities (too few)
- Not cherry-picking: full universe of places where this policy exists at scale

## Data Already Built
- All LODES cached as RDS in `data/lodes/` (OD main + aux, RAC, federal JT04)
- Rate panels: `data/oh_rate_panel.rds`, `data/pa_rate_panel.rds`
- City OD panels: `data/oh_panel.rds`, `data/pa_panel.rds` (+ high earner versions)
- Neighbor pairs: `data/oh_neighbors.rds`, `data/pa_neighbors.rds` (80km radius)
- PA property tax: `data/pa_property_tax_panel.csv` (2015-2021)
- ACS balance: `data/oh_acs_balance.rds`, `data/pa_acs_balance.rds`
- TIGER geometries: `data/oh_places.rds`, `data/pa_cousub.rds`

## Target Journals
- Journal of Urban Economics
- Journal of Public Economics
- National Tax Journal
- Regional Science and Urban Economics
