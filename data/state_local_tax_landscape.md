# Local Income Tax Landscape -- State Decision Matrix

Companion to `state_local_tax_landscape.csv`. Tier states by viability for the LODES residential-sorting design.

## Tier 1 -- Primary states (current paper)
- **PA** -- muni + SD, continuous, max-rule credit, Act 32 (2012) enforcement shock, DCED PSD/AFR data, ~2500+500 units. Philly exempt (Sterling Act).
- **OH** -- muni cities + no-tax townships, continuous 0-3%, ~600 units. Binary tax/no-tax design with clean controls.

## Tier 2 -- Best candidates for extension
- **KY** -- work-based occupational license; ~250+ counties+cities; continuous rates 0.5-2.5%; 2008 uniformity (KRS 67.750-790) and 2012 HB 277 are plausible Act 32 analogs. Some cities have differential resident/nonresident rates (Louisville 2.2/1.45). Best mechanical match to PA. KLC/KACo annual rate data; KY Dept for Local Govt UFIRs for fiscal.
- **IN** -- residence-based, all 92 counties, continuous LIT 0.5-3%+, **2017 LIT consolidation** is a governance natural experiment (not enforcement). Indiana Gateway is gold-standard fiscal portal. Limitation: county-level only -- cross-county border ID, large counties limit movers, weak Stage 2 tract design. Inverse of PA structurally (residence vs work, county vs muni).

## Tier 3 -- Robustness states only
- **MI** -- 24 cities under Act 284 of 1964; max-rule credit; rates pinned at 1%/0.5% statutory cap (Detroit 2.4/1.2). Compressed within-city variation, no Act 32 analog. F-65 fiscal data clean.
- **MD** -- 24 counties, continuous 2.25-3.20%, residence-based, mandatory withholding via state piggyback. No extensive margin (all 24 always levied). DLS Local Govt Finances annually. Best for "third state robustness" but N=24 and county-level granularity is above where LODES detected effects.

## Tier 4 -- Marginal / case-study only
- **OR** -- Metro SHS + Multnomah PFA (true PITs, high-income only, 2021+); Portland Arts $35 flat; TriMet/Lane = employer payroll (wrong instrument). Possible single-event case study; not a panel design.
- **AL** -- work-location occupational license, ~25 cities at 0.5-2.0%. Wrong sign for residence sorting (work-based, no resident incidence). Useful as placebo or jobs-side study. Poor fiscal data (no uniform chart of accounts).
- **IA** -- school district surtax (~285 SDs) on state tax liability, not earnings. No employer withholding (kills the enforcement channel). LODES doesn't align with SD geography (NCES SDB crosswalk needed).

## Tier 5 -- Drop
- **CO** -- 4 cities, $2-$5.75/month flat head tax, work-location. No rate variation, N=4.
- **DE** -- Wilmington only, 1.25% constant for decades. N=1.
- **MO** -- KCMO + STL only, both flat 1% statutory, never varies. Hancock Amendment (1980) blocks new adoptions.
- **NJ** -- Newark + Jersey City employer payroll tax (not personal income). Wrong margin. JC 2019 adoption is a clean firm-relocation event but not residential sorting.
- **NY** -- NYC + Yonkers, N=2, NYC dwarfs Yonkers, commuter-tax repeal 1999 pre-LODES. Centralized through NYS PIT return.
- **KS** -- local intangibles tax (interest/dividends/AR). Tax base is financial assets, not wages -- LODES has zero relevance.
- **CA-SF** -- payroll expense / gross receipts on **firms**, not personal income. Wrong instrument.
- **DC** -- state-equivalent, N=1, uniform within jurisdiction, prohibited from taxing nonresidents (Home Rule Act).

## Headline takeaways

1. **OH and PA are unique** -- only U.S. settings combining (a) widespread true local PITs on residents, (b) continuous cross-sectional rate variation, (c) panel coverage matching LODES, (d) clean enforcement shock (PA Act 32 specifically).
2. **Best add for a third state**: **KY** if framing the paper around work-location withholding mechanics; **IN** if framing around residence-based vs work-based. KY mirrors PA structurally; IN is the natural contrast.
3. **Many "local income taxes" aren't**: AL/CO/KY are work-location occupational fees; NJ/CA-SF/OR-transit are employer payroll/GR taxes; KS taxes intangibles; WV is a flat weekly worker fee. Always check whose income is the base before counting a state.
4. **Adoption events post-2002 are scarce**: PA (rare), OH (some), IA SDs (steady spread 2004->2024), KY (continuing), Jersey City 2019, Multnomah/Metro 2021. The extensive margin is thin everywhere except IA SDs.
5. **Fiscal data quality varies wildly**: PA AFR, IN Gateway, MD DLS, NJ DCA, NY OSC are gold standard. AL has none. Most others sit between.

## Provenance
Built from web research by 4 parallel subagents (2026-05-04). Sources cited inline in the CSV; primary anchor is Tax Foundation 2019: https://taxfoundation.org/research/all/state/local-income-taxes-2019/. Each cell is a synthesis from authoritative sources (state revenue departments, municipal leagues, statutes) -- not a single citation. Verify before citing in paper.
