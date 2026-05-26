# Property-tax leg: pre-regression diagnostics & data scoping

Generated 2026-05-24. Inputs: `pa_muni_multitax_panel.csv`,
`pa_sd_millage_panel_filled.csv`, published county reassessment list, web scoping.
**No regressions yet** — this is the report that informs two design decisions
(reassessment-clean? separate-vs-joint?).

## 1. Millage-variation profile  (`multitax_millage_profile.py`)

- Millage is a **high-variation treatment**: 24.3% of muni-year transitions have
  any millage change; **16.9% material** (|Δ| ≥ 0.5 mills). Median change 1.0
  mill, p90 6 mills, 68% increases. (EIT, by contrast, changes rarely.)
- Material changes **cluster in 2003–2006** (25–31%/yr), tapering to ~9–15%.

### Entanglement with EIT (the key diagnostic)
- **corr(Δ muni millage, Δ muni resident EIT) = +0.059** (n=41,872). On
  magnitudes, the two treatments are essentially **separable** — comparable to
  the muni-vs-SD EIT correlation (−0.12) that cleared Inc-becomes.
- corr vs SD EIT = +0.008 (independent).
- **But event co-occurrence shows a 2× lift:** P(material millage change) = 16.5%,
  but conditional on a material EIT change = 33.5% (odds ratio 2.56). Munis
  having an EIT-change *year* are ~twice as likely to also touch millage that
  year — modest *timing* entanglement even though magnitudes don't co-move.
- **Read:** separate elasticities are feasible; the property-tax design should
  carry concurrent-EIT level controls (it already does), with a joint spec as
  robustness.

## 2. County reassessment inventory  (`pa_county_reassessment.csv`, `multitax_reassessment_inventory.py`)

- **24 of 67 counties reassessed in-panel (2002–2019):** Dauphin '02; Fayette,
  Greene, Lawrence, Lebanon '03; Lycoming, Sullivan, Venango '04; Montour, Union,
  York '06; Clinton, Luzerne '09; Adams, Cumberland '10; Perry '11; Allegheny '12
  (court-ordered, values eff. 2013); Bedford, Erie, Lehigh '13; Indiana '16;
  Blair, Washington '17 (court-ordered); Lancaster '18. The other 43 had frozen
  assessments (last reassessment pre-2002, or post-2019: Delaware '21, Phila/Wayne '23).
- **Validated link:** material millage changes are **1.69× more likely** within
  ±1 yr of a reassessment (28.5% vs 16.8% baseline). But reassessment-driven is a
  **minority — only ~10.6%** (~778) of all material millage changes; most are
  discretionary.
- **CRITICAL CAVEAT (revenue-neutrality).** PA law makes countywide
  reassessments revenue-neutral: taxing bodies must roll millage *back* so total
  revenue doesn't jump (anti-windfall), with any real increase requiring a
  separate capped vote. So a reassessment-driven millage *drop* is largely
  mechanical and **does NOT represent a burden change** — using raw statutory
  millage around reassessments is misleading. The credible object is the
  **effective rate on market value = millage × CLR**, and the exogenous bite is
  the *deviation from revenue-neutrality*. That requires STEB CLR / market-value
  data (see §4). Reassessment is a promising instrument **only paired with the
  effective-rate construction**, not as raw millage.

## 3. Capitalization (the property-tax leg's central interpretive issue)

A millage increase partly capitalizes into lower house prices, so the burden
reaching a mobile household is less than the statutory change; the estimated
property-tax elasticity blends tax and price response. Not fully solvable
without parcel transactions, but **addressable and defensible** with:

| Source | Geography | Years | Use |
|---|---|---|---|
| **STEB certified market value** | county + **muni/SD** | annual, ~2002+ | tax base; capitalization proxy; **effective-rate denominator** |
| **STEB Common Level Ratio** | county | annual | assessed/market; build millage×CLR effective rate |
| **FHFA House Price Index** | county (ZIP3) | 2002–2019 | repeat-sales price control / capitalization check |
| Zillow ZHVI | ZIP / place | ~2000+ | finer price level |
| ACS median home value | tract / place | 2005–2019 | level + composition |
| County parcel assessed values | parcel | varies | only if parcel-level needed (per-county) |

STEB market values do double duty: capitalization proxy AND the denominator that
turns statutory millage into an effective rate — resolving the §2 caveat too.

## 4. County millage acquisition (now non-optional: burden = muni + SD + county)

- Full parcel property burden = muni millage + SD millage + **county millage**.
  Muni + SD are in this panel; county is the missing layer.
- **Good news on feasibility:** DCED MunStats hosts a **County Annual Financial
  Report** (`ReportInformation2.aspx?report=cAfrForm`) and a county Tax
  Information Form (DCED-CLGS-67) — counties report to the *same portal* as
  munis. County millage is likely scrapable via the existing scraper pattern,
  **not** 67 separate county sites. **Next step:** probe `cAfrForm` (and any
  county tax register) to confirm it carries county RE millage 2002–2019.

## Decisions this report teases up (for the PI, before any regression)

1. **Is reassessment clean enough?** Reassessment-driven *raw* millage variation
   is limited (~11%) and largely revenue-neutral → weak/misleading as raw
   millage. **Recommendation:** pursue it only as **effective-rate (millage×CLR)
   variation** using STEB data; then reassessment + deviation-from-neutral is the
   credible identifying subset. Requires acquiring STEB CLR/market values.
2. **Separate elasticities vs joint?** The +0.059 magnitude correlation supports
   **separate elasticities** (EIT design as-is; property-tax design separately;
   then compare), with concurrent-EIT controls and a joint spec as robustness.

## Triggered next acquisitions (pending decisions)

- STEB CLR + certified market values (county + muni/SD), 2002–2019 — needed for
  effective rates AND capitalization.
- County RE millage via DCED `cAfrForm` — needed for full burden.
