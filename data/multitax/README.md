# PA Multi-Tax Local Fiscal Panel — `data/multitax/`

Muni-year and SD-year panels of all major PA local taxes, 2002–2019, for a
future analysis ranking local tax instruments by mobility cost per dollar of
revenue. Built **register-first**: all taxes (incl. property millage) come from
the DCED tax register already scraped for the EIT project. See
`../../multitax_coverage_report.py` for the finding that the register supersedes
the spec's planned county-by-county millage hunt.

## Build order (reproducible)

```
python3 multitax_coverage_report.py     # coverage report + millage validation
python3 multitax_build_muni_panel.py    # -> pa_muni_multitax_panel.csv
python3 multitax_build_sd_panel.py      # -> pa_sd_millage_panel.csv (register-only)
python3 download_pde_retax.py           # PDE RE tax rate files (Wayback 2002-15 + live 2016-19)
python3 build_pde_sd_millage.py         # -> pde_sd_millage.csv (parsed, keyed to LEAID)
python3 multitax_validate_pde.py        # year-mapping test + register cross-validation
python3 multitax_gapfill_sd.py          # -> pa_sd_millage_panel_filled.csv (PDE-filled, 99%)
python3 multitax_sales_tax.py           # -> pa_local_sales_tax.csv (statutory constants)
# --- property-tax burden layer (muni + SD + county, on market value) ---
python3 scrape_pa_dced_county_tax.py    # -> pa_county_tax_panel.csv (county millage)
python3 parse_clr_history.py            # -> pa_clr_panel.csv + pa_reassessment_from_clr.csv
python3 multitax_effective_rate.py      # -> pa_property_burden_panel.csv (effective rate)
# --- pre-regression diagnostics ---
python3 multitax_millage_profile.py     # millage variation + EIT-millage entanglement
python3 multitax_reassessment_inventory.py  # -> pa_county_reassessment.csv + cross-check
```
The register build scripts import `load_register()` from `multitax_coverage_report.py`.
Sole register input: `../pa_dced_raw/pa_taxes_{2002..2019}.xls` (from
`scrape_pa_dced_eit.py`). Crosswalk/EIT slices (`muni_cousub_xwalk.csv`,
`eit_slice.csv`, `sd_id_xwalk.csv`) are exported from `pa_rate_panel.rds` /
`pa_sd_id_xwalk.rds` (R). PDE files are downloaded to `../pde_retax_raw/`.

## Files

| File | Contents |
|------|----------|
| `COVERAGE_REPORT.md` | Human-readable coverage + millage-validation report |
| `coverage_by_year.csv` | Munis with a positive rate, per tax × year (+ SD millage) |
| `coverage_by_county.csv` | Per tax × county populated share (geographic gaps) |
| `millage_sp_categories.csv` | Special-purpose RE levy set + year stability |
| `millage_total_validation.csv` | Summed total vs native TOTAL, 2015–19 (99.96%) |
| `millage_afr_reconciliation.csv` | Register GP vs retired AFR panel |
| `pa_muni_multitax_panel.csv` | **Muni-year panel** (key: `muni_id`, `cousub_fips`) |
| `pa_sd_millage_panel.csv` | SD-year millage, register-only (key: `sd_id`, `ncesid`/LEAID) |
| `pa_sd_millage_panel_filled.csv` | **SD-year millage, PDE-backfilled to 99%** (use this one) |
| `pde_sd_millage.csv` / `pde_sd_millage_long.csv` | parsed PDE millage (district × school-year) |
| `pde_register_validation.csv` | year-mapping + register/PDE agreement by year |
| `pde_register_discrepancies.csv` | per-cell register−PDE diffs (incl. register scale errors) |
| `pa_local_sales_tax.csv` | Allegheny/Philadelphia local sales tax (statutory constants) |
| `pa_county_tax_panel.csv` | **County-year millage** (RE GP + total, hotel, per-capita, sales) |
| `pa_clr_panel.csv` | County-year Common Level Ratio (assessed/market), 1986–2026 |
| `pa_reassessment_from_clr.csv` | Reassessment years from CLR footnotes (more precise than published list) |
| `pa_county_reassessment.csv` | Published 67-county last-reassessment-year inventory |
| `pa_property_burden_panel.csv` | **Muni-year full property burden** (muni+SD+county mills × CLR = effective rate on market value) |
| `DIAGNOSTICS_AND_SCOPING.md` | Pre-regression diagnostics + design decisions |
| `muni_cousub_xwalk.csv`, `eit_slice.csv`, `sd_id_xwalk.csv` | merge inputs |

## Coverage flag (every tax carries `<tax>_flag`)

Three states — **missing ≠ zero**:
- `levied` — register row with a positive rate.
- `not_levied` — unit present in that year's register but the tax is a zero-row
  or has no row → genuine non-levy; analysis-ready value = **0**.
- `not_observed` — value unknown; analysis-ready value = **NaN**, not 0.
  - Muni panel: unit absent from that year's register entirely (rare).
  - SD panel, RE millage only: a zero is *not credible* (every PA SD levies
    property tax), so any non-positive register cell is treated as a gap
    (`not_observed`, NaN) — these are the PDE backfill targets. (`percap_code`,
    `sd_eit` keep credible-zero `not_levied` semantics.)

## Millage definitions

- `re_gp_mills` / `re_gp_mills_sd` — Real Estate – General Purpose (headline rate).
- `re_total_mills` / `re_total_mills_sd` — General Purpose + all special-purpose
  RE levies; max within each levy, then summed; excludes the native `… TOTAL`
  roll-up. Validated to **99.96%** against DCED's native TOTAL (2015–19).
  Critical: dedupe per (unit, levy) first — the register repeats a unit's rate
  once per school district it spans (naive row-sum 2–3× over-counts multi-SD munis).
- `re_gp_county_dispersion` (SD panel) — within-SD spread of GP millage across
  counties (CLR equalization); >0 marks a multi-county SD (headline uses max).

## Scope boundary

- **Muni + SD + COUNTY levies: complete.** Muni/SD from the muni register (SD
  PDE-backfilled to 99%); county millage from the DCED `CountyTaxSummary` register
  (`scrape_pa_dced_county_tax.py`). Full property burden = muni + SD + county
  mills × CLR = **effective rate on market value** (`pa_property_burden_panel.csv`;
  median 2.06% of market value, complete for 99% of muni-years; Philadelphia the
  main incomplete case, no separate county layer).
- **Local sales tax:** statutory constants only (Allegheny 1%, Philadelphia
  1%→2% in 2009); no other PA jurisdiction levies one. See `pa_local_sales_tax.csv`.

## Status

- ✅ Coverage report, muni-year panel, SD-year millage (register + PDE backfill
  to 99.2%), year-mapping validated (B_start, stable), sales-tax constants.
- PDE cross-validation flagged **123 register SD-millage scale/data-entry errors**
  (e.g. Riverside Beaver 6555 vs 65.55); flagged in `re_gp_qc`, not overwritten.
- Retired `../retired/pa_property_tax_panel.csv` (superseded; see that dir's README).
