"""
multitax_build_muni_panel.py
============================

Assemble the MUNICIPALITY-YEAR multi-tax panel (2002-2019) from the DCED tax
register, keyed to DCED municipality_id + Census COUSUBFP so it merges to the
EIT/LODES project. Register-first: every tax here comes from
data/pa_dced_raw/pa_taxes_YYYY.xls (see multitax_coverage_report.py for the
coverage report that validated this source).

DESIGN DECISIONS (confirmed with PI)
------------------------------------
* Minor multi-row taxes (business-privilege, per-capita, occupation) kept as
  GRANULAR sub-columns, not collapsed.
* Keyed on municipality_id, with cousub_fips attached via the existing
  crosswalk in pa_rate_panel.rds (exported to data/multitax/muni_cousub_xwalk.csv).
* EIT is REUSED from the canonical pa_rate_panel.rds (exported to eit_slice.csv),
  not recomputed, so it matches the paper exactly.

THREE-STATE COVERAGE FLAG (per tax, per muni-year)  -- "missing != zero"
  levied        : a register row for this tax with amount > 0.
  not_levied    : muni IS present in that year's register but the tax is either
                  a zero-amount row or has no row -> muni reported its schedule
                  and does not impose this tax (genuine non-levy; rate = 0).
  not_observed  : muni is ABSENT from that year's register entirely -> the
                  muni-year was not reported; rate is unknown (NaN), not 0.
For analysis: not_levied -> treat rate as 0 (burden is genuinely 0);
              not_observed -> treat as missing. The flag preserves the choice.

MILLAGE
  re_gp_mills    : Real Estate - General Purpose (the headline policy rate).
  re_total_mills : validated summed total = General Purpose + all special-purpose
                   RE levies, deduped per (muni, levy) before summing, excluding
                   the native '... TOTAL' roll-up. Reproduces DCED's native TOTAL
                   at 99.96% in 2015-2019 (see coverage report sec.4).

Output: data/multitax/pa_muni_multitax_panel.csv  (+ a coverage_flag column per tax)
"""

import os
import warnings
import numpy as np
import pandas as pd

from multitax_coverage_report import load_register, PROJECT_DIR, OUT_DIR

warnings.filterwarnings("ignore")

# --- Tax extraction rules -------------------------------------------------
# Each entry: output column -> predicate over the (lower-cased) tax name.
# Amount aggregated per (muni, year) as max of muni-side amount over matching
# rows (the register repeats a muni's rate once per school district it spans;
# the rate is constant across those rows, so max == the rate, and == 0 when only
# zero-rows exist). re_total_mills is handled separately (dedup-then-sum).
SINGLE_TAXES = {
    "re_gp_mills":          lambda t: t == "real estate - general purpose (mills)",
    "realty_transfer_pct":  lambda t: t == "realty transfer (percent)",
    # LST is the continuous series across its statutory renamings:
    #   Occupational Privilege (pre-2005) -> Emergency & Municipal Services Tax
    #   (EMST, 2005-2007) -> Local Services Tax (LST, 2008+).
    "lst_dollars":          lambda t: ("local services tax" in t
                                       or "emergency and municipal services" in t
                                       or "occupational privilege" in t),
    "bpt_retail_mills":     lambda t: "mercantile/business privilege - retail" in t,
    "bpt_wholesale_mills":  lambda t: "mercantile/business privilege - wholesale" in t,
    "bpt_other_mills":      lambda t: "mercantile/business privilege - other" in t,
    "bpt_flat_dollars":     lambda t: "mercantile/business privilege - flat rate" in t,
    "percap_dollars":       lambda t: t == "per capita (dollars)",
    # NOTE: "Per Capita Code (dollars)" is the §679 *school-district* per-capita
    # levy (its amount sits in the SD column, muni column is 0). It is captured
    # in the SD-year panel, not here.
    "occ_mills":            lambda t: t.startswith("occupation") and "(mills)" in t,
    "occ_flat_dollars":     lambda t: (t.startswith("occupation") and "(dollars)" in t)
                                      or t == "occupation-flat rate (dollars)",
}


def build():
    reg = load_register()
    reg["tn"] = reg["tax_name"].astype(str).str.strip().str.lower()
    reg["muni_id"] = pd.to_numeric(reg["muni_id"], errors="coerce").astype("Int64")
    reg = reg[reg.muni_id.notna()].copy()

    years = sorted(reg.year.unique())
    universe = sorted(reg.muni_id.unique())
    # Full (muni, year) grid + the set actually present each year.
    grid = pd.MultiIndex.from_product([universe, years], names=["muni_id", "year"])
    present = set(map(tuple, reg[["muni_id", "year"]].drop_duplicates().values.tolist()))
    panel = pd.DataFrame(index=grid).reset_index()
    panel["muni_present"] = [ (m, y) in present for m, y in zip(panel.muni_id, panel.year) ]

    # Muni identity (most recent non-null name/county).
    ident = (reg.sort_values("year").groupby("muni_id")
             .agg(municipality=("muni_name", "last"), county=("county", "last"))
             .reset_index())
    panel = panel.merge(ident, on="muni_id", how="left")

    def amount_series(pred, agg="max"):
        sub = reg[reg.tn.map(pred)]
        if agg == "max":
            s = sub.groupby(["muni_id", "year"])["muni_amt"].max()
        else:  # sum: max within each levy (the rate; constant across a muni's
               # SD rows except for a handful of source inconsistencies, where
               # max matches re_gp and keeps re_total >= re_gp), then sum levies.
            per_levy = sub.groupby(["muni_id", "year", "tax_name"])["muni_amt"].max()
            s = per_levy.groupby(["muni_id", "year"]).sum(min_count=1)
        return s

    def assemble(colname, s):
        m = panel[["muni_id", "year", "muni_present"]].merge(
            s.rename("amt").reset_index(), on=["muni_id", "year"], how="left")
        amt = m["amt"]
        flag = np.where(~m["muni_present"], "not_observed",
                np.where(amt.fillna(0) > 0, "levied", "not_levied"))
        # analysis-ready amount: levied -> value; not_levied -> 0; not_observed -> NaN
        out_amt = np.where(flag == "not_observed", np.nan,
                   np.where(flag == "levied", amt, 0.0))
        panel[colname] = out_amt
        panel[colname + "_flag"] = flag

    for col, pred in SINGLE_TAXES.items():
        assemble(col, amount_series(pred, "max"))

    # Validated summed RE total (dedup-then-sum, excl native TOTAL roll-up).
    re_no_total = lambda t: "real estate" in t and "total" not in t
    assemble("re_total_mills", amount_series(re_no_total, "sum"))

    # --- merge cousub_fips crosswalk -------------------------------------
    xw_path = os.path.join(OUT_DIR, "muni_cousub_xwalk.csv")
    xw = pd.read_csv(xw_path, dtype={"cousub_fips": str})
    xw["cousub_fips"] = xw["cousub_fips"].str.zfill(5)
    panel = panel.merge(xw, left_on="muni_id", right_on="municipality_id", how="left")
    panel.drop(columns=[c for c in ["municipality_id"] if c in panel.columns], inplace=True)

    # --- merge canonical EIT (reused, not recomputed) --------------------
    eit = pd.read_csv(os.path.join(OUT_DIR, "eit_slice.csv"))
    eit = eit.rename(columns={"municipality_id": "muni_id"})
    panel = panel.merge(eit, on=["muni_id", "year"], how="left")

    # Order columns
    front = ["muni_id", "municipality", "county", "cousub_fips", "year", "muni_present"]
    rate_cols = [c for c in panel.columns if c not in front and not c.endswith("_flag")
                 and c not in ("resident_eit", "muni_nonres_eit", "sd_eit", "total_eit")]
    eit_cols = ["resident_eit", "muni_nonres_eit", "sd_eit", "total_eit"]
    flag_cols = [c for c in panel.columns if c.endswith("_flag")]
    panel = panel[front + sorted(rate_cols) + flag_cols + eit_cols]

    out = os.path.join(OUT_DIR, "pa_muni_multitax_panel.csv")
    panel.to_csv(out, index=False)
    return panel, out


def report(panel, out):
    print(f"\nWrote {out}\n  rows: {len(panel):,}  | munis: {panel.muni_id.nunique()}  "
          f"| years: {panel.year.min()}-{panel.year.max()}")
    unmatched = panel.loc[panel.cousub_fips.isna(), "muni_id"].nunique()
    print(f"  munis without cousub_fips: {unmatched} (known ~99.6% DCED->Census match)")
    print("\nThree-state flag distribution by tax (share of muni-years):")
    for c in [c for c in panel.columns if c.endswith("_flag")]:
        vc = panel[c].value_counts(normalize=True).mul(100).round(1).to_dict()
        print(f"  {c[:-5]:22s} levied={vc.get('levied',0):5}%  "
              f"not_levied={vc.get('not_levied',0):5}%  not_observed={vc.get('not_observed',0):4}%")


if __name__ == "__main__":
    panel, out = build()
    report(panel, out)
