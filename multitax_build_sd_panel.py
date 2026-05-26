"""
multitax_build_sd_panel.py
=========================

Assemble the SCHOOL-DISTRICT-YEAR millage panel (2002-2019) from the DCED tax
register SD side, keyed to NCES LEAID via the existing crosswalk
(data/pa_sd_id_xwalk.rds -> data/multitax/sd_id_xwalk.csv).

Register-first. The register carries SD real-estate millage on every (muni, SD)
row; this rolls it up to one row per (sd_id, year).

COLUMNS
  re_gp_mills_sd        : Real Estate - General Purpose, SD side (headline rate).
  re_total_mills_sd     : summed SD RE total (max-within-levy, then sum; excl
                          native '... TOTAL' roll-up).
  percap_code_dollars_sd: the §679 school per-capita levy (SD column).
  sd_eit_resident       : SD resident EIT rate (max over EIT-resident SD rows).
  *_flag                : three-state coverage (levied / not_levied / not_observed),
                          same semantics as the muni panel.
  re_gp_county_dispersion: max-min of SD General-Purpose millage across the SD's
                          (muni) rows within the year. >0 flags a multi-county SD
                          whose millage is county-equalized (CLR); the headline
                          uses the max. Documented, not silently collapsed.

GAP-FILL (separate workstream, NOT done here)
  The register leaves ~115 SDs with missing SD-side millage in some years
  (esp. 2012-2016; see COVERAGE_REPORT.md sec.2). Those cells are flagged
  not_levied/not_observed here and are the targets for PDE 'Pennsylvania School
  Finance' historical SD millage backfill. County assessment records only as a
  last resort. Backfilled values must carry source='PDE' (vs source='DCED').

Output: data/multitax/pa_sd_millage_panel.csv
"""

import os
import warnings
import numpy as np
import pandas as pd

from multitax_coverage_report import load_register, OUT_DIR

warnings.filterwarnings("ignore")


def build():
    reg = load_register()
    reg["tn"] = reg["tax_name"].astype(str).str.strip().str.lower()
    reg["sd_id"] = pd.to_numeric(reg["sd_id"], errors="coerce").astype("Int64")
    reg = reg[reg.sd_id.notna() & (reg.sd_id != 0)].copy()

    years = sorted(reg.year.unique())
    universe = sorted(reg.sd_id.unique())
    grid = pd.MultiIndex.from_product([universe, years], names=["sd_id", "year"])
    present = set(map(tuple, reg[["sd_id", "year"]].drop_duplicates().values.tolist()))
    panel = pd.DataFrame(index=grid).reset_index()
    panel["sd_present"] = [(s, y) in present for s, y in zip(panel.sd_id, panel.year)]

    ident = (reg.sort_values("year").groupby("sd_id")
             .agg(sd_name=("sd_name", "last")).reset_index())
    panel = panel.merge(ident, on="sd_id", how="left")

    def sd_amount(pred, agg="max"):
        sub = reg[reg.tn.map(pred)]
        if agg == "max":
            return sub.groupby(["sd_id", "year"])["sd_amt"].max()
        per_levy = sub.groupby(["sd_id", "year", "tax_name"])["sd_amt"].max()
        return per_levy.groupby(["sd_id", "year"]).sum(min_count=1)

    def assemble(colname, s, zero_credible=True):
        """zero_credible=True: a non-positive value is a genuine non-levy
        (rate=0). zero_credible=False (RE millage): a zero is NOT credible —
        every PA school district levies property tax — so a non-positive cell
        is a register GAP, labeled not_observed with value NaN (the PDE
        backfill target), never silently zeroed."""
        m = panel[["sd_id", "year", "sd_present"]].merge(
            s.rename("amt").reset_index(), on=["sd_id", "year"], how="left")
        amt = m["amt"]
        pos = amt.fillna(0) > 0
        if zero_credible:
            flag = np.where(~m["sd_present"], "not_observed",
                    np.where(pos, "levied", "not_levied"))
            val = np.where(flag == "not_observed", np.nan,
                   np.where(flag == "levied", amt, 0.0))
        else:
            flag = np.where(pos, "levied", "not_observed")
            val = np.where(pos, amt, np.nan)
        panel[colname] = val
        panel[colname + "_flag"] = flag
        panel[colname + "_source"] = np.where(pos, "DCED", "")

    assemble("re_gp_mills_sd", sd_amount(lambda t: t == "real estate - general purpose (mills)", "max"),
             zero_credible=False)
    assemble("re_total_mills_sd", sd_amount(lambda t: "real estate" in t and "total" not in t, "sum"),
             zero_credible=False)
    assemble("percap_code_dollars_sd", sd_amount(lambda t: t == "per capita code (dollars)", "max"))
    assemble("sd_eit_resident", sd_amount(
        lambda t: "earned income" in t and "resident" in t and "non" not in t, "max"))

    # Multi-county dispersion of SD general-purpose millage (CLR equalization).
    gp = reg[reg.tn == "real estate - general purpose (mills)"]
    disp = (gp.groupby(["sd_id", "year"])["sd_amt"]
            .agg(lambda x: x.max() - x.min()).rename("re_gp_county_dispersion").reset_index())
    panel = panel.merge(disp, on=["sd_id", "year"], how="left")

    xw = pd.read_csv(os.path.join(OUT_DIR, "sd_id_xwalk.csv"))
    xw = xw[["sd_id", "ncesid"]].drop_duplicates("sd_id")
    panel = panel.merge(xw, on="sd_id", how="left")

    front = ["sd_id", "ncesid", "sd_name", "year", "sd_present", "re_gp_county_dispersion"]
    rest = [c for c in panel.columns if c not in front]
    panel = panel[front + rest]
    out = os.path.join(OUT_DIR, "pa_sd_millage_panel.csv")
    panel.to_csv(out, index=False)
    return panel, out


def report(panel, out):
    print(f"\nWrote {out}\n  rows: {len(panel):,}  | SDs: {panel.sd_id.nunique()}  "
          f"| years: {panel.year.min()}-{panel.year.max()}")
    print(f"  SDs without ncesid: {panel.loc[panel.ncesid.isna(),'sd_id'].nunique()}")
    print("\nThree-state flags (share of SD-years):")
    for c in [c for c in panel.columns if c.endswith("_flag")]:
        vc = panel[c].value_counts(normalize=True).mul(100).round(1).to_dict()
        print(f"  {c[:-5]:24s} levied={vc.get('levied',0):5}%  "
              f"not_levied={vc.get('not_levied',0):5}%  not_observed={vc.get('not_observed',0):4}%")
    # PDE gap-fill target accounting (on the headline rate)
    g = panel[panel.re_gp_mills_sd_flag != "levied"]
    sds_gap = g.sd_id.nunique()
    print(f"\nPDE gap-fill target: {len(g):,} SD-year cells lacking register millage "
          f"across {sds_gap} distinct SDs.")
    md = panel[panel.re_gp_county_dispersion > 0.001]
    print(f"Multi-county SDs (re_gp dispersion>0): {md.sd_id.nunique()} SDs, "
          f"{len(md)} SD-years (headline uses max).")


if __name__ == "__main__":
    panel, out = build()
    report(panel, out)
