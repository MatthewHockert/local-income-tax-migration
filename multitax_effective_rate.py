"""
multitax_effective_rate.py
=========================

Build the muni-year PROPERTY-TAX BURDEN panel: combine the three statutory
millage layers (muni + school district + county) and convert to an EFFECTIVE
rate on MARKET value using the county CLR.

  effective_rate = (muni_mills + sd_mills + county_mills) / 1000 * CLR
  (statutory mills are levied on ASSESSED value; CLR = assessed/market, so
   multiplying by CLR expresses the burden as a share of MARKET value -- the
   economically meaningful, cross-county-comparable rate, and the right
   treatment given that reassessments are revenue-neutral in statutory mills.)

Year alignment of CLR is EMPIRICALLY VALIDATED: keyed by acceptance-start year,
CLR = 1.00 in every county's reassessment year (100% of cases), so
accept_start_year == tax year (no lag).

Joins:
  muni millage   : pa_muni_multitax_panel.csv         (muni_id, year)
  SD millage     : pa_sd_millage_panel_filled.csv      via muni's modal sd_id
  county millage : pa_county_tax_panel.csv             via county name
  CLR            : pa_clr_panel.csv                     via county + year

Output: data/multitax/pa_property_burden_panel.csv
  muni_id, year, county, sd_id, {muni,sd,county}_re_total_mills,
  total_statutory_mills, clr, eff_property_tax_rate, plus *_eff components
  and a coverage flag.
"""

import os
import numpy as np
import pandas as pd

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data", "multitax")
DATA = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")


def modal_sd_per_muni():
    """Each muni's primary (modal) school district from the DCED rate panel."""
    eit = pd.read_csv(os.path.join(DATA, "pa_eit_rate_panel.csv"))
    eit = eit.dropna(subset=["municipality_id", "sd_id"])
    eit["municipality_id"] = eit["municipality_id"].astype(int)
    eit["sd_id"] = eit["sd_id"].astype(int)
    mode = (eit.groupby("municipality_id")["sd_id"]
            .agg(lambda s: s.mode().iloc[0]).rename("sd_id").reset_index())
    return mode.rename(columns={"municipality_id": "muni_id"})


def main():
    muni = pd.read_csv(os.path.join(OUT, "pa_muni_multitax_panel.csv"))
    muni = muni[["muni_id", "year", "county", "re_total_mills", "re_total_mills_flag"]] \
        .rename(columns={"re_total_mills": "muni_re_total_mills"})
    muni["county"] = muni["county"].str.upper().str.strip()

    sd = pd.read_csv(os.path.join(OUT, "pa_sd_millage_panel_filled.csv"))
    sd = sd[["sd_id", "year", "re_total_mills_sd", "re_total_mills_sd_flag"]]

    cty = pd.read_csv(os.path.join(OUT, "pa_county_tax_panel.csv"))
    cty = cty[["county", "year", "cty_re_total_mills"]]
    cty["county"] = cty["county"].str.upper().str.strip()

    clr = pd.read_csv(os.path.join(OUT, "pa_clr_panel.csv")).rename(
        columns={"accept_start_year": "year"})[["county", "year", "clr"]]
    clr["county"] = clr["county"].str.upper().str.strip()

    df = muni.merge(modal_sd_per_muni(), on="muni_id", how="left")
    df = df.merge(sd, on=["sd_id", "year"], how="left")
    df = df.merge(cty, on=["county", "year"], how="left")
    df = df.merge(clr, on=["county", "year"], how="left")

    df["total_statutory_mills"] = (df[["muni_re_total_mills", "re_total_mills_sd",
                                       "cty_re_total_mills"]].sum(axis=1, min_count=1))
    # effective rate on market value (fraction); components too
    for src, col in [("muni", "muni_re_total_mills"), ("sd", "re_total_mills_sd"),
                     ("county", "cty_re_total_mills")]:
        df[f"{src}_eff_rate"] = df[col] / 1000.0 * df["clr"]
    df["eff_property_tax_rate"] = df["total_statutory_mills"] / 1000.0 * df["clr"]

    # coverage: which layers are present
    have = (df[["muni_re_total_mills", "re_total_mills_sd", "cty_re_total_mills", "clr"]]
            .notna())
    df["burden_complete"] = have.all(axis=1)
    df["layers_missing"] = (~have).apply(
        lambda r: ",".join(c for c in have.columns if r[c]), axis=1)

    df = df.rename(columns={"re_total_mills_sd": "sd_re_total_mills"})
    keep = ["muni_id", "year", "county", "sd_id",
            "muni_re_total_mills", "sd_re_total_mills", "cty_re_total_mills",
            "total_statutory_mills", "clr",
            "muni_eff_rate", "sd_eff_rate", "county_eff_rate", "eff_property_tax_rate",
            "burden_complete", "layers_missing"]
    df = df[keep].sort_values(["muni_id", "year"])
    df.to_csv(os.path.join(OUT, "pa_property_burden_panel.csv"), index=False)

    n = len(df)
    print(f"Wrote pa_property_burden_panel.csv: {n:,} muni-years")
    print(f"  burden_complete (all 3 layers + CLR): {df.burden_complete.mean()*100:.1f}%")
    c = df[df.burden_complete]
    print(f"\nEffective property-tax rate on market value (complete rows, n={len(c):,}):")
    print(f"  median {c.eff_property_tax_rate.median()*100:.2f}% of market value; "
          f"IQR [{c.eff_property_tax_rate.quantile(.25)*100:.2f}%, "
          f"{c.eff_property_tax_rate.quantile(.75)*100:.2f}%]")
    print(f"  component medians (eff rate): muni {c.muni_eff_rate.median()*100:.3f}%, "
          f"SD {c.sd_eff_rate.median()*100:.2f}%, county {c.county_eff_rate.median()*100:.3f}%")
    print(f"  statutory mills median: muni {c.muni_re_total_mills.median():.2f}, "
          f"SD {c.sd_re_total_mills.median():.2f}, county {c.cty_re_total_mills.median():.2f}")
    miss = df[~df.burden_complete]
    if len(miss):
        from collections import Counter
        print(f"\n  incomplete rows: {len(miss):,}; "
              f"Philadelphia (no county layer) is the main case.")


if __name__ == "__main__":
    main()
