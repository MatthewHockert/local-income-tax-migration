"""
multitax_reassessment_inventory.py
=================================

County reassessment-timing inventory (the highest-value diagnostic for the
property-tax leg). A millage change that follows a county reassessment is a
revenue-neutrality adjustment to a jump in assessed values -- plausibly more
exogenous to a muni's own fiscal politics than a discretionary hike. Tagging
reassessment-driven vs discretionary millage variation is what makes the
property-tax design credible rather than just-another-DiD.

SOURCE: published county-by-county "last reassessment year" list (APTC /
Allegheny Institute compilations; cross-checked against news for the
court-ordered ones). PA reassesses rarely and irregularly, so "last
reassessment year" == the in-panel event for counties whose last reassessment
falls in 2002-2019. NOTE: a few effective-year ambiguities (esp. Allegheny:
certified 2012, court-ordered values effective for the 2013 tax year) -> we
match millage changes within a +/-1 year window. For publication these should
be corroborated against the STEB Common Level Ratio series (CLR jumps toward
100% the reassessment year) and/or county confirmation.

Outputs:
  data/multitax/pa_county_reassessment.csv  (67 counties; in-panel flag + year)
  console: cross-check of reassessment timing vs observed millage changes.
"""

import os
import numpy as np
import pandas as pd

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data", "multitax")

# county -> last reassessment year (all 67; published compilation)
LAST_REASSESS = {
    "ADAMS": 2010, "ALLEGHENY": 2012, "ARMSTRONG": 1997, "BEAVER": 1982,
    "BEDFORD": 2013, "BERKS": 1994, "BLAIR": 2017, "BRADFORD": 1999,
    "BUCKS": 1972, "BUTLER": 1969, "CAMBRIA": 1972, "CAMERON": 1986,
    "CARBON": 2001, "CENTRE": 1995, "CHESTER": 1998, "CLARION": 1975,
    "CLEARFIELD": 1989, "CLINTON": 2009, "COLUMBIA": 1992, "CRAWFORD": 1969,
    "CUMBERLAND": 2010, "DAUPHIN": 2002, "DELAWARE": 2021, "ELK": 1986,
    "ERIE": 2013, "FAYETTE": 2003, "FOREST": 1974, "FRANKLIN": 1961,
    "FULTON": 1990, "GREENE": 2003, "HUNTINGDON": 1978, "INDIANA": 2016,
    "JEFFERSON": 1987, "JUNIATA": 1974, "LACKAWANNA": 1968, "LANCASTER": 2018,
    "LAWRENCE": 2003, "LEBANON": 2003, "LEHIGH": 2013, "LUZERNE": 2009,
    "LYCOMING": 2004, "MCKEAN": 1998, "MERCER": 1974, "MIFFLIN": 1999,
    "MONROE": 1998, "MONTGOMERY": 1998, "MONTOUR": 2006, "NORTHAMPTON": 1995,
    "NORTHUMBERLAND": 1972, "PERRY": 2011, "PHILADELPHIA": 2023, "PIKE": 1996,
    "POTTER": 1977, "SCHUYLKILL": 1997, "SNYDER": 1973, "SOMERSET": 1998,
    "SULLIVAN": 2004, "SUSQUEHANNA": 1993, "TIOGA": 2001, "UNION": 2006,
    "VENANGO": 2004, "WARREN": 1989, "WASHINGTON": 2017, "WAYNE": 2023,
    "WESTMORELAND": 1973, "WYOMING": 1988, "YORK": 2006,
}
PANEL = (2002, 2019)


def build_inventory():
    rows = []
    for c, y in sorted(LAST_REASSESS.items()):
        in_panel = PANEL[0] <= y <= PANEL[1]
        rows.append(dict(county=c, last_reassessment_year=y,
                         reassessed_in_panel=in_panel,
                         in_panel_year=y if in_panel else np.nan))
    df = pd.DataFrame(rows)
    df.to_csv(os.path.join(OUT, "pa_county_reassessment.csv"), index=False)
    return df


def crosscheck(inv):
    """Do munis in reassessment county-years actually show millage changes?"""
    p = pd.read_csv(os.path.join(OUT, "pa_muni_multitax_panel.csv"))
    p = p.sort_values(["muni_id", "year"])
    p["d_gp"] = p.groupby("muni_id")["re_gp_mills"].diff()
    p["consec"] = p["year"] == p.groupby("muni_id")["year"].shift(1) + 1
    p["mat"] = (p["d_gp"].abs() >= 0.5) & p["consec"]
    p["county"] = p["county"].str.upper().str.strip()

    inv_year = dict(zip(inv.county, inv.in_panel_year))
    def near_reassess(row, w=1):
        ry = inv_year.get(row["county"])
        return (not np.isnan(ry)) and abs(row["year"] - ry) <= w if ry == ry else False
    p["reassess_window"] = p.apply(near_reassess, axis=1)

    base = p.loc[p.consec, "mat"].mean()
    in_w = p.loc[p.consec & p.reassess_window, "mat"].mean()
    out_w = p.loc[p.consec & ~p.reassess_window, "mat"].mean()
    n_in = int(p.loc[p.consec & p.reassess_window].shape[0])
    print("===== REASSESSMENT TIMING vs MILLAGE CHANGES =====")
    print(f"in-panel reassessment counties: {int(inv.reassessed_in_panel.sum())} of 67")
    print(f"  {sorted(inv.loc[inv.reassessed_in_panel,'county'])}")
    print(f"\nP(material millage change | muni-year):")
    print(f"  baseline (all):                      {100*base:4.1f}%")
    print(f"  within +/-1yr of county reassessment:{100*in_w:4.1f}%  (n={n_in:,}, lift {in_w/base:.2f}x)")
    print(f"  outside reassessment window:         {100*out_w:4.1f}%")

    # share of all material millage changes that fall in a reassessment window
    tot_mat = int(p.loc[p.consec, "mat"].sum())
    mat_in = int(p.loc[p.consec & p.reassess_window, "mat"].sum())
    print(f"\nOf {tot_mat:,} material millage changes, {mat_in:,} ({100*mat_in/tot_mat:.1f}%) "
          f"fall within +/-1yr of a county reassessment.")
    print("=> reassessment-driven is the minority but is the cleaner identifying subset.")


if __name__ == "__main__":
    inv = build_inventory()
    print(f"Wrote pa_county_reassessment.csv ({len(inv)} counties; "
          f"{int(inv.reassessed_in_panel.sum())} reassessed in 2002-2019)\n")
    crosscheck(inv)
