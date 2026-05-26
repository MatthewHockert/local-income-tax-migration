"""
multitax_millage_profile.py
==========================

Pre-regression diagnostic on the muni millage panel. Two questions:
  (1) Millage-variation profile: how often do munis change millage, by how much,
      clustered when?
  (2) ENTANGLEMENT: are property-millage changes correlated with EIT changes?
      This is the analog of the muni-vs-SD EIT correlation (-0.12) that cleared
      the Inc-becomes cell. If munis that move the EIT also move millage in the
      same window, the two treatments are entangled (-> joint design needed);
      if they move on independent schedules, they are separable (-> separate
      elasticities, cleaner). Reported explicitly here.

Changes are computed only across consecutive years where BOTH the year and its
lag are OBSERVED (flag levied or not_levied -> numeric; not_observed skipped).
'own-government' comparison = muni millage vs muni resident EIT (the muni sets
both); SD EIT reported alongside.

Output: data/multitax/millage_variation_profile.csv (by-year), console summary.
"""

import os
import numpy as np
import pandas as pd

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data", "multitax")
MILL_MATERIAL = 0.5   # mills (q3_unified_property_tax.R convention)
EIT_MATERIAL = 0.05   # percentage points


def observed_diff(df, col):
    """Year-over-year change within muni, only when both ends are observed."""
    df = df.sort_values(["muni_id", "year"]).copy()
    obs = df[col + "_flag"].isin(["levied", "not_levied"]) if col + "_flag" in df else df[col].notna()
    v = df[col].where(obs)
    g = df.groupby("muni_id")
    lag = g[col].shift(1)
    lag_ok = g.apply(lambda x: (x[col + "_flag"].isin(["levied", "not_levied"]) if col + "_flag" in x
                                else x[col].notna()).shift(1)).reset_index(level=0, drop=True) \
        if col + "_flag" in df else g[col].shift(1).notna()
    consecutive = df["year"] == g["year"].shift(1) + 1
    d = (v - lag).where(consecutive & obs & lag_ok)
    return d


def main():
    p = pd.read_csv(os.path.join(OUT, "pa_muni_multitax_panel.csv"))

    d_gp = observed_diff(p, "re_gp_mills")
    d_tot = observed_diff(p, "re_total_mills")
    # EIT cols from canonical panel have no _flag; treat NaN as not observed
    d_eit = observed_diff(p, "resident_eit")
    d_sd = observed_diff(p, "sd_eit")

    df = pd.DataFrame({"muni_id": p.muni_id, "year": p.year,
                       "d_gp": d_gp, "d_tot": d_tot, "d_eit": d_eit, "d_sd": d_sd})

    # ---- (1) millage-variation profile ----
    g = df["d_gp"].dropna()
    n_trans = g.shape[0]
    any_chg = (g.abs() > 0.001).mean()
    mat_chg = (g.abs() >= MILL_MATERIAL).mean()
    print("===== MILLAGE-VARIATION PROFILE (muni re_gp_mills) =====")
    print(f"observed year-over-year transitions: {n_trans:,}")
    print(f"  any change (|d|>0.001):     {100*any_chg:5.1f}% of transitions")
    print(f"  material change (|d|>=0.5): {100*mat_chg:5.1f}%")
    nz = g[g.abs() > 0.001]
    print(f"  among changes: median |d|={nz.abs().median():.3f}, "
          f"p90 |d|={nz.abs().quantile(.9):.2f} mills; "
          f"share increases={100*(nz>0).mean():.0f}%")

    # clustering by year (material changes per year) -- reassessment years cluster
    by_year = (df.assign(mat=(df.d_gp.abs() >= MILL_MATERIAL))
               .groupby("year")["mat"].agg(["sum", "mean"]))
    by_year.columns = ["n_material_millage_chg", "share"]

    # ---- (2) ENTANGLEMENT: millage vs EIT ----
    print("\n===== ENTANGLEMENT: property millage vs EIT (muni-year changes) =====")
    pairs = [("d_gp", "d_eit", "muni millage vs muni resident EIT (own-govt)"),
             ("d_tot", "d_eit", "muni TOTAL millage vs muni resident EIT"),
             ("d_gp", "d_sd", "muni millage vs SD EIT")]
    for a, b, lab in pairs:
        s = df[[a, b]].dropna()
        r = s[a].corr(s[b])
        print(f"  corr({lab}) = {r:+.3f}   (n={len(s):,})")

    # co-occurrence (own-govt): does a material EIT change predict a material millage change?
    s = df.dropna(subset=["d_gp", "d_eit"]).copy()
    s["mill_chg"] = s.d_gp.abs() >= MILL_MATERIAL
    s["eit_chg"] = s.d_eit.abs() >= EIT_MATERIAL
    base = s.mill_chg.mean()
    cond = s.loc[s.eit_chg, "mill_chg"].mean()
    ct = pd.crosstab(s.eit_chg, s.mill_chg)
    # odds ratio
    try:
        orr = (ct.loc[True, True] * ct.loc[False, False]) / (ct.loc[True, False] * ct.loc[False, True])
    except Exception:
        orr = np.nan
    print(f"\n  P(material millage change)             = {100*base:4.1f}%")
    print(f"  P(material millage change | EIT change)= {100*cond:4.1f}%  (lift {cond/base:.2f}x)")
    print(f"  odds ratio (millage chg ~ EIT chg)     = {orr:.2f}")
    print("\n  contingency (rows=EIT material change, cols=millage material change):")
    print(ct.to_string())

    by_year.to_csv(os.path.join(OUT, "millage_variation_profile.csv"))
    print("\nMaterial millage changes by year (clustering -> possible reassessments):")
    print(by_year.round(3).to_string())

    # verdict hint
    r_own = df[["d_gp", "d_eit"]].dropna().pipe(lambda x: x.d_gp.corr(x.d_eit))
    print(f"\nHEADLINE: corr(d muni millage, d muni EIT) = {r_own:+.3f}.")
    print("  |r| small (~<0.15) => treatments separable (separate elasticities feasible);")
    print("  |r| large => entangled (joint specification advisable).")


if __name__ == "__main__":
    main()
