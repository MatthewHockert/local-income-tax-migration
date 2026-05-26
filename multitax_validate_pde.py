"""
multitax_validate_pde.py
=======================

Empirical school-year -> calendar-year mapping test + register/PDE
cross-validation, BEFORE merging PDE millage into the SD panel.

Two candidate mappings of PDE school year YYYY-(YY+1):
  A_end   : calendar = YYYY+1   (2012-13 -> 2013)   [school-year END]
  B_start : calendar = YYYY     (2012-13 -> 2012)   [school-year START]

For each mapping, on register-OBSERVED SD-years (re_gp_mills_sd_flag=='levied'),
compare the PDE district mills against BOTH register objects:
  - re_gp_mills_sd     (Real Estate - General Purpose)
  - re_total_mills_sd  (summed RE total)
Reports, by year: n, Pearson r, mean & median |diff|, count |diff|>1 mill.

WHAT TO WATCH (per PI):
  * One mapping should agree sharply; the other visibly worse -> picks the mapping.
  * If NEITHER agrees cleanly, the register and PDE measure different millage
    objects -> a finding to surface before any SD-millage result is used.
  * If the best mapping FLIPS across years, the register changed its
    reporting-year convention mid-panel -> the merge rule must be year-dependent.
Single-county districts are the clean test (multi-county max-vs-max is noisy
from STEB equalization); both single-county and all-districts are reported.

Output: data/multitax/pde_register_validation.csv  (+ console verdict)
"""

import os
import numpy as np
import pandas as pd

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data", "multitax")


def agreement(reg, pde, mapping, regcol, single_county_only):
    p = pde.copy()
    p["year"] = p["school_year"].str[:4].astype(int) + (1 if mapping == "A_end" else 0)
    if single_county_only:
        p = p[(p.n_counties == 1) & (~p.split_rate)]
    p = p.dropna(subset=["sd_id"])[["sd_id", "year", "mills"]]
    r = reg[reg[regcol + "_flag"] == "levied"][["sd_id", "year", regcol]]
    j = r.merge(p, on=["sd_id", "year"], how="inner").dropna()
    j["d"] = (j[regcol] - j["mills"]).abs()
    rows = []
    for yr, g in j.groupby("year"):
        rows.append(dict(mapping=mapping, regcol=regcol,
                         scope="single_cty" if single_county_only else "all",
                         year=yr, n=len(g),
                         r=round(g[regcol].corr(g["mills"]), 3),
                         pct_exact=round(100 * (g.d < 0.01).mean(), 1),
                         pct_within1=round(100 * (g.d < 1).mean(), 1),
                         median_abs=round(g.d.median(), 3),
                         n_off_gt1=int((g.d > 1).sum())))
    return pd.DataFrame(rows)


def main():
    reg = pd.read_csv(os.path.join(OUT, "pa_sd_millage_panel.csv"))
    pde = pd.read_csv(os.path.join(OUT, "pde_sd_millage.csv"))

    frames = []
    for mapping in ("A_end", "B_start"):
        for regcol in ("re_gp_mills_sd", "re_total_mills_sd"):
            frames.append(agreement(reg, pde, mapping, regcol, single_county_only=True))
            frames.append(agreement(reg, pde, mapping, regcol, single_county_only=False))
    res = pd.concat(frames, ignore_index=True)
    res.to_csv(os.path.join(OUT, "pde_register_validation.csv"), index=False)

    # ---- verdict (robust to register data-entry outliers; Pearson r is NOT
    #      used to choose because a single mis-keyed decimal destroys it) ----
    def summ(mapping, regcol, scope):
        s = res[(res.mapping == mapping) & (res.regcol == regcol) & (res.scope == scope)]
        n = s.n.sum()
        return dict(n=int(n),
                    pct_within1=round((s.pct_within1 * s.n).sum() / n, 1),
                    wt_medabs=round((s.median_abs * s.n).sum() / n, 3))

    print("\n===== CROSS-VALIDATION VERDICT (single-county districts) =====")
    print(f"{'mapping':9s} {'register obj':17s} {'n':>6s} {'% within 1 mill':>16s} {'wt med|diff|':>13s}")
    best = None
    for mapping in ("A_end", "B_start"):
        for regcol in ("re_gp_mills_sd", "re_total_mills_sd"):
            d = summ(mapping, regcol, "single_cty")
            print(f"{mapping:9s} {regcol:17s} {d['n']:6d} {d['pct_within1']:15.1f}% {d['wt_medabs']:13.3f}")
            score = (d["pct_within1"], -d["wt_medabs"])
            if best is None or score > best[0]:
                best = (score, mapping, regcol, d)
    _, bmap, bcol, bd = best
    print(f"\nBEST: mapping={bmap} vs {bcol}  "
          f"({bd['pct_within1']}% within 1 mill, wt med|diff|={bd['wt_medabs']})")

    print(f"\n===== MAPPING STABILITY by year (vs {bcol}, single-county) =====")
    piv = (res[(res.regcol == bcol) & (res.scope == "single_cty")]
           .pivot(index="year", columns="mapping", values="median_abs"))
    piv["best"] = piv.idxmin(axis=1)
    print(piv.round(3).to_string())
    print("\nMapping is", "STABLE" if piv["best"].nunique() == 1
          else f"NOT stable (flips: {sorted(piv['best'].unique())})", "across the panel.")
    if bd["pct_within1"] < 80:
        print("\n*** WARNING: best mapping agrees for <80% within 1 mill -> register and PDE "
              "may measure different objects. Investigate before using SD millage. ***")
    else:
        print(f"\nObjects AGREE ({bd['pct_within1']}% within 1 mill). Residual disagreements are a "
              "thin tail, dominated by register data-entry/scale errors (see below).")

    # register data-entry error diagnostic (best mapping/regcol, single-county)
    p = pde.copy(); p["year"] = p.school_year.str[:4].astype(int) + (1 if bmap == "A_end" else 0)
    p = p[(p.n_counties == 1) & (~p.split_rate)].dropna(subset=["sd_id"])[["sd_id", "year", "mills"]]
    r = reg[reg[bcol + "_flag"] == "levied"][["sd_id", "year", bcol, "sd_name"]]
    j = r.merge(p, on=["sd_id", "year"]).dropna()
    j["d"] = (j[bcol] - j["mills"]).abs()
    j["ratio"] = (j[bcol] / j["mills"]).round(2)
    scale = j[(j.d > 1) & (j.ratio.isin([0.1, 0.01, 10.0, 100.0]) | (j.d > 50))]
    print(f"\nLikely register scale/data-entry errors (PDE is credible value): {len(scale)} cells, "
          f"e.g.:\n{scale.sort_values('d', ascending=False).head(5)[['sd_name','year',bcol,'mills']].to_string(index=False)}")
    j.sort_values("d", ascending=False).to_csv(os.path.join(OUT, "pde_register_discrepancies.csv"), index=False)


if __name__ == "__main__":
    main()
