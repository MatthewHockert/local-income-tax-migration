"""
multitax_acs_dollar_base.py
==========================

Per-muni median household income (B19013) and median home value (B25077),
2002-2019, for the EIT-vs-property dollar-basis conversion. Inventory-first,
with a FLAGGED fallback for thin/missing ACS cells and ONE documented rule for
the pre-ACS5 years.

Source: Census API ACS 5-year (county subdivision, state=42), endyears 2009-2019.
Key read from the shell env CENSUS_API_KEY. GEOID->cousub_fips via the project
crosswalk (data/multitax/acs_geoid_xwalk.csv, from pa_acs_balance.rds).

Coverage / fallback rules (every cell carries a source tag, like the other panels):
  - acs5            : the cousub's own ACS5 value for that endyear (preferred).
  - county_fallback : cousub value missing/thin -> the cousub's COUNTY median that
                      year (from non-missing cousubs). For small munis where ACS
                      suppresses median home value especially.
  - acs5_2009_held  : panel years 2002-2008 (no ACS5 cousub data that far back)
                      -> HELD CONSTANT at the cousub's 2009 (2005-2009) ACS5 value.
                      ONE documented rule; NOT mixed with 2000-Census values.

Output: data/multitax/pa_acs_dollar_base.csv (cousub_fips, year, medinc, medhome,
        inc_source, home_source) + console coverage inventory.
"""

import os
import json
import subprocess
import warnings
import numpy as np
import pandas as pd

warnings.filterwarnings("ignore")
OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data", "multitax")
KEY = os.environ.get("CENSUS_API_KEY", "")
ACS_YEARS = range(2009, 2020)        # ACS5 endyears available for cousubs
PANEL_YEARS = range(2002, 2020)
NA_SENTINEL = -666666666


def pull_year(endyear):
    url = (f"https://api.census.gov/data/{endyear}/acs/acs5"
           f"?get=NAME,B19013_001E,B25077_001E&for=county%20subdivision:*"
           f"&in=state:42&key={KEY}")
    r = subprocess.run(["curl", "-s", "-m", "60", url], capture_output=True, text=True)
    rows = json.loads(r.stdout)
    df = pd.DataFrame(rows[1:], columns=rows[0])
    # cousub_fips = the 5-digit COUSUBFP (the project's muni key), which the ACS
    # returns directly in the "county subdivision" field. (pa_acs_balance's
    # cousub_fips uses a different, non-matching construction -- do NOT use it.)
    df["cousub_fips"] = df["county subdivision"].str.zfill(5)
    df["county_fips"] = df["state"] + df["county"]
    for c, nm in [("B19013_001E", "medinc"), ("B25077_001E", "medhome")]:
        v = pd.to_numeric(df[c], errors="coerce")
        df[nm] = v.where(v > 0)          # negative sentinels / 0 -> NA
    df["year"] = endyear
    return df[["cousub_fips", "county_fips", "year", "medinc", "medhome"]]


def main():
    if not KEY:
        raise SystemExit("CENSUS_API_KEY not set in env")
    frames = [pull_year(y) for y in ACS_YEARS]
    acs = pd.concat(frames, ignore_index=True)

    # ---- coverage inventory (before fallback) ----
    print("===== ACS5 coverage inventory (PA county subdivisions) =====")
    inv = acs.groupby("year").agg(
        n_cousub=("cousub_fips", "nunique"),
        inc_present=("medinc", lambda s: s.notna().sum()),
        home_present=("medhome", lambda s: s.notna().sum())).reset_index()
    inv["inc_%"] = (100*inv.inc_present/inv.n_cousub).round(1)
    inv["home_%"] = (100*inv.home_present/inv.n_cousub).round(1)
    print(inv.to_string(index=False))
    print(f"\nThin cells: median HOME value is the binding gap (small munis, few "
          f"owner-occ units). Worst year home coverage: {inv['home_%'].min()}%.")

    # ---- flagged fallback: county median for thin/missing cells ----
    def fill(df, col):
        cty_med = df.groupby(["county_fips", "year"])[col].transform("median")
        src = np.where(df[col].notna(), "acs5",
              np.where(cty_med.notna(), "county_fallback", "missing"))
        val = df[col].where(df[col].notna(), cty_med)
        return val, src
    acs["medinc"], acs["inc_source"] = fill(acs, "medinc")
    acs["medhome"], acs["home_source"] = fill(acs, "medhome")
    base = acs[["cousub_fips", "year", "medinc", "medhome", "inc_source", "home_source"]]

    # ---- pre-2009 panel years: ONE rule = held constant at 2009 ACS5 ----
    y2009 = base[base.year == 2009].copy()
    pre = []
    for y in range(2002, 2009):
        t = y2009.copy(); t["year"] = y
        t["inc_source"] = "acs5_2009_held"; t["home_source"] = "acs5_2009_held"
        pre.append(t)
    full = pd.concat([base] + pre, ignore_index=True).sort_values(["cousub_fips", "year"])
    full = full[full.year.isin(PANEL_YEARS)]
    full.to_csv(os.path.join(OUT, "pa_acs_dollar_base.csv"), index=False)

    print(f"\nWrote pa_acs_dollar_base.csv: {len(full):,} cousub-years, "
          f"{full.cousub_fips.nunique()} cousubs, {full.year.min()}-{full.year.max()}")
    print("Source-tag composition:")
    print("  income:", full.inc_source.value_counts().to_dict())
    print("  home:  ", full.home_source.value_counts().to_dict())
    print(f"\nCross-muni base spread (ACS5 cells only, 2009-2019):")
    a = base[(base.inc_source == "acs5") & (base.home_source == "acs5")]
    print(f"  median income: p10 ${a.medinc.quantile(.1):,.0f} / median "
          f"${a.medinc.median():,.0f} / p90 ${a.medinc.quantile(.9):,.0f}")
    print(f"  median home:   p10 ${a.medhome.quantile(.1):,.0f} / median "
          f"${a.medhome.median():,.0f} / p90 ${a.medhome.quantile(.9):,.0f}")
    a = a.assign(ratio=a.medhome / a.medinc)
    print(f"  home/income ratio (drives EIT-vs-prop multiple): p10 {a.ratio.quantile(.1):.2f} "
          f"/ median {a.ratio.median():.2f} / p90 {a.ratio.quantile(.9):.2f}")


if __name__ == "__main__":
    main()
