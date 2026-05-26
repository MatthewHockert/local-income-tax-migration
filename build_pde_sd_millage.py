"""
build_pde_sd_millage.py
======================

Parse the downloaded PDE Real Estate Tax Rate files into a long
district x school-year nominal-millage table, keyed to the project's sd_id /
NCES LEAID via district-name normalization. Source for the SD millage gap-fill.

FILE FORMAT (varies by vintage; handled here)
  Columns: AUN | School District | Tax County | [Municipality/Other Info] |
           "<sy> Real Estate Mills" | "<sy> Additional Commitments"
  - Multi-county districts: one row per county, DIFFERENT mills per county
    (STEB common-level-ratio equalization). We keep all, then aggregate with
    max (matching the register's SD rule) and record dispersion.
  - Split-rate (land-value-tax) districts: separate Land / Buildings rows.
    pre-2008 .xls encodes this in the county string ("Beaver (Land)"); later
    files use the Municipality/Other Info column. We keep the BUILDINGS
    (improvement) rate as the comparable uniform millage and flag split_rate;
    the Land rate is dropped from the headline (recorded in the long file).
  - Millage column header varies ("Real Estate Mills", "\nReal Estate Mills",
    "CY YYYY\nReal Estate Mills"); matched by substring.

PDE's own 2019-20 header reads "CY 2020", hinting school-year-end == calendar
year, but the merge mapping is decided EMPIRICALLY downstream (not assumed).

Outputs (data/multitax/)
  pde_sd_millage_long.csv  : school_year, aun, district_raw, county, mills,
                             role(uniform/buildings/land), one row per source row
  pde_sd_millage.csv       : school_year, aun, district_raw, sd_id, ncesid,
                             mills (max), mills_dispersion, n_counties, split_rate,
                             match_method
"""

import os
import re
import glob
import warnings
import numpy as np
import pandas as pd

warnings.filterwarnings("ignore")
PROJECT_DIR = os.path.dirname(os.path.abspath(__file__))
RAW = os.path.join(PROJECT_DIR, "data", "pde_retax_raw")
OUT = os.path.join(PROJECT_DIR, "data", "multitax")


def normalize_sd(name):
    """Normalize a district name for matching: upper, drop SD/school-district
    suffixes and punctuation, collapse spaces, expand a couple of abbreviations."""
    s = str(name).upper()
    s = re.sub(r"[.,/\-]", " ", s)   # punctuation + hyphen -> space
    s = re.sub(r"\bSCH(OOL)?\s+DIST(RICT)?\b", " ", s)
    s = re.sub(r"\bS\s*D\b", " ", s)
    s = re.sub(r"\bSD\b", " ", s)
    # drop district-type descriptor words (both PDE and crosswalk spellings/abbrevs)
    for w in ("AREA", "JOINT", "JT", "BOROUGH", "BORO", "COMMUNITY", "COMM",
              "CMNTY", "CONSOLIDATED", "CONSOLIDATD", "CONS", "CITY",
              "TOWNSHIP", "TWP"):
        s = re.sub(r"\b" + w + r"\b", " ", s)
    s = re.sub(r"\bCO\b", "COUNTY", s)
    s = re.sub(r"\bMT\b", "MOUNT", s)
    s = re.sub(r"\bST\b", "SAINT", s)
    s = re.sub(r"\s+", " ", s).strip()
    return s


# Known full renames (PDE name <-> the project's crosswalk name), per CLAUDE.md.
MANUAL_RENAMES = {
    "SOUTH BUTLER COUNTY": "KNOCH",
    "SOUTH BUTLER": "KNOCH",
    "BLAIRSVILLE SALTSBURG": "RIVER VALLEY",
}


def parse_file(path, school_year):
    eng = "xlrd" if path.endswith(".xls") else None
    df = pd.read_excel(path, sheet_name=0, engine=eng)
    df.columns = [str(c).replace("\n", " ").strip() for c in df.columns]
    aun_c = next(c for c in df.columns if c.upper() == "AUN")
    dist_c = next(c for c in df.columns if "school district" in c.lower())
    cty_c = next(c for c in df.columns if "county" in c.lower())
    muni_c = next((c for c in df.columns if "municipality" in c.lower()
                   or "other info" in c.lower()), None)
    mill_c = next(c for c in df.columns if "real estate mills" in c.lower()
                  and "additional" not in c.lower())
    out = pd.DataFrame({
        "school_year": school_year,
        "aun": df[aun_c].astype(str).str.extract(r"(\d+)")[0],
        "district_raw": df[dist_c].astype(str).str.strip(),
        "county": df[cty_c].astype(str).str.strip(),
        "muni_info": (df[muni_c].astype(str).str.strip() if muni_c else ""),
        "mills": pd.to_numeric(df[mill_c], errors="coerce"),
    })
    # classify role: land / buildings (split-rate) vs uniform
    cc = out["county"].str.lower()
    mi = out["muni_info"].str.lower()
    is_land = cc.str.contains("land") | mi.str.contains(r"\bland\b")
    is_bldg = cc.str.contains("bldg|building") | mi.str.contains("building")
    out["role"] = np.where(is_land, "land", np.where(is_bldg, "buildings", "uniform"))
    out["county"] = out["county"].str.replace(r"\s*\((land|bldg|building)s?\)", "",
                                              regex=True, case=False).str.strip()
    out = out[out["district_raw"].str.len() > 0]
    out = out[~out["district_raw"].str.lower().str.contains("total|statewide|average", na=False)]
    return out.dropna(subset=["mills"])


def crosswalk(districts):
    xw = pd.read_csv(os.path.join(OUT, "sd_id_xwalk.csv"))
    xw["n_dced"] = xw["sd_name_dced"].map(normalize_sd)
    xw["n_f33"] = xw["sd_name_f33"].map(normalize_sd)
    lut = {}
    for _, r in xw.iterrows():
        for key in (r["n_dced"], r["n_f33"]):
            lut.setdefault(key, (r["sd_id"], r["ncesid"]))
    keys = list(lut)
    import difflib
    res = {}
    for d in districts:
        nd = normalize_sd(d)
        nd = MANUAL_RENAMES.get(nd, nd)
        if nd in lut:
            res[d] = (*lut[nd], "exact")
            continue
        close = difflib.get_close_matches(nd, keys, n=1, cutoff=0.9)
        if close:
            res[d] = (*lut[close[0]], "fuzzy")
        else:
            res[d] = (np.nan, np.nan, "unmatched")
    return res


def main():
    files = sorted(glob.glob(os.path.join(RAW, "pde_*.xls")) +
                   glob.glob(os.path.join(RAW, "pde_*.xlsx")))
    longs = []
    for f in files:
        sy = re.search(r"pde_(\d{4}-\d{2})", os.path.basename(f)).group(1)
        longs.append(parse_file(f, sy))
    long = pd.concat(longs, ignore_index=True)
    long.to_csv(os.path.join(OUT, "pde_sd_millage_long.csv"), index=False)

    # headline: drop land rows; aggregate to (school_year, district)
    head = long[long.role != "land"].copy()
    g = (head.groupby(["school_year", "aun", "district_raw"])
         .agg(mills=("mills", "max"),
              mills_min=("mills", "min"),
              n_counties=("county", "nunique"),
              split_rate=("role", lambda r: (r == "buildings").any()))
         .reset_index())
    g["mills_dispersion"] = (g["mills"] - g["mills_min"]).round(4)
    g.drop(columns="mills_min", inplace=True)

    cw = crosswalk(g["district_raw"].unique())
    g["sd_id"] = g["district_raw"].map(lambda d: cw[d][0])
    g["ncesid"] = g["district_raw"].map(lambda d: cw[d][1])
    g["match_method"] = g["district_raw"].map(lambda d: cw[d][2])
    g.to_csv(os.path.join(OUT, "pde_sd_millage.csv"), index=False)

    print(f"Parsed {len(files)} files | long rows: {len(long):,}")
    print(f"District-school-year rows: {len(g):,} | distinct districts: {g.district_raw.nunique()}")
    um = g[g.match_method == "unmatched"]["district_raw"].nunique()
    print(f"Crosswalk: {g.sd_id.notna().sum()} matched rows; UNMATCHED districts: {um}")
    if um:
        print("  unmatched:", sorted(g[g.match_method == 'unmatched']['district_raw'].unique())[:25])
    print(f"Split-rate (LVT) district-years: {int(g.split_rate.sum())}")
    print(f"Multi-county district-years: {(g.n_counties>1).sum()} "
          f"(max dispersion {g.mills_dispersion.max():.1f} mills)")
    print("Mills by school year (median):")
    print(g.groupby('school_year')['mills'].median().round(2).to_string())


if __name__ == "__main__":
    main()
