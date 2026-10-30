"""
2_build_property_rates.py
Works out each town's property-tax rate as a share of market value, for the
town, its school district and its county, and saves it to
data/derived/pa_property_burden_panel.csv.

Run it with the pkg-test Python (it needs pandas, xlrd and openpyxl):
  ~/packaging-test/pkg-test/bin/python scripts/2_build_data/2_build_property_rates.py
"""

import os
import re
import glob
import difflib
import subprocess
import warnings
import numpy as np
import pandas as pd

warnings.filterwarnings("ignore")
# repo root is two folders up (scripts/2_build_data/<this file>)
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
RAW = os.path.join(ROOT, "data", "raw")
DERIVED = os.path.join(ROOT, "data", "derived")
YEARS = range(2002, 2020)


# Muni tax register ----------------------------------------------------------
REGISTER_COLS = {
    "Reporting Year": "year",
    "County": "county",
    "Municipality ID": "muni_id",
    "Municipality": "muni_name",
    "School District ID": "sd_id",
    "School District": "sd_name",
    "Tax Name": "tax_name",
    "Municipal Tax Amount": "muni_amt",
    "School District Tax Amount": "sd_amt",
}


def load_register():
    """All register years in one long table: one row per (muni, SD, tax)."""
    frames = []
    for yr in YEARS:
        f = os.path.join(RAW, "pa_dced_raw", f"pa_taxes_{yr}.xls")
        df = pd.read_excel(f, engine="xlrd")
        df.columns = [str(c).strip() for c in df.columns]
        df = df[[c for c in REGISTER_COLS if c in df.columns]].rename(columns=REGISTER_COLS)
        df["year"] = pd.to_numeric(df["year"], errors="coerce").fillna(yr).astype(int)
        for c in ("muni_amt", "sd_amt"):
            df[c] = pd.to_numeric(df[c], errors="coerce")
        frames.append(df)
    reg = pd.concat(frames, ignore_index=True)
    reg["tn"] = reg["tax_name"].astype(str).str.strip().str.lower()
    print(f"[register] {len(reg):,} rows, {reg.year.min()}-{reg.year.max()}")
    return reg


def is_re_levy(t):
    """A real-estate levy, excluding DCED's own TOTAL roll-up rows."""
    return "real estate" in t and "total" not in t


def summed_mills(reg, unit, amt):
    """Total RE mills per (unit, year). The register repeats a rate once per
    muni-SD pair, so take the max within each levy, then sum the levies."""
    sub = reg[reg.tn.map(is_re_levy)]
    per_levy = sub.groupby([unit, "year", "tax_name"])[amt].max()
    return per_levy.groupby([unit, "year"]).sum(min_count=1).rename("amt").reset_index()


def muni_millage(reg):
    """Muni-year RE mills on the full muni x year grid.
    Absent from that year's register -> NaN; present but no levy -> 0."""
    reg = reg.copy()
    reg["muni_id"] = pd.to_numeric(reg["muni_id"], errors="coerce").astype("Int64")
    reg = reg[reg.muni_id.notna()]

    grid = pd.MultiIndex.from_product([sorted(reg.muni_id.unique()), sorted(reg.year.unique())],
                                      names=["muni_id", "year"])
    panel = pd.DataFrame(index=grid).reset_index()
    present = set(map(tuple, reg[["muni_id", "year"]].drop_duplicates().values.tolist()))
    panel["muni_present"] = [(m, y) in present for m, y in zip(panel.muni_id, panel.year)]

    # county = the muni's most recent non-missing county name
    county = reg.sort_values("year").groupby("muni_id").agg(county=("county", "last")).reset_index()
    panel = panel.merge(county, on="muni_id", how="left")

    amt = panel[["muni_id", "year"]].merge(summed_mills(reg, "muni_id", "muni_amt"),
                                           on=["muni_id", "year"], how="left")["amt"]
    levied = amt.fillna(0) > 0
    panel["muni_re_total_mills"] = np.where(~panel.muni_present, np.nan,
                                            np.where(levied, amt, 0.0))
    print(f"[muni] {panel.muni_id.nunique():,} munis, {len(panel):,} muni-years, "
          f"{panel.muni_re_total_mills.isna().mean()*100:.1f}% missing")
    return panel[["muni_id", "year", "county", "muni_re_total_mills"]]


def sd_millage(reg):
    """SD-year RE mills on the full SD x year grid. Every PA school district
    levies property tax, so a zero or missing value is a register gap (NaN);
    the PDE gap-fill below targets exactly these cells."""
    reg = reg.copy()
    reg["sd_id"] = pd.to_numeric(reg["sd_id"], errors="coerce").astype("Int64")
    reg = reg[reg.sd_id.notna() & (reg.sd_id != 0)]

    grid = pd.MultiIndex.from_product([sorted(reg.sd_id.unique()), sorted(reg.year.unique())],
                                      names=["sd_id", "year"])
    panel = pd.DataFrame(index=grid).reset_index()
    amt = panel.merge(summed_mills(reg, "sd_id", "sd_amt"), on=["sd_id", "year"], how="left")["amt"]
    levied = amt.fillna(0) > 0
    panel["sd_re_total_mills"] = np.where(levied, amt, np.nan)
    print(f"[sd] {panel.sd_id.nunique():,} SDs, {len(panel):,} SD-years, "
          f"{(~levied).sum():,} gaps for the PDE fill")
    return panel


# PDE school-district millage ------------------------------------------------
# The state education department's millage files, used to fill gaps in the DCED register.

# PDE names that the crosswalk spells completely differently
MANUAL_RENAMES = {
    "SOUTH BUTLER COUNTY": "KNOCH",
    "SOUTH BUTLER": "KNOCH",
    "BLAIRSVILLE SALTSBURG": "RIVER VALLEY",
}


def normalize_sd(name):
    """Upper case, drop 'school district' and district-type words, expand Co/Mt/St."""
    s = str(name).upper()
    s = re.sub(r"[.,/\-]", " ", s)
    s = re.sub(r"\bSCH(OOL)?\s+DIST(RICT)?\b", " ", s)
    s = re.sub(r"\bS\s*D\b", " ", s)
    s = re.sub(r"\bSD\b", " ", s)
    for w in ("AREA", "JOINT", "JT", "BOROUGH", "BORO", "COMMUNITY", "COMM",
              "CMNTY", "CONSOLIDATED", "CONSOLIDATD", "CONS", "CITY",
              "TOWNSHIP", "TWP"):
        s = re.sub(r"\b" + w + r"\b", " ", s)
    s = re.sub(r"\bCO\b", "COUNTY", s)
    s = re.sub(r"\bMT\b", "MOUNT", s)
    s = re.sub(r"\bST\b", "SAINT", s)
    return re.sub(r"\s+", " ", s).strip()


def parse_pde_file(path, school_year):
    eng = "xlrd" if path.endswith(".xls") else None
    df = pd.read_excel(path, sheet_name=0, engine=eng)
    # column headers vary by vintage, so match them by substring
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
    # split-rate rows: pre-2008 files mark them in the county name ("Beaver (Land)"),
    # later files in the Municipality/Other Info column
    cc, mi = out["county"].str.lower(), out["muni_info"].str.lower()
    is_land = cc.str.contains("land") | mi.str.contains(r"\bland\b")
    is_bldg = cc.str.contains("bldg|building") | mi.str.contains("building")
    out["role"] = np.where(is_land, "land", np.where(is_bldg, "buildings", "uniform"))
    out["county"] = out["county"].str.replace(r"\s*\((land|bldg|building)s?\)", "",
                                              regex=True, case=False).str.strip()
    # a few districts tax land and buildings at different rates: keep the buildings rate
    out = out[out["district_raw"].str.len() > 0]
    out = out[~out["district_raw"].str.lower().str.contains("total|statewide|average", na=False)]
    return out.dropna(subset=["mills"])


def match_pde_names(districts, xwalk_path):
    """PDE district name -> sd_id: exact on normalized names, then a 0.9 fuzzy
    match (difflib). The fuzzy step matches 5 names; 14 rows stay unmatched."""
    xw = pd.read_csv(xwalk_path)
    lut = {}
    for _, r in xw.iterrows():
        for key in (normalize_sd(r["sd_name_dced"]), normalize_sd(r["sd_name_f33"])):
            lut.setdefault(key, r["sd_id"])
    res = {}
    for d in districts:
        nd = MANUAL_RENAMES.get(normalize_sd(d), normalize_sd(d))
        if nd in lut:
            res[d] = lut[nd]
            continue
        close = difflib.get_close_matches(nd, list(lut), n=1, cutoff=0.9)
        res[d] = lut[close[0]] if close else np.nan
    return res


def pde_millage(xwalk_path):
    """One PDE rate per (sd_id, calendar year). School year 2010-11 maps to
    calendar 2010: checked against the register, this lines up 90.5% of
    district-years within 1 mill, while PDE's own "CY" label (2011) does not."""
    files = sorted(glob.glob(os.path.join(RAW, "pde_retax_raw", "pde_*.xls")) +
                   glob.glob(os.path.join(RAW, "pde_retax_raw", "pde_*.xlsx")))
    long = pd.concat([parse_pde_file(f, re.search(r"pde_(\d{4}-\d{2})", os.path.basename(f)).group(1))
                      for f in files], ignore_index=True)
    # a district spanning several counties can list different mills per county: keep the highest
    dist = (long[long.role != "land"]
            .groupby(["school_year", "aun", "district_raw"])
            .agg(mills=("mills", "max")).reset_index())
    names = match_pde_names(dist["district_raw"].unique(), xwalk_path)
    dist["sd_id"] = dist["district_raw"].map(names)
    print(f"[pde] {len(files)} files, {len(dist):,} district-years, "
          f"{dist.sd_id.isna().sum()} unmatched")

    dist = dist.dropna(subset=["sd_id"])
    dist["sd_id"] = dist["sd_id"].astype(int)
    dist["year"] = dist["school_year"].str[:4].astype(int)
    return (dist.groupby(["sd_id", "year"]).agg(pde_mills=("mills", "max")).reset_index())


def fill_sd_gaps(sd, pde):
    """Fill register gaps with PDE mills. Register values are never overwritten."""
    out = sd.merge(pde, on=["sd_id", "year"], how="left")
    gap = out["sd_re_total_mills"].isna() & out["pde_mills"].notna()
    out.loc[gap, "sd_re_total_mills"] = out.loc[gap, "pde_mills"]
    print(f"[sd fill] {gap.sum():,} of {len(out):,} SD-years filled from PDE, "
          f"{out.sd_re_total_mills.isna().sum():,} still missing")
    return out.drop(columns="pde_mills")


# County millage --------------------------------------------------------------
def county_millage():
    """County RE total mills per county-year: the register's explicit total,
    or the sum of the individual mill levies when the total is missing."""
    frames = []
    for yr in YEARS:
        df = pd.read_excel(os.path.join(RAW, "pa_county_raw", f"pa_county_taxes_{yr}.xls"),
                           engine="xlrd")
        df.columns = [str(c).strip() for c in df.columns]
        df["tn"] = df["Tax Name"].astype(str).str.strip().str.lower()
        df["amt"] = pd.to_numeric(df["Tax Amount"], errors="coerce")
        df["county"] = df["County"].astype(str).str.upper().str.strip()

        total = (df[df.tn.map(lambda t: "real estate total" in t and "mill" in t)]
                 .groupby("county")["amt"].max())
        levies = df[df.tn.map(lambda t: "mill" in t and "total" not in t)]
        summed = (levies.groupby(["county", "Tax Name"])["amt"].max()
                  .groupby("county").sum(min_count=1))
        out = pd.DataFrame({"county": sorted(df["county"].dropna().unique()), "year": yr})
        out["cty_re_total_mills"] = out["county"].map(total).fillna(out["county"].map(summed))
        frames.append(out)
    cty = pd.concat(frames, ignore_index=True)
    print(f"[county] {cty.county.nunique()} counties, {len(cty):,} county-years")
    return cty


# Common level ratio ------------------------------------------------------------
# The CLR converts mills on assessed value into a rate on market value.
CLR_ROW = re.compile(r"(\d{1,2})-(\d{1,2})-(\d{4})\s+\d{1,2}-\d{1,2}-\d{4}\s+(\d*\.\d+)")


def clr_panel():
    pdf = os.path.join(RAW, "clr_raw", "clr_factor_historical.pdf")
    txt = subprocess.run(["pdftotext", "-layout", pdf, "-"],
                         capture_output=True, text=True, check=True).stdout
    rows = []
    for pg in txt.split("\f"):   # one page per county
        m = re.search(r"FOR\s*\n\s*([A-Z][A-Z .'-]+?)\s+COUNTY", pg) \
            or re.search(r"\n\s*([A-Z][A-Z .'-]+?)\s+COUNTY\s*\n", pg)
        if not m:
            continue
        county = re.sub(r"\s+", " ", m.group(1)).strip().upper()
        for mm, dd, yyyy, factor in CLR_ROW.findall(pg):
            f = float(factor)
            # the PDF prints market / assessed, so the CLR is 1 over it. The year it
            # starts is the tax year (that's what makes CLR = 1 in reassessment years).
            rows.append(dict(county=county, year=int(yyyy), month=int(mm),
                             clr=round(1.0 / f, 4) if f else None))
    clr = pd.DataFrame(rows)
    # several factors in one start year: keep the latest-starting one (the 7/1 row)
    clr = clr.sort_values("month").groupby(["county", "year"], as_index=False).last()
    print(f"[clr] {clr.county.nunique()} counties, {len(clr):,} county-years")
    return clr[["county", "year", "clr"]]


# Effective rates ----------------------------------------------------------------
def modal_sd_per_muni():
    """Each muni's school district: the sd_id it reports most often (2.4% of
    munis span several districts)."""
    eit = pd.read_csv(os.path.join(RAW, "pa_eit_rate_panel.csv"))
    eit = eit.dropna(subset=["municipality_id", "sd_id"])
    eit["municipality_id"] = eit["municipality_id"].astype(int)
    eit["sd_id"] = eit["sd_id"].astype(int)
    mode = (eit.groupby("municipality_id")["sd_id"]
            .agg(lambda s: s.mode().iloc[0]).rename("sd_id").reset_index())
    return mode.rename(columns={"municipality_id": "muni_id"})


def effective_rates(muni, sd, cty, clr):
    df = muni.merge(modal_sd_per_muni(), on="muni_id", how="left")
    df = df.merge(sd, on=["sd_id", "year"], how="left")
    df["county"] = df["county"].str.upper().str.strip()
    df = df.merge(cty, on=["county", "year"], how="left")
    df = df.merge(clr, on=["county", "year"], how="left")

    layers = {"muni": "muni_re_total_mills", "sd": "sd_re_total_mills", "county": "cty_re_total_mills"}
    df["total_statutory_mills"] = df[list(layers.values())].sum(axis=1, min_count=1)
    for name, col in layers.items():
        df[f"{name}_eff_rate"] = df[col] / 1000.0 * df["clr"]
    df["eff_property_tax_rate"] = df["total_statutory_mills"] / 1000.0 * df["clr"]

    have = df[list(layers.values()) + ["clr"]].notna()
    df["burden_complete"] = have.all(axis=1)
    df["layers_missing"] = (~have).apply(lambda r: ",".join(c for c in have.columns if r[c]), axis=1)
    keep = ["muni_id", "year", "county", "sd_id",
            "muni_re_total_mills", "sd_re_total_mills", "cty_re_total_mills",
            "total_statutory_mills", "clr",
            "muni_eff_rate", "sd_eff_rate", "county_eff_rate", "eff_property_tax_rate",
            "burden_complete", "layers_missing"]
    return df[keep].sort_values(["muni_id", "year"])


XWALK = os.path.join(DERIVED, "sd_id_xwalk.csv")


def main():
    reg = load_register()
    sd = fill_sd_gaps(sd_millage(reg), pde_millage(XWALK))
    out = effective_rates(muni_millage(reg), sd, county_millage(), clr_panel())
    path = os.path.join(DERIVED, "pa_property_burden_panel.csv")
    out.to_csv(path, index=False)
    c = out[out.burden_complete]
    print(f"[done] {len(out):,} muni-years -> {os.path.relpath(path, ROOT)}; "
          f"{len(c)/len(out)*100:.1f}% have all three layers + CLR; "
          f"median effective rate {c.eff_property_tax_rate.median()*100:.2f}% of market value")


if __name__ == "__main__":
    main()
