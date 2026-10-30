"""
Scrape Census Bureau F-33 Annual Survey of School System Finances data
for PA school districts, fiscal years 2002-2019.

Source: U.S. Census Bureau, Annual Survey of School System Finances (F-33).
This is the original survey NCES uses for its Common Core of Data finance
files. Provides revenues by source (federal, state, local, property tax,
income tax, etc.) and expenditures by category (instruction, support
services, capital outlay, debt service, etc.) for all US public school
districts.

URL pattern:
  https://www.census.gov/programs-surveys/school-finances/tables/{YYYY}/secondary-education-finance/elsec{YY}.xls
  (or elsec{YY}.txt for fixed-width)

Output: data/raw/pa_sd_finance_raw.csv with one row per (sd_id_census, fy) and
all raw revenue/expenditure variables, filtered to PA (FIPS 42).

Usage: python3 scrape_nces_sd_finance.py
"""

import os
import time
import requests
import pandas as pd
import warnings

warnings.filterwarnings("ignore", message="Unverified HTTPS request")

# repo root is two folders up (scripts/1_get_data/<this file>)
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
DATA_DIR = os.path.join(ROOT, "data", "raw")
RAW_DIR = os.path.join(DATA_DIR, "pa_sd_afr_raw")
os.makedirs(RAW_DIR, exist_ok=True)

YEARS = range(2002, 2020)

URL_PATTERN_XLS = (
    "https://www2.census.gov/programs-surveys/school-finances/tables/"
    "{yyyy}/secondary-education-finance/elsec{yy}.xls"
)
URL_PATTERN_TXT = (
    "https://www2.census.gov/programs-surveys/school-finances/tables/"
    "{yyyy}/secondary-education-finance/elsec{yy}.txt"
)
# Alternate: some early years (pre-2010) live in a slightly different
# directory. Try the simpler pattern first; fall back to common variants.
ALT_PATTERNS = [
    "https://www2.census.gov/govs/school/{yyyy}/elsec{yy}.xls",
    "https://www2.census.gov/govs/school/{yyyy}/elsec{yy}.txt",
]


def download_year(year, session):
    yy = str(year)[-2:]
    yyyy = str(year)
    outfile_xls = os.path.join(RAW_DIR, f"elsec{yy}.xls")
    outfile_txt = os.path.join(RAW_DIR, f"elsec{yy}.txt")
    if os.path.exists(outfile_xls):
        return outfile_xls
    if os.path.exists(outfile_txt):
        return outfile_txt
    candidates = [
        (URL_PATTERN_XLS.format(yyyy=yyyy, yy=yy), outfile_xls),
        (URL_PATTERN_TXT.format(yyyy=yyyy, yy=yy), outfile_txt),
    ]
    for alt in ALT_PATTERNS:
        url = alt.format(yyyy=yyyy, yy=yy)
        out = os.path.join(RAW_DIR, os.path.basename(url))
        candidates.append((url, out))

    for url, out in candidates:
        try:
            r = session.get(url, verify=False, timeout=120, stream=True,
                            headers={"User-Agent": "Research scraper (academic)"})
            if r.status_code == 200:
                size = int(r.headers.get("Content-Length", 0))
                if size > 1000 or "html" not in r.headers.get("Content-Type", ""):
                    with open(out, "wb") as f:
                        for chunk in r.iter_content(chunk_size=8192):
                            f.write(chunk)
                    print(f"  {year}: downloaded {url} ({size:,} bytes)")
                    return out
        except Exception:
            continue
    print(f"  {year}: FAILED")
    return None


def parse_year(path, year):
    if path is None or not os.path.exists(path):
        return None
    try:
        if path.endswith(".xls"):
            df = pd.read_excel(path, dtype=str, engine="xlrd")
        else:
            # Fixed-width text — Census F-33 publishes these with a header
            # row; try CSV first, then fall back to whitespace-delimited.
            try:
                df = pd.read_csv(path, dtype=str, low_memory=False)
            except Exception:
                df = pd.read_csv(path, dtype=str, sep=r"\s+", low_memory=False,
                                  engine="python")
    except Exception as e:
        print(f"  {year}: parse error: {e}")
        return None

    # Filter to PA via NCESID prefix (NCESID is FIPS-based: first 2 digits
    # are the state FIPS code, where 42 = Pennsylvania). The F-33 STATE
    # column is the Census Bureau internal state code which is NOT FIPS.
    if "NCESID" not in df.columns:
        print(f"  {year}: no NCESID column")
        return None
    pa = df["NCESID"].astype(str).str.strip().str.startswith("42")
    if not pa.any():
        # Some early years pad NCESID to 7 chars; try with leading zero strip
        pa = df["NCESID"].astype(str).str.lstrip("0").str.startswith("42")
    if not pa.any():
        print(f"  {year}: no PA SDs found via NCESID prefix")
        return None
    df_pa = df.loc[pa].copy()
    df_pa["fy"] = year
    print(f"  {year}: PA SDs = {len(df_pa)}, cols = {df.shape[1]}")
    return df_pa


def main():
    session = requests.Session()
    all_years = []
    for year in YEARS:
        print(f"Year {year}:")
        path = download_year(year, session)
        df = parse_year(path, year)
        if df is not None:
            all_years.append(df)
        time.sleep(0.5)
    if not all_years:
        print("\nNo data parsed.")
        return
    raw = pd.concat(all_years, ignore_index=True, sort=False)
    out = os.path.join(DATA_DIR, "pa_sd_finance_raw.csv")
    raw.to_csv(out, index=False)
    print(f"\nSaved {out}: {len(raw)} rows, {raw.shape[1]} cols.")
    # Quick column preview
    print("\nSample columns:")
    for c in raw.columns[:20]:
        print(f"  {c}")


if __name__ == "__main__":
    main()
