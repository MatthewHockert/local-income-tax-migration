"""
Scrape NCES F-33 Local Education Agency Finance Survey data for PA school
districts, 2002-2019 (academic years 2001-02 through 2018-19).

Source: NCES Common Core of Data, F-33 finance files.
Provides revenues by source (federal, state, local) and expenditures by
category (instruction, support services, capital outlay, debt service,
etc.) for all US public school districts.

Output: data/pa_sd_afr_panel.csv with one row per (sd_leaid, year) and
columns parallel to data/pa_afr_panel.csv (the muni AFR file).

Usage: python scrape_nces_sd_finance.py
"""

import os
import re
import io
import time
import zipfile
import requests
import pandas as pd
import warnings

warnings.filterwarnings("ignore", message="Unverified HTTPS request")

PROJECT_DIR = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(PROJECT_DIR, "data")
RAW_DIR = os.path.join(DATA_DIR, "pa_sd_afr_raw")
os.makedirs(RAW_DIR, exist_ok=True)

# NCES F-33 file pattern. The F-33 file naming convention:
#   sdf{YY}_1a.zip   for fiscal year YY (academic year ending YY)
# e.g. sdf18_1a.zip = FY 2018 = academic year 2017-18
# Files are at:
#   https://nces.ed.gov/ccd/Data/zip/sdf{YY}_1a.zip
# Some years use slightly different patterns (sdf{YY}c_1a.zip, etc.).
# This script tries a small set of known patterns per year.

YEARS = range(2002, 2020)  # FY year (ending year of academic year)

URL_PATTERNS = [
    "https://nces.ed.gov/ccd/Data/zip/sdf{yy}_1a.zip",
    "https://nces.ed.gov/ccd/Data/zip/sdf{yy}c_1a.zip",
    "https://nces.ed.gov/ccd/Data/zip/sdf{yy}b_1a.zip",
]


def download_year(year, session):
    """Try downloading the F-33 zip for FY=year. Returns local path or None."""
    yy = str(year)[-2:]
    outfile = os.path.join(RAW_DIR, f"sdf{yy}_1a.zip")
    if os.path.exists(outfile):
        return outfile
    for pat in URL_PATTERNS:
        url = pat.format(yy=yy)
        try:
            r = session.get(url, verify=False, timeout=120, stream=True)
            if r.status_code == 200 and int(r.headers.get("Content-Length", 0)) > 1000:
                with open(outfile, "wb") as f:
                    for chunk in r.iter_content(chunk_size=8192):
                        f.write(chunk)
                print(f"  {year}: downloaded {url}")
                return outfile
        except Exception as e:
            continue
    print(f"  {year}: FAILED to download (tried {len(URL_PATTERNS)} patterns)")
    return None


def parse_year(zip_path, year):
    """Extract the F-33 data file from the zip and load PA rows."""
    if zip_path is None or not os.path.exists(zip_path):
        return None
    with zipfile.ZipFile(zip_path) as z:
        names = z.namelist()
        # Look for the main data file: usually .txt or .csv
        candidates = [n for n in names if n.lower().endswith((".csv", ".txt"))
                      and "layout" not in n.lower()
                      and "doc" not in n.lower()]
        if not candidates:
            print(f"  {year}: no data file in {zip_path}")
            return None
        data_name = candidates[0]
        with z.open(data_name) as f:
            # F-33 files are typically tab- or comma-delimited; try both
            content = f.read()
        # Try CSV first, fall back to tab
        for sep in [",", "\t", "|"]:
            try:
                df = pd.read_csv(io.BytesIO(content), sep=sep, dtype=str,
                                  encoding="latin-1", low_memory=False,
                                  on_bad_lines="skip")
                if df.shape[1] > 5:
                    break
            except Exception:
                df = None
                continue
        if df is None or df.shape[1] < 5:
            print(f"  {year}: parse failed")
            return None
    # Filter to PA (FIPS 42); column name varies by year
    state_cols = [c for c in df.columns if c.upper() in
                  ("STABBR", "STATE", "STATENAME", "FIPST", "STATE_CODE")]
    pa_filter = None
    for sc in state_cols:
        if df[sc].astype(str).str.contains("PA", na=False).any():
            pa_filter = df[sc].astype(str).str.upper().eq("PA")
            break
        if df[sc].astype(str).eq("42").any():
            pa_filter = df[sc].astype(str).eq("42")
            break
    if pa_filter is None:
        # Some years use FIPST as 2-digit numeric
        for sc in df.columns:
            if df[sc].astype(str).eq("42").any():
                pa_filter = df[sc].astype(str).eq("42")
                break
    if pa_filter is None:
        print(f"  {year}: could not identify PA filter column")
        return None
    df_pa = df[pa_filter].copy()
    df_pa["fy"] = year
    print(f"  {year}: PA rows = {len(df_pa)}")
    return df_pa


def main():
    session = requests.Session()
    session.headers.update({"User-Agent": "Research scraper (academic)"})
    all_years = []
    for year in YEARS:
        print(f"Year {year}:")
        zp = download_year(year, session)
        df = parse_year(zp, year)
        if df is not None:
            all_years.append(df)
        time.sleep(1)
    if not all_years:
        print("No data parsed. Check URL patterns or run year-by-year manually.")
        return
    raw = pd.concat(all_years, ignore_index=True, sort=False)
    raw.to_csv(os.path.join(DATA_DIR, "pa_sd_finance_raw.csv"), index=False)
    print(f"\nSaved raw data: {len(raw)} rows. "
          "See pa_sd_finance_raw.csv to inspect column names.")
    print("\nNext step: build share-based controls (run build_sd_controls.R "
          "after raw data is verified).")


if __name__ == "__main__":
    main()
