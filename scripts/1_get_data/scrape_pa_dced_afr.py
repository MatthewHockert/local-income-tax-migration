"""
Scrape PA DCED Statewide Municipal Annual Financial Reports (AFR), 2002-2023.
Source: https://apps.dced.pa.gov/munstats-public/ReportInformation2.aspx?report=StatewideMuniAfr

Each year returns ~1.5 MB XLS with all PA municipalities (2,572 rows x 71 cols).
Includes: tax revenue by type, expenditures, debt, fund balance, per capita.

Usage: python scrape_pa_dced_afr.py
Output: data/raw/pa_afr_panel.csv
"""

import os
import re
import time
import requests
import pandas as pd
import warnings

warnings.filterwarnings("ignore", message="Unverified HTTPS request")

# repo root is two folders up (scripts/1_get_data/<this file>)
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
DATA_DIR = os.path.join(ROOT, "data", "raw")
RAW_DIR = os.path.join(DATA_DIR, "pa_afr_raw")
os.makedirs(RAW_DIR, exist_ok=True)

URL = "https://apps.dced.pa.gov/munstats-public/ReportInformation2.aspx?report=StatewideMuniAfr"

YEARS = range(2002, 2024)


def extract_tokens(html):
    vs = re.search(r'id="__VIEWSTATE" value="([^"]*)"', html)
    vsg = re.search(r'id="__VIEWSTATEGENERATOR" value="([^"]*)"', html)
    ev = re.search(r'id="__EVENTVALIDATION" value="([^"]*)"', html)
    return {
        "__VIEWSTATE": vs.group(1) if vs else "",
        "__VIEWSTATEGENERATOR": vsg.group(1) if vsg else "",
        "__EVENTVALIDATION": ev.group(1) if ev else "",
    }


def download_year(year, session):
    outfile = os.path.join(RAW_DIR, f"pa_afr_{year}.xls")
    if os.path.exists(outfile):
        print(f"  {year}: already downloaded, skipping")
        return outfile

    r = session.get(URL, verify=False, timeout=60)
    r.raise_for_status()
    tokens = extract_tokens(r.text)

    payload = {
        **tokens,
        "ctl00$ContentPlaceHolder1$ddREPORTING_YEAR": str(year),
        "ctl00$ContentPlaceHolder1$btnDisplay": "Display Report",
    }

    r2 = session.post(URL, data=payload, verify=False, timeout=120)
    r2.raise_for_status()

    if "application/vnd.ms-excel" in r2.headers.get("Content-Type", ""):
        with open(outfile, "wb") as f:
            f.write(r2.content)
        print(f"  {year}: downloaded ({len(r2.content) / 1e6:.1f} MB)")
        return outfile
    else:
        print(f"  {year}: unexpected response: {r2.headers.get('Content-Type')}")
        return None


def parse_xls(filepath, year):
    try:
        df = pd.read_excel(filepath, engine="xlrd")
    except Exception as e:
        print(f"  Error reading {filepath}: {e}")
        return None

    # Normalize column names
    df.columns = [str(c).strip().lower().replace(" ", "_").replace("/", "_") for c in df.columns]

    # Add year from Reporting Year column if available, else from argument
    year_col = [c for c in df.columns if "reporting" in c and "year" in c]
    if year_col:
        df["year"] = pd.to_numeric(df[year_col[0]], errors="coerce")
    else:
        df["year"] = year

    return df


def main():
    print("Downloading PA DCED Municipal AFR data...")
    session = requests.Session()

    all_years = []
    for year in YEARS:
        filepath = download_year(year, session)
        if filepath:
            parsed = parse_xls(filepath, year)
            if parsed is not None and len(parsed) > 0:
                all_years.append(parsed)
                print(f"  {year}: parsed {len(parsed)} rows, {len(parsed.columns)} cols")
        time.sleep(3)

    if not all_years:
        print("No data collected!")
        return

    panel = pd.concat(all_years, ignore_index=True)

    outpath = os.path.join(DATA_DIR, "pa_afr_panel.csv")
    panel.to_csv(outpath, index=False)
    print(f"\nSaved {len(panel)} rows to {outpath}")
    print(f"Years: {panel['year'].min()} - {panel['year'].max()}")
    print(f"Municipalities: {panel['municipality_name'].nunique() if 'municipality_name' in panel.columns else 'unknown'}")
    print(f"Columns: {list(panel.columns)[:20]}...")


if __name__ == "__main__":
    main()
