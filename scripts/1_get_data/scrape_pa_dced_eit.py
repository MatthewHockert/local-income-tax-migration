"""
Scrape PA DCED MunStats for municipal tax rates (including EIT), 2002-2026.
Source: https://apps.dced.pa.gov/munstats-public/ReportInformation2.aspx?report=taxes_Dyn_Excel

Each year returns an .xls file (~12 MB) with all PA municipalities.
We extract EIT resident/nonresident rates and save a clean CSV panel.

Usage: python scrape_pa_dced_eit.py
Output: data/raw/pa_eit_rate_panel.csv
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
RAW_DIR = os.path.join(DATA_DIR, "pa_dced_raw")
os.makedirs(RAW_DIR, exist_ok=True)

URL = "https://apps.dced.pa.gov/munstats-public/ReportInformation2.aspx?report=taxes_Dyn_Excel"

# LODES covers 2002-2021; grab a few years before for pre-trends
YEARS = range(2000, 2027)


def extract_tokens(html):
    """Extract ASP.NET form tokens from page HTML."""
    vs = re.search(r'id="__VIEWSTATE" value="([^"]*)"', html)
    vsg = re.search(r'id="__VIEWSTATEGENERATOR" value="([^"]*)"', html)
    ev = re.search(r'id="__EVENTVALIDATION" value="([^"]*)"', html)
    return {
        "__VIEWSTATE": vs.group(1) if vs else "",
        "__VIEWSTATEGENERATOR": vsg.group(1) if vsg else "",
        "__EVENTVALIDATION": ev.group(1) if ev else "",
    }


def download_year(year, session):
    """Download the tax report XLS for a single year."""
    outfile = os.path.join(RAW_DIR, f"pa_taxes_{year}.xls")
    if os.path.exists(outfile):
        print(f"  {year}: already downloaded, skipping")
        return outfile

    # GET fresh tokens
    r = session.get(URL, verify=False, timeout=60)
    r.raise_for_status()
    tokens = extract_tokens(r.text)

    # POST to get XLS
    payload = {
        **tokens,
        "ctl00$ContentPlaceHolder1$ddCOUNTY_ID": "NULL",
        "ctl00$ContentPlaceHolder1$ddMUNICIPALITY_ID": "NULL",
        "ctl00$ContentPlaceHolder1$ddSD_ID": "NULL",
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
        print(f"  {year}: unexpected response type: {r2.headers.get('Content-Type')}")
        # Save HTML for debugging
        with open(os.path.join(RAW_DIR, f"pa_taxes_{year}_debug.html"), "w") as f:
            f.write(r2.text[:5000])
        return None


def parse_xls(filepath, year):
    """Extract EIT rates from a single year's XLS file."""
    try:
        df = pd.read_excel(filepath, engine="xlrd")
    except Exception as e:
        print(f"  Error reading {filepath}: {e}")
        return None

    # Normalize column names
    df.columns = [str(c).strip().lower().replace(" ", "_") for c in df.columns]

    # Look for EIT rows in the tax_name column
    tax_col = None
    for col in df.columns:
        if "tax" in col and "name" in col:
            tax_col = col
            break

    if tax_col is None:
        print(f"  No tax_name column found in {filepath}")
        print(f"  Columns: {list(df.columns)}")
        return None

    # Filter to EIT rows
    eit_mask = df[tax_col].astype(str).str.contains("EIT|Earned Income", case=False, na=False)
    eit_df = df[eit_mask].copy()

    if len(eit_df) == 0:
        print(f"  No EIT rows found for {year}")
        return None

    # Extract key columns (names vary by year; try common patterns)
    result_cols = {}
    for col in df.columns:
        cl = col.lower()
        if "county" in cl and "id" not in cl:
            result_cols["county"] = col
        elif "municipality" == cl or ("municipality" in cl and "id" not in cl and "tax" not in cl):
            result_cols["municipality"] = col
        elif "municipality_id" in cl or "muni" in cl and "id" in cl:
            result_cols["municipality_id"] = col
        elif "municipal_tax_amount" in cl or ("municipal" in cl and "amount" in cl):
            result_cols["muni_rate"] = col
        elif "school_district_tax_amount" in cl or ("school" in cl and "amount" in cl):
            result_cols["sd_rate"] = col
        elif "total_tax_rate" in cl or ("total" in cl and "rate" in cl):
            result_cols["total_rate"] = col

    out = pd.DataFrame()

    # Year: use Reporting Year column from XLS if available, else from argument
    year_col = None
    for col in df.columns:
        if "reporting" in col.lower() and "year" in col.lower():
            year_col = col
            break
    if year_col and year_col in eit_df.columns:
        out["year"] = eit_df[year_col].values
    else:
        out["year"] = year

    if "county" in result_cols:
        out["county"] = eit_df[result_cols["county"]].values
    if "municipality" in result_cols:
        out["municipality"] = eit_df[result_cols["municipality"]].values
    if "municipality_id" in result_cols:
        out["municipality_id"] = eit_df[result_cols["municipality_id"]].values
    if "muni_rate" in result_cols:
        out["muni_eit_rate"] = pd.to_numeric(eit_df[result_cols["muni_rate"]].values, errors="coerce")
    if "sd_rate" in result_cols:
        out["sd_eit_rate"] = pd.to_numeric(eit_df[result_cols["sd_rate"]].values, errors="coerce")
    if "total_rate" in result_cols:
        out["total_eit_rate"] = pd.to_numeric(eit_df[result_cols["total_rate"]].values, errors="coerce")

    # Also check for resident/nonresident distinction in tax_name
    out["tax_type"] = eit_df[tax_col].values

    return out


def main():
    print("Downloading PA DCED municipal tax data...")
    session = requests.Session()

    all_years = []
    for year in YEARS:
        filepath = download_year(year, session)
        if filepath:
            parsed = parse_xls(filepath, year)
            if parsed is not None and len(parsed) > 0:
                all_years.append(parsed)
        time.sleep(3)  # Be polite

    if not all_years:
        print("No data collected!")
        return

    panel = pd.concat(all_years, ignore_index=True)

    # Pivot resident/nonresident into separate columns where possible
    # EIT rows may be labeled "EIT Resident" and "EIT Nonresident" separately
    panel["is_resident"] = panel["tax_type"].str.contains("Resident", case=False, na=False) & \
                           ~panel["tax_type"].str.contains("Non", case=False, na=False)
    panel["is_nonresident"] = panel["tax_type"].str.contains("Nonresident|Non-Resident", case=False, na=False)

    # Save full panel
    outpath = os.path.join(DATA_DIR, "pa_eit_rate_panel.csv")
    panel.to_csv(outpath, index=False)
    print(f"\nSaved {len(panel)} rows to {outpath}")

    # Summary
    print(f"Years: {panel['year'].min()} - {panel['year'].max()}")
    if "municipality" in panel.columns:
        print(f"Municipalities: {panel['municipality'].nunique()}")
    print(f"Tax types: {panel['tax_type'].unique()}")


if __name__ == "__main__":
    main()
