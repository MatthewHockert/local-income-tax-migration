"""
scrape_pa_dced_county_tax.py
===========================

County-government tax register from DCED MunStats (the county analog of the
muni `taxes_Dyn_Excel` register used in scrape_pa_dced_eit.py). Source:
  https://apps.dced.pa.gov/munstats-public/ReportInformation2.aspx?report=CountyTaxSummary_Dyn_Excel

Each year returns one .xls with all 66 counties (Philadelphia is a consolidated
city-county and reports on the muni side). Carries county REAL ESTATE MILLAGE
(General Purpose + Total + special-purpose), plus hotel, per-capita, and the
Allegheny local sales tax. This completes the property-tax burden layer
(muni + SD + COUNTY) needed for the multi-tax paper.

Usage: python scrape_pa_dced_county_tax.py
Outputs: data/pa_county_raw/pa_county_taxes_YYYY.xls
         data/multitax/pa_county_tax_panel.csv  (county-year, RE millage + others)
"""

import os
import re
import io
import time
import warnings
import requests
import numpy as np
import pandas as pd

warnings.filterwarnings("ignore")
PROJECT_DIR = os.path.dirname(os.path.abspath(__file__))
RAW = os.path.join(PROJECT_DIR, "data", "pa_county_raw")
OUT = os.path.join(PROJECT_DIR, "data", "multitax")
os.makedirs(RAW, exist_ok=True)
URL = ("https://apps.dced.pa.gov/munstats-public/ReportInformation2.aspx"
       "?report=CountyTaxSummary_Dyn_Excel")
YEARS = range(2002, 2020)


def tokens(html):
    g = lambda n: (re.search(rf'id="{n}" value="([^"]*)"', html) or [None, ""])[1] \
        if re.search(rf'id="{n}" value="([^"]*)"', html) else ""
    return {k: g(k) for k in ("__VIEWSTATE", "__VIEWSTATEGENERATOR", "__EVENTVALIDATION")}


def download(year, s):
    out = os.path.join(RAW, f"pa_county_taxes_{year}.xls")
    if os.path.exists(out) and os.path.getsize(out) > 10000:
        return out
    r = s.get(URL, verify=False, timeout=60)
    payload = {**tokens(r.text),
               "ctl00$ContentPlaceHolder1$ddreporting_year": str(year),
               "ctl00$ContentPlaceHolder1$btnDisplay": "Display Report"}
    r2 = s.post(URL, data=payload, verify=False, timeout=120)
    if "excel" in r2.headers.get("Content-Type", "").lower():
        open(out, "wb").write(r2.content)
        print(f"  {year}: {len(r2.content)/1e3:.0f} KB")
        return out
    print(f"  {year}: FAILED ({r2.headers.get('Content-Type')})")
    return None


def parse(path, year):
    df = pd.read_excel(path, engine="xlrd")
    df.columns = [str(c).strip() for c in df.columns]
    df["tn"] = df["Tax Name"].astype(str).str.strip().str.lower()
    df["amt"] = pd.to_numeric(df["Tax Amount"], errors="coerce")
    df["county"] = df["County"].astype(str).str.upper().str.strip()

    def pick(pred, agg="max"):
        sub = df[df.tn.map(pred)]
        if not len(sub):
            return pd.Series(dtype=float)
        if agg == "sum":  # dedupe per (county, levy) then sum
            return (sub.groupby(["county", "Tax Name"])["amt"].max()
                    .groupby("county").sum(min_count=1))
        return sub.groupby("county")["amt"].max()

    counties = sorted(df["county"].dropna().unique())
    out = pd.DataFrame({"county": counties, "year": year}).set_index("county")
    out["cty_re_gp_mills"] = pick(lambda t: "general purpose" in t and "mill" in t)
    out["cty_re_total_mills"] = pick(lambda t: "real estate total" in t and "mill" in t)
    # fallback total = sum of all RE/named mill levies if the explicit total is missing
    summed = pick(lambda t: "mill" in t and "total" not in t, "sum")
    out["cty_re_total_mills"] = out["cty_re_total_mills"].fillna(summed)
    out["cty_hotel_pct"] = pick(lambda t: "hotel" in t)
    out["cty_percap_dollars"] = pick(lambda t: t == "per capita (dollars)")
    out["cty_sales_tax_pct"] = pick(lambda t: "local sales tax" in t)
    return out.reset_index()


def main():
    s = requests.Session()
    print("Scraping DCED county tax register...")
    frames = []
    for y in YEARS:
        f = download(y, s)
        if f:
            frames.append(parse(f, y))
        time.sleep(2)
    panel = pd.concat(frames, ignore_index=True)
    panel = panel.sort_values(["county", "year"])
    panel.to_csv(os.path.join(OUT, "pa_county_tax_panel.csv"), index=False)
    print(f"\nWrote pa_county_tax_panel.csv: {len(panel):,} county-years, "
          f"{panel.county.nunique()} counties, {panel.year.min()}-{panel.year.max()}")
    print(f"  cty_re_gp_mills present: {panel.cty_re_gp_mills.notna().mean()*100:.0f}% | "
          f"median {panel.cty_re_gp_mills.median():.2f} mills")
    print(f"  counties w/ local sales tax: "
          f"{sorted(panel.loc[panel.cty_sales_tax_pct>0,'county'].unique())}")


if __name__ == "__main__":
    main()
