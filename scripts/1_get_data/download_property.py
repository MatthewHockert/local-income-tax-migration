"""
download_property.py
Downloads the raw property-tax files: the DCED county tax register, the state
education department's school-district millage, and the PA Revenue common
level ratios. Anything already downloaded is skipped.

Run: ~/packaging-test/pkg-test/bin/python scripts/1_get_data/download_property.py
"""

import os
import re
import time
import subprocess
import warnings
import requests

warnings.filterwarnings("ignore")
# repo root is two folders up (scripts/1_get_data/<this file>)
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
RAW = os.path.join(ROOT, "data", "raw")


# County tax register (DCED MunStats) ----------------------------------------
COUNTY_DIR = os.path.join(RAW, "pa_county_raw")
COUNTY_URL = ("https://apps.dced.pa.gov/munstats-public/ReportInformation2.aspx"
              "?report=CountyTaxSummary_Dyn_Excel")
COUNTY_YEARS = range(2002, 2020)


def tokens(html):
    g = lambda n: (re.search(rf'id="{n}" value="([^"]*)"', html) or [None, ""])[1] \
        if re.search(rf'id="{n}" value="([^"]*)"', html) else ""
    return {k: g(k) for k in ("__VIEWSTATE", "__VIEWSTATEGENERATOR", "__EVENTVALIDATION")}


def download_county(year, s):
    out = os.path.join(COUNTY_DIR, f"pa_county_taxes_{year}.xls")
    if os.path.exists(out) and os.path.getsize(out) > 10000:
        return "exists"
    r = s.get(COUNTY_URL, verify=False, timeout=60)
    payload = {**tokens(r.text),
               "ctl00$ContentPlaceHolder1$ddreporting_year": str(year),
               "ctl00$ContentPlaceHolder1$btnDisplay": "Display Report"}
    r2 = s.post(COUNTY_URL, data=payload, verify=False, timeout=120)
    if "excel" in r2.headers.get("Content-Type", "").lower():
        open(out, "wb").write(r2.content)
        time.sleep(2)
        return "downloaded"
    return "FAILED"


# PDE real estate tax rates, by school year ----------------------------------
# The older years (2002-03 to 2015-16) only survive on the Wayback Machine
PDE_DIR = os.path.join(RAW, "pde_retax_raw")
WB = "http://web.archive.org/web/{ts}id_/{orig}"
WB_BASE = ("http://www.education.pa.gov/Documents/Teachers-Administrators/"
           "School%20Finances/Finances/Financial%20Data%20Elements/"
           "Real%20Estate%20Tax%20Rates/")
WAYBACK = {
    "2002-03": ("20151127050735", "xlsx", "2002-03%20Real%20Estate%20Tax%20Rates.xlsx"),
    "2003-04": ("20151127014805", "xls",  "2003-04%20Real%20Estate%20Tax%20Rates.xls"),
    "2004-05": ("20151127081234", "xls",  "2004-05%20Real%20Estate%20Tax%20Rates.xls"),
    "2005-06": ("20151127045959", "xls",  "2005-06%20Real%20Estate%20Tax%20Rates.xls"),
    "2006-07": ("20151127075250", "xls",  "2006-07%20Real%20Estate%20Tax%20Rates.xls"),
    "2007-08": ("20151126231108", "xls",  "2007-08%20Real%20Estate%20Tax%20Rates.xls"),
    "2008-09": ("20151127063147", "xls",  "2008-09%20Real%20Estate%20Tax%20Rates.xls"),
    "2009-10": ("20151127081249", "xlsx", "2009-10%20Real%20Estate%20Tax%20Rates.xlsx"),
    "2010-11": ("20151127065431", "xlsx", "2010-11%20Real%20Estate%20Tax%20Rates.xlsx"),
    "2011-12": ("20151127035034", "xlsx", "2011-12%20Real%20Estate%20Tax%20Rates.xlsx"),
    "2012-13": ("20151127005714", "xlsx", "2012-13%20Real%20Estate%20Tax%20Rates.xlsx"),
    "2013-14": ("20151127055126", "xlsx", "2013-24%20Real%20Estate%20Tax%20Rates.xlsx"),  # PDE's typo, not ours
    "2014-15": ("20151127020458", "xlsx", "Finances%20RETaxRates%202014-2015.xlsx"),
    "2015-16": ("20151127081251", "xlsx", "Finances%20RETaxRates%202015-2016.xlsx"),
}
PDE_LIVE = ("https://www.pa.gov/content/dam/copapwp-pagov/en/education/documents/"
            "schools/grants-and-funding/school-finances/financial-data-elements/"
            "real-estate-tax-rates/real%20estate%20tax%20rates%20{sy}.xlsx")
PDE_LIVE_YEARS = ["2016-17", "2017-18", "2018-19", "2019-20"]


def fetch(url, out, min_bytes):
    if os.path.exists(out) and os.path.getsize(out) > min_bytes:
        return "exists"
    r = subprocess.run(["curl", "-s", "-L", "-m", "120", url, "-o", out])
    ok = r.returncode == 0 and os.path.exists(out) and os.path.getsize(out) > min_bytes
    time.sleep(1)
    return "downloaded" if ok else "FAILED"


# Common level ratio factors (PA Department of Revenue) ----------------------
CLR_DIR = os.path.join(RAW, "clr_raw")
CLR_URL = ("https://www.pa.gov/content/dam/copapwp-pagov/en/revenue/documents/"
           "taxtypes/rtt/documents/clr_factor_historical.pdf")


def main():
    for d in (COUNTY_DIR, PDE_DIR, CLR_DIR):
        os.makedirs(d, exist_ok=True)
    status = []

    s = requests.Session()
    for y in COUNTY_YEARS:
        status.append((f"county {y}", download_county(y, s)))

    for sy, (ts, ext, fname) in WAYBACK.items():
        out = os.path.join(PDE_DIR, f"pde_{sy}.{ext}")
        status.append((f"PDE {sy}", fetch(WB.format(ts=ts, orig=WB_BASE + fname), out, 5000)))
    for sy in PDE_LIVE_YEARS:
        out = os.path.join(PDE_DIR, f"pde_{sy}.xlsx")
        status.append((f"PDE {sy}", fetch(PDE_LIVE.format(sy=sy), out, 5000)))

    out = os.path.join(CLR_DIR, "clr_factor_historical.pdf")
    status.append(("CLR pdf", fetch(CLR_URL, out, 100000)))

    counts = {}
    for _, st in status:
        counts[st] = counts.get(st, 0) + 1
    print(f"{len(status)} files: {counts}")
    for name, st in status:
        if st == "FAILED":
            print("  FAILED:", name)


if __name__ == "__main__":
    main()
