"""
download_pde_retax.py
====================

Download the PDE "Real Estate Tax Rates" files (per school year), the source
for backfilling SD-year nominal millage gaps in pa_sd_millage_panel.csv.

PROVENANCE / REPRODUCIBILITY
  - 2002-03 .. 2015-16: recovered from the Internet Archive Wayback Machine
    (the legacy education.pa.gov SharePoint download links; snapshots ~2015,
    HTTP 200). Wayback `id_` URLs return the original unmodified .xls/.xlsx.
  - 2016-17 .. 2019-20: live pa.gov DAM (current site only hosts 2016-17+).
  Manifest below was discovered via the Wayback CDX API + live HEAD probes
  (2026-05-24). NOTE the PDE filename for 2013-14 is the typo "2013-24".

Output: data/pde_retax_raw/pde_<schoolyear>.<ext>
"""

import os
import time
import subprocess

PROJECT_DIR = os.path.dirname(os.path.abspath(__file__))
RAW = os.path.join(PROJECT_DIR, "data", "pde_retax_raw")
os.makedirs(RAW, exist_ok=True)

WB = "http://web.archive.org/web/{ts}id_/{orig}"
LIVE = ("https://www.pa.gov/content/dam/copapwp-pagov/en/education/documents/"
        "schools/grants-and-funding/school-finances/financial-data-elements/"
        "real-estate-tax-rates/real%20estate%20tax%20rates%20{sy}.xlsx")

# school_year -> (kind, ext, url)
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
    "2013-14": ("20151127055126", "xlsx", "2013-24%20Real%20Estate%20Tax%20Rates.xlsx"),  # PDE typo
    "2014-15": ("20151127020458", "xlsx", "Finances%20RETaxRates%202014-2015.xlsx"),
    "2015-16": ("20151127081251", "xlsx", "Finances%20RETaxRates%202015-2016.xlsx"),
}
WB_BASE = ("http://www.education.pa.gov/Documents/Teachers-Administrators/"
           "School%20Finances/Finances/Financial%20Data%20Elements/"
           "Real%20Estate%20Tax%20Rates/")
LIVE_YEARS = ["2016-17", "2017-18", "2018-19", "2019-20"]


def fetch(url, out):
    if os.path.exists(out) and os.path.getsize(out) > 5000:
        print(f"  exists, skip: {os.path.basename(out)}")
        return True
    r = subprocess.run(["curl", "-s", "-L", "-m", "90", url, "-o", out])
    ok = r.returncode == 0 and os.path.exists(out) and os.path.getsize(out) > 5000
    print(f"  {'ok ' if ok else 'FAIL'} {os.path.basename(out)} "
          f"({os.path.getsize(out) if os.path.exists(out) else 0} bytes)")
    return ok


def main():
    for sy, (ts, ext, fname) in WAYBACK.items():
        url = WB.format(ts=ts, orig=WB_BASE + fname)
        fetch(url, os.path.join(RAW, f"pde_{sy}.{ext}"))
        time.sleep(1)
    for sy in LIVE_YEARS:
        fetch(LIVE.format(sy=sy), os.path.join(RAW, f"pde_{sy}.xlsx"))
        time.sleep(1)
    print("\nDownloaded files:")
    for f in sorted(os.listdir(RAW)):
        print("  ", f, os.path.getsize(os.path.join(RAW, f)), "bytes")


if __name__ == "__main__":
    main()
