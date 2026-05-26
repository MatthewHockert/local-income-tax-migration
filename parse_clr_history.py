"""
parse_clr_history.py
===================

Parse the PA Dept of Revenue historical Common Level Ratio (CLR) factor
document into a county-year CLR panel, for building EFFECTIVE property-tax
rates (statutory millage is levied on ASSESSED value; CLR = assessed/market
converts it to an effective rate on MARKET value: eff_rate = mills/1000 * CLR).

Source PDF (one page per county, CLR factors 1986-2026):
  https://www.pa.gov/.../rtt/documents/clr_factor_historical.pdf
The published "factor" is the reciprocal of the CLR (factor = market/assessed),
so CLR = 1/factor. Footnotes ("Adjusted ... assessment base change effective
<date>") mark countywide REASSESSMENTS -> we extract them as an independent
cross-check on the published reassessment inventory, with exact effective years.

YEAR ALIGNMENT is deliberately NOT hard-coded here: we key the factor by its
acceptance-period START year and decide the map to the millage tax year
empirically downstream (a reassessment year should show CLR -> ~100%).

Requires: pdftotext (poppler). Outputs (data/multitax/):
  pa_clr_panel.csv            county, accept_start_year, clr_factor, clr
  pa_reassessment_from_clr.csv county, reassessment_years (from footnotes)
"""

import os
import re
import subprocess
import pandas as pd

PROJECT_DIR = os.path.dirname(os.path.abspath(__file__))
RAW = os.path.join(PROJECT_DIR, "data", "clr_raw")
OUT = os.path.join(PROJECT_DIR, "data", "multitax")
os.makedirs(RAW, exist_ok=True)
PDF_URL = ("https://www.pa.gov/content/dam/copapwp-pagov/en/revenue/documents/"
           "taxtypes/rtt/documents/clr_factor_historical.pdf")
PDF = os.path.join(RAW, "clr_factor_historical.pdf")
TRIPLE = re.compile(r"(\d{1,2})-(\d{1,2})-(\d{4})\s+\d{1,2}-\d{1,2}-\d{4}\s+(\d*\.\d+)")


def ensure_pdf():
    if not (os.path.exists(PDF) and os.path.getsize(PDF) > 100000):
        subprocess.run(["curl", "-s", "-L", "-m", "120", PDF_URL, "-o", PDF], check=True)
    return PDF


def main():
    ensure_pdf()
    txt = subprocess.run(["pdftotext", "-layout", PDF, "-"],
                         capture_output=True, text=True).stdout
    pages = txt.split("\f")
    rows, reassess = [], []
    for pg in pages:
        m = re.search(r"FOR\s*\n\s*([A-Z][A-Z .'-]+?)\s+COUNTY", pg) \
            or re.search(r"\n\s*([A-Z][A-Z .'-]+?)\s+COUNTY\s*\n", pg)
        if not m:
            continue
        county = re.sub(r"\s+", " ", m.group(1)).strip().upper()
        for mm, dd, yyyy, factor in TRIPLE.findall(pg):
            f = float(factor)
            rows.append(dict(county=county, accept_start_year=int(yyyy),
                             accept_start_month=int(mm), clr_factor=f,
                             clr=round(1.0 / f, 4) if f else None))
        for yr in re.findall(r"assessment base change effective\s+\w+\s+\d{1,2},?\s*(\d{4})", pg):
            reassess.append(dict(county=county, reassessment_year=int(yr)))

    clr = pd.DataFrame(rows)
    # one factor per (county, acceptance-start-year): prefer the 7/1 (mid-year) row
    clr = (clr.sort_values("accept_start_month")
           .groupby(["county", "accept_start_year"], as_index=False).last())
    clr = clr[["county", "accept_start_year", "clr_factor", "clr"]].sort_values(
        ["county", "accept_start_year"])
    clr.to_csv(os.path.join(OUT, "pa_clr_panel.csv"), index=False)

    re_df = pd.DataFrame(reassess).drop_duplicates().sort_values(["county", "reassessment_year"])
    re_df.to_csv(os.path.join(OUT, "pa_reassessment_from_clr.csv"), index=False)

    print(f"CLR panel: {len(clr):,} county-years, {clr.county.nunique()} counties, "
          f"years {clr.accept_start_year.min()}-{clr.accept_start_year.max()}")
    print(f"  CLR sanity (assessed/market): median {clr.clr.median():.3f}, "
          f"range {clr.clr.min():.3f}-{clr.clr.max():.3f}")

    # cross-check footnote reassessments vs the published inventory, in-panel
    inv = pd.read_csv(os.path.join(OUT, "pa_county_reassessment.csv"))
    inv_inpanel = set(zip(inv.loc[inv.reassessed_in_panel, "county"],
                          inv.loc[inv.reassessed_in_panel, "in_panel_year"].astype(int)))
    clr_inpanel = set((c, y) for c, y in zip(re_df.county, re_df.reassessment_year)
                      if 2002 <= y <= 2019)
    print(f"\nReassessments from CLR footnotes (in-panel): {len(clr_inpanel)}")
    print(f"  agree with published inventory (county+/-1yr): "
          f"{sum(any(c==ic and abs(y-iy)<=1 for ic,iy in inv_inpanel) for c,y in clr_inpanel)}"
          f" / {len(clr_inpanel)}")
    only_clr = sorted(clr_inpanel - {(c,y) for c in inv.county for y in range(2002,2020)
                                     if (c,y) in clr_inpanel and (c,y) in inv_inpanel})
    print(f"  CLR-footnote reassessments (county, year): {sorted(clr_inpanel)}")


if __name__ == "__main__":
    main()
