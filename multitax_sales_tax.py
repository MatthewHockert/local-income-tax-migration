"""
multitax_sales_tax.py
====================

PA LOCAL SALES & USE TAX — hard-coded statutory constants (NO scraper).

Pennsylvania authorizes a local sales tax in EXACTLY TWO jurisdictions; no
other municipality or county levies one. So this is a fixed reference table,
not a data acquisition:

  * Allegheny County (FIPS 42003): 1% local sales & use tax, enacted by Act 77
    of 1993 (the Allegheny Regional Asset District), effective 1 March 1994.
    Constant at 1% across 2002-2019.
  * Philadelphia (FIPS 42101): 1% local sales & use tax since 1991; INCREASED
    to 2% effective 8 October 2009 (Act 48 of 2009). So 2002-2009 = 1%,
    2010-2019 = 2%. (2009 is coded 1% as the rate in force for most of the year;
    the mid-year change is noted.)

The time-accurate series is encoded here (rather than a flat 2% for Philly)
because the panel spans the 2009 increase. A future reader who only needs the
current headline rate can read Allegheny 1% / Philadelphia 2% off 2019.

Output: data/multitax/pa_local_sales_tax.csv
  county_fips, county, year, local_sales_tax_rate, statute, note
"""

import os
import pandas as pd

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data", "multitax")
YEARS = range(2002, 2020)


def main():
    rows = []
    for y in YEARS:
        rows.append(dict(county_fips="42003", county="ALLEGHENY", year=y,
                         local_sales_tax_rate=0.01,
                         statute="Act 77 of 1993 (Regional Asset District), eff. 1994-03-01",
                         note="constant 1% 2002-2019"))
        phila = 0.02 if y >= 2010 else 0.01
        rows.append(dict(county_fips="42101", county="PHILADELPHIA", year=y,
                         local_sales_tax_rate=phila,
                         statute="1% since 1991; +1% via Act 48 of 2009, eff. 2009-10-08",
                         note=("2% from 2010" if y >= 2010 else
                               "1% (pre-2009-Oct increase)")))
    df = pd.DataFrame(rows)
    path = os.path.join(OUT, "pa_local_sales_tax.csv")
    df.to_csv(path, index=False)
    print(f"Wrote {path}  ({len(df)} county-years)")
    print("PA local sales tax exists ONLY in Allegheny (1%) and Philadelphia "
          "(1%->2% in 2009). All other PA jurisdictions: none.")
    print(df.pivot(index="year", columns="county", values="local_sales_tax_rate").to_string())


if __name__ == "__main__":
    main()
