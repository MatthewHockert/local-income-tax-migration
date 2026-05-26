"""
multitax_gapfill_sd.py
=====================

Merge PDE nominal SD millage into the register SD panel:
  (1) fill the `not_observed` register gaps with PDE values (source='PDE');
  (2) add the PDE value alongside every register-observed cell and flag
      register/PDE mismatches (incl. the 112 register scale/data-entry errors
      the cross-validation surfaced) WITHOUT silently overwriting the register.

Year mapping: B_start (PDE school year YYYY-YY -> calendar YYYY), chosen
empirically in multitax_validate_pde.py (90.5% within 1 mill, stable across
2002-2019; this OVERRODE PDE's own "CY" label, which would imply YYYY+1).

PDE gives one district "Real Estate Mills" (General Fund Budget) = the full SD
real-estate levy; for SDs re_gp ~= re_total, so PDE fills both. Multi-county
PDE districts are aggregated by max (matching the register's SD rule); their
millage dispersion is large (STEB equalization), so PDE-filled multi-county
cells are tagged lower-confidence.

Output: data/multitax/pa_sd_millage_panel_filled.csv
  Adds, for re_gp_mills_sd and re_total_mills_sd:
    <col>            : register value, or PDE value where register was not_observed
    <col>_flag       : levied / not_levied / not_observed  (post-fill)
    <col>_source     : DCED / PDE / ''     (provenance)
    <col>_pde        : the aligned PDE value (for comparison), where available
  Plus:
    re_gp_qc         : '', 'dced_pde_mismatch' (|diff|>1), or 'scale_error'
    pde_multi_county : PDE district spans >1 county (filled value less precise)
"""

import os
import numpy as np
import pandas as pd

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data", "multitax")
MISMATCH = 1.0  # mills


def main():
    reg = pd.read_csv(os.path.join(OUT, "pa_sd_millage_panel.csv"))
    pde = pd.read_csv(os.path.join(OUT, "pde_sd_millage.csv"))
    pde = pde.dropna(subset=["sd_id"]).copy()
    pde["year"] = pde["school_year"].str[:4].astype(int)          # B_start
    pde["sd_id"] = pde["sd_id"].astype(int)
    # one PDE row per (sd_id, year): max mills (matches register SD rule)
    pde_y = (pde.sort_values("mills")
             .groupby(["sd_id", "year"])
             .agg(pde_mills=("mills", "max"),
                  pde_multi_county=("n_counties", lambda s: (s > 1).any()))
             .reset_index())

    out = reg.merge(pde_y, on=["sd_id", "year"], how="left")

    for col in ("re_gp_mills_sd", "re_total_mills_sd"):
        out[col + "_pde"] = out["pde_mills"]
        gap = (out[col + "_flag"] == "not_observed") & out["pde_mills"].notna()
        out.loc[gap, col] = out.loc[gap, "pde_mills"]
        out.loc[gap, col + "_flag"] = "levied"
        out.loc[gap, col + "_source"] = "PDE"

    # QC on register-observed cells: flag mismatches / scale errors (don't overwrite)
    d = (out["re_gp_mills_sd"] - out["pde_mills"]).abs()
    ratio = out["re_gp_mills_sd"] / out["pde_mills"]
    observed_dced = out["re_gp_mills_sd_source"] == "DCED"
    out["re_gp_qc"] = ""
    mismatch = observed_dced & out["pde_mills"].notna() & (d > MISMATCH)
    out.loc[mismatch, "re_gp_qc"] = "dced_pde_mismatch"
    scale = mismatch & (ratio.round(2).isin([0.1, 0.01, 10.0, 100.0]) | (d > 50))
    out.loc[scale, "re_gp_qc"] = "scale_error"
    out["pde_multi_county"] = out["pde_multi_county"].fillna(False)
    out.drop(columns=["pde_mills"], inplace=True)

    path = os.path.join(OUT, "pa_sd_millage_panel_filled.csv")
    out.to_csv(path, index=False)

    # report
    n = len(out)
    print(f"Wrote {path}  ({n:,} SD-years)")
    for col in ("re_gp_mills_sd", "re_total_mills_sd"):
        src = out[col + "_source"].value_counts().to_dict()
        filled = src.get("PDE", 0)
        still = (out[col + "_flag"] == "not_observed").sum()
        print(f"  {col}: DCED={src.get('DCED',0):,}  PDE-filled={filled:,}  still not_observed={still:,}")
    print(f"\nQC on register-observed re_gp: "
          f"{(out.re_gp_qc=='dced_pde_mismatch').sum()} mismatches (>1 mill), "
          f"{(out.re_gp_qc=='scale_error').sum()} likely register scale errors (flagged, not overwritten).")
    print(f"PDE-filled cells that are multi-county (lower precision): "
          f"{((out.re_gp_mills_sd_source=='PDE') & out.pde_multi_county).sum()}")
    obs = out[out.re_gp_mills_sd_flag == 'levied']
    print(f"\nFINAL re_gp_mills_sd coverage: {len(obs):,}/{n:,} SD-years observed "
          f"({100*len(obs)/n:.1f}%); was {100*(reg.re_gp_mills_sd_flag=='levied').mean():.1f}% register-only.")


if __name__ == "__main__":
    main()
