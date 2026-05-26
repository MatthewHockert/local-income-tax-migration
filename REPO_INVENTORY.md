# Repo Inventory (Phase 1) — replication scope for `UEA_draft.tex` and `muni_tax_sorting.tex`

**Branch:** `repo-cleanup`. **No files moved, edited, or deleted in this phase.**
This is an inventory only; classifications marked *(confirm P2)* are verified by the
end-to-end trace in Phase 2 before anything is touched.

## Method
The two target papers were scanned for every `\input{tables/…}` and
`\includegraphics{figures/…}`; that artifact set was traced backward to the
scripts that write it, then to the result CSVs, panels, and raw inputs those
scripts consume. A script is **on the replication path** if it (transitively)
produces an artifact either paper uses, or a number either paper states.

**Classification key:** **(c)** both papers · **(b)** `muni_tax_sorting` only ·
**(a)** `UEA_draft` only · **(d)** neither (deletion candidate). The two papers
are nearly identical on the EIT side; `muni_tax_sorting` adds the property-tax
leg + EIT-revenue result. No script is **(a)**-unique.

**Important scope note:** `main.tex` (the long 3-level paper) is *not* a
replication target but is kept (do-not-touch). Many **(d)** scripts feed
`main.tex` only; these are flagged **[feeds main.tex]** so removal is your call,
not an accident.

---

## Artifacts the two papers consume (the targets)
**Tables** (`paper/tables/`): `summary_muni`, `summary_block`, `summary_sd`,
`cell_counts_summary`, `act32_cousub`, `act47_robustness`, `headline_muni_combined`,
`pooled_per_pp_cousub`, `es_realized_cousub`, `es_pooled_cousub_summary`,
`app_full_muni_cell_results`.
**Figures** (`figures/`): `pa_map`, `rate_changes_by_year`, `nonres_coverage`,
`pooled_es_muni_only`, `es_realized_log_{live_and_work,in_commuters,total_jobs}`,
`es_realized_share_live_in`, `support_dose_trajectory_{muni_vs_sd,inc_becomes}`,
and `proptax_es_scale_vs_comp` (*muni_tax only*).

---

## KEPT SET — on the replication path

### Raw acquisition (never deletion candidates per the rules)
| Script | Class | Consumes | Produces |
|---|---|---|---|
| `scrape_pa_dced_eit.py` | c | DCED MunStats tax register | `data/pa_dced_raw/`, `pa_eit_rate_panel.csv` (EIT; also the property register) |
| `reparse_dced.R` | c | `pa_dced_raw/*.xls` | `pa_eit_rate_panel.csv` w/ SD id |
| `scrape_pa_dced_afr.py` | c | DCED AFR portal | `pa_afr_panel.csv` (muni fiscal controls) |
| `scrape_nces_sd_finance.py` | c | Census F-33 | `pa_sd_finance_raw.csv` (SD descriptive/controls) |
| `download_lodes.R` | c | LODES v8 | `data/lodes_block/` (block OD) |
| `scrape_pa_dced_county_tax.py` | b | DCED county tax register | `pa_county_tax_panel.csv` |
| `download_pde_retax.py` | b | PDE (Wayback + live) | `data/pde_retax_raw/` |
| `parse_clr_history.py` | b | PA DoR CLR PDF | `pa_clr_panel.csv`, reassessment dates |

### Panel builders
| Script | Class | Consumes | Produces |
|---|---|---|---|
| `build_data.R` | c | LODES, rate panel, AFR | `pa_panel.rds`, `pa_rate_panel.rds`, `pa_residence_panel.rds`, block→cousub xwalk |
| `build_block_panel.R` | c | LODES block, rates/shares | `pa_block_panel.rds` (backs `summary_block`) |
| `build_sd_controls.R` | c | F-33 | `pa_sd_controls.rds`, NCES↔DCED xwalk (backs `summary_sd`) |
| `build_pde_sd_millage.py` | b | `pde_retax_raw/` | `pde_sd_millage.csv` |
| `multitax_coverage_report.py` | b | `pa_dced_raw/` | coverage report + millage validation |
| `multitax_build_muni_panel.py` | b | register, xwalks | `pa_muni_multitax_panel.csv` |
| `multitax_build_sd_panel.py` | b | register | `pa_sd_millage_panel.csv` |
| `multitax_validate_pde.py` | b | register + PDE | year-mapping validation |
| `multitax_gapfill_sd.py` | b | SD panel + PDE | `pa_sd_millage_panel_filled.csv` |
| `multitax_effective_rate.py` | b | muni/SD/county millage + CLR | `pa_property_burden_panel.csv` |
| `multitax_acs_dollar_base.py` | b | Census ACS API | `pa_acs_dollar_base.csv` |
| `multitax_sales_tax.py` | b | (statutory constants) | `pa_local_sales_tax.csv` |
| `multitax_millage_profile.py` | b | muni panel | millage-variation diagnostic |
| `multitax_reassessment_inventory.py` | b | CLR + panel | `pa_county_reassessment.csv` |

### Estimation (EIT)
| Script | Class | Produces (consumed result CSV) |
|---|---|---|
| `q3_unified_full_table.R` | c | `headline_full_table.csv`, `headline_diagnostics.csv` (→ headline/app tables) |
| `q3_unified_headline_es.R` | c | `headline_es_att.csv`, `headline_es_coefs.csv` (→ `es_realized_cousub`, ES figs) |
| `q3_unified_pooled.R` | c | `pooled_results.csv`, `pooled_diagnostics.csv` (→ `pooled_per_pp_cousub`) *(confirm P2)* |
| `q3_unified_pooled_es.R` | c | `pooled_es_cousub_att.csv` (→ `es_pooled_cousub_summary`, `pooled_es_muni_only`) *(confirm P2)* |
| `q3_unified_act32_3level.R` | c | `act32_split_cousub.csv` (→ `act32_cousub`); also block/sd splits [feeds main.tex] |
| `q3_unified_act47_robustness.R` | c | `act47_robustness.tex` directly |
| `q3_unified_eit_revenue.R` | b | `eit_revenue_decomp.csv`, `eit_revenue_split.csv`, `eit_revenue_offset.csv` (muni_tax revenue result) |
| `paper_helpers.R` | c | shared helpers sourced by ES scripts |

### Estimation (property — muni_tax only)
| Script | Class | Produces |
|---|---|---|
| `q3_proptax_es.R` | b | property realized-dose ES → `proptax_es_*.csv`, `proptax_es_scale_vs_comp.png` |
| `q3_eit_es_decomp.R` | b | EIT scale/composition decomp (asymmetry check) → `eit_es_decomp_*.csv` |
| `q3_proptax_compare.R` | b | per-$1k EIT-vs-property comparison + honest bounds → `proptax_eit_comparison.csv` |

### Table / figure builders
| Script | Class | Produces (consumed) | Note |
|---|---|---|---|
| `build_paper_tables.R` | c | `act32_cousub`, `headline_muni_combined`, `pooled_per_pp_cousub`, `es_realized_cousub`, `es_pooled_cousub_summary`, `app_full_muni_cell_results` | **MONOLITH** — also reads block/SD/industry/tercile/altctrl/sa_binary CSVs to build main.tex tables; **must be trimmed in P3** if those estimation scripts are retired |
| `build_summary_stats.R` | c | `summary_{muni,block,sd}`, `cell_counts_summary`, `pa_map`, `rate_changes_by_year`, `nonres_coverage` | |
| `build_appendix_es_figures.R` | c | `es_realized_log_*` figures (dynamic names) | |
| `build_uea_aggregate_figures.R` | c | `pooled_es_muni_only`, `support_dose_trajectory_muni_vs_sd` | |
| `build_dose_trajectory_inc_becomes.R` | c | `support_dose_trajectory_inc_becomes` | |

### Orchestrator
| Script | Class | Note |
|---|---|---|
| `run_active.R` | c | master pipeline (gated flags); re-point/trim in P3 |

---

## (d) DELETION CANDIDATES — not on either target paper's path
*(moved to `_retired/` in Phase 4, never `rm`; your call to actually remove)*

### Block/SD as estimating units — [feeds main.tex]
`q3_unified_block_es.R`, `q3_unified_block_es_full.R`, `q3_unified_block_es_sd.R`,
`q3_unified_block_es_sd_full.R`, `q3_unified_block_joint.R`, `q3_unified_block_level.R`,
`q3_unified_sd_es.R`, `q3_unified_sd_level.R`, `q3_unified_pooled_es_block_only.R`,
`q3_unified_pooled_es_block_sd.R`, `q3_unified_pooled_es_sd_only.R`,
`q3_unified_pooled_sd_only.R`, `sd_heterogeneity.R`,
`build_block_es_overlay_figures.R`, `build_sa_binary_overlay_figures.R`,
`q3_unified_sa_binary_es.R` — block/SD-unit estimates the muni-only papers don't use.

### Industry decomposition — [feeds main.tex]
`q3_unified_industry.R`, `q3_unified_industry_sd.R`, `build_industry_figure.R`,
`build_industry_table.R`, `build_industry_panel.R` — neither target paper has an
industry section.

### 3-level robustness / other — [feeds main.tex]
`q3_unified_tercile_3level.R`, `q3_unified_altctrl_3level.R`,
`q3_unified_honestdid.R` (EIT honest-DiD; muni_tax's property honest bounds come
from `q3_proptax_compare.R`), `q3_unified_heterogeneity.R` *(confirm P2)*.

### Superseded by current pipeline
`q3_unified.R` (old 5-estimator), `q3_unified_act32.R` (old cousub Act-32),
`q3_unified_es_realized.R`, `q3_unified_compare.R`,
`q3_unified_property_tax.R` (old AFR-based property; superseded by multitax),
`q3_unified_revenue_returns.R` (duplicate of `q3_unified_eit_revenue.R`).

### ⚠ Clobber risk — verify before retiring (P2)
`q3_unified_becomes_marginal.R` — appears to **also write `headline_es_att.csv`**,
the canonical ES output; running it would overwrite the headline ES. Retire, but
confirm it is not the canonical producer first.

### Exploratory / legacy / one-off
`local_income_tax_migration.R` (monolith), `run_slide_models.R`, `run_dcdh.R`,
`run_paper_models.R`, `make_dcdh_es_figure.R`, `build_cdh_es_figures.R`,
`q3_cdh_inc_cells.R`, `q3_acr_dose_bins.R`, `q3_acr_dose_bins_pooledInc.R`,
`q3_unified_becomes_crossing.R`, `q3_unified_binding_decomp.R`,
`q3_unified_spillover.R`, `q3_unified_support_graphics.R`,
`q3_unified_spending_decomposition.R`, `q3_unified_robustness_passes.R`,
`q3_robustness.R`, `q3_stacked.R`, `q3_residence.R`, `q3_nonres.R`,
`pretreatment_balance.R`, `robustness.R`, `pilot_block_lodes.R`,
`build_residence_panel.R`, `build_neighbors_rings.R`, `build_highway_panel.R`,
`build_cell_homestack_nr_scatter.R`, `build_cell_rate_change_diagnostics.R`,
`build_design_timeline_figure.R`, `build_fiscal_controls_figure.R`,
`companion_outcomes.R`, `download_pde_afr.R`,
`tmp_robust_drop_rev_controls.R` (explicitly a temp file).

---

## DATA FILES
**Never deletion candidates (raw/scraped/expensive — per the rules):**
`data/pa_dced_raw/` (DCED register), `data/lodes_block/` (~4 GB), `data/lodes/`,
`data/pde_retax_raw/`, `data/clr_raw/`, `data/pa_county_raw/`, `data/pa_afr_raw/`,
`data/pa_sd_afr_raw/`, `data/pde_afr/`, and the assembled panels
(`pa_panel.rds`, `pa_rate_panel.rds`, `pa_block_panel.rds`,
`data/multitax/*` panels). Most raw dirs are already gitignored.

**Derived result CSVs (`data/q3_unified/`, `data/multitax/`):** regenerated by the
kept estimation scripts; kept (they back the tables). A subset
(`block_*`, `sd_*`, `industry_*`, `tercile_*`, `altctrl_*`, `sa_binary_*`,
`pooled_es_block*`) is consumed only by main.tex tables — *(confirm P2 whether
trimming `build_paper_tables.R` lets these be retired)*.

**Likely-stale / non-target data (review in P4, not auto-retired):**
`oh_*` (Ohio — dropped from the paper), `pa_highway_*`, `pa_neighbors*`,
`pa_pair_panel.rds`, `pa_tract_*`, `pa_industry_panel.rds`,
`tax_foundation_*`, `state_local_tax_landscape.*`, `data/crime_data/`,
`data/irs_soi/`. These map to (d) scripts above.

---

## Open items to resolve in Phase 2 (trace + verify)
1. Confirm producers of `pooled_results.csv` / `pooled_es_cousub_att.csv`
   (`q3_unified_pooled.R` vs `q3_unified_pooled_es.R`).
2. Confirm `q3_unified_becomes_marginal.R` does **not** clobber the canonical
   `headline_es_att.csv`.
3. Determine whether `build_paper_tables.R` errors if block/SD/industry CSVs are
   absent (it reads them) — decides whether it needs trimming before those
   scripts can be retired.
4. Run the full kept-set path end-to-end and confirm it reproduces the canonical
   cited numbers (Inc-bind −0.573, pre-trend Wald p=0.33, dose 0.24, revenue
   +1.06/36%, property per-$1k comparison) **before** any (d) script is moved.
