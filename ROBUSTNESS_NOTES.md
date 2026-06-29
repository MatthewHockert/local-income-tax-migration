# Robustness / exploratory estimations (for joint review)

These scripts are **not part of the paper's canonical pipeline** (that's documented in
`../README.md` §3 and §5). They are the alternative specifications and diagnostics built
while deciding the main model. Kept here so we can review them together. Each sources the
canonical panel build (`q3_unified_full_table.R`) and writes to `../data/q3_unified/`.

All paths are package-relative (they source `config.R` like the canonical scripts), so they
run from the package root with `Rscript scripts/<name>.R`. One extra dependency:
`q3_proptax_compare.R` needs the **`HonestDiD`** R package (`install.packages("HonestDiD")`).
Several scripts also read `../data/multitax/` (the property-tax burden panels, vendored here).

## Levels vs Δτ (point estimates)
- **`q3_unified_levels_robustness.R`** — runs the aggregate, decomposition, and an ES in BOTH
  level (τ) and dose (Δτ) form. **Finding:** under `muni×stack` FE the level and dose point
  estimates are *identical* (baseline is FE-absorbed); Inc-bind = −0.573 either way. The ES
  section is where they diverge (see below).

## Property tax as controls (instead of revenue/expenditure shares)
- **`q3_unified_eit_levels_ptax_controls.R`** — headline decomposition in levels, with
  `share_eit` + `share_re` replaced by city/SD/county property-tax **effective-rate levels**
  (keeping police/streets/debt/intergov). **Finding:** Inc-bind log-LW moves −0.573 → **−0.435**
  (wider SE, drops to one star — EIT and property-tax movements are mildly collinear); the
  composition margin and Inc-becomes are stable. Output: `eit_levels_ptax_controls_table.csv`.
- **`q3_proptax_layer_split.R`** — decomposes the property-tax dose into muni / SD / county
  layers. **Finding:** on the clean margins all three layers are individually null; the only
  significant layer (muni, on the residence counts) sits on pre-trend-contaminated margins and
  near-zero dose. Output: `proptax_layer_split.csv`.

## Event-study variants (why the paper uses the realized-dose ES)
- **`q3_unified_eit_levels_ptax_es.R`** — the realized-**dose** ES with property controls
  (the paper's ES form). Placebo behaves: Inc-bind post ≈ −0.67, Inc-nobind ≈ 0 (null).
- **`q3_unified_eit_levels_es.R`** — the **level-form** ES `i(rel, τ)`. **DO NOT use as the
  main ES:** it makes the Inc-nobind *placebo* come out **significant** (≈ −0.17) and equal to
  Inc-bind, because the level tag is dominated by the baseline cross-section (common across
  cells). Clean-looking pre-trends here are the symptom of that dilution, not a virtue.
  Output: `eit_levels_es_coefs.csv`, `../figures/eit_levels_es.png`.
  - The decisive contrast (log work in own muni): level-ES placebo = −0.168\*\*\* (significant,
    wrong); realized-dose-ES placebo = +0.065 (null, correct). This is why the event study
    stays in dose form even though the headline is in levels.

## Property-tax leg (the earlier two-tax comparison — not in the current paper)
- **`q3_proptax_es.R`** — property-tax realized-dose ES (treatment = muni+SD+county statutory
  millage, policy-only/CLR-stripped). The make-or-break identification check for that leg.
- **`q3_proptax_compare.R`** — per-$1,000-of-tax-bill comparison of EIT vs property tax +
  Rambachan–Roth honest-DiD bounds on the contaminated residence margins. Output:
  `proptax_eit_comparison.csv`. (This is the engine behind the retired two-tax-comparison
  version of the paper.)

## ACR aggregation: non-negative weights + dose linearity (backs `subsec:rob-acr`)
The headline β is an **average causal response (ACR)**: the dose-weighted average per-pp
effect. These scripts check that the *linear* aggregation isn't distorted by negative weights
or by curvature in the dose-response.
- **`q3_unified_dcdh_robustness.R`** — de Chaisemartin–D'Haultfœuille `did_multiplegt_dyn`
  (continuous treatment, non-negative weights) vs the linear feols ACR. **Finding:** the ES
  path is nearly identical in sign and magnitude → the linear ACR is not flipped by negative
  weights. Output: `dcdh_robustness.csv`, `../figures/dcdh_vs_feols_es.png`.
- **`q3_unified_curvature_robustness.R`** — does the marginal effect (ACRT) bend with dose?
  Part A: feols quadratic ACRT(d) curve. Part C: ACRT(d) matrix (outcome × cell, deg 2 vs 3).
  Part B: dCDH `continuous=1` vs `=2`. **Finding:** the *aggregate* bends slightly (a
  composition effect across cells), but within the headline **Inc-bind** cell curvature is not
  identified and BIC selects the linear model → report and plot the **linear** ACR; the
  quadratic stays a robustness check, never the headline equation. Output:
  `curvature_acrt_feols.csv`, `curvature_att_curve.csv`, `curvature_acrt_cells.csv`,
  `curvature_dcdh_lin_vs_quad.csv` + 4 figs (incl. `curvature_att_curve.png`, the ATT(d)
  parabola = total effect vs never-treated). The pooled $d^2$ is significant at 5% for the two
  *residence* outcomes (work-in-own-muni $p=.007$, share $p=.024$) — a mild **concave**
  dose-response (diminishing marginal response) — but below BIC's large-$N$ bar
  ($|t|>\sqrt{\ln N}\approx3.5$), which is why BIC still picks linear.
- **`build_acr_acrt_att_explainer.R`** — pedagogical figure putting **ATT(d)** (the parabola),
  **ACRT(d)** (its slope), and **ACR** (the flat one-number average of ACRT) side by side per
  outcome. Output: `../figures/acr_acrt_att_explainer.png`. Not a result — an intuition aid for
  the ACR/ACRT/ATT distinction.
- **`q3_unified_contdid_robustness.R`** — attempt at the Callaway–Goodman-Bacon–Sant'Anna
  nonparametric ATT(d|d) via `contdid::cont_did`. **INFEASIBLE here:** contdid needs a balanced
  panel with enough within-cohort dose variation; our cohorts are thin (~5 treated/cohort, 18
  distinct doses), so `cont_did` errors (boundary-knot / non-conformable). Kept as a documented
  dead-end; **not** wired into `run_repro.sh`. The feasible curvature answer is the parametric
  `q3_unified_curvature_robustness.R` + dCDH above.

## Multi-test pre-trend battery + results workbook
- **`q3_unified_pretrend_battery.R`** — four pre-trend tests per cell × outcome: joint Wald,
  individual leads, linear pre-trend slope, and HonestDiD breakdown.
  `robust_clean := no individual lead significant at 5% AND linear pre-slope p > 0.10`.
  **Finding:** Inc-bind is `robust_clean` on all four outcomes even where the joint Wald
  over-rejects. Output: `pretrend_battery.csv`.
- **`build_results_workbook.R`** — collects every result CSV into `../results/all_results.xlsx`
  (one sheet per table + an INDEX sheet). Run after the battery so its sheet is current.

## Bottom line we landed on
- **Headline:** levels decomposition + property-tax controls (Inc-bind −0.435).
- **Main event study:** realized-**dose** ES (the only one where the placebo is null).
- **ACR is linear:** dCDH ≈ feols and BIC selects the linear dose model, so the headline
  reports the linear ACR; the quadratic/curvature work is a check, not part of the main spec.
- Everything else above is robustness/diagnostic — kept here, not wired into the manuscript.
