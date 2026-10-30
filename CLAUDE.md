# CLAUDE.md

Repo for the paper `manuscript/muni_tax_sorting.tex`. `README.md` has the run order, the script map and where each exhibit comes from. Read it before touching the pipeline.

## Model

Continuous stacked difference-in-differences at the municipality (Census county subdivision) level, PA, 2002 to 2019.

- Treatment: the muni resident EIT rate `dose_stack`, interacted with the binding cell (`treat_cell`: Inc-bind, Inc-becomes, Inc-nobind, reference NC). The SD resident rate enters the same way through `cell_sd` x `dose_sd`.
- A worker's bill is `max(muni resident + SD resident, work-side nonresident)`. A cell is assigned once per municipality, from the home stack versus the nonresident rate the year before and the year of the first rate change. Inc-nobind is the within-design placebo.
- Fixed effects `stack_muni + stack_year`. Clustering `~muni`. Controls: `share_police`, `share_streets`, `share_debt_svc`, `share_intergov`, and the muni, SD and county effective property-tax rates.
- Pre-trends: realized-dose event study (each unit's cumulative change at +5 as a constant tag) plus the four-test battery in `3_robustness.R`.

Current headline (`data/results/headline_full_table.csv`), log live-and-work per pp:
Inc-bind -0.537 (0.141), Inc-becomes -0.250 (0.048), Inc-nobind -0.110 (0.151).
Inc-bind event study: post -0.617, pre -0.116, joint Wald p = 0.053, linear pre-slope p = 0.47.
Act 32 split, Inc-bind: pre-2012 -0.544 (0.201), post -0.531 (0.162).

## Open issues (not yet fixed)

- The headline sample is effectively 2006 to 2019: `share_debt_svc` and `share_intergov` come from post-2005 AFR columns, so complete cases drop 2002 to 2005.
- The paper says Sun and Abraham (lines ~652 and ~931), but `sunab()` is not run anywhere.
- Erie's rate above the cap is an Act 205 pension surcharge, not an EIT increase. 16 Act 47 municipalities (Pittsburgh, Altoona, Johnstown, ...) are not in the estimation panel at all, so the Act 47 exclusion removes fewer cities than its list suggests.
- `1_build_panels.R` rewrites `data/raw/pa_eit_rate_panel.csv`, so raw is not strictly download-only.
- Specifications that differ from the headline (found in the 2026-10 code pass, not yet changed):
  - The decomposition Act 32 split has no SD co-treatment term. The pooled Act 32 split has one.
  - Fake timing: the decomposition placebo uses the realized dose, the pooled placebo uses the rate level.
  - Pre-trend battery: the decomposition version ignores the joint Wald and uses a regression pre-slope. The pooled version needs Wald p >= 0.10 and fits its slope by lm() on the lead estimates.
  - The EIT revenue section uses the dose as a change from the pre-event mean, not the level.
  - The pooled event study does not condition on dose_stack, so it runs on ~1,300 more rows than the pooled regression.
  - The averaged ATT SEs in `2_event_studies.R` treat event-time coefficients as independent.

## Working rules

- Push scripts only. `data/`, `figures/`, `tables/` and PDFs stay out of git.
- Never edit `manuscript/references.bib`. It is exported from Zotero. Hand over BibTeX instead.
- R: tidyverse, with data.table only for the large merges and the panel and stack builds. Comments: `# Section ----` headings plus a line or two, and one-line notes where a decision is made.
- Python: `~/packaging-test/pkg-test/bin/python`.
- New specs go into the existing script as a flagged section. Sections not in the paper are `PARKED` (flag FALSE), not deleted.
- Paper prose: no semicolons, no em dashes, no bold or italics for emphasis. Simple edits, no rewrites, never delete the author's comments.
- Check any number against `data/results/` before it goes into the paper.
