# OD-Pair Binding Decomposition: Companion Paper Build Spec

## Project setup

Create a new top-level folder parallel to the existing muni-level project:

```
local_income_tax_migration_od/
├── R/
│   ├── 00_config.R
│   ├── 01_build_od_panel.R
│   ├── 02_build_rates_and_homestack.R
│   ├── 03_assign_cells.R
│   ├── 04_pooled_benchmark.R
│   ├── 05_binding_decomposition.R
│   ├── 06_realized_dose_event_study.R
│   ├── 07_robustness.R
│   └── 99_tables_and_figures.R
├── data/
│   ├── raw/         # symlink or copy of muni-level DCED rate panel
│   ├── interim/
│   └── final/
├── figures/
├── tables/
└── logs/
```

Use renv or a project-local library. Match the package stack from the existing
project: data.table for the OD panel (it will be large; dplyr will be slow),
fixest for the regressions (clustering and high-dim FE), ggplot2 for figures.

## Unit of observation

The unit is the **ordered (home-muni j, work-muni i, year t) triple**, restricted
to PA-internal flows. Each row is one OD-pair-year.

Build from LODES OD v8 main file (intra-state PA), 2002-2019. Aggregate from
block-pair to muni-pair using the existing block-to-COUSUBFP crosswalk.

**Restrict the panel** to OD pairs with positive flow in at least one pre-2007
year and a pre-period (2002-2006) mean flow of ≥10 workers. This protects
against LODES synthetic-noise artifacts at the pair level (the noise infusion
is calibrated at tract scale, so small pair-level counts are dominated by
noise). Keep this threshold as a tunable parameter in 00_config.R; report
sensitivity at thresholds {5, 10, 25} in robustness.

Expected panel size after filtering: roughly 200k-400k OD pairs × 18 years.
Verify in the build log.

## Outcome variables

Primary outcome:
- `log_flow_ji` = log(workers commuting from j to i in year t), with log(1+x)
  for zero-flow years within retained pairs.

Secondary outcomes (run the same regressions on each):
- `log_total_in_commuters_i` = log of total in-commuters to work-muni i
  (collapses across j; this is the work-side aggregate analog of the muni
  paper's log in-commuters outcome). Run as a separate regression at muni-i-year
  level, not OD-pair level. This is the bridge between the OD-pair design and
  the muni-level work-side mirror.
- `share_of_i_jobs_from_j` = flow_ji / total_jobs_i. Bounded share outcome,
  useful for sensitivity.

Do NOT use log(total_jobs_i) at the OD-pair level — total jobs is a muni-i
attribute, not an OD-pair attribute, and including it at the OD-pair level
mechanically over-weights work munis with many origins.

## Treatment and binding classification

For each OD-pair (j, i) and year t, define:

- `tau_home_j_t` = τ^{res,M}_{j,t} + τ^{SD}_{j,t} (home stack at the worker's
  residence)
- `tau_nr_i_t` = τ^{nr,M}_{i,t} (nonresident rate at the worker's workplace)
- `binding_ji_t` = 1 if tau_nr_i_t > tau_home_j_t (work-side binds), 0 otherwise

**Treatment event** is a change in τ^{nr,M}_i — the work-muni's nonresident
rate. Assign each OD pair (j, i) to a cohort based on the first year work-muni
i changes its nonresident rate (call it c_i), regardless of any subsequent
changes. Pairs whose work-muni i never changes τ^{nr} are the never-changed
(NC) control pool.

**Cell classification** for an OD-pair (j, i) on a τ^{nr,i} increase event:

- `Inc-bind`: τ^{nr,i} > τ^{home,j} both before and after the event (the bill
  was and remains binding on the work side from j's perspective)
- `Inc-becomes`: τ^{nr,i} ≤ τ^{home,j} before, τ^{nr,i} > τ^{home,j} after (the
  work-side rate flips into binding from j's perspective)
- `Inc-nobind`: τ^{nr,i} ≤ τ^{home,j} both before and after (the work-side
  never binds from j's perspective — within-design placebo)
- `NC`: work-muni i never changes τ^{nr}

Important: the cell is assigned at the OD-pair level using j's home stack at
c_i − 1 (the year before the work-muni's first event) and the work-muni's
post-event nonresident rate. This means a single τ^{nr,i} event will sort its
in-commuter origins j into different cells based on j's home stack — that
within-event variation is the design's key strength.

Mirror Decrease cells exist by construction but follow the muni-paper convention:
do not estimate them in the main spec (thin cohort, expect pre-trend failures).
Report Dec cell counts in the descriptives table.

## Dose

The continuous dose for OD-pair (j, i) in year t is:

`dose_ji_t = tau_nr_i_t − tau_nr_i_{c_i − 1}`

i.e. the cumulative change in work-muni i's nonresident rate from the
pre-event baseline. This is zero for all t < c_i and for NC pairs.

Do NOT use a home-side dose — the treatment is the work-side rate change. The
home stack enters only through the cell classification.

## Specifications

### Pooled benchmark (Section equivalent of muni-paper §5.1)

For each OD-pair-year (j̃, ĩ, t̃) on the stacked panel:

```
log_flow_ji_t = α_(ji,c) + γ_(t,c)
              + β^nr_Inc · dose_ji_t · 1{d_i = Inc}
              + X'_ji_t · φ + ε
```

Where:
- (ji, c) is the OD-pair-by-cohort identifier (the stacking)
- (t, c) is the year-by-cohort identifier
- d_i ∈ {NC, Inc} is the work-muni's direction tag
- X_ji_t is the control vector (see below)

### Binding decomposition (Section equivalent of muni-paper §5.2)

```
log_flow_ji_t = α_(ji,c) + γ_(t,c)
              + Σ_{k ∈ {Inc-bind, Inc-becomes, Inc-nobind}}
                   β^nr_k · dose_ji_t · 1{cell_ji = k}
              + X'_ji_t · φ + ε
```

The Inc-nobind coefficient is the within-design placebo.

### Realized-dose event study (Section equivalent of muni-paper §5.3)

Use the same Sun-Abraham realized-dose construction as the muni paper. The
realized-dose tag is:

`D̃_ji = τ^{nr,i}_{c_i + 5} − τ^{nr,i}_{c_i − 1}`

Run separately for each cell, restricted to (NC, cell) pairs. Window k ∈
[−5, +5]. Joint Wald test on k ≤ −2 as the parallel-trends diagnostic.

## Fixed effects

Use **OD-pair × cohort** and **year × cohort** fixed effects:

```r
feols(log_flow_ji ~ ... | pair_id^cohort + year^cohort, ...)
```

Do NOT use separate home-muni and work-muni fixed effects in the main spec —
the OD-pair FE subsumes both. A robustness check can decompose into
(home-muni × cohort) + (work-muni × cohort) + (year × cohort) to ask how much
of the identifying variation comes from cross-origin vs cross-destination
differences, but that is a secondary specification.

## Control variables

At the OD-pair-year level, use the controls that vary at home-muni-year,
work-muni-year, or both. Six fiscal share controls from the muni paper enter
twice (home-side and work-side):

**Home-muni (j) fiscal shares** at year t:
- police-services share (home)
- streets share (home)
- debt-service share (home)
- EIT-revenue share (home)
- real-estate-tax share (home)
- intergovernmental-revenue share (home)

**Work-muni (i) fiscal shares** at year t:
- same six shares, computed for the work muni

Twelve fiscal controls total. All are share-based and time-varying. Source
from the same DCED AFR database the muni paper uses.

Additionally include:
- log(home-muni j total employed residents at t) — captures origin-side scale
- log(work-muni i total jobs at t) — captures destination-side scale

These two scale controls handle a mechanical concern: an OD-pair flow can
shrink because j shrinks, because i shrinks, or because the bilateral
relationship changes. The two log-scale controls absorb the first two channels
so the dose coefficient picks up the bilateral response.

Do not include commute distance as a control — it is time-invariant within
OD-pair and is absorbed by the OD-pair fixed effect.

## Clustering

Cluster standard errors **two-way at the home-muni level and the work-muni
level**:

```r
feols(..., cluster = c("home_muni", "work_muni"))
```

Rationale: τ^{nr,i} events are shared across all origin munis j commuting to
i (work-muni cluster), and origin-side amenity shocks are shared across all
destinations a single j sends commuters to (home-muni cluster). Two-way
clustering handles both correlation structures.

Do not cluster at the OD-pair level — within-pair serial correlation is
already absorbed by the pair FE, and pair-level clustering will inflate SEs
without addressing the cross-pair correlation that actually matters.

## Sample construction details

1. **Drop Philadelphia as origin and destination** in the main spec
   (Philadelphia operates under the Sterling Act with no offset — same logic
   as the muni paper). Report a robustness specification that retains
   Philadelphia.

2. **Flip-flopper drop**: drop work-munis whose first-event direction is
   reversed by a later event within the K=5 window. Match the muni paper's
   logic exactly.

3. **Pre-period flow filter**: as described above, require pre-2007 mean flow
   ≥ 10. This is the main difference from the muni paper's sample logic.

4. **Cross-state inflows are excluded** in the main spec (LODES OD intra-state
   file only). A note in the limitations section acknowledges that cross-state
   commuters from NJ, NY, DE, MD, OH, WV are dropped; the auxiliary file
   integration is deferred.

## Diagnostic outputs to produce

In `99_tables_and_figures.R`, produce at minimum:

1. **Descriptive table** of cell counts: number of OD pairs, work-munis, and
   home-munis in each Inc cell. Compare to the muni paper's Table 3.

2. **Within-event variation table**: for each work-muni i in the Inc-bind
   work-muni set, count how many of its origin OD pairs land in each cell
   (Inc-bind, Inc-becomes, Inc-nobind). The headline statistic is "the median
   τ^{nr} event sorts its origins into X% bind, Y% becomes, Z% nobind." This
   is the within-event placebo evidence that's missing from the muni paper.

3. **Dose-by-event-time figures**: equivalent of muni paper Figures 4 and 5,
   but cumulative work-muni τ^{nr} change rather than home-muni τ^M change.

4. **Spatial map**: equivalent of muni paper Figure 3, showing which munis
   appear as work-muni Inc-bind, work-muni Inc-becomes, etc.

5. **Headline regression table**: pooled benchmark + binding decomposition, on
   the three outcomes (log_flow_ji, log_total_in_commuters_i,
   share_of_i_jobs_from_j).

6. **Event study figures**: one per cell per outcome.

## Robustness checks (in 07_robustness.R)

1. Pre-period flow filter sensitivity: re-run at thresholds {5, 10, 25}.
2. Pre/post Act 32 split — particularly important here, since work-side
   enforcement only binds mechanically after 2012. Expect a stronger Inc-bind
   coefficient post-Act-32.
3. Act 47 exclusion: drop work-munis with Act 47 designation.
4. Retain Philadelphia: re-run with Philadelphia as origin and destination.
5. Distance heterogeneity: split OD pairs by commute-distance terciles
   (compute from muni centroid coordinates). The binding-channel response
   should be strongest for short-distance pairs where job-switching to a
   different work-muni is feasible.
6. Home-stack-binding interaction: for OD pairs where j's home stack is
   already very high (top quartile), the work-side rate is mechanically
   irrelevant to the bill. This is a secondary within-design placebo.

## Things to NOT do in this build

- Do not estimate a separate home-side (Δτ^M) decomposition in this paper.
  The companion paper is work-side; the resident-side paper is the muni-level
  one already drafted.
- Do not attempt to merge in cross-state commuter flows — defer to future
  work.
- Do not run firm-side or industry decompositions. The muni paper's industry
  decomposition was underpowered; the OD-pair version would be worse.
- Do not introduce gravity-equation-style bilateral controls (distance,
  border indicators). They are absorbed by the OD-pair FE.
- Do not estimate the Dec cells. Cohort is thin and pre-trends will fail.
  Report cell counts only.

## Expected timeline and milestones

1. Build OD-pair panel and verify panel size (01-03): one work session.
2. Pooled benchmark and binding decomposition headline (04-05): one session.
3. Realized-dose event study and pre-trend tests (06): one session.
4. Robustness suite (07): one session.
5. Tables, figures, and write-up scaffolding (99): one session.

Verify after step 2 that the Inc-nobind coefficient on log_flow_ji is small
and statistically indistinguishable from zero. If it is not — i.e. if a
work-side rate increase reduces flows even from origins whose home stack
already dominates the new τ^{nr} — the binding-channel mechanism is not
identifying cleanly on the OD-pair design and the spec needs revisiting
before proceeding to robustness.
