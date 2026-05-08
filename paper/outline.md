# Paper Outline — Local Income Taxes and Residential Sorting

## 1. Introduction

- The question: do local income tax rate changes induce residential
  sorting?
- The institutional opportunity: PA EIT's piecewise rule + Act 32 (2012)
  consolidation
- Headline findings preview (per-pp dose-response on Inc-bind cell,
  three estimating units)
- Contribution to the continuous stacked DiD literature
  (Cengiz 2019; Callaway, Goodman-Bacon & Sant'Anna 2024;
  de Chaisemartin & D'Haultfoeuille 2024)

## 2. Institutional Background

- PA EIT structure: muni resident rate + SD resident rate (home stack)
  vs. work-muni nonresident rate
- Piecewise rule: $T = \max(\tau^M + \tau^{SD},\ \tau^{nr})$
- DCED rate-setting and reporting
- Act 32 of 2008 (effective 2012): consolidated employer withholding;
  the natural experiment for binding

## 3. Conceptual Framework

- Why piecewise creates a within-design placebo (Inc-nobind)
- Cell decomposition:
  Inc-bind / Inc-becomes / Inc-nobind /
  Dec-bind / Dec-becomes-non / Dec-nobind
- Predicted-null cell: Inc-nobind
- Predicted-magnitude ordering across cells

## 4. Data

### 4.1 LEHD LODES Origin–Destination at the block level

- Coverage 2002–2019; PA only
- Block-level OD; aggregated up where required

### 4.2 Tax rate panel

- DCED muni and SD rate panels
- Construction of total_eit, home_stack, muni_nonres_eit
- Coverage and event counts

### 4.3 Municipal fiscal controls

- AFR (Annual Financial Report) muni share-based controls:
  share_police, share_streets, share_debt_svc,
  share_eit, share_re, share_intergov

### 4.4 School district fiscal controls

- PDE-equivalent SD revenue/expenditure data (status: TBD)
- Either: build SD-level share controls; or use weighted within-SD
  muni controls; or acknowledge gap

### 4.5 Three estimating panels

- **4.5a Municipality-level panel.** Unit = cousub × year.
  Workplace and residence outcomes constructed by aggregating LODES
  OD over blocks within the cousub. Treatment side: muni resident
  rate is exact; school district resident rate is assigned as
  max(sd_eit) across the school districts the muni reports to in
  DCED. Justification: 97.6% of munis report to one SD; the max
  is correct for those, conservative for the 2.4% multi-SD munis.

- **4.5b School-district-level panel.** Unit = SD × year.
  Outcomes constructed by aggregating LODES OD over blocks within
  the SD. Treatment side: SD resident rate is exact; municipal
  resident rate is assigned as the within-SD aggregation across the
  SD's constituent munis (specific function: TBD — pop-weighted mean
  or max).

- **4.5c Block-level panel.** Unit = block × year. Each block is
  assigned its own muni and its own SD. Both muni and SD resident
  rates enter the regression at their native level with no
  within-jurisdiction aggregation. Outcome variables are
  block-defined: log_home_count, log_work_count,
  home_in_muni_share, share_work_outside. These are block-level
  analogues of the cousub-level outcomes — not disaggregations —
  because workplace-panel and residence-panel categories collapse
  into block-defined quantities at this scale.

### 4.6 Sample restrictions

- 2002–2019 (drops COVID-affected and ARPA-affected fiscal years)
- Min thresholds (e.g., total jobs > 50 at cousub level) where
  applicable

## 5. Empirical Strategy

### 5.1 Continuous stacked DiD: main framework

- Stacked cohort design (Cengiz)
- Treatment intensity:
  $\Delta\tau_{it} = \tau_{it} - \tau_{i,c-1}$, time-varying, zero
  pre-event
- NC controls (never-changed jurisdictions)

### 5.2 Cell-decomposed dose-response specification

- Estimating equation:
  $Y_{it} = \alpha_{i \times s} + \gamma_{t \times s}
   + \sum_{c \ne NC} \beta^{M}_c \mathbf{1}\{\text{cell}^M_i = c\}
       \cdot \Delta\tau^M_{it}
   + \sum_{c \ne NC} \beta^{SD}_c \mathbf{1}\{\text{cell}^{SD}_i = c\}
       \cdot \Delta\tau^{SD}_{it}
   + X_{it}\phi + \varepsilon_{it}$
- Per-pp interpretation
- Run at all three estimating units: muni, SD, block

### 5.3 Fixed effects

- Unit-by-stack and year-by-stack
- Cengiz-style sub-experiment isolation

### 5.4 Clustering

- Cluster at the policy-jurisdiction level
- Muni: cluster ~muni
- SD: cluster ~sd_id
- Block: two-way cluster ~muni + sd_id

### 5.5 Pre-trend diagnostic: realized-dose event study

- Spec: i(rel, realized_dose_m, ref = -1), with realized_dose
  defined as the unit's eventual cumulative Delta tau at +5
- Pre-event leads identified from cross-unit variation in eventual
  treatment intensity
- Reported for each estimating unit and each headline outcome

## 6. Main Results

### 6.1 Municipality-level results
- All cells × all outcomes
- Highlight: Inc-bind, Inc-becomes, Inc-nobind (placebo)

### 6.2 School-district-level results
- Same structure
- Discuss SD-side framing

### 6.3 Block-level results
- Block-defined outcomes
- Note: treatment variation now exact (no aggregation function)

### 6.4 Cross-level comparison
- Triangulation table: same cell × outcome at all three levels

## 7. Heterogeneity Decompositions

### 7.1 Pre-/post-Act 32 split
- Run for each estimating unit (muni, SD, block)
- Predicted: Inc-bind effects concentrated post-2012 (when EIT
  became binding via consolidated withholding)
- Headline cell × pre-Act 32 vs post-Act 32 interactions

### 7.2 Industry decomposition (job-side outcomes)
- Services vs non-services using LODES WAC supersector codes
- Run on log_total_jobs (or log_work_count at block level)
- Predicted: services more elastic than non-services (more
  local-demand exposure)
- Run for each estimating unit

### 7.3 Muni-type heterogeneity
- City vs borough vs township
- Run on each level
- Predicted: cities (denser, more exit options) show larger effects

## 8. Robustness

### 8.1 Realized-dose event-study (pre-trend test)
- Per cell × per outcome × per estimating unit
- Pass/fail summary table
- ES figures for Inc-bind on each headline outcome

### 8.2 Alternative control definitions
- Tighter "no events in 8-year window" controls (Cengiz-style)
- Compare to NC-only headline

### 8.3 Sample restrictions
- Drop flip-flopper munis
- Drop top-quartile pre-event growth munis (gentrification check)

### 8.4 Single-event subset
- Munis with one event only

## 9. Discussion

- Magnitude in welfare / fiscal-calibration terms
- Implications for local government fiscal autonomy
- Limits: PA-only, residence margin only, etc.

## 10. Conclusion

---

## What's dropped from the previous paper structure

- Single-event TWFE as a separate estimator section
- DCDH cell-by-cell as a separate estimator
- Stacked binary DiD as a separate estimator
- Piecewise binding model on cousub-pair flows
- Resident vs nonresident-side parallel build (nonresident deferred)

## What's added

- Three-level estimation structure (muni / SD / block) with explicit
  treatment-aggregation rules
- Industry decomposition (services vs non-services) on job-side outcomes
- Pre/post Act 32 split as a heterogeneity section, run at each level
- Realized-dose event study as the canonical pre-trend diagnostic
