# Property-tax leg: estimation spec (for sign-off)

Draft 2026-05-24. **Design document — no code written until signed off.** Mirrors
the EIT cousub headline so the two elasticities are comparable. Incorporates the
four requested fixes (muni-year unit; compare to clean EIT Inc-bind; resolve the
binding asymmetry; dose-form concurrent controls both ways) and makes the
realized-dose event study the central identification check.

---

## 0. Object and comparison target  (Fix 2)

Estimate the **elasticity of residential location to the municipal property-tax
burden**, at the municipality(cousub)-year level, in a continuous stacked DiD
identical in structure to the EIT headline. The comparison is against the
**clean EIT _Inc-bind_ cousub estimates** (not the pooled block −0.21):

| Outcome | EIT Inc-bind (cousub) | property-tax analog estimated here |
|---|---|---|
| `log_live_and_work` (log workers living+working in muni) | **−0.401\*\*\*** | β̂ᴾ |
| `share_live_in` | **−0.045\*\*\*** | — |
| `log_total_residents` | **−0.100\*\*\*** | — |

"Per pp" for EIT means per 1 pp of income-tax rate. The property-tax dose is in
pp of **effective rate on market value** (see §2). The two are *mechanically*
different bases (income vs house value), so the headline comparison is the **sign
and order of magnitude of the behavioral response to a resident-bill-raising tax
change**; the dollar-for-dollar "mobility cost per dollar of revenue" conversion
is the eventual paper's payoff, not this regression.

## 1. Unit and sample  (Fix 1)

- **Unit: municipality(cousub)-year, 2002–2019** — where the property-tax
  treatment varies (muni + SD + county millage all map to the cousub) and the
  exact panel/`total_jobs>50` screen the EIT cousub headline uses.
- Treatment data: `pa_property_burden_panel.csv` (muni+SD+county mills × CLR).
- Controls: the muni AFR shares used by the EIT headline
  (`share_police, share_streets, share_debt_svc, share_eit, share_re,
  share_intergov`).

## 2. Treatment: the policy-driven effective property-tax rate

Effective rate on market value: **τᴾ_{it} = (mills^muni+mills^SD+mills^county)_{it}/1000 × CLR_{c(i),t}.**

**The identification threat unique to this leg.** τᴾ has two moving parts:
(a) statutory millage = the *policy* lever; (b) CLR = assessed/market, which
**drifts down mechanically between reassessments** as market values rise and
assessments lag. The CLR-drift component is **endogenous** — it is mechanically
correlated with local house-price growth, itself a driver of residential choice.
Raw Δτᴾ therefore cannot be the treatment. Two contaminations to net out:

1. **CLR drift** (no millage change, τᴾ falls passively) — must not count as treatment.
2. **Revenue-neutral reassessment** (PA anti-windfall law: mills roll back as the
   base jumps; CLR → 100%) — statutory mills move a lot but the **burden is
   unchanged**, so Δτᴾ ≈ 0 — must not count as treatment.

**Event definition (handles both):** muni *i* is treated in the first year with
- a material **statutory millage** change: |Δ(total mills)_{it}| ≥ 0.5 (the
  project's millage-event threshold), **AND**
- a non-trivial resulting **burden** change: |Δτᴾ_{it}| ≥ ε.

The first condition requires a real policy action (excludes pure CLR drift); the
second requires the burden to actually move (excludes revenue-neutral
reassessment rollbacks). ε calibrated so the median CLR-drift year falls below it
(to be reported; candidate ε ≈ 0.05 pp of market value).

**PRIMARY treatment = the POLICY-ONLY dose** (decision 2026-05-24). A confounded
treatment is fixed by using the clean *variable* as the dose, not by a clean
sample trigger; and the EIT–property comparison requires both legs to be
**policy-variable** treatments. So the headline dose is the cumulative
**discretionary** millage change, valued at the muni's **baseline** assessment
ratio:

  dose^{P,pol}_{it} = CLR_{c(i),c-1} · Σ_{τ≤t, τ≥c_i} Δ^{disc}mills_{iτ} / 1000

where the discretionary millage increment **handles revenue-neutral reassessment
rollbacks explicitly**:
- **Non-reassessment year:** Δ^{disc}mills_{it} = mills_{it} − mills_{i,t-1}
  (CLR stable, so the millage move is policy).
- **Reassessment year** (flagged from `pa_reassessment_from_clr.csv`, the
  CLR-footnote dates): Δ^{disc}mills_{it} = mills_{it} − mills_{i,t-1}·(CLR_{t-1}/CLR_t)
  = the **deviation from revenue-neutral**. A pure anti-windfall rollback sets
  mills_{it} = mills_{i,t-1}·CLR_{t-1}/CLR_t exactly ⇒ Δ^{disc}=0 (correctly not a
  treatment); only mills set **above** revenue-neutral register as a positive
  policy dose.

Valuing at baseline CLR (fixed assessment ratio) strips the endogenous CLR-drift
entirely from the treatment. **Direction** Inc/Dec; scope **Inc**. Stacked on
event cohorts (first year with a material discretionary increment, |Δ^{disc}mills|≥0.5),
window ±5, NC = munis with no discretionary millage event. Demeaned within
stack×muni over the pre-window.

**ROBUSTNESS = effective-rate dose** `(τᴾ_{it} − τᴾ_{i,c-1})`, the realized
mills×CLR burden change, to show the headline is not sensitive to fixing CLR at
baseline. (It carries CLR drift, hence robustness not primary.)

ε (the burden-change threshold, used only for the effective-rate robustness and
for reporting) is **calibrated empirically**: set so the median CLR-drift year
falls below it; the CLR-drift magnitude distribution and ε-sensitivity are reported.

## 3. Estimating equation  (mirror of EIT headline; Fix 4 controls)

For municipality *i*, year *t*, cohort-stack *s*, on the stacked panel
{treated cohort *s* ∪ NC}, window rel = t − c ∈ [−5, 5]:

  y_{ist} = α_{s,i} + γ_{s,t}
          + **βᴾ · dose^P_{ist}·1{Inc}**            (property-tax treatment)
          + **δ · Δτ^{EIT}_{ist}**                    (concurrent-EIT dose control, Fix 4)
          + X_{it}′φ + ε_{ist}

- **FE:** `stack_muni + stack_year` (stack-interacted, as EIT). **Cluster: ~muni.**
- `Δτ^{EIT}_{ist}` = the muni's cumulative change in (resident EIT + SD EIT) over
  the same stacked window — entered as a **dose**, not a level (Fix 4), so the
  property-tax estimate is purged of co-moving EIT changes that drive the 2×
  event co-occurrence (P(millage chg | EIT chg)=33.5% vs 16.5% base).
- Estimator object: first-difference ATT per pp of effective rate (β̂ᴾ),
  compared to the EIT Inc-bind β̂ᴱᴵᵀ = −0.401 on `log_live_and_work`.

## 4. The binding-only vs average asymmetry — resolved  (Fix 3)

The EIT has a piecewise structure (T = max(τ^M+τ^{SD}, τ^{nr})); the clean
**Inc-bind** estimate isolates EIT increases that *actually raise the resident's
bill* (the rate already exceeds the nonresident floor, so the increase passes
through), and the Inc-nobind cell is its within-design placebo.

**The property tax has no such decomposition because every property-tax increase
binds:** there is no nonresident floor, no max() — an effective-rate increase
raises the bill of every resident owner directly (and renters via capitalization,
§6). So **all** property-tax Inc events are "binding" by construction.

**What is compared to what, and why it is meaningful:** β̂ᴾ (response to a
binding property-tax increase) vs β̂ᴱᴵᵀ from **Inc-bind** (response to a binding
EIT increase). Both are "how residents re-sort when a tax change actually raises
their own bill" — like with like. Comparing β̂ᴾ to the EIT *pooled/average* (which
dilutes with non-binding cells) would understate the EIT and bias the comparison;
Inc-bind is the right benchmark.

**Consequence — why the ES is paramount:** the property-tax leg has **no
within-design placebo** (no Inc-nobind analog). The EIT can lean on its
placebo cell; the property tax cannot. So **parallel trends is checked, not
asserted, entirely through the realized-dose event study (§5).** It is the single
most important element of this spec.

## 5. Pre-trend test — realized-dose Sun-Abraham continuous ES  (the centerpiece)

Exact analog of `q3_unified_headline_es.R`. Replace the treatment with a
realized-dose × event-time interaction; the realized dose is a **constant
per-muni tag** = the muni's eventual cumulative Δτᴾ at +5:

  y_{ist} = α_{s,i} + γ_{s,t}
          + **Σ_{k≠−1} β_k · 1{rel=k} · realized_dose^P_i**
          + δ · Δτ^{EIT}_{ist} + X_{it}′φ + ε_{ist}

- `realized_dose^P_i` = the muni's eventual cumulative **policy-only** dose at +5
  (CLR_{c-1} × cumulative discretionary millage change through c+5; 0 for NC),
  interacted with relative-event-time indicators, ref = −1. Cluster ~muni.
  (Built FIRST, ahead of the headline, per the 2026-05-24 decision — the
  pre-trend verdict decides whether the cousub unit is defensible for this leg.)
- **Test (Callaway–Goodman-Bacon–Sant'Anna 2024 strong parallel trends):** the
  **pre-period coefficients β_k for k ≤ −2 must be ≈ 0** — i.e. high-eventual-dose
  munis were not already on different trajectories before the millage change.
  Report β_k by event time, the pre-mean (k≤−2) with SE, and a Wald joint pre-trend
  test (`wald_pretrend`, as in the EIT ES).
- **Pass = clean pre (|t| on pre-mean ≲ 1.5, Wald p not rejecting).** Given there
  is no placebo to fall back on, a failed pre-trend here is disqualifying for the
  property-tax leg — unlike the EIT, where the block-level pre-trend was the clean
  one (−0.027, |t|=0.51) and the cousub failed (+0.149, |t|=2.66). The property-tax
  ES result determines whether the cousub unit is even defensible here, or whether
  the leg must move to a finer unit / the policy-only dose.
- Companion: **binary Sun-Abraham IW ES** (`fixest::sunab()`) on the Inc event for
  the average-parallel-trends check, as in `q3_unified_sa_binary_es.R`.

## 6. Capitalization (interpretation, carried as a caveat)

A millage increase partly capitalizes into lower house prices, so the burden
reaching a *mobile* household is less than the statutory Δτᴾ; β̂ᴾ blends tax and
price response. Not solved here. Scoped data (STEB market values by muni/SD; FHFA
county HPI; Zillow ZHVI) supports a "what we can/can't say" treatment and a
robustness controlling for local house-price growth — which *also* absorbs part of
the CLR-drift threat (§2). Outcome stays LODES counts (net displacement, not gross
flows) — same limitation as the EIT paper, carried over explicitly.

## 7. Symmetric fix to the EIT design  (Fix 4)

Add a **concurrent-millage dose** control to the EIT headline and its ES:
`dose^P_{ist}` (cumulative Δτᴾ over the EIT event window) on the RHS of
`q3_unified_full_table.R` / `q3_unified_headline_es.R`. This purges the EIT
Inc-bind estimate of co-moving property-tax changes, making the two legs
symmetric and the −0.401 vs β̂ᴾ comparison clean from both sides. Report whether
−0.401 moves when the concurrent-millage dose is added (it should not, if the
legs are truly separable per the +0.059 correlation).

## 8. Outcomes
Primary `log_live_and_work` and `share_live_in` (match EIT headline); secondary
`log_total_residents`, `log_in_commuters`, `log_total_jobs`.

## 9. Deliverables (on sign-off)
- `q3_proptax_headline.R` — §3 estimator, all outcomes, Inc-bind-comparable table.
- `q3_proptax_es.R` — §5 realized-dose ES + binary SA-IW; pre-trend verdict.
- Patch `q3_unified_full_table.R` / `_headline_es.R` with the §7 concurrent-millage dose.
- Treatment build folded into `multitax_effective_rate.py` (event flag + dose).
```
```

---

## 10. Pre-trend verdict + EIT-vs-property asymmetry (2026-05-24, corrected)

Built the realized-dose ES FIRST (`q3_proptax_es.R`), continuous policy dose,
muni+year FE. **Structural findings:** property millage is high-frequency
(SD Act-1 churn) so there is NO never-treated group and "first change" is
degenerate (collapses to 2003, no pre-period) — events were defined as each
muni's LARGEST discretionary increase to recover staggered timing. Identification
is therefore from DOSE MAGNITUDE, not a never-treated pool (continuous
realized-dose framing, adopted).

**Pre-trend (cousub):** `log_live_and_work` and `log_total_residents` fail
(Wald p=0.046, 0.010); `log_total_jobs`/`log_in_commuters` and both shares are
clean. Localized to the **residence scale**.

**EIT-vs-property asymmetry — measured IDENTICALLY** (`q3_eit_es_decomp.R` vs
`q3_proptax_es.R`, same estimator/outcomes/Wald), CORRECTED framing:
- **EIT shows NO residence-scale selection; property tax does.** Anchor on
  `log_total_residents` (pure residence scale): EIT pre-leads flat into the event
  (t at −2 = −0.34, Wald p=0.33); property tax has a significant near-event ramp
  (t at −2 = −2.60, Wald p=0.010). Residents already diverge before a property-tax
  hike, not before an EIT hike.
- `log_live_and_work` is **noisy for both** (each has one stray significant
  pre-lead; clean near-event) — flag it, don't lean on it.
- **DROP the "EIT confound was composition / block-absorbable" story.** It was an
  inference; the identical-measurement check refuted it (composition is clean for
  both). The simpler true claim: property tax has a whole-muni residence-scale
  selection problem that EIT does not, and because the treatment is muni-wide,
  block-level analysis cannot absorb it.

**Decision: option 3** — reconsider the design to rescue the (contaminated but
central) residence-count margin against residence-trajectory selection:
candidate tools are muni-specific pre-trends, the more-exogenous
reassessment-deviation dose, and Rambachan–Roth honest-DiD bounds (caveat: with a
significant same-signed −2 lead, honest bounds may include zero quickly). Lead
with the clean composition + workplace margins; bound, don't drop, the residence
margin.

---

## 11. Comparison framework (supersedes the joint-spec framing in §3, §7)

**Drop "identify both taxes in one model."** That framing claims something the
property-tax leg cannot deliver: its residence-count margin is contaminated by
muni-level residence-scale selection (§10), so a joint specification would inherit
that contamination and a single "property-tax elasticity" coefficient would not be
credibly identified. The two legs are estimated **separately**, each in its own
clean municipality-level design, and **compared in a common framework**:

- **EIT elasticity (clean):** the Inc-bind municipality estimate (passes its
  realized-dose pre-trend at the muni level). This is a genuine elasticity.
- **Property-tax — clean margins only:** the **composition** margin (`share_live_in`)
  and the **workplace** counts (`log_total_jobs`, `log_in_commuters`), all of which
  clear the property-tax realized-dose pre-trend. Reported as point estimates.
- **Property-tax — residence counts (`log_live_and_work`, `log_total_residents`):**
  NOT a point estimate. Reported as **Rambachan–Roth honest-DiD bounds** that take
  the observed pre-trend as a bias budget. Given the significant same-signed −2
  lead, these bounds are expected to be wide / include zero — that is the honest
  statement, not a contaminated point estimate dressed as causal.

The concurrent other-tax dose (the §3/§7 control) is retained only as a **nuisance
control within each separate design**, not as a co-identified treatment. The
deliverable is a comparison table, not a joint regression.
