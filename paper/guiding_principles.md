# Guiding Principles for the Empirical Design

This paper estimates whether local income tax rate changes induce
residential sorting across municipal boundaries. The empirical design
is built around Pennsylvania's institutional structure: workers face a
home-side tax stack composed of the municipal resident rate and the
school district resident rate, while the work-side nonresident rate
determines the tax claim of the workplace municipality. Each worker's
bill is the larger of the home stack and the work nonresident rate,

$$
T_{\text{worker}} = \max\bigl(\tau^{M} + \tau^{SD},\ \tau^{nr}\bigr),
$$

so a statutory rate change only changes the worker's actual tax burden
when the relevant side of the formula binds. The design therefore
decomposes rate changes by whether they change the actual tax bill.

## Main framework

The main empirical framework is a continuous stacked
difference-in-differences design. Within each stacked cohort design,
treatment intensity is defined as the change in the relevant tax rate
relative to the unit's pre-event rate:

$$
\Delta \tau_{it} = \tau_{it} - \tau_{i,c-1},
$$

where $c$ is the first event year for unit $i$. This treatment dose is
time-varying after the first rate event and is zero in all pre-event
periods by construction. The main stacked continuous specification
estimates post-event dose-response effects: the effect of a one
percentage-point change in the relevant tax rate. Control units are
*never-changed* jurisdictions whose relevant rate does not change over
2002–2019; this strict comparison group is feasible because the
overwhelming majority of Pennsylvania cousubs are never-changed on at
least one rate side.

## Cell decomposition

The model decomposes each rate change by binding-state cell:

$$
c \in \{
\text{Inc-bind},\ \text{Inc-becomes},\ \text{Inc-nobind},\
\text{Dec-bind},\ \text{Dec-becomes-non},\ \text{Dec-nobind}
\}.
$$

The key comparison is between rate changes that move the worker's
actual bill and rate changes that do not. The Inc-bind cell captures
rate increases where the home-side tax stack binds both before and
after the event, so the worker's bill rises by the full rate change.
The Inc-becomes cell captures rate increases where the home stack
moves from non-binding to binding, so the worker's bill rises only by
the portion of the rate change that exceeds the work-side nonresident
rate. The Inc-nobind cell captures rate increases where the home stack
remains below the work-side nonresident rate, so the statutory rate
changes but the worker's actual bill does not. This cell serves as the
within-design placebo. The decrease cells (Dec-bind, Dec-becomes-non,
Dec-nobind) mirror this logic for rate cuts and are reported for
symmetry.

## Three units of observation

The model is estimated at three units of observation — municipality,
school district, and census block — because the level of aggregation
interacts directly with how the two rate sources can be incorporated
into the regression.

- **Municipality level.** The unit is the muni-year, so the regression
  carries the muni's own resident rate cleanly but enters the
  school-district rate through an aggregation function: each muni-year
  is assigned the maximum school-district rate across the school
  districts the muni reports to under DCED.
- **School district level.** The unit is the SD-year, so the SD rate
  enters cleanly while the municipal resident rate enters through a
  corresponding within-SD aggregation across the SD's constituent munis.
- **Block level.** Neither aggregation is needed: each block is
  assigned both its own muni and its own school district, and the
  muni rate and SD rate are allowed to vary independently across
  blocks. The block level thus provides the only specification in
  which both rate-setters' policy variation enters the regression at
  its native level, with no within-jurisdiction maximization or
  averaging step.

## Outcome panels

The outcome panels are constructed to capture residential sorting from
complementary perspectives. In the workplace panel, the unit is
observed as a workplace location, with outcomes such as log resident
workers employed in the unit, log total jobs, and the share of workers
who live in the unit. In the residence panel, the unit is observed as
a residence location, with outcomes such as log employed residents and
the share of residents who work outside the unit.

At the block level the outcome set differs because the workplace and
residence categories collapse into block-defined quantities. The
block-level outcomes are: log workers living in the block, log workers
working in the block, the share of block residents who work in the
block's municipality, and the share of block residents who work
outside the municipality. These quantities replace, rather than
disaggregate, the cousub-level outcomes.

## Fixed effects and clustering

Across all specifications, the fixed effects are defined at the same
level as the unit of observation in the stacked panel. In the
municipality-level model, the unit is the municipality, so the stacked
specification includes municipality-by-stack fixed effects and
year-by-stack fixed effects. In the school-district-level model, the
unit is the school district, so the specification includes
school-district-by-stack fixed effects and year-by-stack fixed
effects. In the block-level model, the unit is the census block, so
the specification includes block-by-stack fixed effects and
year-by-stack fixed effects.

The clustering level follows the source of policy variation rather
than the artificial stacked observation. In municipality-level
specifications, standard errors are clustered by municipality because
the municipal resident rate varies at the municipality-year level. In
school-district-level specifications, standard errors are clustered by
school district. In block-level specifications, treatment varies along
both the municipal and school-district dimensions, since each block is
assigned both a municipal resident rate and a school district rate
independently. I therefore use two-way clustering by municipality and
school district in the block-level models.

Importantly, the stack identifier enters the fixed effects but not the
clustering variable. Stacking creates repeated copies of the same real
policy jurisdictions across cohort-specific sub-experiments. The fixed
effects must account for this stacked structure, so they are
interacted with the stack. The standard errors, however, should
recognize dependence across repeated observations from the same real
municipality or school district. I therefore cluster by the original
policy jurisdictions, not by municipality-by-stack,
school-district-by-stack, or block-by-stack clusters.

## Pre-trend diagnostic

Because the continuous first-difference dose is mechanically zero
before the first event, the main stacked continuous model cannot
estimate conventional pre-treatment lead coefficients. I therefore
assess pre-treatment trends using a separate realized-dose
event-study diagnostic. In this specification, each treated unit's
eventual cumulative dose, measured at event time $+5$, is interacted
with relative-event-time indicators. The pre-event coefficients are
identified from cross-unit differences in eventual rate-change
magnitude and test whether units that later experience larger tax
changes were already trending differently before treatment. This
diagnostic complements the main continuous stacked design: the main
model estimates post-event dose-response effects, while the
realized-dose event study evaluates selection into treatment intensity.

## Guiding rule

Fixed effects follow the stacked estimating unit, while clustering
follows the real policy jurisdiction that generates the tax
variation.
