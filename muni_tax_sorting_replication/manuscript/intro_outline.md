# Introduction outline

Working document for `muni_tax_sorting.tex` §1. Seven paragraphs in two movements,
each running wide to narrow: the first through policy (federal → state → local),
the second through evidence (theory → state → local → gap).

Status as of 2026-07-29: ¶1–¶3 are drafted in the manuscript. ¶4–¶7 are the
literature review and are not yet written.

---

## Movement I — the political climate (drafted)

### ¶1 Recent state action
*Job: establish that taxing income and wealth is live and contested.*

- Growing debate at the federal level and across states and localities, driven by
  inequality and revenue shortfalls.
- Massachusetts abandoned its flat 5% rate with a 4% surtax above $1 million.
- Washington, which has never levied a broad income tax, enacted a 9.9% rate above
  $1 million, effective 2028.
- California has a nominally one-time 5% levy on billionaires' net worth headed for
  the 2026 ballot.

`vermeerMassachusettsSurtax2022`, `waSB6346_2026`, `walczakCaliforniaWealthTax2026`

### ¶2 The same pressure below the state level
*Job: move the debate to local government, where this paper operates.*

- Municipal budgets squeezed by inflation and shrinking transfers.
- Local income taxation is more common than generally recognized: roughly 5,000
  jurisdictions across sixteen states, and anything but uniform in form.
- New York City has levied its own income tax since 1966; the current administration
  has proposed raising it on residents earning more than $1 million.
- Local taxing authority is delegated by the state — hence Albany. Characteristic of
  local income taxation generally, and the same structure as Act 511.

`walczakLocalIncomeTaxes2023`, `nycMayorsOfficePrelimBudget2026`

### ¶3 The objection, and why it is hard to test locally
*Job: convert the political fight into the empirical question, and hand off.*

- The standard criticism: high earners will simply move.
- That objection is testable — a claim about how much of the base relocates when a
  rate changes, and how far a jurisdiction can raise rates before the base walks away.
- But the evidence comes almost entirely from variation in top marginal rates, and
  that variation does not exist below the state level, where local income taxes are
  generally flat and fall on every earner alike.
- Testing the objection where it is now being raised requires a different source of
  variation.

Ends on the unanswered question so ¶4 opens as the answer.

---

## Movement II — the literature (to write)

### ¶4 Why taxes should move anyone: theory
*Job: supply the mechanism. Keep this paragraph theoretical — evidence belongs below.*

- Tiebout: households choose jurisdictions by the tax-amenity bundle and vote with
  their feet.
- Sorting is an equilibrium object, not a partial-equilibrium relocation decision.
- The margin is sharpest where jurisdictions are dense and moving costs low.

`tiebout1956`, `epple1999`, `oates2006tiebout` (optional)

### ¶5 State-level evidence
*Job: establish that subnational taxes do move people, and bound what that shows.*

- Top earners respond to state rate differences, most clearly where differentials
  are large.
- The populations studied are narrow and already mobile: millionaires, star
  scientists, foreigners under preferential schemes.
- State taxation of nonresident income already shifts where work is located — the
  first appearance of the sourcing-rule theme that ¶6 develops.
- So this literature speaks to a thin slice of taxpayers facing large gaps, not to
  ordinary workers facing sub-percentage-point ones.

`young2016`, `young2011`, `agrawalRelocationRichMigration2019`,
`morettiEffectStateTaxes2017`, `agrawalStateTaxationNonresident2024`,
`kleven2020` as the survey anchor

### ¶6 Local-level evidence
*Job: narrow to sub-state; show the US evidence is thin, the strong evidence is
Swiss, and the local case is structurally different. Sets up ¶7.*

- Few states permit local income taxes and the US evidence is a handful of studies:
  small county-level responses in Indiana, and now Indiana and Maryland.
- Pennsylvania's EIT has been studied on compliance rather than sorting.
- The nearest Pennsylvania precedent is Philadelphia's wage tax and suburbanization.
- The strongest sub-state sorting evidence is Swiss, where municipalities set their
  own income taxes. (Antecedent for ¶7 — do not move these up to ¶4.)
- What makes local different: residence and workplace are routinely different
  jurisdictions, so place-of-taxation rules do real work, and municipalities have an
  incentive to set nonresident rates to export burden.
- Bridge: this is why sub-state geography is where small rates should still bite —
  2,500 Pennsylvania municipalities within commuting distance of one another, so a
  fraction of a percentage point can be avoided without changing jobs or schools.

`yangRESPONSIVENESSINCOMELOCAL2017`, Yushkov (2025),
`bagchiEffectsIntroducingWithholding2022`, Livas & Jacob (2026),
`schmidheiny2012zurich`, `basten2017`, `agrawal2015gradient`.
`coomes2008`, `haughwout2004` only if there is room.

### ¶7 The gap, and what this paper supplies
*Job: name the gap in Agrawal's own terms, then answer it point by point.*

- The survey's verdict: the US local income tax literature is almost non-existent
  and the evidence base is Swiss.
- Why that matters: Switzerland taxes on a purely residential basis while US local
  systems use non-residential place-of-taxation rules, so Swiss estimates do not
  transfer. He calls this unsatisfactory from a US perspective.
- He asks for single-state case studies of localities, and for datasets on municipal
  rates, revenues, and sourcing rules.
- He notes that whether mobility shows up as households moving or jobs moving depends
  on the sourcing rule.
- What this paper supplies against each: Pennsylvania 2002–2019; a mixed
  residence/source rule; a purpose-built municipal and school-district rate panel;
  and a design that uses the sourcing rule itself to separate the workers whose bill
  moves from those whose does not.

`agrawalLessonsStateLocal` throughout. `scharffSurprisinglyStrongCase2022` if a line
on renewed policy interest is wanted.

---

## Notes on the seams

**Pennsylvania jurisdiction counts.** ¶2 stops at national scale (sixteen states,
~5,000 jurisdictions). The Pennsylvania figures — 2,506 municipalities and 472 school
districts — belong where the setting is introduced, cited to Walczak (2019) rather
than to Bagchi and Agrawal, who are not sources for a jurisdiction count. Do not use
both counts in the same breath: they disagree (4,964 in seventeen states in 2019;
5,055 in sixteen in 2023).

**Keep the Tax Foundation descriptive.** Institutional facts and jurisdiction counts
only. Every behavioral claim rests on `young2016`,
`agrawalRelocationRichMigration2019`, `kleven2020`. Their Massachusetts piece projects
a $6 billion contraction by applying elasticities borrowed from other settings — a
simulation, not an estimate.

**Dropped.** `agrawal2018commuting` (Agrawal & Hoyt) is out.

**Bibliography.** Zotero is the source of truth; `references.bib` is an export.
Outstanding: `nycMayorsOfficePrelimBudget2026`. Yushkov (2025) and Livas & Jacob
(2026) are in Zotero but not yet in the current export. Everything else the outline
names is present.
