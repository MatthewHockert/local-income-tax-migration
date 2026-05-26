# Property-tax leg — section draft

_Draft 2026-05-24. Flowing prose, declarative; numbers from
`proptax_eit_comparison.csv`, `proptax_es_att.csv`, `pa_acs_dollar_base.csv`.
Not yet placed in a manuscript — structure for review._

---

## Does the property tax move residents the way the income tax does?

The earned income tax relocates residents; the property tax shows no detectable
sorting response. Per dollar of annual tax, residents respond to a municipal
income-tax increase between fifty and one hundred times more strongly than to a
property-tax increase of equal dollar burden, and on the cleanly identified
margins the property-tax response cannot be statistically distinguished from
zero. This is the central comparison of the paper. The absence of a detectable
property-tax sorting response is consistent with capitalization—the burden being
absorbed into house prices rather than relocating the household—but I do not
observe house prices here, so I read the null as consistent with capitalization,
not as evidence that proves it.

I estimate the property-tax response in the same continuous stacked
difference-in-differences framework used for the income tax, at the municipality,
so the two elasticities are directly comparable. The treatment is the
policy-driven component of the effective property-tax rate on market value: the
statutory municipal, school-district, and county millage a parcel faces,
converted to an effective rate by the county's common level ratio of assessed to
market value. I use the policy component rather than the raw effective rate
because the common level ratio drifts mechanically between reassessments as market
values outrun stale assessments, and that drift is correlated with the local
house-price dynamics that themselves drive location choice. Valuing each
municipality's cumulative discretionary millage change at its baseline assessment
ratio strips this drift out, and treating revenue-neutral reassessments as the
deviation from the anti-windfall rollback ensures that a mechanical millage
reset—where the rate falls only because the base jumped—does not register as a tax
cut.

The property tax differs from the income tax in two ways that shape
identification. It changes frequently—the school-district component moves almost
every year under the Act 1 index—so there is no never-treated comparison group;
identification comes instead from variation in the magnitude of the cumulative
policy dose. And it binds mechanically: every effective-rate increase raises the
bill of every resident owner, so there is no non-binding placebo cell of the kind
the income tax's piecewise max-rule provides. With no within-design placebo to
fall back on, the parallel-trends assumption rests entirely on the realized-dose
event study, which is therefore the leg's only identification check and the
diagnostic I lead with.

That diagnostic delivers a split verdict, and an instructive one. The composition
margin—the share of local jobs filled by residents—and the workplace counts show
no differential pre-trend across dose magnitudes; the residence counts do.
Municipalities that go on to raise the property tax are already on diverging
resident-population trajectories beforehand, while their job base and internal
composition are not. The same diagnostic run on the income tax shows no such
residence-scale divergence. The contamination is thus a whole-municipality
residential trajectory, not a composition shift, which is why it cannot be
absorbed by moving to a finer geography: the treatment is municipality-wide, so a
block-level estimate inherits the same municipal trend. I therefore report the
residence-count margins as Rambachan–Roth honest-DiD bounds rather than point
estimates, and carry the cleanly identified composition and workplace margins as
the causal comparison. Because those bounds include zero, the residence-count
margin is unidentified—not a clean null: it rules out an income-tax-sized response
but cannot distinguish a small effect from none.

The comparison is starkest on a common dollar basis. A one-percentage-point income
tax falls on earned income; a one-percentage-point effective property tax falls on
market value; the two are equal-sized policy changes only after converting each to
the dollar burden it imposes, using each municipality's own median household income
and median home value from the American Community Survey. Expressed per thousand
dollars of annual tax, the income-tax elasticity of the local-employment share is
−0.12, while the property-tax estimate is −0.001 with a ninety-five percent
confidence interval of [−0.003, +0.001]—a precise zero that excludes the income-tax
effect by a factor of about one hundred. The workplace-count margins tell the same
story: per dollar, the income tax moves jobs and in-commuters roughly fifty to
sixty times more than the property tax, and the property-tax confidence interval
excludes the income-tax magnitude in every case. The multiple is not an artifact
of two representative numbers; built from the cross-municipality distribution of
the home-to-income ratio, it ranges from roughly forty to one hundred fifty across
the tenth to ninetieth percentiles. Even the contaminated residence-count margins,
under the honest-DiD bounds, include zero and still exclude an income-tax-sized
response.

The interpretation consistent with this pattern is capitalization. A property-tax
increase is substantially absorbed into lower house prices, so the burden reaching
a mobile household is smaller than the statutory change and is, in part, borne by
the immobile asset rather than the person; an income-tax increase falls on the
mobile earner directly. I do not estimate the house-price response here, so
capitalization is the mechanism the null is consistent with, not one I demonstrate
directly; what the data establish is that the property-tax sorting response per
dollar of revenue is bounded an order of magnitude below the income-tax response. Two caveats bound the
claim. The comparison's magnitude depends on the income-tax estimate, which is the
binding-cell effect and is large; under any reasonable discount of it the
property-tax response remains far smaller. And the residence-count margin is not a
clean null but an unidentified one—the honest bounds rule out an income-tax-sized
effect but not zero—so the cleanly identified statement is the composition and
workplace comparison, with the residence margin reported as bounded.
