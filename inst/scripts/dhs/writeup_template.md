---
title: "Household wealth, inequality, and work across RESPOND community profiles: Malawi DHS 2015-16 and 2024"
subtitle: "Internal methods and results note"
date: "8 October 2026"
---

# Summary

We used the 2024 Malawi DHS to compare six community profiles on four themes: household
wealth, wealth inequality, informal (non-wage) work, and seasonal work. We then repeated
the analysis with the 2015-16 DHS. All estimates are direct, design-based survey
estimates. Their confidence intervals cover two sources of
error: sampling error, and uncertainty about which catchment each displaced DHS cluster
actually falls in.

- **Wealth separates the two urban profiles from the rest.** Most residents of
  profiles 1 (Urban seasonal-mobility corridor) and 2 (High-income urban) are in the
  national richest quintile (79% and 87%). In profiles 3-5, 24-40% of residents are in the
  national poorest 40%. The rural border profile (5) is the poorest of the well-measured
  profiles.
- **Relative asset inequality is lower in the urban profiles than in profiles 3-5.**
  The wealth-score Gini depends on an arbitrary zero point, so we computed it two ways.
  - *National zeroing*: 0.20-0.26 in the urban profiles and 0.30-0.34 in profiles 3-5.
  - *Groupwise zeroing* (the DHS report method): 0.26-0.28 in the urban profiles and
    0.42-0.48 in profiles 3-5.
  - Under both versions, profile 3 (up-and-coming suburban and peri-urban) is the most
    unequal, and the gap between profiles 2 and 3 is significant after adjustment.
  - Whether the rural border profile (5) is less unequal than the peri-urban profiles
    depends on the zero point, so that comparison is not robust.
  - Profile 1 has the widest *absolute* spread of wealth (SD) but a moderate Gini,
    because its average wealth is high.
- **Non-cash and non-wage work rises from urban to rural profiles.** Among working women,
  non-wage work rises from 44% in profile 1 to 75% in profile 5. Among working women in
  the rural border profile, 57% have no cash earnings, against 21% in profile 1.
- **Seasonal and occasional work is the norm outside the high-income urban profile.**
  Among working adults, 58-66% in profiles 3-5 work seasonally or occasionally, against
  30% in profile 2. The type differs: in profile 1, irregular work is mostly *occasional*
  (24%); in profiles 3-5, it is mostly *seasonal* (41-49%).
- **Profiles 2 and 6 rest on few clusters.** Profile 2 (High-income urban) has about 8
  expected DHS clusters; treat it with caution. Profile 6 (Rural/remote) has about 3, and
  its estimates are uninformative. Their point estimates appear in the tables, but no
  conclusions should rest on them.
- **The 2015-16 DHS shows the same profile ordering.**
  - The urban profiles are the wealthiest, and the rural border and remote profiles the
    poorest.
  - Asset inequality is again highest in profile 3 under both zero points (Gini 0.41
    with national zeroing, 0.49 with groupwise zeroing).
  - Seasonal work again rises from urban to rural.
- **Work became more monetised between the surveys.** Non-wage work among working women
  fell in every well-measured profile, for example from 82% to 61% in profile 3 and from
  79% to 75% in profile 5. Women's no-cash work fell by 12-38 points in profiles 3-5.
- **Some changes may come partly from changes in the survey.** The share of workers in
  agricultural occupations roughly halved, and occasional work rose. Part of these shifts
  may reflect how the two DHS rounds code occupations and ask about work.

# Background

The RESPOND community profiles group 43 health-facility catchments in seven districts into
six types, based on GIS indicators and community workshop input. The 2024 DHS adds
household survey measures of wealth and livelihoods that the profiles were not built from.
This makes it an independent check on how the profiles differ socially and economically.
We chose these four themes because each plausibly affects HIV treatment continuity:
income level, inequality, reliance on unwaged or non-cash work, and work that moves with
the seasons.

DHS clusters are sampled to represent districts, not catchments. Our profiles are
therefore *unplanned domains*. The number of sampled clusters in each profile is
small and random, and DHS displaces cluster GPS points by up to 2-10 km to protect
respondents. Both problems sit at the centre of small area estimation (Pfeffermann 2013;
Wakefield, Okonek and Pedersen 2020). The approach below handles them for direct estimates.
A model-based extension is described under next steps.

# Data

We analysed the two most recent Malawi DHS surveys separately, using the same methods.

| Source | 2024 DHS | 2015-16 DHS |
|:---|:---|:---|
| Household recode (HR) | 22,409 households with a wealth index, in 769 clusters. The 24 Dzaleka refugee camp clusters have no wealth index. | 26,337 households with a wealth index and household members, in 850 clusters |
| Women's recode (IR) | 20,849 women aged 15-49 with completed interviews | 24,562 women aged 15-49 with completed interviews |
| Men's recode (MR) | 8,125 men aged 15-49 with completed interviews (men aged 50-54 dropped) | 7,138 men aged 15-49 with completed interviews (men aged 50-54 dropped) |
| Cluster GPS (GE) | 793 displaced cluster points | 850 displaced cluster points |
| Strata | 58 (district by urban/rural, with the four cities separate) | 56 (28 districts by urban/rural) |
| Population prior | WorldPop 2024 constrained population, 100 m | WorldPop 2016 constrained population, 100 m |

**Profiles.** We used the final expert-vetted profiles (2026-03-16): 43 catchments. The
excluded "ABC only" catchment is left out. The same catchment boundaries are applied to
both surveys, so the 2015-16 estimates describe today's profile areas as they were in
2015-16.

Expected DHS clusters and average sample sizes in each profile, 2024:

{{profile_key_mwi_2024}}

*Expected clusters* is the sum, over all clusters, of the probability that the cluster lies
in the profile (see Methods). *Clusters (naive)* counts clusters whose displaced point falls
inside the profile. Household and respondent counts are averaged across imputations.

# Methods

## Indicators

| Theme | Indicator | Population | Definition |
|:---|:---|:---|:---|
| Wealth | Mean wealth score | De jure household population | DHS wealth index factor score (`hv271`/100,000); national mean ≈ 0 |
| Wealth | Poorest 40%; quintile composition | De jure household population | National wealth quintiles (`hv270`) |
| Inequality | Gini coefficient (national zeroing) | De jure household population | Gini of the wealth score, shifted so that the poorest household nationally scores 0 |
| Inequality | Gini coefficient (groupwise zeroing) | De jure household population | Gini of the wealth score, shifted so that the poorest household in each profile scores 0 (the DHS report method) |
| Inequality | SD of wealth score | De jure household population | Absolute dispersion; does not depend on the shift |
| Work | Worked in last 12 months | Adults 15-49 | `v731` in past year, currently working, or on leave |
| Informal work | Non-wage work | **Women** 15-49 who worked | Self-employed or works for a family member (`v719`), or not paid / paid in kind only (`v741`) |
| Informal work | No cash earnings | Adults 15-49 who worked | Not paid or paid in kind only (`v741`) |
| Informal work | Agricultural occupation | Adults 15-49 who worked | Agriculture, self-employed or employee (`v717`) |
| Seasonal work | Seasonal or occasional work | Adults 15-49 who worked | Works seasonally or occasionally rather than all year (`v732`); each part is also reported separately |

The 2024 men's questionnaire did not ask who the respondent works for (`mv719`). The
full non-wage definition is therefore available for women only. We tested a proxy for
men: no cash earnings, or self-employed in agriculture. Among working women, it agreed
with the full definition for only 76% of respondents and caught 68% of non-wage workers,
so we dropped it. For men and for both sexes combined, we report no cash earnings and
agricultural occupation separately.

Wealth indicators are weighted by person (household weight × de jure members), as in DHS
wealth tables. Estimates for both sexes combined rescale each sex's normalised weights to
its weighted population aged 15-49, taken from the household roster.

**About the Gini.** DHS does not measure income or consumption. The wealth score
(`hv271`) is a principal-components asset index. It is centred on the national mean and
has no true zero: a household with no assets does not score 0. A Gini coefficient
needs non-negative values measured from a meaningful zero, because it compares each
group's share of the total. So any Gini on this score depends on an origin chosen by the
analyst. We report two versions:

- **National zeroing.** Scores are shifted so that the poorest household in the national
  sample scores 0. Every profile is measured from the same origin, so the profiles'
  Gini values are on a common scale.
- **Groupwise zeroing.** Scores are shifted so that the poorest sampled household *in each
  profile* scores 0. This follows the DHS Program's method for the Gini in standard
  report Table 2.6, which takes the minimum within each group reported (national,
  urban/rural, region). Each profile then has its own origin, set by a single
  household. The minimum is recomputed in every location imputation, because profile
  membership changes between imputations. The variance ignores the sampling variability
  of the minimum itself, as DHS does.

Shifting the score so it has a zero point is common practice. Even so, Wittenberg and
Leibbrandt (2017) show that Gini coefficients change with the shift and that the
ordering of subgroups can reverse. We therefore report both versions and treat a
finding as robust only if both agree. Neither version can be compared with published
consumption Gini coefficients. We also report the standard deviation of the score,
which measures absolute spread and does not depend on any choice of origin (McKenzie
2005).

## Design-based domain estimation

Each estimate is a Hájek ratio estimator for the profile, computed on the full national
design. The design uses 58 strata (district by urban/rural), clusters as primary sampling
units, and DHS weights. Variances are Taylor-linearised (`survey`). Gini coefficients and
their variances come from `convey::svygini`, which uses the linearisation behind the
`laeken` indicators (Alfons and Templ 2013). Estimating on the full design keeps the
variance honest about the random number of sampled units in each profile.

## Accounting for GPS displacement

Most catchments are only a few kilometres across. Assigning clusters by their displaced
coordinates would put a sizeable share of them in the wrong profile. For each cluster
within 10.5 km of a profiled catchment (171 clusters in 2024 and 181 in 2015-16), we
computed the posterior probability that the cluster's true location lies in each
catchment:

> p(x | y) ∝ k(|y − x|) / A(x) × pop(x) × 1[x in the cluster's district]

The terms are:

- **y** is the displaced point and **x** a candidate true location.
- **k** is the DHS displacement kernel (Burgert et al. 2013). The distance is uniform up
  to 2 km for urban clusters and 5 km for rural ones, with 1% of rural clusters up to
  10 km, and the angle is uniform. Together these give a 2-D density proportional to 1/r.
- **A(x)** is the chance that an unrestricted displacement from x stays inside its
  district. DHS redraws any displacement that leaves the district, so this term corrects
  for it. The restricting districts differ between surveys. In 2024 they are the 28
  districts plus the four cities. In 2015-16 they are the 28 districts, with the cities
  inside them.
- **pop(x)** is the WorldPop population for the survey year (2024 or 2016). It reflects
  the fact that DHS records the centre of a populated place.

The posterior was approximated with 20,000 Monte Carlo candidates per cluster. The minimum
effective sample size was 163 in 2024 and 926 in 2015-16. No cluster needed a fallback.

We then drew profile membership for each cluster {{n_imputations}} times from these
probabilities. Each draw was estimated in full, and the draws were combined with Rubin's
rules (Rubin 1996), using Barnard-Rubin degrees of freedom. Each draw's complete-data
degrees of freedom are the domain's clusters minus its strata, as recommended for small
domains (Korn and Graubard 1999).

Confidence intervals by indicator type:

- **Proportions:** Korn-Graubard intervals (Clopper-Pearson at the design effective
  sample size).
- **Gini:** logit-scale intervals.
- **SD:** log-scale intervals.
- **Means and differences:** t intervals.

Pairwise profile differences were computed within each draw from the joint domain
covariance and pooled the same way. P-values are Holm-adjusted across the 15 profile pairs
for each indicator.

## Reliability rules

Estimates are flagged when a profile has fewer than 10 expected clusters (*caution*,
marked * in tables) or fewer than 5 (*unreliable*, marked †). With so few clusters, the
design-based variance estimate is itself unstable. No average denominator fell below the
DHS suppression threshold of 25.

# Results: 2024 DHS

Values are estimates with 95% CIs. Profile numbers follow the key above. Shares are
percentages; wealth scores and Gini coefficients are in their own units.

{{headline_table_mwi_2024}}

![Headline indicators by community profile. Hollow points: caution; crosses: unreliable. Arrows: the interval runs past the panel edge.](mwi_2024/fig_dhs_indicators.png)

## Household wealth

The two urban profiles are far wealthier than the others. Their mean wealth scores are
1.3-1.5 points above the national mean, about 1.5 national standard deviations (the
national SD is 0.90), and 79-87% of their residents are in the national top quintile. Profiles 3-5 sit at or below the national mean.
In each of them, roughly a quarter to two-fifths of residents fall in the national poorest
40%. After Holm adjustment, profiles 1 and 2 differ significantly from profile 3 and
profile 5 on mean wealth, and profile 2 also differs from profile 4.

![Composition of each profile by national wealth quintile (point estimates).](mwi_2024/fig_dhs_wealth_quintiles.png)

The quintile bars show a contrast within the non-urban profiles. Profiles 3 and 4 draw
residents almost evenly from all five national quintiles. The rural border profile (5)
is concentrated in the bottom three quintiles, with only 13% in the top.

![Wealth score distribution by profile, weighted by each cluster's probability of lying in the profile.](mwi_2024/fig_dhs_wealth_density.png)

## Wealth inequality

![Lorenz curves of the wealth score with national zeroing, by profile.](mwi_2024/fig_dhs_lorenz.png)

The two Gini versions agree on most of the ordering:

| Gini of wealth score | Profile 1 | Profile 2 | Profile 3 | Profile 4 | Profile 5 |
|:---|---:|---:|---:|---:|---:|
| National zeroing | 0.26 | 0.20 | 0.34 | 0.34 | 0.30 |
| Groupwise zeroing | 0.28 | 0.26 | 0.48 | 0.42 | 0.43 |

Groupwise values are higher throughout. Each profile's poorest household sits above
the national poorest, so the shift is smaller and the spread is larger relative to the
mean. The findings that hold under both versions:

- the urban profiles (1 and 2) are less unequal than profiles 3-5;
- profile 3 is the most unequal;
- the difference between profiles 2 and 3 is significant after Holm adjustment (profile
  2 against 4 is significant only with national zeroing).

The rural border profile (5) looks less unequal than the peri-urban profiles with
national zeroing (0.30 against 0.34). With groupwise zeroing it is about as unequal
(0.43 against 0.42-0.48). That comparison therefore depends on the chosen zero and
should not be reported as a finding.

Absolute dispersion tells a different story. Profile 1 has the largest standard
deviation of the wealth score (1.21), and profile 5 the smallest (0.62). Two patterns
underlie this:

- **Profile 1:** large absolute gaps between wealthy households and poorer neighbours,
  set against a high mean.
- **Profiles 3 and 4:** a socially mixed population with a lower mean, so similar
  absolute gaps are large *relative* to typical wealth.

The rural border profile is poor and comparatively homogeneous. Which measure matters
depends on the question. Gini fits questions of relative deprivation and social
comparison. SD fits questions about how far apart households' material circumstances are.

## Informal and non-wage work

Among working women, non-wage work rises steadily from profile 1 (44%) through
profiles 2-4 (57-65%) to the rural border profile (75%). The gap between profiles 1 and 5
is large (31 points). It is significant before adjustment (p = 0.004) and borderline
after (Holm p = 0.064).

No cash earnings follows the same pattern for women: 57% in profile 5 against 21% in
profile 1, a difference that remains significant after adjustment. Among men, the share
with no cash earnings is similar across profiles 1 and 3-5 (25-36%). Agricultural
occupation rises from under 10% in the urban profiles to about a third in the rural border
profile. Among women the gap between profiles 1 and 5 (2% against 40%) remains significant
after adjustment.

The agricultural shares are lower than many readers would expect. DHS codes much casual
farm labour (*ganyu*) as "unskilled manual" rather than agriculture, so this indicator
understates how much people depend on farming.

{{sex_table_mwi_2024}}

![Work indicators by sex.](mwi_2024/fig_dhs_work_by_sex.png)

## Seasonal and occasional work

Most working adults outside the high-income urban profile do not work all year: 58-66% in
profiles 3-5, and 41% in profile 1. In profiles 3-5 this is mainly *seasonal* work
(41-49%), consistent with the farming calendar. Profile 1 has the highest share of
*occasional* work (24%), which fits its label as a seasonal-mobility corridor:
irregular, short-term urban work rather than farm seasons. Seasonal work is
significantly lower in profiles 1 and 2 than in profile 5 after adjustment, for all adults
and for women.

Men report more seasonal or occasional work than women in profiles 1-4. The pattern
reverses in the rural border profile, where 65% of working women do, against 55% of working
men.

## Differences between profiles

![Pairwise differences between profiles (row minus column), Holm-adjusted.](mwi_2024/fig_dhs_contrasts.png)

With six profiles and only 3-15 clusters each, few pairwise differences survive
adjustment for multiple comparisons. The robust contrasts are:

- urban (1 and 2) versus peri-urban (3) and rural border (5) on mean wealth and the
  poorest 40%;
- urban (1 and 2) versus rural border (5) on seasonal work;
- high-income urban (2) versus peri-urban (3 and 4) on Gini;
- urban versus rural border (5) on cash earnings: profile 1 for women, and profile 2 for
  men and for all adults;
- urban corridor (1) versus rural border (5) on women's agricultural work.

Many other gaps of 15-30 percentage points are consistent in direction, but too imprecise
to confirm.

## How much does cluster location matter?

![Displacement-aware (multiple imputation) versus naive (displaced point in polygon) estimates.](mwi_2024/fig_dhs_sensitivity.png)

Location uncertainty accounts for a median of 21% of total variance. This ranges from 6%
in profile 2 to 40% in profile 6. Point estimates move by a median of 0.1-0.5 standard
errors relative to the naive assignment, and the displacement-aware standard errors are
about 10-20% larger for profiles 1 and 3-6. The naive approach therefore gives broadly
similar point estimates, but its intervals are falsely precise. Profile 2 is the exception:
its displacement-aware standard errors are about 15% *smaller* than the naive ones. Naive
assignment finds only 6 of its clusters, while the imputations draw on about 8 on average,
almost all of them urban Lilongwe clusters with similar wealth.

![DHS clusters near profiled catchments, sized by the probability of lying in a profiled catchment.](mwi_2024/fig_dhs_cluster_map.png)

The map shows why profile 6 cannot be estimated. Its Kasungu catchment (Kaluluma) has no
DHS cluster within range, and its Lilongwe catchment (Chioza) contributes about 0.4 expected
clusters. Nearly all of its 3.3 expected clusters come from the two Mulanje catchments
(Chambe and Chisitu). Its estimates therefore describe those two catchments rather than
the profile as a whole.

## Sensitivity to degrees of freedom

The primary intervals use domain-level degrees of freedom, which is conservative. Many
DHS subgroup tables instead use the full survey's degrees of freedom (735). Intervals on
that basis are below. They mainly narrow the intervals for profiles 2 and 6.

{{full_df_table_mwi_2024}}

# Results: 2015-16 DHS

The 2015-16 survey was designed for district-level estimates and has more clusters
nationally (850) than the 2024 survey (793). The study areas gain from this unevenly:

- the urban profiles have fewer expected clusters than in 2024 (about 10 and 6);
- the peri-urban and rural profiles have more;
- profile 6 has about 6 expected clusters (against 3 in 2024). Its estimates are marked
  *caution* rather than *unreliable*, but its intervals are still very wide.

Expected DHS clusters and average sample sizes in each profile, 2015-16:

{{profile_key_mwi_2016}}

The men's questionnaire did not ask who the respondent works for in 2015-16 either, so
non-wage work is again reported for women only.

Values are estimates with 95% CIs, with profile numbers as in the key. The wealth score
and quintiles are relative to Malawi in 2015-16 (see the next section).

{{headline_table_mwi_2016}}

![Headline indicators by community profile, 2015-16 DHS.](mwi_2016/fig_dhs_indicators.png)

**Wealth.** The ordering is the same as in 2024:

- The urban profiles (1 and 2) are the wealthiest. About 65% of their residents are in
  the national richest quintile, which is lower than in 2024, and their intervals are
  wide.
- The rural border (5) and rural/remote (6) profiles are the poorest. Their mean scores
  are about 0.3-0.4 below the national mean, and only 4-7% of residents are in the
  richest quintile.

![Composition of each profile by national wealth quintile, 2015-16 DHS.](mwi_2016/fig_dhs_wealth_quintiles.png)

**Inequality.** Profile 3 is again the most unequal under both zero points:

| Gini of wealth score | Profile 1 | Profile 2 | Profile 3 | Profile 4 | Profile 5 | Profile 6 |
|:---|---:|---:|---:|---:|---:|---:|
| National zeroing | 0.31 | 0.31 | 0.41 | 0.37 | 0.31 | 0.29 |
| Groupwise zeroing | 0.35 | 0.35 | 0.49 | 0.38 | 0.40 | 0.41 |

- **National zeroing:** profile 4 comes second.
- **Groupwise zeroing:** profiles 4-6 are close together, and the rural profiles (5 and
  6) rise above profile 4.
- **Significance:** no Gini difference survives Holm adjustment in 2015-16 under either
  version.
- **Absolute dispersion (SD)** falls steadily from the urban profiles (1.3-1.4) to the
  rural ones (0.5). The SD gap between profiles 3 and 5 is significant after adjustment.

![Lorenz curves of the wealth score with national zeroing, by profile, 2015-16 DHS.](mwi_2016/fig_dhs_lorenz.png)

**Work.**

- **Non-wage work** among working women rises from 56% in profile 1 to about 80% in
  profiles 3-5.
- **No cash earnings** among working women rises from 21% to 69% between profiles 1 and
  5. This gap remains significant after adjustment.
- **Agricultural occupation** rises from about 10% in the urban profiles to over half of
  workers in profiles 3 and 5. The profile 1 against 5 gap among women (9% against 67%) is
  significant after adjustment.
- **Seasonal or occasional work** rises from about a third of workers in the urban
  profiles to 62-68% in profiles 4-6. Most of it is seasonal; occasional work is 5-13%
  everywhere.

{{sex_table_mwi_2016}}

![Work indicators by sex, 2015-16 DHS.](mwi_2016/fig_dhs_work_by_sex.png)

![Pairwise differences between profiles, 2015-16 DHS (row minus column), Holm-adjusted.](mwi_2016/fig_dhs_contrasts.png)

With fewer urban clusters than in 2024, only three pairwise contrasts survive Holm
adjustment:

- women's agricultural work, profile 1 against 5;
- women's no-cash work, profile 1 against 5;
- the SD of wealth, profile 3 against 5.

The direction of nearly all other gaps matches 2024.

![DHS 2015-16 clusters near profiled catchments.](mwi_2016/fig_dhs_cluster_map.png)

Intervals using the full-survey degrees of freedom, 2015-16:

{{full_df_table_mwi_2016}}

# Comparing 2015-16 and 2024

The two surveys use the same work questions, so work indicators can be compared directly.
The wealth score and quintiles are rebuilt for each survey from that year's assets, so
their levels cannot be compared across years. The share of a profile in the national
poorest 40% *can* be compared, but only as the profile's position within each year's
national distribution.

{{comparison_table}}

What changed:

- **Relative position.** The profiles hold the same order in the national distribution.
  The urban profiles moved further from the bottom: 9-12% in the poorest 40% in 2015-16,
  against 0-4% in 2024. The peri-urban and rural border profiles stayed at 24-40%.
- **Non-wage and no-cash work fell.** Non-wage work among working women fell by 4-21
  points in every well-measured profile. No cash earnings among working adults fell by
  9-22 points in profiles 3-5, but not in the urban profiles. This fits a gradual move toward paid work. The intervals for
  single profiles overlap, but the change is consistent across profiles.
- **Agricultural occupation fell sharply** in the peri-urban and rural profiles, for
  example from 55% to 23% in profile 3. **Occasional work rose** from 5-13% to 11-24%.
  Changes this large over eight years should be treated with care. DHS-8 changed how
  occupations are coded and how work is asked about. Part of the shift is likely to be
  casual farm labour (*ganyu*) recorded as unskilled manual or occasional work rather than
  agricultural work.
- **Seasonal or occasional work overall** stayed in the range of roughly 53-68% in the
  peri-urban and rural profiles, and 30-40% in the urban ones.

Across the two surveys, the profiles' relative ordering on wealth, inequality and
seasonal work is the same. This independent replication supports the profile typology:
different survey samples, eight years apart, place the profiles in the same order.

# Interpretation for RESPOND

- The DHS confirms the main economic gradient built into the profiles. Urban profiles
  are richer, have more cash income, and have less seasonal work. Peri-urban and rural
  border profiles are poorer, less monetised, and more seasonal. This is outside
  validation of the profile typology: the DHS measures were not used to build the
  profiles.
- The two urban profiles look alike on wealth but differ in kind of work. The urban
  corridor (1) has more occasional work and more inequality than the high-income
  profile (2). This supports keeping them separate, although profile 2's few clusters
  limit what can be said.
- Profile 3 is the most internally unequal in asset terms, in both surveys and under both
  zero points. Services designed for the "typical" household may miss a large poor
  minority in these communities.
- In profiles 3-5, seasonal work combined with non-cash earnings implies cash-flow
  shortages at predictable times of year. This is a plausible mechanism for interruptions
  in treatment (for example, transport costs to clinics). It is worth testing against
  the DHAMIS treatment-interruption data by month.

# Limitations

- **Small domains.** Profiles 2 and 6 have about 8 and 3 expected clusters. Direct
  estimates for them are imprecise, and for profile 6 uninformative.
- **Relative wealth measure.** The DHS wealth index is a relative asset index. It is not
  income, and the Gini depends on the chosen origin.
- **Men's informal work.** Men's informal work can only be described through cash
  earnings and occupation.
- **Definitions are DHS's.** Occupation coding and the seasonal/occasional distinction
  are respondent-reported, using DHS categories.
- **Location model assumptions.** The location model assumes that DHS's displacement
  boundaries match the Naomi district and city polygons we used, and that 2024 WorldPop
  is a good prior for where populated places sit.
- **Urban/rural status is not used.** The prior does not use each cluster's urban/rural
  class, so outside the four cities a rural cluster can be placed in a town, and the
  reverse. A sensitivity run with an urban extent mask is a worthwhile check.
- **Comparing across years.** The 2015-16 estimates apply today's catchments and profile
  assignments to 2015-16 cluster locations. They describe these areas in 2015-16, not
  how the areas were defined then. Wealth scores are relative to each survey year.
  Occupation coding and work questions changed between DHS-7 and DHS-8.
- **Profiles cover only parts of districts.** The profiles' catchments cover parts of
  seven districts. The estimates describe the sampled population living in those
  catchments, not whole districts.

# Next steps

1. **Model-based small area estimates.**
   - A geostatistical model (`mbg`) fitted to cluster-level outcomes, with the existing
     covariate stack and the DHS covariate extract (GC), would borrow strength across
     space.
   - It would produce catchment-level surfaces aggregated by population, which would
     stabilise profiles 2 and 6.
   - For the Gini, an empirical best predictor approach would simulate household-level
     wealth within catchments (Molina and Rao 2010; Corral et al. 2022).
   - Comparing direct and model-based estimates is the standard check in SAE (Fuglstad,
     Li and Wakefield 2021).
2. **Pool with the 2015-16 MDHS** for indicators that are stable over time, to roughly
   double the clusters available.
3. **Link to HIV outcomes.** Relate seasonal-work and cash-earnings profiles to the timing
   of treatment interruption in DHAMIS.

# Decisions made during the analysis

These choices were made without consultation and are recorded for review.

{{decision_log}}

# Files

All outputs are in `~/data/respond-community-profiles/dhs/`:

- `analysis/2026-10-08/` holds this writeup (`.md` and `.docx`), with each survey's
  outputs in `mwi_2024/` and `mwi_2016/`;
- `prepared/2026-10-08/mwi_2024/` and `.../mwi_2016/` hold the prepared inputs (cleaned
  DHS records and cluster location probabilities);
- `logs/` holds run logs, and `decision_log.md` the decisions listed above.

Each survey's analysis folder holds:

- `dhs_profile_estimates.csv`: all indicators by profile and population group, with:
  - estimates, SEs, primary and full-df CIs, and degrees of freedom;
  - the share of variance from location uncertainty;
  - average cluster and respondent counts, and the reliability flag.
- `dhs_profile_contrasts.csv`: all pairwise profile differences with raw and
  Holm-adjusted p-values.
- `dhs_profile_wealth_quintiles.csv`: quintile composition by profile.
- `dhs_profile_sample_sizes.csv`: expected and naive cluster counts, and respondent
  counts.
- `dhs_profile_estimates_naive_assignment.csv`: the sensitivity analysis.
- `dhs_profile_estimates_by_draw.csv.gz`: estimates from each imputation.
- `fig_dhs_*.png`: figures.

Code: `R/dhs_ingest.R`, `R/dhs_displacement.R`, `R/dhs_estimation.R`, `R/dhs_paths.R`,
and `inst/scripts/dhs/01-04` in the `mwi.hiv.factors` repository. Scripts 01-03 take the
survey as an argument (`Rscript 01_prepare_dhs.R mwi_2016`); script 04 renders both.
Configuration, including each survey's files, is in the `dhs` block of `config.yaml`.

# References

Alfons A, Templ M (2013). Estimation of social exclusion indicators from complex surveys:
the R package laeken. *Journal of Statistical Software* 54(15).

Burgert CR, Colston J, Roy T, Zachary B (2013). *Geographic displacement procedure and
georeferenced data release policy for the Demographic and Health Surveys.* DHS Spatial
Analysis Reports No. 7. ICF International.

Corral P, Molina I, Cojocaru A, Segovia S (2022). *Guidelines to small area estimation for
poverty mapping.* World Bank.

Fuglstad G-A, Li ZR, Wakefield J (2021). The two cultures for prevalence mapping: small
area estimation and spatial statistics. arXiv:2110.09576.

ICF (n.d.). Wealth quintiles. In *Guide to DHS Statistics (DHS-8)*. The DHS Program.
https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Wealth_Quintiles.htm

Korn EL, Graubard BI (1999). *Analysis of Health Surveys.* Wiley.

McKenzie DJ (2005). Measuring inequality with asset indicators. *Journal of Population
Economics* 18(2): 229-260.

Molina I, Rao JNK (2010). Small area estimation of poverty indicators. *Canadian Journal
of Statistics* 38(3): 369-385.

Pfeffermann D (2013). New important developments in small area estimation. *Statistical
Science* 28(1): 40-68.

Rubin DB (1996). Multiple imputation after 18+ years. *Journal of the American
Statistical Association* 91(434): 473-489.

Wakefield J, Okonek T, Pedersen J (2020). Small area estimation for disease prevalence
mapping. *International Statistical Review* 88(2): 398-418.

Wittenberg M, Leibbrandt M (2017). Measuring inequality by asset indices: a general
approach with application to South Africa. *Review of Income and Wealth* 63(4): 706-730.
