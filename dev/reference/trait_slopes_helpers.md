# Trait-mediated environmental slopes for `jsdgam()`

Internal helpers that rewrite `(obs_formula, trait_slopes)` into the
brms non-linear formula that parameterises the fourth-corner regression

## Details

\$\$\beta\_{s,k} = \sum_l T\_{s, l}\\ \gamma\_{l, k} + \epsilon\_{s,
k}\$\$

where every fixed slope on an environmental covariate becomes a
trait-regressed nlpar carrying its own species-level random deviation.
The rewrite expands

`obs_formula = y ~ env1 + env2` `trait_slopes = ~ trait1`

into

`bf(y ~ a + b1 * env1 + b2 * env2, a ~ trait1 + (1 | sp | species), b1 ~ trait1 + (1 | sp | species), b2 ~ trait1 + (1 | sp | species), nl = TRUE)`

The `|sp|` correlated-random-effect tag jointly parameterises the
species-level intercept and slope deviations under an LKJ prior so the
trait wrapper composes with the rest of the jsdgam factor model. The
actual nl plumbing (priors, parameter aliasing, discovery for
marginaleffects) is in `R/insight.mvgam.R`, `R/as.data.frame.mvgam.R`
and `R/conditional_effects.mvgam.R`.
