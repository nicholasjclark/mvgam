## Version 2.0.0

## Summary of changes
This is a major release that rebuilds the package on 'brms'. Observation models now use the full 'brms' formula syntax. A separate `trend_formula` specifies the latent process model. Estimation is still performed in 'Stan'. Because the modelling interface has changed, this release contains breaking changes, all of which are listed in NEWS.md. The `trend_model` argument is replaced by `trend_formula`. Priors are inspected with the `get_prior()` and `default_prior()` methods in place of `get_mvgam_priors()`. Several deprecated functions have been removed in favour of S3 methods.

## Test environments
* Local: Ubuntu 24.04, R 4.6.2, with `--as-cran`
* Local: Ubuntu 24.04, R 4.6.2, with `_R_CHECK_DEPENDS_ONLY_=true`

## R CMD check results
* There were no ERRORs or WARNINGs.
* There is one NOTE from listing 'cmdstanr' in Suggests. 'cmdstanr' provides an optional 'Stan' backend that users may select when fitting models. 'brms' follows the same convention. The DESCRIPTION includes the `Additional_repositories` field to tell users where to obtain the package. Without 'cmdstanr', models are fitted with 'rstan'.
* Tests and examples that need a suggested package run only when it is installed.

## Reverse dependencies
* There is one reverse dependency, 'marginaleffects', which lists mvgam among the model classes it supports. We ran mvgam's 'marginaleffects' tests against the current CRAN releases of 'marginaleffects' (1.0.0) and 'insight'. All passed.

Maintainer: 'Nicholas J Clark <nicholas.j.clark1214@gmail.com>'
