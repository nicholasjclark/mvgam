## Version 2.0.0

## Summary of changes
This is a major release that builds the package on 'brms'. Observation models now use the full 'brms' formula syntax. A separate `trend_formula` specifies the latent process model. Estimation is still performed in 'Stan'. Because the modelling interface has changed, this release contains breaking changes, which are described in NEWS.md. The `trend_model` argument is replaced by `trend_formula`. Priors are now inspected with the `get_prior()` and `default_prior()` methods in place of `get_mvgam_priors()`.

## Test environments
* win-builder: R-devel
* win-builder: R-release
* ubuntu-latest: R-release
* ubuntu-latest: R-devel
* macOS-latest: R-release

## R CMD check results
* There were no ERRORs or WARNINGs. There is a NOTE from listing 'cmdstanr' in Suggests. 'cmdstanr' is an optional dependency that provides an additional 'Stan' backend users may select when fitting models. 'brms' follows the same convention. The DESCRIPTION includes the `Additional_repositories` field to tell users where to obtain the package.

## `valgrind` memory check results
* We ran all examples with `--run-donttest` and all package tests, including those skipped on CRAN, under `R -d "valgrind --tool=memcheck --leak-check=full"`. The runs produced no WARNINGs or ERRORs.

Maintainer: 'Nicholas J Clark <nicholas.j.clark1214@gmail.com>'
