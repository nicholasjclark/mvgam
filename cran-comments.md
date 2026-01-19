## Version 1.1.594

## Summary of changes
This version is a minor patch update to fix a test that spawned more than two cores. It also brings several cosmetic updates to improve the way summaries are printed and stored. There are no major structural changes or modifications that would break pre-existing workflows

## Test environments
* Windows install: R 4.4.3
* win-builder: R-devel
* win-builder: R-release
* ubuntu-latest: R-release
* ubuntu-latest: R-devel
* macOS-latest: R-release

## R CMD check results
* There were no ERRORs or WARNINGs. There were 2 NOTEs due to listing 'cmdstanr' in Suggests. This package is not a dependency but provides an additional backend option for users to select when fitting 'Stan' models, if they wish. A similar package that has been available on CRAN for quite some time ('brms') uses the same convention. I have included the `Additional_repositories` field in the DESCRIPTION to appropriately tell users where they can find this package.

* There is one compilation WARNING about RcppArmadillo fallback compilation that appears in the CRAN check system. This warning originates from the RcppArmadillo system headers (suggesting to define -DARMA_USE_CURRENT) and is not related to any code in the mvgam package. It is a system-level compilation flag recommendation that would typically be addressed by system administrators or RcppArmadillo maintainers.

## `valgrind` memory check results
* Running all examples using `--run-donttest`, and all package tests (including those skipped on CRAN) with `R -d "valgrind --tool=memcheck --leak-check=full"` resulted in no WARNINGs or ERRORs

Maintainer: 'Nicholas J Clark <nicholas.j.clark1214@gmail.com>'
