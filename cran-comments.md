## Version 1.1.1

## Response to previous check comments
* Changed indexing of an internal c++ function after Prof Brian Ripley’s email: Dear maintainer, Please see the problems shown on https://cran.r-project.org/web/checks/check_results_mvgam.html. Please correct before 2024-05-22 to safely retain your package on CRAN. The CRAN Team. I presume this was triggered by a memory 'Invalid read of size' message from `valgrind`, which occurred in one of the examples and one of the tests. Strangely this behaviour did not occur in other examples that use identical codes, so I suspect it was a false positive. But nevertheless I have made some changes and checked with `valgrind` (see '`valgrind` memory check results' below)
* Also reduced sizes of vignette html files in response to several NOTEs about the large package install size

## Test environments
* Windows install: R 4.3.1
* win-builder: R-devel
* win-builder: R-release
* ubuntu-latest: R-release
* ubuntu-latest: R-devel
* macOS-latest: R-release

## R CMD check results
* There were no ERRORs or WARNINGs. There were 2 NOTEs due to listing 'cmdstanr' in Suggests. This package is not a dependency but provides an additional backend option for users to select when fitting 'Stan' models, if they wish. A similar package that has been available on CRAN for quite some time ('brms') uses the same convention. I have included the `Additional_repositories` field in the DESCRIPTION to appropriately tell users where they can find this package.

## `valgrind` memory check results
* Running all examples using `--run-donttest`, and all package tests (including those skipped on CRAN) with `R -d "valgrind --tool=memcheck --leak-check=full"` resulted in no WARNINGs or ERRORs

Maintainer: 'Nicholas J Clark <nicholas.j.clark1214@gmail.com>'
