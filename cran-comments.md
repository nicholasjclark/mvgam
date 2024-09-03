## Version 1.1.3

## Summary of changes
This version brings a few efficiency updates and added functionality to enhance the user interface. There are no major structural changes or modifications that would break pre-existing workflows

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
