## Initial release (version 1.1.0)

## Response to previous check comments
* DESCRIPTION has been shortened appropriately
* Missing Rd tags have been added for all exported functions
* Examples that previously used non-exported functions have been fixed
* `\dontrun` was replaced with `\donttest` throughout, but this is likely to cause problems in any later CRAN checks because many of these examples take some time to run due to the need for 'Stan' models to compile. Using `--run-donttest`, the examples take between 38 and 50 minutes, depending on the test environment. A very similar R package that is on CRAN ('brms') sticks to the `\dontrun` convention because of this, so I have elected to only use `\donttest` in the examples for the package's primary function `mvgam()` 
* `cat()` has been replaced with `message()` throughout
* `onexit()` has been used as suggested to ensure the user's `par` is not changed

## Test environments
* Windows install: R 4.3.1
* win-builder: R-devel
* win-builder: R-release
* ubuntu-latest: R-release
* ubuntu-latest: R-devel
* macOS-latest: R-release

## R CMD check results
* There were no ERRORs or WARNINGs. There were 2 NOTEs due to listing 'cmdstanr' in Suggests. This package is not a dependency but provides an additional backend option for users to select when fitting 'Stan' models, if they wish. A similar package that has been available on CRAN for quite some time ('brms') uses the same convention.

Maintainer: 'Nicholas J Clark <nicholas.j.clark1214@gmail.com>'
