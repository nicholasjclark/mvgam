## Initial release (version 1.1.0)

## Response to previous check comments
* DESCRIPTION has been shortened appropriately
* Missing Rd tags have been added for all exported functions
* Code that previously wrote home filespace has been removed
* Examples that previously used non-exported functions have been fixed
* Replacing `\dontrun` with `\donttest` throughout will almost certainly cause problems in any later CRAN checks because many of these examples take some time due to the need for 'Stan' models to compile. Specifically, using `--run-donttest` the examples take between 48 and 60 minutes, depending on the test environment. A very similar R package that is on CRAN ('brms') sticks to the `\dontrun` convention because of this, so I have elected to only use `\donttest` in the examples for the package's primary functions `mvgam()` and `get_mvgam_priors()` 
* `onexit()` has been used as suggested to ensure the user's `par` is not changed
* `options()` have been reset to user defaults in `man` pages as suggested

## Test environments
* Windows install: R 4.3.1
* win-builder: R-devel
* win-builder: R-release
* ubuntu-latest: R-release
* ubuntu-latest: R-devel
* macOS-latest: R-release

## R CMD check results
* There were no ERRORs or WARNINGs. There were 2 NOTEs due to listing 'cmdstanr' in Suggests. This package is not a dependency but provides an additional backend option for users to select when fitting 'Stan' models, if they wish. A similar package that has been available on CRAN for quite some time ('brms') uses the same convention. I have included the `Additional_repositories` field in the DESCRIPTION to appropriately tell users where they can find this package.

Maintainer: 'Nicholas J Clark <nicholas.j.clark1214@gmail.com>'
