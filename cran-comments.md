## Initial release (version 1.1.0)

## Response to previous check comments
* DESCRIPTION has been shortened appropriately
* Missing Rd tags have been added for all exported functions
* Examples that previously used non-exported functions have been fixed
* \dontrun has been replaced with \donttest throughout
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
* There were no ERRORs or WARNINGs

Maintainer: 'Nicholas J Clark <nicholas.j.clark1214@gmail.com>'
