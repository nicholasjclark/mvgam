## Initial release (version 1.1.0)

## Test environments
* Windows install: R 4.3.1
* win-builder: R-devel
* win-builder: R-release
* ubuntu-latest: R-release
* ubuntu-latest: R-devel
* macOS-latest: R-release

## R CMD check results
* There were no ERRORs
* win-builder and macOS issued WARNINGs that vignettes are not in 'inst/doc', but this is not a true warning because the package uses `VignetteBuilder: knitr` and all vignette files are appropriately located in `doc`

Maintainer: 'Nicholas J Clark <nicholas.j.clark1214@gmail.com>'
