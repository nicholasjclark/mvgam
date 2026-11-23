# Pre-build the vignettes for a CRAN release. Run from the package root.
#
# Each vignette fits models only when NOT_CRAN is "true". This script
# sets it, knits every vignette with its code evaluated and writes the
# .Rmd, the evaluated .html and the purled .R into inst/doc. The package
# is then built and checked with `vignettes = FALSE`. CRAN serves the
# pre-built html and rebuilds the vignettes without fitting anything.

Sys.setenv(NOT_CRAN = "true")
vignettes <- list.files("vignettes", pattern = "\\.Rmd$")
dir.create("inst/doc", recursive = TRUE, showWarnings = FALSE)

# Clear outputs whose vignette is absent from vignettes/
stale <- setdiff(
  list.files("inst/doc", pattern = "\\.(Rmd|R|html)$"),
  c(vignettes, sub("\\.Rmd$", ".R", vignettes),
    sub("\\.Rmd$", ".html", vignettes))
)
file.remove(file.path("inst/doc", stale))

build_vignette <- function(x) {
  rmd <- file.path("vignettes", x)
  html <- sub("\\.Rmd$", ".html", rmd)
  knitr::purl(rmd, output = file.path("inst/doc", sub("\\.Rmd$", ".R", x)),
              quiet = TRUE)
  devtools::build_rmd(rmd, quiet = TRUE)
  file.copy(rmd, "inst/doc", overwrite = TRUE)
  file.copy(html, "inst/doc", overwrite = TRUE)
  file.remove(html)
}

# Two vignettes knit at once. Each knit installs the package into a
# library of its own. Each fit samples its chains in parallel.
built <- parallel::mclapply(vignettes, build_vignette, mc.cores = 2L)
failed <- vapply(built, inherits, logical(1L), "try-error")
if (any(failed)) {
  stop("Vignettes failed to build: ",
       paste(vignettes[failed], collapse = ", "), "\n",
       paste(unlist(built[failed]), collapse = "\n"), call. = FALSE)
}
