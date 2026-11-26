# Pre-submission steps for a CRAN release, in the order they run. Run
# from the package root. Output goes to
# cran_checks/. The win-builder and CRAN steps upload the package. They
# run only when called explicitly.

out_dir <- "cran_checks"
dir.create(out_dir, showWarnings = FALSE)

# 1. The current CRAN check results of the released version
cran_status <- function() {
  utils::browseURL(
    "https://cran.r-project.org/web/checks/check_results_mvgam.html"
  )
}

# 2. URLs and spelling. The /dev/ site pages resolve only once the
# release site deploys.
check_text <- function() {
  list(urls = urlchecker::url_check(),
       spelling = spelling::spell_check_package())
}

# 3. The one reverse dependency. mvgam's marginaleffects tests run
# against the current CRAN releases of marginaleffects and insight,
# installed in a separate library.
check_marginaleffects <- function() {
  lib <- file.path(out_dir, "lib")
  dir.create(lib, showWarnings = FALSE)
  utils::install.packages(c("marginaleffects", "insight"), lib = lib,
                          repos = "https://cloud.r-project.org")
  callr::r(function(lib) {
    .libPaths(c(lib, .libPaths()))
    devtools::load_all(quiet = TRUE)
    testthat::test_dir("tests/testthat", filter = "marginaleffects",
                       reporter = "summary")
  }, args = list(lib = lib), show = TRUE)
}

# 4. Check the package without its vignettes or tests, for a quick
# look at the documentation and examples
check_package <- function() {
  devtools::check(vignettes = FALSE, run_dont_test = FALSE,
                  args = c("--timings", "--no-tests"),
                  check_dir = out_dir)
}

# 5. Build the tarball to submit. Under NOT_CRAN = "true", R CMD build
# knits each vignette with its fits. It writes the evaluated outputs
# and the vignette index into the tarball. On CRAN the vignettes
# rebuild without fitting anything.
build_tarball <- function() {
  withr::with_envvar(c(NOT_CRAN = "true"),
                     devtools::build(path = out_dir, vignettes = TRUE))
}

# 6. The tarball must hold the vignette index. Each vignette needs its
# R script and an html with evaluated output.
check_vignette_outputs <- function(tarball) {
  exdir <- tempfile()
  utils::untar(tarball, exdir = exdir)
  pkg <- list.files(exdir, full.names = TRUE)[1L]
  doc <- file.path(pkg, "inst", "doc")
  rmds <- list.files(file.path(pkg, "vignettes"), pattern = "\\.Rmd$")
  outputs <- c(sub("\\.Rmd$", ".R", rmds), sub("\\.Rmd$", ".html", rmds))
  evaluated <- vapply(rmds, function(x) {
    html <- file.path(doc, sub("\\.Rmd$", ".html", x))
    file.exists(html) &&
      any(grepl("#&gt;|data:image/png", readLines(html, warn = FALSE)))
  }, logical(1L))
  problems <- c(
    no_index = if (!file.exists(file.path(pkg, "build", "vignette.rds"))) {
      "build/vignette.rds"
    },
    missing_output = outputs[!file.exists(file.path(doc, outputs))],
    not_evaluated = rmds[!evaluated]
  )
  if (length(problems)) {
    print(problems)
    stop("Rebuild the tarball with build_tarball().", call. = FALSE)
  }
  message("The tarball's vignettes are indexed, complete and evaluated.")
  invisible(TRUE)
}

# 7. Check the tarball as CRAN will, tests included. NOT_CRAN = "false"
# skips the tests that call `skip_on_cran()` and leaves the vignette
# code unevaluated. CRAN's soft limit on the whole check is 10 minutes.
check_as_cran <- function(tarball) {
  withr::with_envvar(
    c(NOT_CRAN = "false", `_R_CHECK_CRAN_INCOMING_REMOTE_` = "true"),
    rcmdcheck::rcmdcheck(tarball, args = c("--as-cran", "--timings"),
                         check_dir = out_dir, error_on = "never")
  )
}

# 7b. The check CRAN runs without suggested packages. Under
# `_R_CHECK_DEPENDS_ONLY_`, R CMD check links into a fresh library
# only the packages that DESCRIPTION lists in Depends, Imports and
# LinkingTo. A test or example that calls a suggested package without
# guarding it fails here.
check_no_suggests <- function(tarball) {
  withr::with_envvar(
    c(NOT_CRAN = "false", `_R_CHECK_DEPENDS_ONLY_` = "true"),
    rcmdcheck::rcmdcheck(tarball, args = c("--as-cran", "--no-manual"),
                         check_dir = file.path(out_dir, "no_suggests"),
                         error_on = "never")
  )
}

# 8. win-builder release and devel. Results arrive by email to the
# maintainer within about an hour.
submit_win_builder <- function() {
  devtools::check_win_release()
  devtools::check_win_devel()
}

# Usage:
#   cran_status(); check_text(); check_marginaleffects(); check_package()
#   tarball <- build_tarball(); check_vignette_outputs(tarball)
#   check_as_cran(tarball); check_no_suggests(tarball)
#   submit_win_builder()
#   devtools::submit_cran()
