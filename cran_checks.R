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

# 4. Pre-build the vignettes into inst/doc with their code evaluated
build_vignettes <- function() {
  source("build_vignettes_CRAN.R")
}

# 4b. The pre-built vignettes as R CMD check judges them. R CMD check
# warns when any file in vignettes/ is more than 10 s newer than the
# newest file in inst/doc, or than its own copy there. Because a
# tarball keeps file times, an edit or a git checkout after the
# pre-build fails CRAN. Each inst/doc .Rmd must also match its source.
# Each .R must match a fresh purl. Each html must hold evaluated output.
# Pass the package root, or the tarball from `build_tarball()`.
check_vignette_outputs <- function(path = ".") {
  if (grepl("\\.tar\\.gz$", path)) {
    exdir <- tempfile()
    utils::untar(path, exdir = exdir)
    path <- list.files(exdir, full.names = TRUE)[1L]
  }
  vign <- file.path(path, "vignettes")
  doc <- file.path(path, "inst", "doc")
  src <- list.files(vign, recursive = TRUE)
  # A package folder holds files that .Rbuildignore keeps out of the
  # tarball, such as vignettes/articles. R CMD check never sees them.
  ignore_file <- file.path(path, ".Rbuildignore")
  if (file.exists(ignore_file)) {
    patterns <- readLines(ignore_file)
    # A pattern that matches a folder drops everything inside it
    ignored <- vapply(file.path("vignettes", src), function(f) {
      parts <- strsplit(f, "/", fixed = TRUE)[[1L]]
      prefixes <- vapply(seq_along(parts), function(i) {
        paste(parts[seq_len(i)], collapse = "/")
      }, character(1L))
      any(vapply(patterns, function(p) any(grepl(p, prefixes,
                                                  perl = TRUE)),
                 logical(1L)))
    }, logical(1L))
    src <- src[!ignored]
  }
  out <- list.files(doc, recursive = TRUE)
  tol <- 10
  src_time <- file.mtime(file.path(vign, src))
  out_time <- file.mtime(file.path(doc, out))
  shared <- intersect(src, out)
  rmds <- list.files(vign, pattern = "\\.Rmd$")
  # purl() writes the evaluated params, which depend on NOT_CRAN as the
  # pre-build sets it
  purl_matches <- vapply(rmds, function(x) {
    fresh <- withr::with_envvar(
      c(NOT_CRAN = "true"),
      knitr::purl(file.path(vign, x), output = tempfile(), quiet = TRUE)
    )
    identical(readLines(fresh), readLines(file.path(doc, sub(
      "\\.Rmd$", ".R", x))))
  }, logical(1L))
  evaluated <- vapply(rmds, function(x) {
    html <- readLines(file.path(doc, sub("\\.Rmd$", ".html", x)),
                      warn = FALSE)
    any(grepl("#&gt;|data:image/png", html))
  }, logical(1L))
  problems <- c(
    newer_than_all = src[src_time > max(out_time) + tol],
    newer_than_copy = shared[file.mtime(file.path(vign, shared)) >
                               file.mtime(file.path(doc, shared)) + tol],
    missing_output = setdiff(c(rmds, sub("\\.Rmd$", ".R", rmds),
                               sub("\\.Rmd$", ".html", rmds)), out),
    rmd_differs = rmds[!vapply(rmds, function(x) identical(
      readLines(file.path(vign, x)), readLines(file.path(doc, x))),
      logical(1L))],
    purl_differs = rmds[!purl_matches],
    not_evaluated = rmds[!evaluated]
  )
  if (length(problems)) {
    print(problems)
    stop("Rebuild the vignettes with build_vignettes().", call. = FALSE)
  }
  message("Vignette outputs are current, complete and evaluated.")
  invisible(TRUE)
}

# 5. Check the package without rebuilding the vignettes
check_package <- function() {
  devtools::check(vignettes = FALSE, run_dont_test = FALSE,
                  args = c("--timings", "--no-tests"),
                  check_dir = out_dir)
}

# 6. Build the tarball to submit. Its vignettes are the pre-built ones.
build_tarball <- function() {
  devtools::build(path = out_dir, vignettes = FALSE)
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

# 8. win-builder release and devel. Results arrive by email to the
# maintainer within about an hour.
submit_win_builder <- function() {
  devtools::check_win_release()
  devtools::check_win_devel()
}

# Usage:
#   cran_status(); check_text(); check_marginaleffects()
#   build_vignettes(); check_vignette_outputs(); check_package()
#   tarball <- build_tarball(); check_vignette_outputs(tarball)
#   check_as_cran(tarball)
#   submit_win_builder()
#   devtools::submit_cran()
