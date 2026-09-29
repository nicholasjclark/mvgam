# Checks several local files share. testthat sources this file before
# each local test file in this directory.


# The path of a fixture. testthat sets the working directory to the
# test file's own and Rscript runs from the package root. The
# directory depends on which of the two runs the file: asking whether
# `fixtures` exists picks the wrong branch on a clean tree and writes
# tests/local/tests/local/fixtures.
#
# @param name File name of the fixture
cache_path <- function(name) {
  dir <- if (dir.exists(file.path("tests", "local"))) {
    file.path("tests", "local", "fixtures")
  } else {
    "fixtures"
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}


# Whether this run should refit a fixture. `MVGAM_REFIT` names the
# fixtures to refit, comma-separated, with or without `.rds`, or
# `all`. Unset, every cached fixture is loaded as it is:
#   MVGAM_REFIT=all Rscript -e "..."
#   MVGAM_REFIT=val_mvgam_var,val_trend_map Rscript -e "..."
#
# A fixture is refitted at most once per run. A file that loads one
# fixture in several tests gets the refitted copy after the first.
#
# @param name File name of the fixture
refit_requested <- function(name) {
  if (name %in% refitted_this_run$names) return(FALSE)
  wanted <- trimws(strsplit(Sys.getenv("MVGAM_REFIT"), ",")[[1L]])
  "all" %in% wanted ||
    sub("\\.rds$", "", name) %in% sub("\\.rds$", "", wanted)
}
refitted_this_run <- new.env()
refitted_this_run$names <- character(0)


# A fit loaded from the fixture cache. It is refitted when the cache is
# absent, was built from other data, or `MVGAM_REFIT` names it. Every
# local file loads its fits through this.
#
# @param name File name of the fixture
# @param build A function of no arguments returning the fit
# @param key Any value identifying the data the fit belongs to, such
#   as the simulated truth. A cached fit carrying another key is stale.
# @return The fit, carrying `key` as its `fixture_key` attribute
cached_fit <- function(name, build, key = NULL) {
  path <- cache_path(name)
  if (file.exists(path) && !refit_requested(name)) {
    fit <- readRDS(path)
    if (identical(attr(fit, "fixture_key"), key)) {
      message("[cache] ", name)
      return(fit)
    }
  }
  message("[fit  ] ", name)
  fit <- build()
  attr(fit, "fixture_key") <- key
  part <- paste0(path, ".part")
  saveRDS(fit, part)
  file.rename(part, path)
  refitted_this_run$names <- c(refitted_this_run$names, name)
  fit
}


# A ggplot is returned whether or not a layer received any data.
# Building the plot forces the layers to resolve, and the row count
# says something was drawn.
#
# @param p A ggplot
# @return The built layers, invisibly
expect_drawn <- function(p) {
  testthat::expect_s3_class(p, "ggplot")
  layers <- ggplot2::ggplot_build(p)$data
  testthat::expect_gt(sum(vapply(layers, nrow, integer(1L))), 0L)
  invisible(layers)
}


# The range of x a plot draws over, from its built layers.
#
# @param p A ggplot
# @return The finite x range
drawn_x <- function(p) {
  xs <- unlist(lapply(ggplot2::ggplot_build(p)$data,
                      function(l) if ("x" %in% names(l)) l$x))
  xs <- xs[is.finite(xs)]
  testthat::expect_gt(length(xs), 0L)
  range(xs)
}


# The facet labels of a plot, in panel order.
#
# @param p A ggplot
# @return Character vector, empty for an unfaceted plot
panel_order <- function(p) {
  lay <- ggplot2::ggplot_build(p)$layout$layout
  facets <- setdiff(names(lay),
                    c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y"))
  if (!length(facets)) return(character(0))
  as.character(lay[[facets[1L]]])
}


# Evaluate an expression and collect the warnings it raises.
#
# @param expr Expression to evaluate
# @return List with `value` and the warning messages
with_warnings <- function(expr) {
  seen <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    seen <<- c(seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = seen)
}


# Ground truth for the stationary distribution of an AR() trend,
# written from the definition.
#
# The companion state is `(y_t, ..., y_{t-p+1}[, eps_t])`, block i at
# rows `(i - 1) m + 1` to `i m`, with diagonal coefficients. A
# moving-average term adds the block holding `eps_t`, which enters
# `y_{t+1}` through `theta`. The stationary covariance is the Neumann
# sum `Omega = sum_k A^k Q A'^k`. It calls nothing from the package,
# and every assertion taking it as ground truth tests the package
# against the definition.
#
# @param Sigma Innovation covariance (m x m)
# @param phi List of coefficient vectors, one per lag from 1 to p
# @param theta Moving-average coefficient vector, or NULL
# @param n_terms Terms of the sum. At a spectral radius of 0.99 the
#   terms past 3000 total less than the precision a stored draw holds.
# @return The covariance of the companion state, side `(p + q) m`
neumann_omega <- function(Sigma, phi, theta = NULL, n_terms = 3000L) {
  m <- nrow(Sigma)
  p <- length(phi)
  n <- p + if (is.null(theta)) 0L else 1L
  d <- n * m
  blk <- function(i) ((i - 1L) * m + 1L):(i * m)
  A <- matrix(0, d, d)
  for (k in seq_len(p)) A[blk(1L), blk(k)] <- diag(phi[[k]], nrow = m)
  for (i in seq_len(p)[-1L]) A[blk(i), blk(i - 1L)] <- diag(m)
  Q <- matrix(0, d, d)
  Q[blk(1L), blk(1L)] <- Sigma
  if (!is.null(theta)) {
    A[blk(1L), blk(n)] <- diag(theta, nrow = m)
    Q[blk(n), blk(n)] <- Sigma
    Q[blk(1L), blk(n)] <- Sigma
    Q[blk(n), blk(1L)] <- Sigma
  }
  Omega <- matrix(0, d, d)
  Ak <- diag(d)
  for (k in 0:n_terms) {
    Omega <- Omega + Ak %*% Q %*% t(Ak)
    Ak <- Ak %*% A
  }
  (Omega + t(Omega)) / 2
}
