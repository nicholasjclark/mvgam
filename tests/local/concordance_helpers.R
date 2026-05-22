# Helpers for brms-vs-mvgam concordance tests in tests/local/.
#
# These tests are NOT run by devtools::test() / CI; they live under
# tests/local/ and are invoked manually with
#   testthat::test_file("tests/local/test-predictions-brms-concordance.R")
# after fixtures have been built via
#   Rscript tests/local/build_fixtures.R
#
# The point is numerical correctness — comparing mvgam predictions
# against an equivalent brms fit, not just verifying shape.

# Fixture directory. testthat sets the working directory to the
# location of the test file (tests/local/) when running, so the
# fixtures live at the bare "fixtures" relative to that. When the
# helpers are sourced from elsewhere (e.g. R console at the package
# root), fall back to the explicit tests/local/fixtures path.
local_fixture_dir <- function() {
  short <- "fixtures"
  if (dir.exists(short)) return(short)
  long <- file.path("tests", "local", "fixtures")
  if (dir.exists(long)) return(long)
  short
}

# Skip a test_that block cleanly when its fixture is not on disk.
# Returns invisibly TRUE if all fixtures present so the caller can
# choose to abort early; otherwise calls testthat::skip().
require_fixtures <- function(...) {
  names <- c(...)
  fdir <- local_fixture_dir()
  if (!dir.exists(fdir)) {
    testthat::skip(
      paste0("Fixture dir ", fdir, " missing. ",
             "Run Rscript tests/local/build_fixtures.R first.")
    )
  }
  missing <- character(0)
  for (n in names) {
    if (!file.exists(file.path(fdir, n))) {
      missing <- c(missing, n)
    }
  }
  if (length(missing) > 0) {
    testthat::skip(
      paste0("Missing fixture(s): ",
             paste(missing, collapse = ", "),
             ". Run Rscript tests/local/build_fixtures.R.")
    )
  }
  invisible(TRUE)
}

# Load a brms fixture by short name. Names follow the
# val_brms_<name>.rds convention from build_fixtures.R.
load_brms <- function(name) {
  readRDS(file.path(local_fixture_dir(), paste0("val_brms_", name, ".rds")))
}

# Load an mvgam fixture by short name.
load_mvgam <- function(name) {
  readRDS(file.path(local_fixture_dir(), paste0("val_mvgam_", name, ".rds")))
}

# Per-observation summary statistics on a draws matrix
# (rows = draws, cols = observations).
summarize_pred <- function(pred_matrix) {
  list(
    mean   = colMeans(pred_matrix),
    median = apply(pred_matrix, 2, stats::median),
    q025   = apply(pred_matrix, 2, stats::quantile, probs = 0.025),
    q975   = apply(pred_matrix, 2, stats::quantile, probs = 0.975),
    sd     = apply(pred_matrix, 2, stats::sd)
  )
}

# Compare two vectors of summary statistics. Returns a list with
# `cor`, `rmse` and a `constant` / `mismatch` flag for the
# zero-variance cases.
compare_vectors <- function(v1, v2) {
  sd1 <- stats::sd(v1)
  sd2 <- stats::sd(v2)
  if (sd1 < 1e-10 && sd2 < 1e-10) {
    return(list(cor = NA_real_,
                rmse = abs(mean(v1) - mean(v2)),
                constant = TRUE))
  }
  if (sd1 < 1e-10 || sd2 < 1e-10) {
    return(list(cor = NA_real_, rmse = NA_real_,
                constant = FALSE, mismatch = TRUE))
  }
  list(cor = stats::cor(v1, v2),
       rmse = sqrt(mean((v1 - v2)^2)),
       constant = FALSE)
}

# Headline assertion: posterior_linpred(brms_fit) and
# extract_component_linpred(mvgam_fit, component = "obs") have the
# same shape and their per-observation mean concords above
# `threshold`. Returns the realised correlation invisibly.
assert_linpred_concordance <- function(brms_fit, mvgam_fit, newdata,
                                       threshold = 0.88,
                                       incl_autocor = FALSE,
                                       component = "obs") {
  brms_pred <- brms::posterior_linpred(
    brms_fit, newdata = newdata, incl_autocor = incl_autocor
  )
  mvgam_pred <- extract_component_linpred(
    mvgam_fit, newdata, component = component
  )
  testthat::expect_equal(dim(brms_pred), dim(mvgam_pred))

  brms_mean <- colMeans(brms_pred)
  mvgam_mean <- colMeans(mvgam_pred)
  comp <- compare_vectors(brms_mean, mvgam_mean)
  if (isTRUE(comp$constant)) {
    # Intercept-only fits collapse to a near-constant linpred. cor()
    # is undefined; accept tight rmse instead.
    testthat::expect_lt(comp$rmse, 0.5)
  } else if (isTRUE(comp$mismatch)) {
    testthat::fail("One linpred is constant, the other is not.")
  } else {
    testthat::expect_gte(comp$cor, threshold)
  }
  invisible(comp$cor)
}

# Concordance on posterior_epred (response scale). The Jensen
# amplification through inverse links pushes typical cor down vs
# linpred-scale comparison, so the default threshold is laxer.
assert_epred_concordance <- function(brms_fit, mvgam_fit, newdata,
                                     threshold = 0.85) {
  brms_pred <- brms::posterior_epred(brms_fit, newdata = newdata)
  mvgam_pred <- posterior_epred(mvgam_fit, newdata = newdata)
  testthat::expect_equal(dim(brms_pred), dim(mvgam_pred))

  comp <- compare_vectors(colMeans(brms_pred), colMeans(mvgam_pred))
  if (isTRUE(comp$constant)) {
    testthat::expect_lt(comp$rmse, 1.0)
  } else {
    testthat::expect_gte(comp$cor, threshold)
  }
  invisible(comp$cor)
}

# Family-specific scale checks for posterior_predict draws.
assert_predict_scale_constraints <- function(predict_matrix, family) {
  testthat::expect_true(all(is.finite(predict_matrix)))
  fam <- family$family
  if (fam %in% c("poisson", "binomial", "beta_binomial",
                  "negbinomial", "negative_binomial",
                  "hurdle_poisson", "hurdle_negbinomial",
                  "zero_inflated_poisson",
                  "zero_inflated_negbinomial",
                  "zero_inflated_binomial")) {
    testthat::expect_true(all(predict_matrix == floor(predict_matrix)))
  }
  nonneg <- c("poisson", "negbinomial", "negative_binomial",
              "gamma", "lognormal", "exponential",
              "hurdle_poisson", "hurdle_negbinomial",
              "hurdle_gamma", "hurdle_lognormal",
              "zero_inflated_poisson",
              "zero_inflated_negbinomial")
  if (fam %in% nonneg) {
    testthat::expect_true(all(predict_matrix >= 0))
  }
  if (fam == "beta") {
    testthat::expect_true(all(predict_matrix >= 0 & predict_matrix <= 1))
  }
  if (fam == "binomial") {
    # Bound by sup(trials); just check non-negative.
    testthat::expect_true(all(predict_matrix >= 0))
  }
  invisible(NULL)
}
