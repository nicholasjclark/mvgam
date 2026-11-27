# Helpers shared by test-closure-unit-pp-check.R and test-extra-closure-unit-pp-check.R.
# Tests for the closure-unit pp_check helpers. Structural coverage
# of the `closure_unit_pp_check_setup()` aggregation hook and
# the `refuse_unit_varying()` group / x validator;
# end-to-end MCMC integration on the cached occ fixture lives in
# tests/local/test-closure-unit-pp-check.R.

# Helpers shared with the residuals tests.
make_pp_check_grid <- function(n_unit = 4L, n_visit = 3L,
                                seed = 17L) {
  set.seed(seed)
  n_total <- n_unit * n_visit
  data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_total),
    visit  = rep(seq_len(n_visit), n_unit),
    y      = sample(c(0L, 1L), n_total, replace = TRUE),
    elev   = rep(rnorm(n_unit), each = n_visit),
    tod    = stats::runif(n_total)
  )
}

mock_pp_check_object <- function(data, family = occ()) {
  structure(
    list(data = data, formula = y ~ 1, family = family),
    class = "mvgam"
  )
}
