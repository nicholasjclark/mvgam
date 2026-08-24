# Local fitting tests for closure-unit families on survey designs
# with unvisited occasions. A missing response is a visit that did
# not happen, which is the normal case in repeat-visit data, and NA
# is its natural encoding.
#
# These families were refusing such data outright. Their Stan code
# aggregates repeat visits per unit through `visit_idx` and `n_rep`,
# and those arrays were built from the raw data frame while brms
# sized the likelihood to the observed rows, so the indices would
# have pointed past the end of the response. Rejecting NA hid the
# misalignment instead of resolving it.
#
# The index bookkeeping is checked without Stan in
# `tests/testthat/test-stancode-standata.R`; these tests confirm the
# models also sample and predict.
#
# Run with:
#   Rscript -e "devtools::load_all('.'); testthat::test_file('tests/local/closure_unit_missing_visits.R')"

source("setup_tests_local.R")

# brms reports the rows it dropped, once per internal pass.
drop_na_warning <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("Rows containing NAs", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

cu_visits <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    sim <- sim_closure_unit_data(family = occ(), type = 1L,
                                 n_sites = 30L, n_visits = 4L,
                                 seed = 11L)
    complete <- sim$data_train
    gappy <- complete
    # Every sixth occasion goes unvisited, spread across sites.
    gappy$y[seq(2L, nrow(gappy), by = 6L)] <- NA_integer_
    fit_one <- function(d) {
      drop_na_warning(SM(mvgam(
        y ~ 1, data = d, family = occ(), chains = 2L, samples = 400L,
        burnin = 400L, silent = 2L, seed = 11L, backend = "cmdstanr"
      )))
    }
    cached <<- list(complete = fit_one(complete), gappy = fit_one(gappy),
                    data_complete = complete, data_gappy = gappy)
    cached
  }
})


test_that("occ() samples when occasions go unvisited", {
  obj <- cu_visits()
  expect_identical(as.integer(obj$gappy$standata$N),
                   sum(!is.na(obj$data_gappy$y)))
  # Unit arrays cover the observed visits exactly.
  expect_identical(sum(obj$gappy$standata$n_rep),
                   as.integer(obj$gappy$standata$N))
  expect_true(max(obj$gappy$standata$visit_idx) <=
                obj$gappy$standata$N)
  s <- posterior::summarise_draws(
    posterior::as_draws_df(obj$gappy$fit), "rhat"
  )
  expect_true(max(s$rhat, na.rm = TRUE) < 1.1)
})


test_that("dropping visits widens the estimate without moving it", {
  # Fewer detections carry less information about occupancy, so the
  # gappy fit should stay compatible with the complete one rather
  # than drift somewhere else.
  obj <- cu_visits()
  full <- as.numeric(as.array(obj$complete, variable = "b_Intercept"))
  gaps <- as.numeric(as.array(obj$gappy, variable = "b_Intercept"))
  cat(sprintf("  b_Intercept: complete %.3f, with gaps %.3f\n",
              stats::median(full), stats::median(gaps)))
  # The complete-data median stays inside the gappy 90% interval.
  expect_true(stats::quantile(gaps, 0.05) <= stats::median(full))
  expect_true(stats::quantile(gaps, 0.95) >= stats::median(full))
  # Predictions still cover every row of the supplied data.
  pp <- posterior_predict(obj$gappy, ndraws = 20L)
  expect_identical(ncol(pp), nrow(obj$data_gappy))
})
