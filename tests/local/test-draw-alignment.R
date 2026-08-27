# A post-fit answer is assembled from several reads of the posterior:
# the observation predictor, the trend predictor, the process errors,
# the distributional parameters, the ordinal thresholds. They only
# describe the same model if they describe the same iterations.
#
# The failure this guards against is silent. Handed a draw count
# rather than draw indices, each read subsampled on its own, and the
# terms that were then added together came from different iterations.
# Nothing in the output looks wrong: the rows are all finite, all
# plausible, and all mismatched. On a fit with a trend-side formula,
# asking for the whole posterior once returned a thousand rows of
# which one paired a predictor with its own trend.
#
# The test below states the invariant that catches it. Every row a
# subsampled prediction produces must be a row the fully specified
# prediction also produces: subsetting draws may drop rows and reorder
# them, but it cannot invent a combination that no single draw gives.
#
# Run with:
#   Rscript -e "devtools::load_all('.'); testthat::test_file('tests/local/test-draw-alignment.R')"

source("setup_tests_local.R")


# Rows as comparable keys. Rounding guards against a last-bit
# difference between two routes to the same arithmetic.
draw_rows <- function(x) {
  unname(apply(round(as.matrix(x), 10), 1, paste, collapse = "|"))
}

trend_fixtures <- c("val_mvgam_ar1_fx_trend",
                    "val_mvgam_ar1_re_smooth_trend")


test_that("a subsampled linear predictor pairs its own trend", {
  for (nm in trend_fixtures) {
    fit <- readRDS(file.path("fixtures", paste0(nm, ".rds")))
    total <- ndraws(fit)
    # The reference: every draw, named explicitly, so each row is the
    # observation predictor and the trend predictor of one iteration.
    reference <- draw_rows(
      posterior_linpred(fit, draw_ids = seq_len(total),
                        process_error = FALSE)
    )
    # A count below the total.
    subset_rows <- draw_rows(
      posterior_linpred(fit, ndraws = 20L, process_error = FALSE)
    )
    expect_length(subset_rows, 20L)
    expect_true(all(subset_rows %in% reference))
    # A count covering the whole posterior. This is the case that used
    # to fail hardest: both reads claimed every draw, and each
    # returned them in its own random order.
    all_rows <- draw_rows(
      posterior_linpred(fit, ndraws = total, process_error = FALSE)
    )
    expect_setequal(all_rows, reference)
  }
})


test_that("a count and the indices it stands for agree", {
  fit <- readRDS(file.path("fixtures", "val_mvgam_ar1_fx_trend.rds"))
  ids <- c(3L, 11L, 47L, 300L)
  by_ids <- posterior_linpred(fit, draw_ids = ids, process_error = FALSE)
  expect_equal(nrow(by_ids), length(ids))
  # Naming the same draws twice gives the same answer, so nothing
  # below the boundary is re-drawing.
  expect_equal(
    by_ids,
    posterior_linpred(fit, draw_ids = ids, process_error = FALSE)
  )
  # Every surface honours the count it was given.
  for (n in c(15L, ndraws(fit))) {
    expect_equal(nrow(posterior_epred(fit, ndraws = n)), n)
    expect_equal(nrow(posterior_predict(fit, ndraws = n)), n)
    expect_equal(nrow(log_lik(fit, ndraws = n)), n)
    expect_equal(nrow(predict(fit, ndraws = n, summary = FALSE)), n)
  }
  # And refuses one it cannot honour, rather than quietly using all.
  expect_error(posterior_epred(fit, ndraws = ndraws(fit) + 1L),
               "more draws than the posterior holds")
})


test_that("ordinal thresholds follow the draws of their predictor", {
  fit <- readRDS(file.path("fixtures", "val_mvgam_cumulative_fx.rds"))
  draws <- posterior::as_draws_matrix(fit$fit)
  ids <- c(2L, 9L, 40L)
  thres <- mvgam:::extract_ordinal_thresholds(
    fit, ndraws = length(ids), draw_ids = ids
  )
  expect_equal(nrow(thres), length(ids))
  # The thresholds are the ones sampled at those iterations, not the
  # first few rows of the posterior.
  expect_equal(as.numeric(thres[, 1]),
               as.numeric(draws[ids, "Intercept[1]"]))
  expect_false(isTRUE(all.equal(
    as.numeric(draws[ids, "Intercept[1]"]),
    as.numeric(draws[seq_along(ids), "Intercept[1]"])
  )))
  # And the surfaces built on them honour a count.
  for (n in c(25L, ndraws(fit))) {
    expect_equal(nrow(log_lik(fit, ndraws = n)), n)
    expect_equal(nrow(posterior_predict(fit, ndraws = n)), n)
  }
})


# The three invariants below guard the composition of the trend's
# contribution rather than the choice of draws. Each failed silently:
# the answers stayed finite, plausibly scaled and wrong.


test_that("a scored row reads the state of its own time", {
  # The latent state used to be looked up by position within whatever
  # frame it was handed, so scoring the later half of a series read
  # the state of the earlier half. The shape was right and nothing
  # warned. Scoring a window has to agree with scoring everything and
  # keeping that window's columns.
  fit <- readRDS(file.path("fixtures", "val_mvgam_ar1_fx.rds"))
  n <- nrow(fit$data)
  window <- seq.int(n - 9L, n)
  full <- log_lik(fit)
  part <- log_lik(fit, newdata = fit$data[window, , drop = FALSE])
  expect_equal(dim(part), c(nrow(full), length(window)))
  expect_equal(unname(as.matrix(part)),
                unname(as.matrix(full)[, window]),
                tolerance = 1e-8)
})


test_that("the trend's innovations are composed once", {
  # `posterior_epred()` and `posterior_predict()` each drew a second,
  # independent set of innovations on top of the set the linear
  # predictor already carried, so a marginal prediction held twice the
  # process variance. Drawn from one seed, the expectation has to be
  # exactly the inverse link of the predictor: a second draw anywhere
  # in the chain breaks the identity.
  fit <- readRDS(file.path("fixtures", "val_mvgam_ar1_fx.rds"))
  set.seed(99L)
  ep <- posterior_epred(fit)
  set.seed(99L)
  lp <- posterior_linpred(fit)
  expect_equal(unname(as.matrix(ep)),
                unname(as.matrix(fit$family$linkinv(lp))),
                tolerance = 1e-10)
})


test_that("a conditional read repeats and a marginal one does not", {
  # Conditioning reads the state the model inferred, so it holds no
  # RNG and answers the same way twice. Marginalising samples the
  # trend afresh, which is the documented reason a seed is needed for
  # a reproducible answer.
  fit <- readRDS(file.path("fixtures", "val_mvgam_ar1_fx.rds"))
  expect_equal(
    posterior_epred(fit, latent_state = "conditional"),
    posterior_epred(fit, latent_state = "conditional")
  )
  expect_equal(log_lik(fit), log_lik(fit))
  expect_false(isTRUE(all.equal(
    posterior_epred(fit, latent_state = "marginal"),
    posterior_epred(fit, latent_state = "marginal")
  )))
})
