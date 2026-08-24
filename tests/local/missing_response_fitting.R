# Local fitting tests for the invariant that makes mvgam useful on
# incomplete panels: a missing response removes a row from the
# likelihood without shortening the latent process. The families
# here each carry something per observation, which is where the
# invariant is easiest to break.
#
# `com_binomial()` did break it. Its binomial denominator was packed
# from the raw data frame while brms sized the likelihood to the
# observed rows, so Stan received more denominators than there were
# observations and every chain died at data-init. Because a
# `trend_formula` needs the padded time grid, the family could not
# be used with a latent trend on any incomplete panel at all.
#
# The code-generation half of this runs without Stan in
# `tests/testthat/test-stancode-standata.R`; these tests confirm the
# models also sample and predict.
#
# Run with:
#   Rscript -e "devtools::load_all('.'); testthat::test_file('tests/local/missing_response_fitting.R')"

source("setup_tests_local.R")

# brms reports the rows it dropped, once per internal pass.
drop_na_warning <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("Rows containing NAs", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

# Blank out roughly a tenth of the responses on a regular stride so
# the gaps are spread across the series rather than clustered.
blank_responses <- function(data, stride) {
  data$y[seq(stride, nrow(data), by = stride)] <- NA
  data
}


cmb_missing <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    set.seed(2027)
    n_t <- 120L
    trials <- pmax(rpois(n_t, 30L), 1L)
    df <- data.frame(
      y      = mvgam:::rcmb_vec(mu = rep(0.5, n_t), nu = rep(0.5, n_t),
                                T = trials),
      trials = trials,
      series = factor("s1"),
      time   = seq_len(n_t)
    )
    df <- blank_responses(df, 10L)
    cached <<- list(
      fit = drop_na_warning(SM(mvgam(
        bf(y | trials(trials) ~ 1),
        trend_formula = ~ AR(p = 1),
        data = df, family = com_binomial(),
        chains = 2L, samples = 500L, burnin = 500L,
        silent = 2L, seed = 2027L, backend = "cmdstanr"
      ))),
      data = df,
      n_t  = n_t
    )
    cached
  }
})


test_that("com_binomial() samples with gaps under a latent trend", {
  obj <- cmb_missing()
  n_obs <- sum(!is.na(obj$data$y))
  # The likelihood sees the observed rows; the process spans them all.
  expect_identical(as.integer(obj$fit$standata$N), as.integer(n_obs))
  expect_identical(as.integer(obj$fit$standata$N_time_trend),
                   as.integer(obj$n_t))
  expect_identical(length(obj$fit$standata$trials), as.integer(n_obs))
  s <- posterior::summarise_draws(
    posterior::as_draws_df(obj$fit$fit), "rhat"
  )
  expect_true(max(s$rhat, na.rm = TRUE) < 1.1)
})


test_that("predictions cover the timepoints that were never observed", {
  # The denominator brms puts in `standata` is sized to the
  # likelihood, so reusing it here would misalign against the
  # prediction grid. A hindcast has to return draws at the gaps.
  obj <- cmb_missing()
  hc <- hindcast(obj$fit)$hindcasts[[1L]]
  expect_identical(ncol(hc), as.integer(obj$n_t))
  expect_true(all(is.finite(hc[, which(is.na(obj$data$y))])))

  newdata <- data.frame(
    trials = pmax(rpois(10L, 30L), 1L),
    series = factor("s1", levels = levels(obj$data$series)),
    time   = obj$n_t + seq_len(10L),
    y      = NA_integer_
  )
  fc <- forecast(obj$fit, newdata = newdata)$forecasts[[1L]]
  expect_identical(ncol(fc), 10L)
  expect_true(all(is.finite(fc)))
  expect_true(all(fc >= 0L))
  expect_true(all(fc <= matrix(newdata$trials, nrow(fc), 10L,
                               byrow = TRUE)))
})


test_that("beta_nb() keeps its trend grid when responses go missing", {
  sim <- SM(sim_mvgam(
    family = beta_nb(), n_series = 3, n_timepoints = 90,
    trend_model = AR(), proportional_train = 0.85,
    family_pars = list(shape = 2, mtail = 2.5)
  ))
  train <- blank_responses(sim$data_train, 7L)
  mod <- drop_na_warning(SM(mvgam(
    y ~ 1, trend_formula = ~ AR(p = 1), data = train,
    newdata = sim$data_test, family = beta_nb(),
    backend = "cmdstanr", chains = 2, cores = 2, iter = 1500,
    seed = 7, silent = 2
  )))
  expect_identical(as.integer(mod$standata$N),
                   as.integer(sum(!is.na(train$y))))
  expect_identical(as.integer(mod$standata$N_time_trend),
                   length(unique(train$time)))
  expect_true(all(is.finite(hindcast(mod)$hindcasts[[1L]])))
})


test_that("a heavy-tailed process tolerates missing responses", {
  sim <- SM(sim_mvgam(
    family = poisson(), n_series = 3, n_timepoints = 80,
    trend_model = AR(), proportional_train = 0.85
  ))
  train <- blank_responses(sim$data_train, 9L)
  mod <- drop_na_warning(SM(mvgam(
    y ~ 1, trend_formula = ~ AR(p = 1, df = NA), data = train,
    newdata = sim$data_test, family = poisson(),
    backend = "cmdstanr", chains = 2, cores = 2, iter = 1000,
    seed = 5, silent = 2
  )))
  expect_identical(as.integer(mod$standata$N),
                   as.integer(sum(!is.na(train$y))))
  expect_identical(as.integer(mod$standata$N_time_trend),
                   length(unique(train$time)))
  # The degrees of freedom stay in their supported range.
  expect_true(all(as.numeric(as.array(mod, variable = "nu_trend")) > 2))
})
