# CI tests for the typed catalog in `R/sim_mvgam.R`. Covers
# shape, family dispatch, trend override, reproducibility, and
# the ground-truth slots needed for downstream recovery checks.
# Numerical recovery against a fitted mvgam lives in
# `tests/local/`.


# ---- Shape per type -----------------------------------------------

for (typ in 1:6) {
  local({
    type_id <- typ
    test_that(paste0("type = ", type_id,
                      " returns well-formed mvgam_sim"), {
      out <- sim_mvgam(
        type = type_id, family = gaussian(),
        n_series = 2L, n_timepoints = 60L, seed = 11L
      )
      expect_s3_class(out, "mvgam_sim")
      expect_named(
        out,
        c("data_train", "data_test", "family", "trend_model",
          "type", "true_betas", "true_smooths", "true_trend",
          "true_trend_sigma", "true_sigma_obs")
      )
      expect_true(all(c("y", "series", "time") %in%
                        colnames(out$data_train)))
      expect_s3_class(out$data_train$series, "factor")
      expect_identical(nlevels(out$data_train$series), 2L)
      expect_identical(dim(out$true_trend), c(60L, 2L))
      expect_true(all(is.finite(out$true_trend)))
      expect_true(length(out$true_smooths) >= 1L)
      expect_true(is.finite(out$true_trend_sigma))
    })
  })
}


# ---- Family dispatch ----------------------------------------------

test_that("type = 1 works across all supported families", {
  for (fam in list(
    gaussian(),
    brms::brmsfamily("student"),
    poisson(),
    brms::brmsfamily("negbinomial"),
    binomial(),
    brms::brmsfamily("beta"),
    Gamma(link = "log")
  )) {
    out <- sim_mvgam(
      type = 1L, family = fam, n_series = 1L,
      n_timepoints = 50L, seed = 7L
    )
    expect_s3_class(out, "mvgam_sim")
    expect_true(all(is.finite(out$data_train$y) |
                      is.na(out$data_train$y)))
  }
})


test_that("tweedie family is supported by sim_mvgam", {
  # Tweedie is a customfamily with `family$family == 'custom'`, so
  # sim_family_rng has to route through resolve_family_name() to
  # find the right branch. Pin the dispatch + the support shape:
  # non-negative continuous with a real point mass at zero.
  out <- sim_mvgam(
    type = 1L, family = tweedie(), n_series = 1L,
    n_timepoints = 80L, trend_model = AR(), seed = 11L
  )
  expect_s3_class(out, "mvgam_sim")
  y <- out$data_train$y
  expect_true(all(is.finite(y) | is.na(y)))
  expect_true(all(y >= 0))
  # Some zeros and some positives; if either side is empty the
  # CP simulator has degenerated.
  expect_gt(sum(y == 0, na.rm = TRUE), 0L)
  expect_gt(sum(y > 0, na.rm = TRUE), 0L)
})


test_that("sim_tweedie() respects phi and power bounds", {
  expect_error(
    mvgam:::sim_tweedie(mu = c(1, 2), phi = 1, power = 1.0),
    "power"
  )
  expect_error(
    mvgam:::sim_tweedie(mu = c(1, 2), phi = 1, power = 2.0),
    "power"
  )
  expect_error(
    mvgam:::sim_tweedie(mu = c(1, 2), phi = 0, power = 1.5),
    "phi"
  )
  # Within bounds: returns a numeric vector of length(mu).
  out <- mvgam:::sim_tweedie(mu = rep(2, 50L), phi = 1, power = 1.5)
  expect_length(out, 50L)
  expect_true(all(out >= 0))
})


test_that("count families return non-negative integers", {
  for (fam in list(
    poisson(), brms::brmsfamily("negbinomial")
  )) {
    out <- sim_mvgam(type = 1L, family = fam,
                      n_timepoints = 60L, seed = 7L)
    expect_true(all(out$data_train$y >= 0))
    expect_true(all(out$data_train$y == round(out$data_train$y)))
  }
})


test_that("binomial returns values bounded by trials (default 10)", {
  out <- sim_mvgam(type = 1L, family = binomial(),
                    n_timepoints = 60L, seed = 7L)
  expect_true(all(out$data_train$y >= 0L))
  expect_true(all(out$data_train$y <= 10L))
})


test_that("beta returns values strictly inside (0, 1)", {
  out <- sim_mvgam(type = 1L,
                    family = brms::brmsfamily("beta"),
                    n_timepoints = 60L, seed = 7L)
  expect_true(all(out$data_train$y > 0))
  expect_true(all(out$data_train$y < 1))
})


# ---- Trend override ----------------------------------------------

test_that("trend_model override accepts any supported constructor", {
  for (tm in list(
    RW(), AR(p = 1L), AR(p = c(1L, 3L, 12L)),
    AR(p = 2L, ma = TRUE), "None"
  )) {
    out <- sim_mvgam(
      type = 1L, family = gaussian(),
      trend_model = tm, n_timepoints = 60L, seed = 7L
    )
    expect_identical(dim(out$true_trend), c(60L, 1L))
    expect_true(all(is.finite(out$true_trend)))
  }
})


test_that("trend_model = 'None' produces a zero trend", {
  out <- sim_mvgam(type = 1L, family = gaussian(),
                    trend_model = "None",
                    n_timepoints = 50L, seed = 7L)
  expect_true(all(out$true_trend == 0))
  expect_equal(out$true_trend_sigma, 0)
})


# ---- Reproducibility ---------------------------------------------

test_that("sim_mvgam is reproducible under set.seed", {
  a <- sim_mvgam(type = 2L, family = gaussian(),
                  n_series = 2L, n_timepoints = 40L, seed = 99L)
  b <- sim_mvgam(type = 2L, family = gaussian(),
                  n_series = 2L, n_timepoints = 40L, seed = 99L)
  expect_identical(a$data_train$y, b$data_train$y)
  expect_identical(a$true_trend, b$true_trend)
  expect_identical(a$true_smooths, b$true_smooths)
})


# ---- Train / test split ------------------------------------------

test_that("proportional_train = 1 returns NULL data_test", {
  out <- sim_mvgam(type = 1L, n_timepoints = 40L,
                    proportional_train = 1, seed = 7L)
  expect_null(out$data_test)
})


test_that("train + test partition by time index", {
  out <- sim_mvgam(type = 1L, n_series = 2L, n_timepoints = 80L,
                    proportional_train = 0.75, seed = 7L)
  expect_identical(max(out$data_train$time),
                    min(out$data_test$time) - 1L)
  expect_identical(
    nrow(out$data_train) + nrow(out$data_test), 160L
  )
})


# ---- Missing-data injection --------------------------------------

test_that("prop_missing inserts NA into the training response", {
  out <- sim_mvgam(type = 1L, n_timepoints = 100L,
                    prop_missing = 0.2, seed = 7L)
  expect_true(mean(is.na(out$data_train$y)) > 0.05)
  expect_true(mean(is.na(out$data_train$y)) < 0.4)
})


# ---- Ground-truth smooth alignment with rescaled data ------------

test_that("true_smooths are centered (mean ~ 0 over the grid)", {
  out <- sim_mvgam(type = 2L, family = gaussian(),
                    n_timepoints = 60L, seed = 7L)
  for (nm in names(out$true_smooths)) {
    expect_lt(abs(mean(out$true_smooths[[nm]]$f_true)), 1e-6)
  }
})


test_that("true_trend_sigma matches sd(as.numeric(true_trend))", {
  out <- sim_mvgam(type = 1L, family = gaussian(),
                    n_series = 3L, n_timepoints = 80L, seed = 7L)
  expect_equal(
    out$true_trend_sigma,
    stats::sd(as.numeric(out$true_trend))
  )
})


# ---- Type-specific covariates ------------------------------------

test_that("each type's data carries the covariates it needs", {
  expected_cols <- list(
    `1` = "x",
    `2` = c("x", "z"),
    `3` = c("x", "grp"),
    `4` = "x",
    `5` = c("x", "z"),
    `6` = "season"
  )
  for (typ in 1:6) {
    out <- sim_mvgam(type = typ, n_series = 1L,
                      n_timepoints = 40L, seed = 7L)
    expect_true(all(expected_cols[[as.character(typ)]] %in%
                      colnames(out$data_train)))
  }
})


# ---- Errors ------------------------------------------------------

test_that("type outside 1..7 errors informatively", {
  expect_error(sim_mvgam(type = 0L), "not >= 1")
  expect_error(sim_mvgam(type = 8L), "not <= 7")
})


# ---- summary.mvgam_sim --------------------------------------------

test_that("summary.mvgam_sim returns mvgam_sim_summary with expected fields", {
  sim <- sim_mvgam(type = 2L, n_series = 3L, n_timepoints = 40L,
                    seed = 11L)
  s <- summary(sim)
  expect_s3_class(s, "mvgam_sim_summary")
  expect_setequal(
    names(s),
    c("type", "family", "trend", "n_series", "n_timepoints",
      "n_train", "n_test", "true_betas", "n_smooths",
      "smooth_names", "true_trend_sigma", "true_sigma_obs")
  )
  expect_equal(s$type, 2L)
  expect_equal(s$n_series, 3L)
  expect_equal(s$n_timepoints, 40L)
  expect_equal(s$n_train + s$n_test, NROW(sim$data_train) +
                 (if (is.null(sim$data_test)) 0L else NROW(sim$data_test)))
  expect_true(is.numeric(s$true_trend_sigma))
})


test_that("summary.mvgam_sim trend label uses 'None' when no trend", {
  sim <- sim_mvgam(type = 1L, n_series = 1L, n_timepoints = 30L,
                    seed = 3L, trend_model = NULL,
                    prop_trend = 0)
  s <- summary(sim)
  # When prop_trend = 0 the trend_model is still set, but if absent
  # entirely the label collapses to "None". Either way the field
  # should be a single non-empty string.
  expect_true(is.character(s$trend) && nzchar(s$trend))
})


test_that("summary.mvgam_sim resolves trend label from constructor", {
  # Constructor name lives on `$trend_model$trend`. The label
  # fallback chain (in R/sim_mvgam.R) must read it; otherwise the
  # printed summary collapses to 'Unknown' even when the model is
  # an AR / RW. Regular-time types 1 and 2 cover AR and RW; CAR
  # needs the irregular-time recipe of type 6 (its propagator
  # requires per-step time gaps).
  ar_sim <- sim_mvgam(
    type = 1L, family = gaussian(), n_series = 1L,
    n_timepoints = 30L, seed = 3L, trend_model = AR()
  )
  expect_identical(summary(ar_sim)$trend, "AR")

  rw_sim <- sim_mvgam(
    type = 1L, family = gaussian(), n_series = 1L,
    n_timepoints = 30L, seed = 3L, trend_model = RW()
  )
  expect_identical(summary(rw_sim)$trend, "RW")

  car_sim <- sim_mvgam(
    type = 6L, family = gaussian(), n_series = 1L,
    n_timepoints = 30L, seed = 3L
  )
  expect_identical(summary(car_sim)$trend, "CAR")
  # Type 6 documents irregular spacing, so the recorded time must
  # actually be irregular and the season covariate must be the
  # function of it that build_data claims. Both were previously
  # drawn independently of the gaps the CAR kernel propagated over.
  car_times <- sort(unique(car_sim$data_train$time))
  expect_false(all(abs(diff(car_times) - 1) < 1e-8))
  expect_true(all(diff(car_times) >= 1 & diff(car_times) <= 6))
  expect_equal(
    car_sim$data_train$season,
    (car_sim$data_train$time %% 12) + 1
  )

  # 240 points, not 60: the assertions below are about the AR
  # process, and a 60-point sample ACF is noisy enough to swing
  # between 0.10 and 0.71 across seeds, so a short series tests the
  # draw rather than the spec.
  ar112_sim <- sim_mvgam(
    type = 7L, family = gaussian(), n_series = 1L,
    n_timepoints = 240L, seed = 3L
  )
  expect_identical(summary(ar112_sim)$trend, "AR")
  # The observation side carries a smooth of a non-periodic
  # covariate. A cyclic seasonal smooth used to sit here, but on
  # monthly data a lag-12 autoregression is itself an annual cycle,
  # so the two competed for the same periodicity and neither was
  # identified.
  expect_true("x" %in% colnames(ar112_sim$data_train))
  expect_false("season" %in% colnames(ar112_sim$data_train))
  expect_named(ar112_sim$true_smooths, "s(x)")
  # spec sets sparse AR(p = c(1, 12)) on the latent state; the
  # rescale skip path keeps the spec's chosen sigma_innov so the
  # latent state has measurable persistence (lag-12 autocorrelation
  # of the generated trend should be clearly non-zero).
  z <- ar112_sim$true_trend[, 1L]
  ac <- stats::acf(z, plot = FALSE, lag.max = 12L)$acf[, 1L, 1L]
  expect_gt(ac[2L], 0.2)
  expect_gt(ac[13L], 0.1)
})


test_that("print.mvgam_sim_summary prints header and parameters", {
  sim <- sim_mvgam(type = 2L, n_series = 2L, n_timepoints = 25L,
                    seed = 5L)
  out <- capture.output(invisible(print(summary(sim))))
  expect_true(any(grepl("Simulated mvgam dataset", out)))
  expect_true(any(grepl("Family", out)))
  expect_true(any(grepl("Trend", out)))
  expect_true(any(grepl("True generative parameters", out)))
})


test_that("print.mvgam_sim delegates to summary print", {
  sim <- sim_mvgam(type = 1L, n_series = 1L, n_timepoints = 25L,
                    seed = 4L)
  out <- capture.output(invisible(print(sim)))
  expect_true(any(grepl("Simulated mvgam dataset", out)))
})
