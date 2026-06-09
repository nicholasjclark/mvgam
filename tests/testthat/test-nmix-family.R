# Tests for the closure-unit family helpers and the nmix()
# constructor. The Stan function block, standata emission, and
# brms make_stancode round-trip live in test-stancode-standata.R
# once the lpdf is wired up; R-side log_lik / posterior_predict /
# latent_N tests live in test-log-lik.R + test-posterior-predict.R
# and arrive with chunk 3.

# ------------------------------------------------------------
# nmix() constructor
# ------------------------------------------------------------

test_that("nmix() returns a customfamily with mu/p dpars and logit/log links", {
  fam <- nmix()
  expect_s3_class(fam, "customfamily")
  expect_s3_class(fam, "brmsfamily")
  expect_identical(fam$name, "nmix")
  expect_identical(fam$dpars, c("mu", "p"))
  # brms stores the primary link in `link` and aux-dpar links in
  # `link_<dpar>` slots, not as a single `links` vector.
  expect_identical(fam$link, "log")
  expect_identical(fam$link_p, "logit")
  expect_identical(fam$type, "int")
  expect_false(fam$loop)
  # brms wraps lb/ub into a named list of character vectors per
  # dpar (the strings become Stan-side bounds at codegen time).
  expect_identical(fam$lb, list(mu = "0", p = NA_character_))
  expect_identical(fam$ub, list(mu = NA_character_, p = NA_character_))
  # linkinv / linkfun helpers are attached so downstream
  # dispatchers (compute_family_epred etc.) don't have to
  # branch on customfamily.
  expect_true(is.function(fam$linkinv))
  expect_true(is.function(fam$linkfun))
  expect_equal(fam$linkinv(0), 1)
})

test_that("nmix() tags closure-unit and predict-type attributes", {
  fam <- nmix()
  expect_true(is_closure_unit_family(fam))
  expect_identical(
    attr(fam, "mvgam_predict_types", exact = TRUE),
    c("latent_N", "detection")
  )
  # The Stan stanvars slot is filled in at data-prep time; chunk 1
  # leaves it NULL so attach_family_stanvars() passes through.
  expect_null(attr(fam, "mvgam_stanvars", exact = TRUE))
})

test_that("is_closure_unit_family() returns FALSE for non-closure families", {
  expect_false(is_closure_unit_family(NULL))
  expect_false(is_closure_unit_family(gaussian()))
  expect_false(is_closure_unit_family(brms::brmsfamily("poisson")))
  expect_false(is_closure_unit_family(tweedie()))
})

# ------------------------------------------------------------
# build_closure_unit_arrays()
# ------------------------------------------------------------

# Helper used across tests in this file.
make_nmix_data <- function(n_unit = 4, n_visit = 3, seed = 1) {
  set.seed(seed)
  n_total <- n_unit * n_visit
  data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_total),
    visit  = rep(seq_len(n_visit), n_unit),
    y      = pmin(rpois(n_total, 5), 20L),
    cap    = rep(20L, n_total),
    elev   = rep(rnorm(n_unit), each = n_visit),
    tod    = stats::runif(n_total)
  )
}

test_that("build_closure_unit_arrays() returns correctly shaped arrays", {
  d <- make_nmix_data(n_unit = 4, n_visit = 3)
  arrs <- build_closure_unit_arrays(d, response_var = "y")
  expect_named(arrs, c(
    "N_unit", "n_rep", "K_max", "Y_max",
    "visit_idx", "max_rep", "unit_labels"
  ))
  expect_identical(arrs$N_unit, 4L)
  expect_identical(arrs$n_rep, rep(3L, 4))
  expect_identical(arrs$K_max, rep(20L, 4))
  expect_identical(arrs$max_rep, 3L)
  # visit_idx[g, ] gives the row indices in d that belong to
  # closure unit g. Each unit should land in a contiguous block.
  for (g in seq_len(arrs$N_unit)) {
    rows_g <- arrs$visit_idx[g, seq_len(arrs$n_rep[g])]
    expect_identical(rows_g, ((g - 1L) * 3L + 1L):(g * 3L))
    expect_identical(arrs$Y_max[g], max(d$y[rows_g]))
  }
})

test_that("build_closure_unit_arrays() handles ragged visit counts", {
  # Unit 1 gets 3 visits, unit 2 gets 2, unit 3 gets 1.
  d <- data.frame(
    series = factor(c(rep(1L, 3), rep(2L, 2), 3L)),
    time   = rep(1L, 6),
    y      = c(2L, 4L, 3L, 1L, 0L, 5L),
    cap    = rep(10L, 6)
  )
  arrs <- build_closure_unit_arrays(d, response_var = "y")
  expect_identical(arrs$N_unit, 3L)
  expect_identical(arrs$n_rep, c(3L, 2L, 1L))
  expect_identical(arrs$max_rep, 3L)
  expect_identical(arrs$Y_max, c(4L, 1L, 5L))
  # Padding columns must be a valid row index (1L) so Stan never
  # indexes outside the array even when those slots are unused.
  expect_identical(arrs$visit_idx[2, 3], 1L)
  expect_identical(arrs$visit_idx[3, 2:3], c(1L, 1L))
})

test_that("build_closure_unit_arrays() supports custom column names (predict path)", {
  d <- data.frame(
    site    = factor(rep(1L:2L, each = 3L)),
    visit_t = rep(1L, 6),
    counts  = c(1L, 2L, 3L, 4L, 5L, 6L),
    nmax    = rep(15L, 6)
  )
  arrs <- build_closure_unit_arrays(
    d,
    response_var = "counts",
    series_var   = "site",
    time_var     = "visit_t",
    cap_var      = "nmax"
  )
  expect_identical(arrs$N_unit, 2L)
  expect_identical(arrs$K_max, c(15L, 15L))
})

test_that("build_closure_unit_arrays() reflects changed cap when called on newdata", {
  d_fit <- make_nmix_data(n_unit = 3, n_visit = 2)
  arrs_fit <- build_closure_unit_arrays(d_fit, response_var = "y")
  expect_identical(arrs_fit$K_max, rep(20L, 3))

  # Same data, but cap raised to 50 in the prediction call.
  d_pred <- d_fit
  d_pred$cap <- 50L
  arrs_pred <- build_closure_unit_arrays(d_pred, response_var = "y")
  expect_identical(arrs_pred$K_max, rep(50L, 3))
  # Visit structure unchanged; K_max is the only thing that
  # responds to the user's edited cap column.
  expect_identical(arrs_pred$N_unit, arrs_fit$N_unit)
  expect_identical(arrs_pred$n_rep, arrs_fit$n_rep)
  expect_identical(arrs_pred$visit_idx, arrs_fit$visit_idx)
})

test_that("build_closure_unit_arrays() errors on missing columns", {
  d <- make_nmix_data()
  d$cap <- NULL
  expect_error(
    build_closure_unit_arrays(d, response_var = "y"),
    "Closure-unit families require column 'cap'"
  )
})

test_that("build_closure_unit_arrays() errors when cap varies within a unit", {
  d <- make_nmix_data(n_unit = 2, n_visit = 3)
  d$cap[1L] <- 30L  # row 1 in unit 1
  expect_error(
    build_closure_unit_arrays(d, response_var = "y"),
    "must be constant within a closure unit"
  )
})

test_that("build_closure_unit_arrays() rejects NA in response or cap", {
  d <- make_nmix_data()
  d_na_y <- d
  d_na_y$y[1L] <- NA_integer_
  expect_error(
    build_closure_unit_arrays(d_na_y, response_var = "y"),
    "missing values in the response"
  )
  d_na_cap <- d
  d_na_cap$cap[1L] <- NA_integer_
  expect_error(
    build_closure_unit_arrays(d_na_cap, response_var = "y"),
    "Missing values in 'cap'"
  )
})

# ------------------------------------------------------------
# validate_closure_unit_data()
# ------------------------------------------------------------

test_that("validate_closure_unit_data() accepts well-formed nmix data", {
  d <- make_nmix_data()
  expect_invisible(validate_closure_unit_data(
    d, response_var = "y", has_obs_covariates = TRUE
  ))
})

test_that("validate_closure_unit_data() errors on cap < observed count", {
  d <- make_nmix_data()
  # cap=2 stays a positive integer, but a count of 5 in the
  # same row exceeds it, tripping the per-row cap >= y check.
  d$cap[1L] <- 2L
  d$cap[2L] <- 2L
  d$cap[3L] <- 2L
  d$y[1L]   <- 5L
  expect_error(
    validate_closure_unit_data(
      d, response_var = "y", has_obs_covariates = TRUE
    ),
    "below the observed counts"
  )
})

test_that("validate_closure_unit_data() errors on cap varying within a unit", {
  d <- make_nmix_data()
  # Unit 1 has rows 1..3; raise cap on the first row only.
  d$cap[1L] <- 50L
  expect_error(
    validate_closure_unit_data(
      d, response_var = "y", has_obs_covariates = TRUE
    ),
    "must be constant within a closure unit"
  )
})

test_that("validate_closure_unit_data() errors on non-integer counts", {
  d <- make_nmix_data()
  d$y <- d$y + 0.5
  expect_error(
    validate_closure_unit_data(
      d, response_var = "y", has_obs_covariates = TRUE
    ),
    "Non-integer values found in response"
  )
})

test_that("validate_closure_unit_data() errors on negative counts", {
  d <- make_nmix_data()
  d$y[1L] <- -1L
  expect_error(
    validate_closure_unit_data(
      d, response_var = "y", has_obs_covariates = TRUE
    ),
    "Negative counts found"
  )
})

test_that("validate_closure_unit_data() errors when all units single-visit with no covariates", {
  d <- make_nmix_data(n_unit = 5, n_visit = 1)
  expect_error(
    validate_closure_unit_data(
      d,
      response_var       = "y",
      has_obs_covariates = FALSE,
      has_det_covariates = FALSE
    ),
    "Closure-unit family is non-identified"
  )
})

test_that("validate_closure_unit_data() accepts single-visit data when a covariate is supplied", {
  d <- make_nmix_data(n_unit = 5, n_visit = 1)
  # With at least one covariate in either layer the model is
  # identifiable from cross-unit shared structure.
  expect_invisible(validate_closure_unit_data(
    d,
    response_var       = "y",
    has_obs_covariates = TRUE,
    has_det_covariates = FALSE
  ))
})

# ------------------------------------------------------------
# validate_supported_family() admits customfamily objects
# ------------------------------------------------------------

test_that("validate_supported_family() admits nmix() and tweedie() customfamily objects", {
  expect_invisible(validate_supported_family(nmix()))
  expect_invisible(validate_supported_family(tweedie()))
})
