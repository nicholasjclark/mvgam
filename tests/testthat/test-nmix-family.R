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
  # Bounds on p keep the scalar-dpar case sampled in (0, 1) so
  # the lpdf's logit(p) call stays valid even when there's no
  # sub-formula for detection.
  expect_identical(fam$lb, list(mu = "0", p = "0"))
  expect_identical(fam$ub, list(mu = NA_character_, p = "1"))
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
# Multi-response family predicates + gate
# ------------------------------------------------------------
#
# The mvgam_multi_response and mvgam_simplex_response attributes
# tag families that aggregate K species rows per closure unit and
# call Stan's native multivariate likelihood (dirichlet,
# multinomial, categorical, multivariate normal, multivariate T).
# Tests below use mock customfamily objects with the attributes
# set directly, so the predicates can be verified independently
# of the family constructors that eventually consume them.

mock_multi_response_family <- function(simplex = FALSE) {
  fam <- brms::custom_family(
    name = "mock_multi", dpars = c("mu"), links = "identity",
    type = "real", loop = FALSE
  )
  attr(fam, "mvgam_multi_response") <- TRUE
  if (simplex) attr(fam, "mvgam_simplex_response") <- TRUE
  fam
}

test_that("is_multi_response_family() detects the mvgam_multi_response attr", {
  expect_true(is_multi_response_family(mock_multi_response_family()))
  expect_true(is_multi_response_family(
    mock_multi_response_family(simplex = TRUE)
  ))
})

test_that("is_multi_response_family() returns FALSE for non-MV families", {
  expect_false(is_multi_response_family(NULL))
  expect_false(is_multi_response_family(gaussian()))
  expect_false(is_multi_response_family(nmix()))
  expect_false(is_multi_response_family(tweedie()))
})

test_that("is_simplex_response_family() detects only simplex MV families", {
  expect_true(is_simplex_response_family(
    mock_multi_response_family(simplex = TRUE)
  ))
  expect_false(is_simplex_response_family(mock_multi_response_family()))
  expect_false(is_simplex_response_family(NULL))
  expect_false(is_simplex_response_family(gaussian()))
})

test_that("validate_supported_family() admits multi-response customfamily objects", {
  expect_invisible(validate_supported_family(mock_multi_response_family()))
  expect_invisible(
    validate_supported_family(mock_multi_response_family(simplex = TRUE))
  )
})

test_that("validate_supported_family() points naked brms multi-category families at the mvgam wrapper", {
  expect_error(validate_supported_family(brms::dirichlet()), "diri\\(\\)")
  expect_error(validate_supported_family(brms::multinomial()),
               "multi\\(\\)")
  expect_error(validate_supported_family(brms::categorical()),
               "categ\\(\\)")
  expect_error(validate_supported_family(brms::logistic_normal()),
               "mvn\\(\\)")
})


# ------------------------------------------------------------
# diri(): family registration, attribute tagging,
# closure-unit grouping, and Stan emission round-trip
# ------------------------------------------------------------

make_dirichlet_long_data <- function(n_sites = 6L, n_species = 4L,
                                     seed = 11L) {
  set.seed(seed)
  species_levels <- paste0("y", seq_len(n_species))
  alpha <- exp(matrix(rnorm(n_sites * n_species, sd = 0.3),
                      n_sites, n_species))
  Y <- t(apply(alpha, 1L, function(a) {
    d <- rgamma(n_species, shape = a, rate = 1)
    d / sum(d)
  }))
  Y <- pmax(Y, 1e-4); Y <- Y / rowSums(Y)
  data.frame(
    series = factor(rep(species_levels, times = n_sites),
                    levels = species_levels),
    time   = rep(seq_len(n_sites), each = n_species),
    y      = as.vector(t(Y)),
    env    = rep(rnorm(n_sites), each = n_species)
  )
}

test_that("diri() returns a custom family with the right tags", {
  fam <- diri()
  expect_s3_class(fam, "customfamily")
  expect_identical(fam$name, "diri")
  expect_identical(fam$dpars, c("mu", "phi"))
  expect_identical(fam$link, "identity")
  expect_identical(fam$link_phi, "log")
  expect_true(is_closure_unit_family(fam))
  expect_true(is_multi_response_family(fam))
  expect_true(is_simplex_response_family(fam))
  expect_identical(
    attr(fam, "mvgam_vars", exact = TRUE),
    c("N_unit", "n_rep", "visit_idx")
  )
})

test_that("diri() composes with validate_supported_family", {
  expect_invisible(validate_supported_family(diri()))
})

test_that("prepare_closure_unit_family() groups dirichlet rows by site, not by (species, site)", {
  fam <- diri()
  dat <- make_dirichlet_long_data(n_sites = 5L, n_species = 4L)
  fam_prep <- mvgam:::prepare_closure_unit_family(
    fam, dat, response_var = "y",
    has_obs_covariates = FALSE, has_det_covariates = FALSE
  )
  sv <- attr(fam_prep, "mvgam_stanvars", exact = TRUE)
  expect_false(is.null(sv))
  expect_identical(fam_prep$vars, c("N_unit", "n_rep", "visit_idx"))
  # Verify the unit grouping by inspecting the standata round-trip.
  mf <- bf(y ~ env, family = fam_prep)
  sd <- brms::make_standata(mf, data = dat, stanvars = sv)
  expect_identical(sd$N_unit, 5L)
  expect_identical(sd$n_rep, rep(4L, 5L))
  expect_identical(dim(sd$visit_idx), c(5L, 4L))
})

test_that("stancode under diri() emits the lpdf, dirichlet_logit_lpdf, and closure-unit arrays", {
  fam <- diri()
  dat <- make_dirichlet_long_data()
  fam_prep <- mvgam:::prepare_closure_unit_family(
    fam, dat, response_var = "y",
    has_obs_covariates = FALSE, has_det_covariates = FALSE
  )
  sv <- attr(fam_prep, "mvgam_stanvars", exact = TRUE)
  sc <- as.character(brms::make_stancode(
    bf(y ~ env, family = fam_prep),
    data = dat, stanvars = sv
  ))
  expect_match(sc, "real diri_lpdf\\(", fixed = FALSE)
  expect_match(sc, "dirichlet_logit_lpdf", fixed = TRUE)
  expect_match(sc, "int<lower=1> N_unit;", fixed = TRUE)
  expect_match(sc, "array[N_unit] int<lower=1> n_rep;", fixed = TRUE)
  expect_match(sc, "array[N_unit, ", fixed = TRUE)
  # Confirm Y_max / K_max are NOT emitted -- dirichlet skips them.
  expect_false(grepl("array[N_unit] int<lower=0> Y_max", sc,
                     fixed = TRUE))
  expect_false(grepl("array[N_unit] int<lower=1> K_max", sc,
                     fixed = TRUE))
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
  # Count families (binary_response = FALSE) hard-error on this
  # configuration because lambda has unbounded support and the
  # lambda*p product is the only identified quantity (Solymos
  # et al. 2012, Dennis et al. 2015). Binary-response families
  # (occ) demote this to a warning per Royle and Dorazio 2008.
  expect_error(
    validate_closure_unit_data(
      d,
      response_var       = "y",
      has_obs_covariates = FALSE,
      has_det_covariates = FALSE
    ),
    "Closure-unit count family is non-identified"
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

# ------------------------------------------------------------
# Stan emission contract tests via brms make_stancode round-trip
# ------------------------------------------------------------
#
# These tests exercise the data-prep hook in
# `generate_stan_components_mvgam_formula()` to confirm that
# closure-unit arrays land in standata and the nmix lpdf
# function block lands in stancode with the expected signature.

test_that("stancode under nmix() includes the lpdf signature and data declarations", {
  d <- make_nmix_data(n_unit = 4, n_visit = 3)
  mf <- mvgam_formula(y ~ elev)
  sc <- as.character(stancode(mf, data = d, family = nmix()))
  # Function block: both overloaded signatures emitted.
  expect_match(sc, "real nmix_lpmf\\(\\s*array\\[\\] int y,", fixed = FALSE)
  expect_match(sc, "vector mu,", fixed = TRUE)
  expect_match(sc, "vector p,", fixed = TRUE)
  expect_match(sc, "real p,", fixed = TRUE)  # scalar broadcast entry point
  # Stable lpmf forms used inside the loop.
  expect_match(sc, "poisson_log_lpmf(k | log_lam)", fixed = TRUE)
  expect_match(sc, "binomial_logit_lpmf(counts | k, lp_visits)", fixed = TRUE)
  expect_match(sc, "log_sum_exp(component_lps)", fixed = TRUE)
  # Data block: closure-unit arrays at unit length, not visit length.
  expect_match(sc, "int<lower=1> N_unit;", fixed = TRUE)
  expect_match(sc, "array[N_unit] int<lower=1> n_rep;", fixed = TRUE)
  expect_match(sc, "array[N_unit] int<lower=1> K_max;", fixed = TRUE)
  expect_match(sc, "array[N_unit] int<lower=0> Y_max;", fixed = TRUE)
  expect_match(sc, "array[N_unit, 3] int<lower=1> visit_idx;", fixed = TRUE)
  # Likelihood call wires the dpars + vint args correctly.
  expect_match(
    sc,
    "nmix_lpmf(Y | mu, p, N_unit, n_rep, K_max, Y_max, visit_idx)",
    fixed = TRUE
  )
  # Scalar-p case: p declared as a bounded probability so the
  # lpdf's logit(p) call is well-defined even without a
  # `p ~ ...` sub-formula.
  expect_match(sc, "real<lower=0, upper=1> p;", fixed = TRUE)
})

test_that("standata under nmix() carries the closure-unit arrays with correct values", {
  d <- make_nmix_data(n_unit = 5, n_visit = 2, seed = 7)
  mf <- mvgam_formula(y ~ elev)
  sd <- standata(mf, data = d, family = nmix())
  arrs <- build_closure_unit_arrays(d, response_var = "y")
  expect_identical(as.integer(sd$N_unit), arrs$N_unit)
  expect_identical(as.integer(sd$n_rep),  arrs$n_rep)
  expect_identical(as.integer(sd$K_max),  arrs$K_max)
  expect_identical(as.integer(sd$Y_max),  arrs$Y_max)
  expect_equal(dim(sd$visit_idx), c(arrs$N_unit, arrs$max_rep))
  expect_equal(as.integer(sd$visit_idx), as.integer(arrs$visit_idx))
})

test_that("standata K_max updates when newdata carries a different cap column", {
  d_fit  <- make_nmix_data(n_unit = 4, n_visit = 3)
  d_pred <- d_fit
  d_pred$cap <- 99L
  mf <- mvgam_formula(y ~ elev)
  sd_fit  <- standata(mf, data = d_fit,  family = nmix())
  sd_pred <- standata(mf, data = d_pred, family = nmix())
  expect_identical(as.integer(sd_fit$K_max),  rep(20L, 4))
  expect_identical(as.integer(sd_pred$K_max), rep(99L, 4))
  # Visit structure is identical; only K_max responds to the
  # edited cap column.
  expect_identical(sd_fit$N_unit, sd_pred$N_unit)
  expect_identical(sd_fit$n_rep,  sd_pred$n_rep)
  expect_identical(sd_fit$visit_idx, sd_pred$visit_idx)
})

test_that("stancode under nmix() emits vector-p path when a detection sub-formula is supplied", {
  d <- make_nmix_data(n_unit = 4, n_visit = 3)
  d$tod <- stats::runif(nrow(d))
  mf <- mvgam_formula(brms::bf(y ~ elev, p ~ tod))
  sc <- as.character(stancode(mf, data = d, family = nmix()))
  # brms emits a vector p with inv_logit applied when the
  # sub-formula supplies a design matrix for the detection dpar.
  expect_match(sc, "vector[N] p", fixed = TRUE)
  expect_match(sc, "p = inv_logit(p)", fixed = TRUE)
  # b_p coefficients become available to the user.
  expect_match(sc, "Intercept_p", fixed = TRUE)
  expect_match(sc, "Xc_p", fixed = TRUE)
})

# ------------------------------------------------------------
# R-side prediction surface (chunk 3): log_lik, posterior_epred,
# posterior_predict, predict(latent_N), predict(detection)
# ------------------------------------------------------------
#
# These tests fit a small nmix() model on simulated data with
# known truth (lambda_intercept = 1, lambda_elev_slope = 0.5,
# p = 0.6) and verify that each surface returns the expected
# shape and recovers the truth within a wide CI.

# Cache one short fit at the top so each test runs fast. The
# truth values are fixed by the seed; recovery is loose because
# the chain is short (300 iter), so the assertions check shape
# + sign + order-of-magnitude rather than tight intervals.
local_nmix_fit <- function() {
  set.seed(42)
  n_unit <- 15
  n_visit <- 3
  elev <- rnorm(n_unit)
  log_lambda <- 1 + 0.5 * elev
  N_per <- rpois(n_unit, exp(log_lambda))
  p_true <- 0.6
  y_sim <- as.integer(unlist(lapply(N_per, function(N) {
    rbinom(n_visit, N, p_true)
  })))
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim,
    cap    = rep(30L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit)
  )
  fit <- mvgam(y ~ elev,
               family = nmix(),
               data = d,
               chains = 1, iter = 300, warmup = 150,
               silent = 2, refresh = 0)
  list(fit = fit, data = d, N_per = N_per, p_true = p_true)
}

test_that("posterior_epred.mvgam returns [S x N_visit] for nmix and recovers lambda * p", {
  bundle <- local_nmix_fit()
  pe <- posterior_epred(bundle$fit)
  expect_equal(dim(pe), c(150L, nrow(bundle$data)))
  # Mean of E[Y] should be in the right ballpark of the data mean.
  expect_lt(abs(mean(pe) - mean(bundle$data$y)), 1.0)
})

test_that("posterior_predict.mvgam returns [S x N_visit] integer counts for nmix", {
  bundle <- local_nmix_fit()
  pp <- posterior_predict(bundle$fit)
  expect_equal(dim(pp), c(150L, nrow(bundle$data)))
  expect_true(all(pp == as.integer(pp)))
  expect_true(all(pp >= 0))
  # Marginal mean within data-mean ballpark.
  expect_lt(abs(mean(pp) - mean(bundle$data$y)), 1.5)
})

test_that("log_lik.mvgam returns [S x N_unit] for nmix (closure-unit grain for LOO)", {
  bundle <- local_nmix_fit()
  ll <- log_lik(bundle$fit)
  n_unit <- length(unique(bundle$data$series))
  expect_equal(dim(ll), c(150L, n_unit))
  expect_true(all(is.finite(ll)))
})

test_that("predict.mvgam(type = 'latent_N') returns [S x N_unit] integer N draws covering truth", {
  bundle <- local_nmix_fit()
  ln <- predict(bundle$fit, type = "latent_N", summary = FALSE)
  expect_equal(dim(ln), c(150L, length(unique(bundle$data$series))))
  expect_true(all(ln == as.integer(ln)))
  # Per-unit mean should land near the simulated N's mean.
  expect_lt(
    abs(mean(colMeans(ln)) - mean(bundle$N_per)),
    1.5
  )
})

test_that("predict.mvgam(type = 'detection') returns [S x N_visit] in (0, 1)", {
  bundle <- local_nmix_fit()
  de <- predict(bundle$fit, type = "detection", summary = FALSE)
  expect_equal(dim(de), c(150L, nrow(bundle$data)))
  expect_true(all(de > 0 & de < 1))
  # Scalar-p case: every visit column should have the same draw.
  expect_true(all(de[, 1L] == de[, 2L]))
  # Posterior mean covers the truth.
  expect_lt(abs(mean(de) - bundle$p_true), 0.2)
})

test_that("predict.mvgam(type = 'latent_N') errors on non-nmix families", {
  set.seed(1)
  d <- data.frame(
    series = factor(rep(1L:3L, each = 4L)),
    time   = 1L:4L,
    y      = rnorm(12L),
    elev   = rnorm(12L)
  )
  fit <- mvgam(y ~ elev, data = d, chains = 1, iter = 100,
               warmup = 50, silent = 2, refresh = 0)
  expect_error(
    predict(fit, type = "latent_N"),
    "not available for this family"
  )
  expect_error(
    predict(fit, type = "detection"),
    "not available for this family"
  )
})

test_that("nmix vector-p R-side prediction recovers the detection-covariate effect", {
  set.seed(31)
  n_unit <- 18; n_visit <- 4
  elev <- rnorm(n_unit)
  tod  <- stats::runif(n_unit * n_visit)
  N_per <- rpois(n_unit, exp(1.2 + 0.6 * elev))
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    visit_rows <- ((g - 1) * n_visit + 1):(g * n_visit)
    for (j in visit_rows) {
      y_sim[j] <- rbinom(1, N_per[g], plogis(-0.2 + 1.3 * tod[j]))
    }
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim, cap = rep(30L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit), tod = tod
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ tod), family = nmix(),
               data = d, chains = 1, iter = 400, warmup = 200,
               silent = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(de), c(200L, nrow(d)))
  # Vector-p must vary across visits (unlike the scalar case).
  expect_gt(stats::sd(apply(de, 2L, median)), 1e-3)
  # Detection covaries strongly with tod by construction.
  expect_gt(stats::cor(apply(de, 2L, median), d$tod), 0.8)
  # All five surfaces should run without error.
  expect_no_error(posterior_epred(fit))
  expect_no_error(posterior_predict(fit))
  expect_no_error(log_lik(fit))
  expect_no_error(predict(fit, type = "latent_N", summary = FALSE))
})

test_that("nmix smooth-in-p recovers a known non-linear effect", {
  set.seed(11)
  n_unit <- 25; n_visit <- 4
  elev <- rnorm(n_unit)
  tod  <- stats::runif(n_unit * n_visit)
  N_per <- rpois(n_unit, exp(1.2 + 0.6 * elev))
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1) * n_visit + 1):(g * n_visit)
    for (j in rows) {
      eta_p <- -0.5 + 2 * sin(2 * pi * tod[j])
      y_sim[j] <- rbinom(1, N_per[g], plogis(eta_p))
    }
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim, cap = rep(40L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit), tod = tod
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ s(tod, k = 8)),
               family = nmix(), data = d,
               chains = 1, iter = 400, warmup = 200,
               silent = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(de), c(200L, nrow(d)))
  de_med <- apply(de, 2L, median)
  truth <- plogis(-0.5 + 2 * sin(2 * pi * d$tod))
  expect_gt(stats::cor(de_med, truth), 0.9)
})

test_that("nmix random-effects-in-p recovers per-observer detection", {
  set.seed(21)
  n_unit <- 25; n_visit <- 4
  n_obs_total <- n_unit * n_visit
  elev <- rnorm(n_unit)
  N_per <- rpois(n_unit, exp(1.2 + 0.6 * elev))
  observer <- factor(sample(letters[1:5], n_obs_total, replace = TRUE))
  obs_effects <- c(a = -0.5, b = 0.3, c = 1.0, d = -0.2, e = 0.8)
  y_sim <- integer(n_obs_total)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1) * n_visit + 1):(g * n_visit)
    for (j in rows) {
      y_sim[j] <- rbinom(1, N_per[g],
                          plogis(0 + obs_effects[observer[j]]))
    }
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_obs_total),
    y      = y_sim, cap = rep(40L, n_obs_total),
    elev   = rep(elev, each = n_visit), observer = observer
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ (1 | observer)),
               family = nmix(), data = d,
               chains = 1, iter = 400, warmup = 200,
               silent = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  de_med <- apply(de, 2L, median)
  truth <- plogis(0 + obs_effects[d$observer])
  expect_gt(stats::cor(de_med, truth), 0.85)
})

test_that("nmix gp-in-p recovers a non-linear detection effect", {
  set.seed(31)
  n_unit <- 25; n_visit <- 4
  elev <- rnorm(n_unit)
  tod  <- stats::runif(n_unit * n_visit)
  N_per <- rpois(n_unit, exp(1.2 + 0.6 * elev))
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1) * n_visit + 1):(g * n_visit)
    for (j in rows) {
      eta_p <- -0.2 + 1.8 * sin(2 * pi * tod[j])
      y_sim[j] <- rbinom(1, N_per[g], plogis(eta_p))
    }
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim, cap = rep(40L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit), tod = tod
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ gp(tod, k = 8, c = 5/4)),
               family = nmix(), data = d,
               chains = 1, iter = 400, warmup = 200,
               silent = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(de), c(200L, nrow(d)))
  de_med <- apply(de, 2L, median)
  truth <- plogis(-0.2 + 1.8 * sin(2 * pi * d$tod))
  expect_gt(stats::cor(de_med, truth), 0.85)
})

test_that("nmix predict(type = 'variance') equals predict(type = 'expected')", {
  bundle <- local_nmix_fit()
  v <- predict(bundle$fit, type = "variance", summary = FALSE)
  e <- predict(bundle$fit, type = "expected", summary = FALSE)
  expect_equal(dim(v), dim(e))
  # Var[Y] = lambda * p = E[Y] under the thinned-Poisson property
  # of the Poisson-Binomial mixture.
  expect_equal(v, e)
})

# ------------------------------------------------------------
# how_to_cite() coverage for nmix
# ------------------------------------------------------------

test_that("how_to_cite reference_db includes the four nmix entries", {
  db <- reference_db()
  expected <- c(
    "royle_nmix_2004",
    "dennis_nmix_2015",
    "kery_nmix_2018",
    "knape_overdispersion_2018"
  )
  expect_true(all(expected %in% names(db)))
  for (key in expected) {
    expect_true(nzchar(db[[key]]$text))
    expect_true(nzchar(db[[key]]$bibtex))
  }
})

test_that("uses_nmix_family() predicate distinguishes the family", {
  expect_false(uses_nmix_family(NULL))
  expect_false(uses_nmix_family(list(family = gaussian())))
  expect_true(uses_nmix_family(list(family = nmix())))
  expect_false(uses_nmix_family(list(family = tweedie())))
  # Royle-Nichols is its own predicate; the PB predicate excludes it
  # so the citation rule picks the variant-specific reference set.
  expect_false(
    uses_nmix_family(list(family = nmix("royle_nichols")))
  )
})

test_that("uses_nmix_royle_nichols_family() predicate distinguishes the RN variant", {
  expect_false(uses_nmix_royle_nichols_family(NULL))
  expect_false(uses_nmix_royle_nichols_family(list(family = gaussian())))
  expect_false(uses_nmix_royle_nichols_family(list(family = nmix())))
  expect_true(
    uses_nmix_royle_nichols_family(list(family = nmix("royle_nichols")))
  )
})

test_that("how_to_cite reference_db carries the Royle-Nichols 2003 entry", {
  db <- mvgam:::reference_db()
  expect_true("royle_nichols_2003" %in% names(db))
  rn <- db[["royle_nichols_2003"]]
  expect_true(grepl("Royle JA and Nichols JD", rn$text))
  expect_true(grepl("royle2003abundance", rn$bibtex))
})

# ------------------------------------------------------------
# nmix("royle_nichols") — Stan emission, dispatcher, recovery
# ------------------------------------------------------------

test_that("nmix('royle_nichols') constructor exposes the RN family name and binary-response flag", {
  fam <- nmix("royle_nichols")
  expect_identical(fam$name, "nmix_royle_nichols")
  expect_true(isTRUE(attr(fam, "mvgam_closure_unit", exact = TRUE)))
  expect_identical(attr(fam, "mvgam_nmix_type", exact = TRUE),
                   "royle_nichols")
  expect_true(isTRUE(attr(fam, "mvgam_binary_response",
                          exact = TRUE)))
  # RN keeps default_cap unset because latent N can exceed 1; the
  # user must supply the cap column.
  expect_null(attr(fam, "mvgam_default_cap", exact = TRUE))
  expect_identical(attr(fam, "mvgam_predict_types", exact = TRUE),
                   c("latent_N", "detection"))
})

test_that("nmix('royle_nichols') Stan emission carries the RN lpmf and the binary Y_max upper bound", {
  set.seed(7)
  n_unit <- 25L; n_visit <- 3L
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(seq_len(n_visit), n_unit),
    y      = rbinom(n_unit * n_visit, 1, 0.4),
    cap    = rep(8L, n_unit * n_visit),
    elev   = rep(rnorm(n_unit), each = n_visit)
  )
  prefit <- mvgam(y ~ elev, family = nmix("royle_nichols"),
                  data = d, algorithm = "sampling", chains = 0)
  sc <- stancode(prefit)
  expect_true(grepl("nmix_royle_nichols_lpmf", sc))
  expect_true(grepl("array\\[N_unit\\] int<lower=0, upper=1> Y_max",
                    sc))
  expect_true(grepl("array\\[N_unit\\] int<lower=1> K_max", sc))
  expect_true(grepl("log1m_exp", sc))
  # The Bollen `+ 1` literal on the occ==0 log-prob must not leak
  # into the emitted lpmf.
  expect_false(grepl("\\+ 1;\\s*//\\s*occ", sc))
})

test_that("nmix('royle_nichols') end-to-end fit returns correct grain for every dispatcher arm", {
  set.seed(101)
  n_unit <- 30L; n_visit <- 4L
  elev <- rnorm(n_unit)
  lambda_true <- exp(1.2 + 0.6 * elev)
  r_true <- 0.3
  N_per <- rpois(n_unit, lambda_true)
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1L) * n_visit + 1L):(g * n_visit)
    p_visit <- 1 - (1 - r_true)^N_per[g]
    y_sim[rows] <- rbinom(n_visit, 1, p_visit)
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim,
    cap    = rep(15L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit)
  )
  fit <- mvgam(y ~ elev,
               family    = nmix("royle_nichols"),
               data      = d,
               chains    = 1, iter = 300, warmup = 150,
               silent    = 2, refresh = 0)
  n_total <- n_unit * n_visit
  yhat <- posterior_predict(fit)
  expect_equal(dim(yhat), c(150L, n_total))
  expect_true(all(yhat %in% c(0L, 1L)))
  ehat <- posterior_epred(fit)
  expect_equal(dim(ehat), c(150L, n_total))
  expect_true(all(ehat > 0 & ehat < 1))
  ll <- log_lik(fit)
  expect_equal(dim(ll), c(150L, n_unit))
  expect_true(all(is.finite(ll)))
  latent <- predict(fit, type = "latent_N", summary = FALSE)
  expect_equal(dim(latent), c(150L, n_unit))
  expect_true(all(latent >= 0L & latent <= 15L))
  expect_true(all(latent == as.integer(latent)))
  det <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(det), c(150L, n_total))
  expect_true(all(det > 0 & det < 1))
})

test_that("nmix('royle_nichols') smooth-r recovers a known non-linear detection effect", {
  set.seed(202)
  n_unit <- 40L; n_visit <- 5L
  elev <- rnorm(n_unit)
  tod  <- stats::runif(n_unit * n_visit)
  N_per <- rpois(n_unit, exp(1.4 + 0.5 * elev))
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1L) * n_visit + 1L):(g * n_visit)
    for (j in rows) {
      r_j <- plogis(-0.5 + 2 * sin(2 * pi * tod[j]))
      p_visit <- 1 - (1 - r_j)^N_per[g]
      y_sim[j] <- rbinom(1, 1, p_visit)
    }
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim,
    cap    = rep(40L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit),
    tod    = tod
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ s(tod, k = 8)),
               family    = nmix("royle_nichols"),
               data      = d,
               chains    = 1, iter = 400, warmup = 200,
               silent    = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(de), c(200L, nrow(d)))
  de_med <- apply(de, 2L, median)
  truth <- plogis(-0.5 + 2 * sin(2 * pi * d$tod))
  # RN binary data is less informative than PB counts per visit,
  # so the recovery threshold is looser than the PB equivalent
  # (which targets > 0.9).
  expect_gt(stats::cor(de_med, truth), 0.7)
})

test_that("nmix('royle_nichols') random-effects-r recovers per-observer detection", {
  set.seed(303)
  n_unit <- 40L; n_visit <- 5L
  n_obs_total <- n_unit * n_visit
  elev <- rnorm(n_unit)
  N_per <- rpois(n_unit, exp(1.4 + 0.5 * elev))
  observer <- factor(sample(letters[1:5], n_obs_total, replace = TRUE))
  obs_effects <- c(a = -0.5, b = 0.3, c = 1.0, d = -0.2, e = 0.8)
  y_sim <- integer(n_obs_total)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1L) * n_visit + 1L):(g * n_visit)
    for (j in rows) {
      r_j <- plogis(0 + obs_effects[observer[j]])
      p_visit <- 1 - (1 - r_j)^N_per[g]
      y_sim[j] <- rbinom(1, 1, p_visit)
    }
  }
  d <- data.frame(
    series   = factor(rep(seq_len(n_unit), each = n_visit)),
    time     = rep(1L, n_obs_total),
    y        = y_sim,
    cap      = rep(40L, n_obs_total),
    elev     = rep(elev, each = n_visit),
    observer = observer
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ (1 | observer)),
               family    = nmix("royle_nichols"),
               data      = d,
               chains    = 1, iter = 400, warmup = 200,
               silent    = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  de_med <- apply(de, 2L, median)
  truth <- plogis(0 + obs_effects[d$observer])
  expect_gt(stats::cor(de_med, truth), 0.6)
})

test_that("nmix('royle_nichols') smooth-on-state recovers a non-linear lambda effect", {
  set.seed(404)
  n_unit <- 50L; n_visit <- 5L
  elev <- stats::runif(n_unit, -2, 2)
  # Non-linear lambda(elev): peaks in the middle of the range
  lambda_true <- exp(1.0 + 1.5 * exp(-elev^2 / 2) - 0.5)
  r_true <- 0.4
  N_per <- rpois(n_unit, lambda_true)
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1L) * n_visit + 1L):(g * n_visit)
    p_visit <- 1 - (1 - r_true)^N_per[g]
    y_sim[rows] <- rbinom(n_visit, 1, p_visit)
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim,
    cap    = rep(30L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit)
  )
  fit <- mvgam(y ~ s(elev, k = 8),
               family    = nmix("royle_nichols"),
               data      = d,
               chains    = 1, iter = 400, warmup = 200,
               silent    = 2, refresh = 0)
  ehat <- posterior_epred(fit)
  ehat_med <- apply(ehat, 2L, median)
  # Truth at each visit: 1 - exp(-r * lambda(elev))
  truth <- 1 - exp(-r_true * exp(1.0 + 1.5 * exp(-d$elev^2 / 2) - 0.5))
  expect_gt(stats::cor(ehat_med, truth), 0.7)
})

# ------------------------------------------------------------
# nmix("poisson_poisson") — Stan emission, dispatcher, recovery
# ------------------------------------------------------------

test_that("nmix('poisson_poisson') constructor exposes the PPM family name and count-response config", {
  fam <- nmix("poisson_poisson")
  expect_identical(fam$name, "nmix_poisson_poisson")
  expect_true(isTRUE(attr(fam, "mvgam_closure_unit", exact = TRUE)))
  expect_identical(attr(fam, "mvgam_nmix_type", exact = TRUE),
                   "poisson_poisson")
  # PPM accepts arbitrary counts so the binary-response check is OFF.
  expect_false(isTRUE(attr(fam, "mvgam_binary_response",
                           exact = TRUE)))
  expect_null(attr(fam, "mvgam_default_cap", exact = TRUE))
  expect_identical(attr(fam, "mvgam_predict_types", exact = TRUE),
                   c("latent_N", "detection"))
  # Log link on p (not logit) so the rate stays on positive reals.
  expect_identical(fam$link_p, "log")
  expect_true(is.na(fam$ub[2L]))
})

test_that("nmix('poisson_poisson') Stan emission carries the factored Poisson lpmf", {
  set.seed(7)
  n_unit <- 25L; n_visit <- 3L
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(seq_len(n_visit), n_unit),
    y      = rpois(n_unit * n_visit, 2),
    cap    = rep(20L, n_unit * n_visit),
    elev   = rep(rnorm(n_unit), each = n_visit)
  )
  prefit <- mvgam(y ~ elev, family = nmix("poisson_poisson"),
                  data = d, algorithm = "sampling", chains = 0)
  sc <- stancode(prefit)
  expect_true(grepl("nmix_poisson_poisson_lpmf", sc))
  expect_true(grepl("array\\[N_unit\\] int<lower=1> K_max", sc))
  # Y_max stays unbounded for PPM (y_t can exceed N).
  expect_true(grepl("array\\[N_unit\\] int<lower=0> Y_max", sc))
  expect_false(grepl("upper=1>\\s+Y_max", sc))
  # Factored constants in the loop body.
  expect_true(grepl("sum_counts", sc))
  expect_true(grepl("sum_y_log_p", sc))
  expect_true(grepl("sum_p_v", sc))
  expect_true(grepl("lgamma_const", sc))
  # No O(n_rep) per-k poisson_log_lpmf(counts | ...) call.
  expect_false(grepl("poisson_log_lpmf\\(counts\\s*\\|\\s*log\\(k\\)",
                     sc))
  # Log link on p (not logit).
  expect_true(grepl("log\\(p\\)", sc))
  expect_false(grepl("logit\\(p\\)", sc))
})

test_that("nmix('poisson_poisson') intercept-only spec emits the identifiability warn from prepare_closure_unit_family()", {
  set.seed(99)
  n_unit <- 15L; n_visit <- 3L
  # 3 visits per closure unit (same series, same time across the
  # n_visit rows) so the validator's "every unit single visit + no
  # covariates" hard-error does not fire and only the PPM
  # identifiability warn raises.
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = rpois(n_unit * n_visit, 2),
    cap    = rep(20L, n_unit * n_visit)
  )
  fam <- nmix("poisson_poisson")
  # Call prepare_closure_unit_family() directly: the warn fires
  # there, and routing through mvgam() would conflate this with
  # unrelated Stan-compile warnings (e.g. E-BFMI from chains = 0).
  # rlang::warn(..., .frequency = "once") is gated by TESTTHAT in
  # the production code; flip it off so the warn raises.
  withr::with_envvar(c(TESTTHAT = ""), {
    expect_warning(
      prepare_closure_unit_family(
        fam,
        data = d,
        response_var = "y",
        has_obs_covariates = FALSE,
        has_det_covariates = FALSE
      ),
      "weakly identified"
    )
  })
})

test_that("nmix('poisson_poisson') end-to-end fit returns correct grain for every dispatcher arm", {
  set.seed(202)
  n_unit <- 30L; n_visit <- 4L
  elev <- rnorm(n_unit)
  lambda_true <- exp(1.0 + 0.5 * elev)
  p_true <- 0.4
  N_per <- rpois(n_unit, lambda_true)
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1L) * n_visit + 1L):(g * n_visit)
    y_sim[rows] <- rpois(n_visit, lambda = N_per[g] * p_true)
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim,
    cap    = rep(30L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit)
  )
  fit <- mvgam(y ~ elev,
               family    = nmix("poisson_poisson"),
               data      = d,
               chains    = 1, iter = 300, warmup = 150,
               silent    = 2, refresh = 0)
  n_total <- n_unit * n_visit
  yhat <- posterior_predict(fit)
  expect_equal(dim(yhat), c(150L, n_total))
  expect_true(all(yhat == as.integer(yhat)))
  expect_true(all(yhat >= 0L))
  ehat <- posterior_epred(fit)
  expect_equal(dim(ehat), c(150L, n_total))
  expect_true(all(ehat > 0))
  ll <- log_lik(fit)
  expect_equal(dim(ll), c(150L, n_unit))
  expect_true(all(is.finite(ll)))
  latent <- predict(fit, type = "latent_N", summary = FALSE)
  expect_equal(dim(latent), c(150L, n_unit))
  expect_true(all(latent >= 0L & latent <= 30L))
  expect_true(all(latent == as.integer(latent)))
  det <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(det), c(150L, n_total))
  expect_true(all(det > 0))
})

test_that("nmix('poisson_poisson') smooth-p recovers a known non-linear encounter-rate effect", {
  set.seed(303)
  n_unit <- 40L; n_visit <- 5L
  elev <- rnorm(n_unit)
  tod  <- stats::runif(n_unit * n_visit)
  N_per <- rpois(n_unit, exp(1.2 + 0.5 * elev))
  y_sim <- integer(n_unit * n_visit)
  for (g in seq_len(n_unit)) {
    rows <- ((g - 1L) * n_visit + 1L):(g * n_visit)
    for (j in rows) {
      p_j <- exp(-1 + 1.2 * sin(2 * pi * tod[j]))
      y_sim[j] <- rpois(1, lambda = N_per[g] * p_j)
    }
  }
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_unit * n_visit),
    y      = y_sim,
    cap    = rep(40L, n_unit * n_visit),
    elev   = rep(elev, each = n_visit),
    tod    = tod
  )
  fit <- mvgam(brms::bf(y ~ elev, p ~ s(tod, k = 8)),
               family    = nmix("poisson_poisson"),
               data      = d,
               chains    = 1, iter = 400, warmup = 200,
               silent    = 2, refresh = 0)
  de <- predict(fit, type = "detection", summary = FALSE)
  expect_equal(dim(de), c(200L, nrow(d)))
  de_med <- apply(de, 2L, median)
  truth <- exp(-1 + 1.2 * sin(2 * pi * d$tod))
  # PPM encounter rates are unidentified up to a scale (banana
  # ridge on the `lambda * p` product), so absolute recovery may
  # be off; the shape correlation with the truth is what the
  # smooth identifies once the elev covariate partially constrains
  # `lambda`. Threshold tightened from 0.7 to 0.80 per stats
  # review: a correct factored Poisson precompute on this
  # simulation should recover the shape well above 0.7, so a
  # weaker threshold has no power to catch an implementation
  # error in the precompute terms.
  expect_gt(stats::cor(de_med, truth), 0.80)
})

test_that("uses_nmix_poisson_poisson_family() predicate distinguishes the PPM variant", {
  expect_false(uses_nmix_poisson_poisson_family(NULL))
  expect_false(uses_nmix_poisson_poisson_family(list(family = gaussian())))
  expect_false(uses_nmix_poisson_poisson_family(list(family = nmix())))
  expect_false(
    uses_nmix_poisson_poisson_family(list(family = nmix("royle_nichols")))
  )
  expect_true(
    uses_nmix_poisson_poisson_family(list(family = nmix("poisson_poisson")))
  )
})

test_that("how_to_cite reference_db carries the Neyman 1939 entry", {
  db <- mvgam:::reference_db()
  expect_true("neyman_type_a_1939" %in% names(db))
  ny <- db[["neyman_type_a_1939"]]
  expect_true(grepl("Neyman J", ny$text))
  expect_true(grepl("neyman1939contagious", ny$bibtex))
})
