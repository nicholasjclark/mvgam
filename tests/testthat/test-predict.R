test_that("a monotonic variable keeps the coding brms gives it", {
  # brms codes a variable with D + 1 levels as 0..D in every frame. A
  # frame holding only the upper levels starts at 1. Its values keep
  # that coding. A shift to start at 0 would put every row a level low.
  expect_identical(
    validate_monotonic_indices(c(0, 1, 2, 1, 0), "Xmo_1", 2L, 5L),
    c(0L, 1L, 2L, 1L, 0L)
  )
  expect_identical(
    validate_monotonic_indices(c(2, 2, 1), "Xmo_1", 2L, 3L),
    c(2L, 2L, 1L)
  )
  expect_error(
    validate_monotonic_indices(c(0, 3), "Xmo_1", 2L, 2L),
    "outside its coding"
  )
  expect_error(
    validate_monotonic_indices(c(0, 1), "Xmo_1", 2L, 5L),
    "does not cover"
  )
})

test_that("the exp_quad density matches brms's Stan function", {
  # brms's anisotropic `spd_gp_exp_quad()`: sdgp^2 * sqrt(2 pi)^D *
  # prod(l) * exp(-0.5 * sum(l^2 * x^2)), one row per draw.
  slambda <- array(c(1, 2, 3, 4), dim = c(2, 2))
  sdgp <- c(0.5, 0.8)
  lscale <- matrix(c(1.0, 1.5, 2.0, 2.5), nrow = 2, ncol = 2)

  result <- gp_spectral_density(slambda, sdgp, lscale, "exp_quad")
  expected <- outer(seq_len(2), seq_len(2), Vectorize(function(d, m) {
    sdgp[d]^2 * sqrt(2 * pi)^2 * prod(lscale[d, ]) *
      exp(-0.5 * sum(lscale[d, ]^2 * slambda[m, ]^2))
  }))
  expect_equal(result, expected)
})

test_that("each draw's spectral density is its own", {
  # An isotropic kernel's density at a draw depends on that draw's
  # length scale alone. The Matern kernels summed the length scale over
  # every draw, which no single-draw call can show.
  slambda <- array(c(0.5, 1, 1.5), dim = c(3, 1))
  sdgp <- c(0.5, 2)
  lscale <- matrix(c(1, 3), nrow = 2)
  for (kernel in c("exp_quad", "matern32", "matern52")) {
    both <- gp_spectral_density(slambda, sdgp, lscale, kernel)
    alone <- rbind(
      gp_spectral_density(slambda, sdgp[1], lscale[1, , drop = FALSE],
                          kernel),
      gp_spectral_density(slambda, sdgp[2], lscale[2, , drop = FALSE],
                          kernel)
    )
    expect_equal(both, alone)
  }
})

test_that("the Matern densities match brms's Stan functions", {
  # In one dimension brms's `spd_gp_matern32()` is sdgp^2 * 2 *
  # sqrt(pi) * gamma(2) * 3^1.5 / (0.5 * sqrt(pi)) * l *
  # (3 + l^2 * x^2)^-2. Its `spd_gp_matern52()` is sdgp^2 * 2 *
  # sqrt(pi) * gamma(3) * 5^2.5 / (0.75 * sqrt(pi)) * l *
  # (5 + l^2 * x^2)^-3.
  x <- c(0.5, 1, 1.5)
  sdgp <- 2
  l <- 3
  slambda <- array(x, dim = c(3, 1))
  expect_equal(
    as.vector(gp_spectral_density(slambda, sdgp, matrix(l), "matern32")),
    sdgp^2 * 4 * 3^1.5 * l * (3 + l^2 * x^2)^-2
  )
  expect_equal(
    as.vector(gp_spectral_density(slambda, sdgp, matrix(l), "matern52")),
    sdgp^2 * 4 * 5^2.5 / 0.75 * l * (5 + l^2 * x^2)^-3
  )
})

# ==============================================================================
# validate_prediction_factor_levels tests
# ==============================================================================

# An axes record as `extract_time_series_dimensions()` writes it.
axes_metadata <- function(series, source = "explicit", gr_var = NULL,
                          subgr_var = NULL, gr = NULL, subgr = NULL) {
  list(axes = list(
    series = list(levels = series, source = source,
                  n = length(series)),
    vars = list(time_var = "time", series_var = "series",
                gr_var = gr_var, subgr_var = subgr_var),
    group_levels = list(gr = gr, subgr = subgr)
  ))
}

test_that("validate_prediction_factor_levels refuses unseen series", {
  metadata <- axes_metadata(c("s1", "s2", "s3"))
  # A series column arrives as a factor or as character. The comparison
  # reaches the same verdict on each.
  for (mk in list(factor, identity)) {
    valid <- data.frame(time = 1:3, series = mk(c("s1", "s2", "s1")))
    expect_silent(validate_prediction_factor_levels(valid, metadata))

    invalid <- data.frame(time = 1:3, series = mk(c("s1", "s4", "s1")))
    expect_error(
      validate_prediction_factor_levels(invalid, metadata),
      "Series in 'newdata' has levels absent"
    )
  }
  # A dead factor level names no unseen series.
  dead <- data.frame(time = 1:2,
                     series = factor(c("s1", "s2"), levels = c("s1", "s2",
                                                               "s9")))
  expect_silent(validate_prediction_factor_levels(dead, metadata))
  # A model whose frame names no axis carries no record to check.
  expect_silent(validate_prediction_factor_levels(invalid, NULL))
})

test_that("a frame is held to the axis columns the fit needs", {
  # `varying_meta_vars()` keeps a meta var only where it varies in
  # training. A fit on one occasion has no time column in that list.
  # The completeness check still requires the time column.
  set.seed(1)
  flat <- data.frame(y = rpois(30, 3), x = rnorm(30),
                     time = rep(1L, 30),
                     series = factor(rep(c("a", "b", "c"), each = 10)))
  prefit <- mvgam(y ~ x, trend_formula = ~ ZMVN(), data = flat,
                  family = poisson(), run_model = FALSE, silent = 2,
                  validate = FALSE)
  no_time <- flat
  no_time$time <- NULL
  expect_error(
    mvgam:::validate_newdata_complete(no_time, prefit),
    "Absent: 'time'"
  )
  # A trend-free fit places no row in time and needs no time column.
  trend_free <- mvgam(y ~ x, data = flat, family = poisson(),
                      run_model = FALSE, silent = 2, validate = FALSE)
  expect_silent(mvgam:::validate_newdata_complete(no_time, trend_free))

  # A frame omitting the series column reaches prediction with every
  # row unmapped. The column is exempt where a grouping names the
  # series; on a fit whose axis is the column itself it is the only
  # route.
  d <- data.frame(y = rpois(40, 3), x = rnorm(40),
                  time = rep(1:20, 2),
                  series = factor(rep(c("a", "b"), each = 20)))
  fit <- mvgam(y ~ x, trend_formula = ~ AR(p = 1), data = d,
               family = poisson(), run_model = FALSE, silent = 2,
               validate = FALSE)
  no_series <- d
  no_series$series <- NULL
  expect_error(
    mvgam:::validate_newdata_complete(no_series, fit),
    "Absent: 'series'"
  )

  # One series needs no column of its own: every row belongs to it.
  one <- data.frame(y = rpois(20, 3), x = rnorm(20), time = 1:20,
                    series = factor(rep("only", 20)))
  solo <- mvgam(y ~ x, trend_formula = ~ AR(p = 1), data = one,
                family = poisson(), run_model = FALSE, silent = 2,
                validate = FALSE)
  bare <- one
  bare$series <- NULL
  expect_silent(mvgam:::validate_newdata_complete(bare, solo))
})

test_that("validate_prediction_factor_levels refuses unseen groups", {
  metadata <- axes_metadata(c("s1"), gr_var = "group",
                            gr = c("group_a", "group_b", "group_c"))
  valid <- data.frame(time = 1:3, series = factor("s1"),
                      group = factor(c("group_a", "group_b", "group_a")))
  expect_silent(validate_prediction_factor_levels(valid, metadata))

  invalid <- valid
  invalid$group <- factor(c("group_a", "group_d", "group_a"))
  expect_error(
    validate_prediction_factor_levels(invalid, metadata),
    "Column 'group' in 'newdata' has levels absent"
  )
})

test_that("a hierarchical series is checked through its grouping", {
  # Each level alone was seen in training. The pairs a_y and b_x were
  # never fitted.
  metadata <- axes_metadata(
    c("a_x", "b_y"), source = "hierarchical",
    gr_var = "g", subgr_var = "sg", gr = c("a", "b"), subgr = c("x", "y")
  )
  newdata <- data.frame(time = 1:2, g = c("a", "b"), sg = c("y", "x"))
  expect_error(
    validate_prediction_factor_levels(newdata, metadata),
    "Series in 'newdata' has levels absent"
  )
})

# ==============================================================================
# compute_family_epred tests
# ==============================================================================

test_that("compute_family_epred handles simple families correctly", {
  # Create test linpred matrix
  set.seed(123)
  linpred <- matrix(rnorm(20), nrow = 4, ncol = 5)

  # Poisson family (log link): epred = exp(linpred)
  poisson_family <- list(
    family = "poisson",
    linkinv = exp
  )
  epred_poisson <- compute_family_epred(linpred, poisson_family)
  expect_equal(epred_poisson, exp(linpred))

  # Gaussian family (identity link): epred = linpred
  gaussian_family <- list(
    family = "gaussian",
    linkinv = identity
  )
  epred_gaussian <- compute_family_epred(linpred, gaussian_family)
  expect_equal(epred_gaussian, linpred)

  # Bernoulli family (logit link): epred = plogis(linpred)
  bernoulli_family <- list(
    family = "bernoulli",
    linkinv = plogis
  )
  epred_bernoulli <- compute_family_epred(linpred, bernoulli_family)
  expect_equal(epred_bernoulli, plogis(linpred))
})

test_that("compute_family_epred handles binomial with trials", {
  set.seed(123)
  linpred <- matrix(rnorm(20), nrow = 4, ncol = 5)

  binomial_family <- list(
    family = "binomial",
    linkinv = plogis
  )

  # Requires trials argument
  expect_error(
    compute_family_epred(linpred, binomial_family),
    "requires.*trials"
  )

  # With trials: epred = p * trials (column-wise via R's recycling)
  # linpred is [ndraws x nobs], trials is per-observation (per-column)
  trials <- c(10, 20, 15, 25, 30)
  epred <- compute_family_epred(linpred, binomial_family, trials = trials)

  # R recycles trials across columns (each column multiplied by its trial count)
  prob <- plogis(linpred)
  expected <- sweep(prob, 2, trials, `*`)
  expect_equal(epred, expected)
})

test_that("compute_family_epred handles lognormal with sigma", {
  set.seed(123)
  linpred <- matrix(rnorm(20, mean = 1), nrow = 4, ncol = 5)
  sigma <- matrix(abs(rnorm(20, mean = 0.5)), nrow = 4, ncol = 5)
  lognormal_family <- brms::lognormal()

  # The Jensen correction needs sigma. The mean is refused without it.
  expect_error(
    compute_family_epred(linpred, lognormal_family),
    "sigma"
  )

  # With sigma: E[Y] = exp(mu + sigma^2/2)
  epred <- compute_family_epred(linpred, lognormal_family,
                                family_pars = list(sigma = sigma))
  expected <- exp(linpred + sigma^2 / 2)
  expect_equal(epred, expected)
})

test_that("compute_family_epred rejects unsupported families", {
  linpred <- matrix(rnorm(20), nrow = 4, ncol = 5)

  nmix_family <- list(
    family = "nmix",
    linkinv = exp
  )
  # The refusal names the family the dispatch was given.
  expect_error(
    compute_family_epred(linpred, nmix_family),
    "reached the unit-free mean dispatch"
  )
  expect_error(compute_family_epred(linpred, nmix_family), "'nmix'")
})

test_that("compute_family_epred validates inputs", {
  linpred <- matrix(rnorm(20), nrow = 4, ncol = 5)

  # Invalid family (missing components)
  expect_error(
    compute_family_epred(linpred, list(family = "poisson")),
    "missing required components"
  )

  expect_error(
    compute_family_epred(linpred, list(linkinv = exp)),
    "missing required components"
  )

  # A distributional parameter that does not match the predictor
  wrong_sigma <- matrix(1, nrow = 2, ncol = 3)
  expect_error(
    compute_family_epred(linpred, brms::lognormal(),
                         family_pars = list(sigma = wrong_sigma)),
    "has the wrong shape"
  )
})

test_that("a response's mean takes its own family and parameters", {
  set.seed(123)
  linpred <- list(
    count = matrix(rnorm(20), nrow = 4, ncol = 5),
    biomass = matrix(rnorm(20, mean = 1), nrow = 4, ncol = 5)
  )
  # The family passed with the formula is neither response's.
  obj <- structure(list(
    formula = brms::bf(count ~ 1, family = poisson()) +
      brms::bf(biomass ~ 1, family = brms::lognormal()),
    family = gaussian(),
    data = data.frame(count = 1:5, biomass = 1:5)
  ), class = "mvgam")

  # The lognormal mean needs `sigma_biomass`, the name brms gives the
  # response's own scale. A plain `sigma` belongs to no response.
  testthat::local_mocked_bindings(
    extract_dpars_from_stanfit = function(stanfit, dpar_names, ndraws,
                                          nobs, draw_ids = NULL) {
      expect_identical(dpar_names, "sigma_biomass")
      list(sigma_biomass = matrix(0.5, nrow = ndraws, ncol = nobs))
    },
    get_combined_linpred = function(..., resp = NULL) {
      if (is.null(resp)) linpred else linpred[[resp]]
    }
  )

  one <- posterior_epred(obj, draw_ids = 1:4, resp = "biomass")
  expect_equal(one, exp(linpred$biomass + 0.5^2 / 2))

  both <- posterior_epred(obj, draw_ids = 1:4)
  expect_named(both, c("count", "biomass"))
  expect_equal(both$count, exp(linpred$count))
  expect_equal(both$biomass, one)
})

# ==============================================================================
# data2draws helper function tests
# ==============================================================================

test_that("data2draws gives every draw the observations' own values", {
  # A scalar fills the matrix. A vector repeats down each row.
  expect_equal(data2draws(5, dim = c(4, 3)), matrix(5, 4, 3))
  expect_equal(data2draws(c(0, 2, 100), dim = c(2, 3)),
               rbind(c(0, 2, 100), c(0, 2, 100)))
  # For categorical models with dim = c(ndraws, nobs, ncats)
  x_mat <- matrix(1:6, nrow = 2, ncol = 3)
  result <- data2draws(x_mat, dim = c(4, 2, 3))
  for (d in 1:4) {
    expect_equal(result[d, , ], x_mat)
  }
  # Neither shape recycles a partial match.
  expect_error(data2draws(c(1, 2), dim = c(4, 3)), "wrong shape")
  expect_error(data2draws(matrix(1:4, 2, 2), dim = c(4, 3, 5)),
               "wrong shape")
})

# ==============================================================================
# dim_mu helper function tests
# ==============================================================================

test_that("dim_mu extracts correct dimensions", {
  expect_equal(dim_mu(list(ndraws = 100, nobs = 50)), c(100, 50))
})

# ==============================================================================
# multiply_dpar_rate_denom helper function tests
# ==============================================================================

test_that("multiply_dpar_rate_denom returns unchanged when no rate_denom", {
  dpar <- matrix(1:12, nrow = 3, ncol = 4)
  prep <- list(data = list())
  result <- multiply_dpar_rate_denom(dpar, prep)
  expect_equal(result, dpar)
})
test_that("multiply_dpar_rate_denom applies vector rate_denom", {
  dpar <- matrix(1:6, nrow = 2, ncol = 3)
  rate_denom <- c(10, 20, 30)
  prep <- list(data = list(rate_denom = rate_denom))
  result <- multiply_dpar_rate_denom(dpar, prep)
  expect_equal(result, sweep(dpar, 2, rate_denom, `*`))
})

# ==============================================================================
# Family-specific posterior_epred_* function tests
# ==============================================================================

test_that("each mean kernel returns mu where E[Y] is mu", {
  mu <- matrix(runif(6), nrow = 2, ncol = 3)
  for (fam in c("gaussian", "student", "beta", "bernoulli", "poisson")) {
    kernel <- get(paste0("posterior_epred_", fam), envir = asNamespace("mvgam"))
    expect_equal(kernel(list(dpars = list(mu = mu), data = list())), mu,
                 label = fam)
  }
  # A count kernel scales its rate by the denominator
  prep <- list(dpars = list(mu = mu), data = list(rate_denom = c(2, 3, 4)))
  expect_equal(posterior_epred_poisson(prep), sweep(mu, 2, c(2, 3, 4), `*`))
  expect_equal(posterior_epred_negbinomial(prep),
               sweep(mu, 2, c(2, 3, 4), `*`))
})

test_that("posterior_epred_lognormal computes E[Y] = exp(mu + sigma^2/2)", {
  mu <- matrix(c(0, 1, 2, 3), nrow = 2, ncol = 2)
  sigma <- matrix(c(0.5, 0.5, 1, 1), nrow = 2, ncol = 2)
  prep <- list(dpars = list(mu = mu, sigma = sigma))
  result <- posterior_epred_lognormal(prep)
  expected <- exp(mu + sigma^2 / 2)
  expect_equal(result, expected)
})

test_that("posterior_epred_shifted_lognormal adds ndt shift", {
  mu <- matrix(c(0, 1), nrow = 1, ncol = 2)
  sigma <- matrix(c(0.5, 0.5), nrow = 1, ncol = 2)
  ndt <- matrix(c(0.1, 0.2), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, sigma = sigma, ndt = ndt))
  result <- posterior_epred_shifted_lognormal(prep)
  expected <- exp(mu + sigma^2 / 2) + ndt
  expect_equal(result, expected)
})

test_that("posterior_epred_binomial multiplies mu by trials", {
  mu <- matrix(c(0.2, 0.5, 0.8, 0.3), nrow = 2, ncol = 2)
  trials <- c(10, 20)
  prep <- list(
    dpars = list(mu = mu),
    data = list(trials = trials),
    ndraws = 2,
    nobs = 2
  )
  expect_equal(posterior_epred_binomial(prep), sweep(mu, 2, trials, `*`))
})

test_that("posterior_epred_beta_binomial multiplies mu by trials", {
  mu <- matrix(c(0.2, 0.5), nrow = 1, ncol = 2)
  trials <- c(10, 20)
  prep <- list(
    dpars = list(mu = mu),
    data = list(trials = trials),
    ndraws = 1,
    nobs = 2
  )
  result <- posterior_epred_beta_binomial(prep)
  expect_equal(result[1, 1], 0.2 * 10)
  expect_equal(result[1, 2], 0.5 * 20)
})

# ==============================================================================
# Zero-inflated family tests
# ==============================================================================

test_that("posterior_epred_zero_inflated_poisson applies zi correction", {
  mu <- matrix(c(5, 10, 15, 20), nrow = 2, ncol = 2)
  zi <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, ncol = 2)
  prep <- list(dpars = list(mu = mu, zi = zi))
  result <- posterior_epred_zero_inflated_poisson(prep)
  expected <- mu * (1 - zi)
  expect_equal(result, expected)
})

test_that("posterior_epred_zero_inflated_negbinomial applies zi correction", {
  mu <- matrix(c(5, 10), nrow = 1, ncol = 2)
  zi <- matrix(c(0.2, 0.3), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, zi = zi))
  result <- posterior_epred_zero_inflated_negbinomial(prep)
  expected <- mu * (1 - zi)
  expect_equal(result, expected)
})

test_that("posterior_epred_zero_inflated_binomial applies zi and trials", {
  mu <- matrix(c(0.5, 0.8), nrow = 1, ncol = 2)
  zi <- matrix(c(0.1, 0.2), nrow = 1, ncol = 2)
  trials <- c(10, 20)
  prep <- list(
    dpars = list(mu = mu, zi = zi),
    data = list(trials = trials),
    ndraws = 1,
    nobs = 2
  )
  result <- posterior_epred_zero_inflated_binomial(prep)
  # E[Y] = mu * trials * (1 - zi)
  expect_equal(result[1, 1], 0.5 * 10 * (1 - 0.1))
  expect_equal(result[1, 2], 0.8 * 20 * (1 - 0.2))
})

test_that("posterior_epred_zero_inflated_beta applies zi correction", {
  mu <- matrix(c(0.3, 0.6), nrow = 1, ncol = 2)
  zi <- matrix(c(0.1, 0.2), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, zi = zi))
  result <- posterior_epred_zero_inflated_beta(prep)
  expected <- mu * (1 - zi)
  expect_equal(result, expected)
})

test_that("posterior_epred_zero_one_inflated_beta computes correctly", {
  mu <- matrix(c(0.5, 0.6), nrow = 1, ncol = 2)
  zoi <- matrix(c(0.2, 0.3), nrow = 1, ncol = 2)
  coi <- matrix(c(0.4, 0.5), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, zoi = zoi, coi = coi))
  result <- posterior_epred_zero_one_inflated_beta(prep)
  # E[Y] = zoi * coi + mu * (1 - zoi)
  expected <- zoi * coi + mu * (1 - zoi)
  expect_equal(result, expected)
})

# ==============================================================================
# Hurdle family tests
# ==============================================================================

test_that("posterior_epred_hurdle_poisson computes E[Y|Y>0] * P(Y>0)", {
  mu <- matrix(c(2, 5), nrow = 1, ncol = 2)
  hu <- matrix(c(0.2, 0.3), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, hu = hu))
  result <- posterior_epred_hurdle_poisson(prep)
  # E[Y] = mu / (1 - exp(-mu)) * (1 - hu)
  expected <- mu / (1 - exp(-mu)) * (1 - hu)
  expect_equal(result, expected)
})

test_that("posterior_epred_hurdle_negbinomial uses shape parameter", {
  mu <- matrix(c(5, 10), nrow = 1, ncol = 2)
  hu <- matrix(c(0.2, 0.3), nrow = 1, ncol = 2)
  shape <- matrix(c(2, 3), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, hu = hu, shape = shape))
  result <- posterior_epred_hurdle_negbinomial(prep)
  # E[Y] = mu / (1 - (shape/(mu+shape))^shape) * (1 - hu)
  expected <- mu / (1 - (shape / (mu + shape))^shape) * (1 - hu)
  expect_equal(result, expected)
})

test_that("posterior_epred_hurdle_gamma applies hu correction", {
  mu <- matrix(c(5, 10), nrow = 1, ncol = 2)
  hu <- matrix(c(0.2, 0.3), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, hu = hu))
  result <- posterior_epred_hurdle_gamma(prep)
  expected <- mu * (1 - hu)
  expect_equal(result, expected)
})

test_that("posterior_epred_hurdle_lognormal combines lognormal E[Y] with hu", {
  mu <- matrix(c(1, 2), nrow = 1, ncol = 2)
  sigma <- matrix(c(0.5, 0.5), nrow = 1, ncol = 2)
  hu <- matrix(c(0.2, 0.3), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, sigma = sigma, hu = hu))
  result <- posterior_epred_hurdle_lognormal(prep)
  # E[Y] = exp(mu + sigma^2/2) * (1 - hu)
  expected <- exp(mu + sigma^2 / 2) * (1 - hu)
  expect_equal(result, expected)
})

# ==============================================================================
# Complex distribution mean helper tests
# ==============================================================================

test_that("mean_discrete_weibull sums the series it approximates", {
  # E[Y] = sum over k >= 1 of mu^(k^shape), truncated once the terms
  # fall below 0.001
  mu <- matrix(c(0.3, 0.5), nrow = 1, ncol = 2)
  shape <- matrix(c(1, 1.5), nrow = 1, ncol = 2)
  full <- c(sum(0.3^(1:2000)), sum(0.5^((1:2000)^1.5)))
  expect_equal(as.numeric(mean_discrete_weibull(mu, shape)), full,
               tolerance = 0.005)
})

test_that("mean_com_poisson returns mu when shape = 1 (Poisson)", {
  mu <- matrix(c(3, 5, 7), nrow = 1, ncol = 3)
  shape <- 1
  result <- mean_com_poisson(mu, shape)
  # When shape = 1, COM-Poisson reduces to Poisson with E[Y] = mu
  # Result preserves mu's dimensions so compare values ignoring attributes
  expect_equal(as.numeric(result), as.numeric(mu))
})

test_that("the large-mu COM-Poisson approximation matches the series", {
  mu <- matrix(c(10, 20), nrow = 1, ncol = 2)
  shape <- matrix(c(2, 2), nrow = 1, ncol = 2)
  expect_equal(mean_com_poisson(mu, shape),
               mean_com_poisson(mu, shape, approx = FALSE),
               tolerance = 0.01)
})

test_that("mean_com_poisson validates shape parameter", {
  mu <- matrix(c(3, 5), nrow = 1, ncol = 2)
  expect_error(
    mean_com_poisson(mu, shape = 0),
    "shape must be positive"
  )
  expect_error(
    mean_com_poisson(mu, shape = Inf),
    "shape must be finite"
  )
})

# ==============================================================================
# Additional family edge cases
# ==============================================================================

test_that("posterior_epred_gen_extreme_value uses xi parameter", {
  mu <- matrix(c(0, 1), nrow = 1, ncol = 2)
  sigma <- matrix(c(1, 1), nrow = 1, ncol = 2)
  xi <- matrix(c(0.1, 0.2), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, sigma = sigma, xi = xi))
  result <- posterior_epred_gen_extreme_value(prep)
  expected <- mu + sigma * (gamma(1 - xi) - 1) / xi
  expect_equal(result, expected)
})

test_that("posterior_epred_asym_laplace uses quantile parameter", {
  mu <- matrix(c(0, 1), nrow = 1, ncol = 2)
  sigma <- matrix(c(1, 1), nrow = 1, ncol = 2)
  quantile <- matrix(c(0.25, 0.75), nrow = 1, ncol = 2)
  prep <- list(dpars = list(mu = mu, sigma = sigma, quantile = quantile))
  result <- posterior_epred_asym_laplace(prep)
  expected <- mu + sigma * (1 - 2 * quantile) / (quantile * (1 - quantile))
  expect_equal(result, expected)
})

# ==============================================================================
# extract_dpars_from_stanfit tests
# ==============================================================================

test_that("extract_dpars_from_stanfit extracts and broadcasts scalar params", {
  # Create mock draws with scalar sigma parameter
  set.seed(123)
  draws_mat <- matrix(
    c(rnorm(10), abs(rnorm(10, mean = 1))),
    nrow = 10, ncol = 2
  )
  colnames(draws_mat) <- c("b_Intercept", "sigma")
  mock_draws <- posterior::as_draws_matrix(draws_mat)

  # Extract sigma with nobs = 5 (should broadcast to [10 x 5])
  result <- extract_dpars_from_stanfit(
    mock_draws,
    dpar_names = "sigma",
    ndraws = 10,
    nobs = 5
  )

  # The scalar repeats across the prediction rows
  expect_equal(result, list(sigma = matrix(draws_mat[, "sigma"], 10, 5)))

  # A parameter with one index repeats as a scalar does
  draws_single_idx <- matrix(abs(rnorm(10, mean = 1)), nrow = 10, ncol = 1)
  colnames(draws_single_idx) <- "zi[1]"
  mock_single <- posterior::as_draws_matrix(draws_single_idx)

  result_single <- extract_dpars_from_stanfit(
    mock_single,
    dpar_names = "zi",
    ndraws = 10,
    nobs = 4
  )

  expect_equal(result_single$zi, matrix(draws_single_idx[, 1L], 10, 4))
})

test_that("extract_dpars_from_stanfit handles indexed and missing params", {
  # Create mock draws with indexed zi parameter and scalar sigma
  set.seed(456)
  ndraws <- 8
  nobs <- 3
  zi_vals <- matrix(runif(ndraws * nobs, 0, 0.5), nrow = ndraws, ncol = nobs)

  draws_mat <- cbind(
    matrix(rnorm(ndraws), ncol = 1),
    abs(rnorm(ndraws, mean = 1)),
    zi_vals
  )
  colnames(draws_mat) <- c("b_Intercept", "sigma", "zi[1]", "zi[2]", "zi[3]")
  mock_draws <- posterior::as_draws_matrix(draws_mat)

  # Extract both existing params
  result <- extract_dpars_from_stanfit(
    mock_draws,
    dpar_names = c("sigma", "zi"),
    ndraws = ndraws,
    nobs = nobs
  )

  # The scalar sigma repeats across rows. The indexed zi keeps its
  # values.
  values <- function(m) matrix(as.numeric(m), nrow(m))
  expect_equal(lapply(result, values), list(
    sigma = matrix(draws_mat[, "sigma"], ndraws, nobs), zi = zi_vals
  ))

  # Request shape (missing) - should return NULL
  result_missing <- extract_dpars_from_stanfit(
    mock_draws,
    dpar_names = c("sigma", "shape"),
    ndraws = ndraws,
    nobs = nobs
  )
  expect_null(result_missing$shape)

  # Empty dpar_names should return empty list
  result_empty <- extract_dpars_from_stanfit(
    mock_draws,
    dpar_names = character(0),
    ndraws = ndraws,
    nobs = nobs
  )
  expect_equal(result_empty, list())
})

test_that("an indexed parameter on another axis is not read per row", {
  # Two columns against three rows: a per-factor scale, say. Taking the
  # first column put that scale on every row.
  draws <- posterior::as_draws_matrix(
    cbind(`sigma[1]` = c(0.2, 0.3), `sigma[2]` = c(0.4, 0.5))
  )
  expect_error(
    extract_dpars_from_stanfit(draws, "sigma", ndraws = 2, nobs = 3),
    "must have one column per prediction row"
  )
})

test_that("extract_dpars_from_stanfit validates inputs correctly", {
  # Create valid mock draws for testing
  set.seed(789)
  draws_mat <- matrix(abs(rnorm(20, mean = 1)), nrow = 10, ncol = 2)
  colnames(draws_mat) <- c("b_Intercept", "sigma")
  valid_draws <- posterior::as_draws_matrix(draws_mat)

  # ndraws exceeds available draws
  expect_error(
    extract_dpars_from_stanfit(valid_draws, "sigma", 100, 5),
    "more draws than the posterior has"
  )

  # draw_ids exceeds available draws
  expect_error(
    extract_dpars_from_stanfit(valid_draws, "sigma", 10, 5, draw_ids = 1:100),
    "exceed available draws"
  )

  # draw_ids with valid subset should work
  result <- extract_dpars_from_stanfit(
    valid_draws,
    dpar_names = "sigma",
    ndraws = 10,
    nobs = 3,
    draw_ids = c(1, 3, 5)
  )
  expect_equal(dim(result$sigma), c(3, 3))
})

# ==============================================================================
# Truncation helper function tests
# ==============================================================================

test_that("extract_truncation_bounds handles NULL and constant bounds", {
  # Mock mvgam object with no truncation bounds in standata
  mock_no_trunc <- structure(
    list(standata = list(N = 10)),
    class = "mvgam"
  )
  result_none <- extract_truncation_bounds(mock_no_trunc, nobs = 10)
  expect_null(result_none$lb)
  expect_null(result_none$ub)

  # Mock mvgam object with constant lb=0, ub=100 in standata
  mock_trunc <- structure(
    list(standata = list(N = 10, lb = 0, ub = 100)),
    class = "mvgam"
  )
  result_const <- extract_truncation_bounds(mock_trunc, nobs = 5)
  expect_equal(result_const$lb, rep(0, 5))
  expect_equal(result_const$ub, rep(100, 5))
})

test_that("truncation reaches sample_from_family without erroring", {
  # The tests below drive `apply_truncation()` directly. A signature
  # change between it and the family sampler once passed them while
  # every truncated model errored. This test drives the entry point.
  set.seed(3)
  ndraws <- 40L
  epred <- matrix(5, nrow = ndraws, ncol = 3L)
  out <- sample_from_family(
    family_name = "gaussian", ndraws = ndraws, epred = epred,
    sigma = matrix(3, nrow = ndraws, ncol = 3L),
    lb = 0, ub = 10, ntrys = 5
  )
  # The family samplers return a vector the callers reshape. The test
  # checks its length.
  expect_length(out, ndraws * 3L)
  expect_true(all(out >= 0 & out <= 10))

  # A count family takes the same route. Its lower bound stays
  # attainable.
  counts <- sample_from_family(
    family_name = "poisson", ndraws = ndraws,
    epred = matrix(2, nrow = ndraws, ncol = 3L),
    lb = 1, ub = Inf, ntrys = 5
  )
  expect_true(all(counts >= 1))
  expect_true(any(counts == 1))
})

test_that("a truncated draw follows the truncated distribution", {
  # An out-of-bounds draw is replaced by inverting the distribution
  # restricted to the bounds. The result follows the truncated law
  # exactly. A draw clamped onto the bound would pile mass there.
  set.seed(8)
  ndraws <- 20000L
  nobs <- 2L
  mu <- matrix(5, ndraws, nobs)
  spec <- family_dist_spec("gaussian", "identity", mu,
                           list(sigma = matrix(3, ndraws, nobs)), NULL)
  draw <- function() matrix(rnorm(ndraws * nobs, 5, 3), nrow = ndraws)
  out <- apply_truncation(
    draw(), lb = 0, ub = Inf, ntrys = 5, ndraws = ndraws,
    nobs = nobs, redraw = draw, spec = spec, discrete = FALSE
  )
  v <- as.numeric(out[, 1L])
  expect_true(all(v >= 0))
  # No draw equals the bound. Clamping would put draws there.
  expect_identical(sum(v == 0), 0L)
  probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  p0 <- stats::pnorm(0, 5, 3)
  truth <- stats::qnorm(p0 + probs * (1 - p0), 5, 3)
  expect_lt(max(abs(stats::quantile(v, probs) - truth)), 0.15)
})

test_that("a replaced draw comes from the fit's own predictive", {
  # The replacement for an out-of-bounds draw comes from the
  # observation's own predictive, with its parameters. Here the
  # predictive centres near 50 and the bound is at zero. A draw from the
  # standard member of the family would land within a few units of
  # zero.
  set.seed(11)
  ndraws <- 200
  nobs <- 3
  centre <- 50
  samples <- matrix(rnorm(ndraws * nobs, mean = centre, sd = 5),
                    nrow = ndraws, ncol = nobs)
  planted <- 1:20
  samples[planted, ] <- -3

  result <- apply_truncation(
    samples = samples,
    lb = 0,
    ub = Inf,
    ntrys = 5,
    ndraws = ndraws,
    nobs = nobs,
    redraw = function() matrix(rnorm(ndraws * nobs, mean = centre,
                                      sd = 5),
                                nrow = ndraws, ncol = nobs)
  )

  # Six predictive SDs below its centre still lie far above any draw
  # from a standard normal.
  expect_gt(min(result[planted, ]), centre - 30)

  # The draws that were already inside the bounds are untouched.
  kept <- setdiff(seq_len(ndraws), planted)
  expect_identical(result[kept, ], samples[kept, ])
})

# Tests for summarize_predictions() ------------------------------------------

test_that("summarize_predictions computes correct statistics", {
  set.seed(123)
  draws <- matrix(rnorm(100 * 5, mean = 5, sd = 2), nrow = 100)

  # Non-robust summary (mean/sd)
  result <- summarize_predictions(draws, probs = c(0.025, 0.975),
                                  robust = FALSE)

  expect_equal(colnames(result), c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  expect_equal(colnames(summarize_predictions(draws, probs = 0.5,
                                              robust = FALSE)),
               c("Estimate", "Est.Error", "Q50"))
  expect_equal(result[, "Estimate"], colMeans(draws), tolerance = 1e-10)
  expect_equal(result[, "Est.Error"], apply(draws, 2, sd), tolerance = 1e-10)
})

test_that("summarize_predictions handles robust = TRUE", {
  set.seed(456)
  draws <- matrix(rnorm(100 * 3, mean = 10, sd = 3), nrow = 100)

  result <- summarize_predictions(draws, probs = c(0.1, 0.9), robust = TRUE)

  expect_equal(colnames(result), c("Estimate", "Est.Error", "Q10", "Q90"))
  expect_equal(result[, "Estimate"], apply(draws, 2, median), tolerance = 1e-10)
  expect_equal(result[, "Est.Error"], apply(draws, 2, mad), tolerance = 1e-10)
})

# predict.mvgam type= dispatch. Each type routes to a different
# posterior_* method (or, for type = "variance", to predict_variance()).
# Tests use mocked S3 methods so the dispatcher can be exercised
# without a real Stan fit.

predict_stub_obj <- function() structure(list(), class = "mvgam")

test_that("predict.mvgam routes each type to its posterior method", {
  routes <- c(response = "posterior_predict.mvgam",
              link = "posterior_linpred.mvgam",
              expected = "posterior_epred.mvgam")
  seen <- list()
  for (type in names(routes)) {
    sentinel <- matrix(rnorm(4 * 3), 4, 3)
    mock <- list(local({
      route <- type
      out <- sentinel
      function(object, ...) {
        seen[[route]] <<- list(...)
        out
      }
    }))
    names(mock) <- routes[[type]]
    do.call(testthat::local_mocked_bindings, c(mock, .package = "mvgam"))
    out <- predict.mvgam(predict_stub_obj(), type = type, summary = FALSE)
    expect_identical(out, sentinel, label = type)
  }
  # The link route asks for the untransformed predictor
  expect_identical(seen$link$transform, FALSE)
})

test_that("predict.mvgam(type = 'terms') errors with a redirect", {
  err <- expect_error(predict.mvgam(predict_stub_obj(), type = "terms"),
                      "no single equivalent")
  expect_match(conditionMessage(err), "posterior_smooths")
})

test_that("predict.mvgam(type = 'latent_state'/'detection') errors clearly", {
  expect_error(
    predict.mvgam(predict_stub_obj(), type = "latent_state"),
    "not available for this family"
  )
  expect_error(
    predict.mvgam(predict_stub_obj(), type = "detection"),
    "not available for this family"
  )
})

# compute_family_variance: family-by-family variance formulas.

test_that("compute_family_variance gives each family's variance", {
  mu <- matrix(c(0.1, 0.2, 0.3, 0.4), 2, 2)
  d <- matrix(c(2, 4, 6, 8), 2, 2)
  fam <- function(name, link = identity) list(family = name, linkinv = link)
  cases <- list(
    gaussian = list(gaussian(), list(sigma = d), d^2),
    poisson = list(poisson(), list(), mu),
    bernoulli = list(fam("bernoulli"), list(), mu * (1 - mu)),
    negbinomial = list(fam("negbinomial"), list(shape = d), mu + mu^2 / d),
    gamma = list(fam("gamma"), list(shape = d), mu^2 / d),
    beta = list(fam("beta"), list(phi = d), mu * (1 - mu) / (1 + d)),
    student = list(fam("student"), list(sigma = d, nu = d + 3),
                   d^2 * (d + 3) / (d + 1)),
    # mu is the response-scale mean, exp(meanlog + sdlog^2 / 2)
    lognormal = list(fam("lognormal"), list(sigma = d),
                     mu^2 * (exp(d^2) - 1))
  )
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    v <- do.call(compute_family_variance,
                 c(list(mu = mu, family = cs[[1]]), cs[[2]]))
    expect_equal(v, cs[[3]], label = nm)
  }
  # A binomial mean is p * trials. Its variance is trials * p * (1 - p).
  v <- compute_family_variance(mu = matrix(c(2, 4, 6), 1, 3),
                               family = fam("binomial"), trials = rep(10, 3))
  p <- c(2, 4, 6) / 10
  expect_equal(as.numeric(v), p * (1 - p) * 10)
})

test_that("compute_family_variance: student returns Inf when nu <= 2", {
  mu <- matrix(1, 2, 2)
  sigma <- matrix(1, 2, 2)
  # Column-major fill: nu[1,1]=1.5, nu[2,1]=2.0, nu[1,2]=5.0, nu[2,2]=10.
  nu <- matrix(c(1.5, 2.0, 5.0, 10.0), 2, 2)
  v <- compute_family_variance(
    mu = mu,
    family = list(family = "student", linkinv = identity),
    sigma = sigma, nu = nu
  )
  expect_identical(is.infinite(v), nu <= 2)
})

test_that("compute_family_variance: errors for unsupported family", {
  mu <- matrix(1, 2, 2)
  expect_error(
    compute_family_variance(
      mu = mu,
      family = list(family = "tweedie", linkinv = exp)
    ),
    "not implemented"
  )
})

test_that("compute_family_variance: errors with dim mismatch on dpar", {
  mu <- matrix(1, 2, 3)
  bad_sigma <- matrix(1, 2, 2)
  expect_error(
    compute_family_variance(
      mu = mu, family = gaussian(), sigma = bad_sigma
    ),
    "has the wrong shape"
  )
})

test_that("compute_family_variance: errors when required dpar missing", {
  mu <- matrix(1, 2, 2)
  expect_error(
    compute_family_variance(
      mu = mu,
      family = list(family = "negbinomial", linkinv = exp)
    ),
    "shape"
  )
})

test_that("a denominator of zero is a legal binomial observation", {
  # A row whose response was never observed has zero trials. brms
  # accepts the padding when fitting. posterior_epred(), loo(),
  # pp_check() and plot() accept it in prediction.
  stub <- structure(
    list(
      formula = brms::bf(y | trials(trials) ~ x),
      data = data.frame(
        y = c(1L, 2L, NA_integer_), trials = c(5L, 5L, 0L),
        x = rnorm(3)
      ),
      standata = list(trials = c(5L, 5L))
    ),
    class = c("mvgam", "brmsfit")
  )
  expect_equal(
    extract_trials_for_family(stub, binomial(), newdata = NULL),
    c(5, 5, 0)
  )
  # com_binomial() reaches the same extractor.
  expect_equal(
    extract_trials_for_family(stub, com_binomial(), newdata = NULL),
    c(5, 5, 0)
  )
  # A negative denominator is still refused, matching brms.
  stub$data$trials <- c(5L, 5L, -1L)
  expect_error(
    extract_trials_for_family(stub, binomial(), newdata = NULL),
    "trials"
  )
})

# A family whose mean differs from the inverse link of its predictor
# writes that mean twice. The per-family kernel copied from brms holds
# one copy. The dispatch `compute_family_epred()` runs holds the other.
# The test drives both and requires them to agree. A family added to
# one of them and missing from the other fails it.
epred_kernel_for <- function(family_name) {
  # `epred_kernel()` resolves the same name the same way. A family
  # with no kernel gives NULL. A kernel that fails to load still
  # raises.
  nm <- paste0("posterior_epred_", family_name)
  if (!exists(nm, mode = "function", envir = asNamespace("mvgam"))) {
    return(NULL)
  }
  get(nm, mode = "function", envir = asNamespace("mvgam"))
}

test_that("compute_family_epred agrees with every family's own mean kernel", {
  dpar_value <- function(dpar) {
    switch(dpar,
      sigma = 0.7, shape = 2.5, phi = 4, nu = 6, hu = 0.3, zi = 0.25,
      zoi = 0.2, coi = 0.6, beta = 1.3, alpha = 0.5, kappa = 2,
      xi = 0.1, quantile = 0.5, ndt = 0.2, 0.5)
  }
  families <- c(
    "gaussian", "poisson", "binomial", "bernoulli", "negbinomial",
    "student", "lognormal", "beta", "beta_binomial", "exponential",
    "weibull", "geometric", "skew_normal", "exgaussian",
    "hurdle_poisson", "hurdle_negbinomial", "hurdle_gamma",
    "hurdle_lognormal", "zero_inflated_poisson",
    "zero_inflated_negbinomial", "zero_inflated_binomial",
    "zero_inflated_beta", "zero_inflated_beta_binomial",
    "zero_one_inflated_beta", "von_mises", "frechet",
    "gen_extreme_value", "asym_laplace", "shifted_lognormal"
  )
  set.seed(19)
  ndraws <- 3L
  nobs <- 4L
  trials <- rep(10, nobs)
  compared <- 0L

  for (family_name in families) {
    kernel <- epred_kernel_for(family_name)
    if (is.null(kernel)) next
    family <- brms::brmsfamily(family_name)
    eta <- matrix(stats::rnorm(ndraws * nobs, 0.3, 0.2), ndraws, nobs)
    extra <- setdiff(family$dpars, "mu")
    dpars <- c(
      list(mu = family$linkinv(eta)),
      stats::setNames(
        lapply(extra, function(d) matrix(dpar_value(d), ndraws, nobs)),
        extra
      )
    )
    from_kernel <- kernel(list(
      dpars = dpars, ndraws = ndraws, nobs = nobs,
      data = list(trials = trials)
    ))
    needed <- epred_extra_dpars(family)
    from_dispatch <- compute_family_epred(
      linpred = eta, family = family, trials = trials,
      family_pars = if (length(needed)) dpars[needed] else NULL
    )
    expect_equal(as.numeric(from_dispatch), as.numeric(from_kernel))
    compared <- compared + 1L
  }
  # Guard against the loop silently comparing nothing.
  expect_gt(compared, 25L)
})

test_that("epred_extra_dpars names what a family's mean needs beyond mu", {
  expect_equal(epred_extra_dpars(brms::hurdle_poisson()), "hu")
  expect_equal(epred_extra_dpars(brms::zero_inflated_poisson()), "zi")
  expect_equal(epred_extra_dpars(brms::lognormal()), "sigma")
  expect_setequal(epred_extra_dpars(brms::hurdle_negbinomial()),
                  c("hu", "shape"))
  expect_setequal(epred_extra_dpars(brms::zero_one_inflated_beta()),
                  c("zoi", "coi"))
  # A family whose mean is the inverse link needs nothing extra.
  expect_equal(epred_extra_dpars(poisson()), character(0))
  expect_equal(epred_extra_dpars(NULL), character(0))
})

test_that("a family's mean kernel refuses to run without its parameters", {
  mu <- matrix(2, 3L, 4L)
  expect_error(
    family_mean_from_kernel("hurdle_poisson", mu, family_pars = list()),
    "hu"
  )
  expect_error(
    family_mean_from_kernel("nosuchfamily", mu, family_pars = list()),
    "No mean kernel"
  )
})

# `sample_from_family()` and `compute_family_epred()` take the same
# quantity, the inverse link of the linear predictor. The average of the
# draws must equal the mean. A sampler that took that quantity as E[Y]
# would shift its draws and fail the test.
test_that("sampled draws average to the mean the same predictor implies", {
  dpar_value <- function(dpar) {
    switch(dpar,
      sigma = 0.4, shape = 6, phi = 8, nu = 8, hu = 0.3, zi = 0.25,
      zoi = 0.2, coi = 0.6, beta = 1.3, ndt = 0.2, 0.5)
  }
  # `hurdle_negbinomial` is left out. mvgam uses brms's sampler for it.
  # That sampler reaches a zero-truncated negative binomial by a tilt
  # exact only for the Poisson. Its draws average about five percent
  # below the analytic mean.
  families <- c(
    "poisson", "negbinomial", "binomial", "bernoulli", "beta",
    "beta_binomial", "lognormal", "hurdle_poisson",
    "hurdle_lognormal", "zero_inflated_poisson",
    "zero_inflated_negbinomial", "zero_inflated_binomial",
    "zero_inflated_beta", "zero_inflated_beta_binomial"
  )
  set.seed(2024)
  ndraws <- 10000L
  # Trial totals differ by observation. Each observation's draws must
  # match its own mean, which fails if a sampler pairs draws with
  # another observation's total.
  trials <- c(0, 3, 40)
  nobs <- length(trials)

  for (family_name in families) {
    family <- brms::brmsfamily(family_name)
    eta <- matrix(0.2, ndraws, nobs)
    mu <- family$linkinv(eta)
    extra <- setdiff(family$dpars, "mu")
    dpars <- stats::setNames(
      lapply(extra, function(d) matrix(dpar_value(d), ndraws, nobs)), extra
    )
    needed <- epred_extra_dpars(family)
    expected <- compute_family_epred(
      linpred = eta, family = family, trials = trials,
      family_pars = if (length(needed)) dpars[needed] else NULL
    )

    args <- dpars[intersect(names(dpars), names(formals(sample_from_family)))]
    drawn <- matrix(do.call(sample_from_family, c(
      list(family_name = family_name, ndraws = ndraws, epred = mu,
           trials = trials, lb = NULL, ub = NULL),
      args
    )), ndraws, nobs)
    # A zero-trial observation has one possible count.
    expect_true(all(drawn[expected == 0] == 0))
    # Monte Carlo error over 10k draws stays near one percent. The
    # defects this guards against were off by a factor.
    expect_equal(colMeans(drawn), expected[1L, ], tolerance = 0.05)
  }
})
