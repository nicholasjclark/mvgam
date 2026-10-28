# Tests for Prior Specification System
# =====================================

create_test_data <- function(n = 20, n_series = 1) {
  if (n_series == 1) {
    data.frame(
      y = rnorm(n),
      x = rnorm(n),
      x1 = rnorm(n),
      x2 = rnorm(n),
      time = 1:n,
      series = factor("A")
    )
  } else {
    data.frame(
      y = rnorm(n * n_series),
      x = rnorm(n * n_series),
      x1 = rnorm(n * n_series),
      x2 = rnorm(n * n_series),
      time = rep(1:n, n_series),
      series = factor(rep(LETTERS[1:n_series], each = n))
    )
  }
}

mock_trend_prior <- function() {
  prior_df <- data.frame(
    prior = c("", ""),
    class = c("ar1_trend", "sigma_trend"),
    coef = c("", ""),
    group = c("", ""),
    resp = c("", ""),
    dpar = c("", ""),
    nlpar = c("", ""),
    bound = c("", ""),
    source = c("default", "default"),
    stringsAsFactors = FALSE
  )
  class(prior_df) <- c("brmsprior", "data.frame")
  prior_df
}

mock_obs_prior <- function() {
  prior_df <- data.frame(
    prior = "",
    class = "Intercept",
    coef = "",
    group = "",
    resp = "",
    dpar = "",
    nlpar = "",
    bound = "",
    source = "default",
    stringsAsFactors = FALSE
  )
  class(prior_df) <- c("brmsprior", "data.frame")
  prior_df
}

# Every observation prior brms offers for `brms_formula` has a row of
# the same class, coefficient, group, response and dpar in `priors`
expect_obs_priors_match <- function(priors, brms_formula, data, family = NULL,
                                    label = "") {
  brms_priors <- if (is.null(family)) {
    brms::get_prior(brms_formula, data = data)
  } else {
    brms::get_prior(brms_formula, data = data, family = family)
  }
  obs <- priors[!grepl("_trend$", priors$class), , drop = FALSE]
  keys <- c("class", "coef", "group", "resp", "dpar")
  key_of <- function(t) {
    do.call(paste, c(as.data.frame(t)[keys], sep = "|"))
  }
  wanted <- brms_priors[nzchar(brms_priors$class), , drop = FALSE]
  expect_identical(setdiff(key_of(wanted), key_of(obs)), character(0),
                   label = paste("observation priors", label))
}

test_that("parse_trend_formula handles all formula types", {
  test_data <- create_test_data()

  # Simple formula defaults to ZMVN
  result <- mvgam:::parse_trend_formula(~ 1)
  expect_equal(result$trend_model$trend, "ZMVN")

  # With trend constructor
  result_rw <- mvgam:::parse_trend_formula(~ RW())
  expect_equal(result_rw$trend_model$trend, "RW")

  # Mixed terms
  result_mixed <- mvgam:::parse_trend_formula(~ x + AR(p = 2))
  expect_equal(result_mixed$trend_model$trend, "AR")
})

test_that("all trend types generate correct prior structures", {
  test_data <- create_test_data()

  trend_specs <- list(
    RW = list(formula = ~ RW(), expected = c("sigma_trend")),
    # A contiguous AR(p >= 2) samples partial autocorrelations and
    # derives the coefficients from them. A derived quantity takes
    # no prior, which makes the partial autocorrelation the class
    # a prior can name.
    AR = list(formula = ~ AR(p = 2),
              expected = c("ar1_pacf_trend", "ar2_pacf_trend",
                           "sigma_trend")),
    VAR = list(formula = ~ VAR(p = 1),
               expected = c("A.*_trend", "sigma_trend")),
    ZMVN = list(formula = ~ ZMVN(), expected = c("sigma_trend")),
    # No `sigma_trend`: the piecewise path is a deterministic
    # function of its changepoints, so there is no innovation.
    # A linear PW takes its level from the observation formula. The
    # `m_trend` offset belongs to logistic growth.
    PW = list(formula = ~ PW(),
              expected = c("k_trend", "delta_trend")),
    CAR = list(formula = ~ CAR(), expected = c("ar1_trend", "sigma_trend"))
  )

  for (trend_name in names(trend_specs)) {
    spec <- trend_specs[[trend_name]]
    mf <- mvgam_formula(y ~ x, trend_formula = spec$formula)
    priors <- get_prior(mf, data = test_data, family = gaussian())
    trend_classes <- priors$class[grepl("_trend$", priors$class)]
    for (expected_param in spec$expected) {
      expect_true(any(grepl(expected_param, trend_classes)),
                  label = paste(trend_name, expected_param))
    }
    # A trend without an innovation must not advertise a scale.
    trend_model <- mvgam:::parse_trend_formula(spec$formula)$trend_model
    expect_equal(
      "sigma_trend" %in% trend_classes,
      mvgam:::samples_innovation_scale(trend_model)
    )
  }

  # The trend side of a formula offers every class brms offers for the
  # same predictors, with the same bounds
  equivalence_tests <- list(
    smooth_RW = list(
      mvgam_trend = ~ s(x) + RW(),
      brms_equivalent = y ~ s(x)
    ),
    smooth_AR = list(
      mvgam_trend = ~ s(x) + AR(p = 1),
      brms_equivalent = y ~ s(x)
    ),
    random_VAR = list(
      mvgam_trend = ~ x + (1 | lv_axis()) + VAR(p = 1),
      brms_equivalent = y ~ x + (1|series)
    ),
    interaction_CAR = list(
      mvgam_trend = ~ x1 * x2 + CAR(),
      brms_equivalent = y ~ x1 * x2
    ),
    mixed_ZMVN = list(
      mvgam_trend = ~ (1 | lv_axis()) + s(x) + x1 * x2 + ZMVN(),
      brms_equivalent = y ~ (1|series) + s(x) + x1 * x2
    ),
    tensor_PW = list(
      mvgam_trend = ~ t2(x1, x2) + PW(),
      brms_equivalent = y ~ t2(x1, x2)
    )
  )

  for (test_name in names(equivalence_tests)) {
    test_spec <- equivalence_tests[[test_name]]
    mf_complex_trend <- mvgam_formula(
      y ~ 1, trend_formula = test_spec$mvgam_trend
    )
    mvgam_priors <- get_prior(
      mf_complex_trend, data = test_data, family = gaussian()
    )

    brms_priors <- brms::get_prior(
      test_spec$brms_equivalent, data = test_data, family = gaussian()
    )
    # The table names the trend's axis 'lv_axis()', which brms labels
    # with the series column.
    mvgam_trend_priors <- mvgam:::translate_lv_axis_labels(
      mvgam_priors[grepl("_trend$", mvgam_priors$class), , drop = FALSE],
      "series", to = "brms"
    )

    # Trend formulas carry no intercept. brms's `sigma` maps onto the
    # innovation scale, which a trend without innovations lacks.
    trend_model <-
      mvgam:::parse_trend_formula(test_spec$mvgam_trend)$trend_model
    unmapped <- c("", "Intercept")
    if (!mvgam:::samples_innovation_scale(trend_model)) {
      unmapped <- c(unmapped, "sigma")
    }

    for (i in which(!brms_priors$class %in% unmapped)) {
      brms_row <- brms_priors[i, ]
      trend_class <- paste0(brms_row$class, "_trend")
      match_row <- mvgam_trend_priors[
        mvgam_trend_priors$class == trend_class &
          mvgam_trend_priors$coef == brms_row$coef &
          mvgam_trend_priors$group == brms_row$group, , drop = FALSE]
      label <- paste(trend_class, brms_row$coef, "in", test_name)
      expect_equal(nrow(match_row), 1L, label = label)
      expect_equal(match_row$lb[1], brms_row$lb, label = label)
      expect_equal(match_row$ub[1], brms_row$ub, label = label)
    }
  }

  # A trend formula dropping its intercept offers no Intercept_trend
  no_intercept_specs <- list(
    RW_no_int = list(formula = ~ -1 + RW(), expected = c("sigma_trend")),
    AR_no_int = list(
      formula = ~ x - 1 + AR(p = 1),
      expected = c("b_trend", "ar1_trend", "sigma_trend")
    ),
    # `~ -1` defaults to ZMVN
    ZMVN_simple = list(formula = ~ -1, expected = c("sigma_trend"))
  )

  for (test_name in names(no_intercept_specs)) {
    spec <- no_intercept_specs[[test_name]]
    mf_no_int <- mvgam_formula(y ~ x, trend_formula = spec$formula)
    priors_no_int <- get_prior(mf_no_int, data = test_data,
                               family = gaussian())
    trend_classes <- priors_no_int$class
    expect_false("Intercept_trend" %in% trend_classes)
    for (expected_param in spec$expected) {
      expect_true(any(grepl(expected_param, trend_classes)))
    }
  }
})

test_that("multivariate models handle priors correctly", {
  test_data <- create_test_data(n = 20, n_series = 2)

  # Basic multivariate
  mf_multi <- mvgam_formula(y ~ x, trend_formula = ~ AR(p = 1))
  priors_multi <- get_prior(mf_multi, data = test_data, family = gaussian())

  expect_true("ar1_trend" %in% priors_multi$class)

  # Factor model (n_lv < n_series) - requires multivariate data
  # Ensure n_series > n_lv
  test_data_mv <- create_test_data(n = 20, n_series = 3)
  mf_factor <- mvgam_formula(
    y ~ x,
    trend_formula = ~ AR(p = 1, cor = TRUE, n_lv = 2)  # n_lv < n_series
  )
  priors_factor <- get_prior(
    mf_factor, data = test_data_mv, family = gaussian()
  )

  # A factor model prices its loadings, so `Z` earns a prior row of its
  # own alongside the suffixed trend parameters.
  expect_true("Z" %in% priors_factor$class)

  # Edge case: Factor models with complex predictors
  edge_cases <- list(
    factor_smooth = list(
      formula = y ~ s(x),
      trend = ~ AR(p = 1, cor = TRUE, n_lv = 1)
    ),
    factor_random = list(
      formula = y ~ x + (1|series),
      trend = ~ ZMVN(n_lv = 1)
    ),
    multivar_correlation = list(
      formula = y ~ x,
      trend = ~ VAR(p = 1)
    )
  )

  for (case_name in names(edge_cases)) {
    case_spec <- edge_cases[[case_name]]

    mf_edge <- mvgam_formula(case_spec$formula, trend_formula = case_spec$trend)
    expect_obs_priors_match(
      get_prior(mf_edge, data = test_data, family = gaussian()),
      case_spec$formula, test_data, gaussian(), case_name
    )
  }
})

test_that("mvgam_formula constructor works correctly", {
  # Basic construction
  mf1 <- mvgam_formula(y ~ x)
  expect_s3_class(mf1, "mvgam_formula")
  expect_equal(mf1$formula, y ~ x)
  expect_null(mf1$trend_formula)

  # With trend
  mf2 <- mvgam_formula(y ~ x, trend_formula = ~ AR())
  expect_equal(mf2$trend_formula, ~ AR())

})

# get_prior.mvgam_formula tests
# ------------------------------
test_that("get_prior.mvgam_formula works with all formula types", {
  test_data <- create_test_data()
  # Without a trend there are no trend classes
  priors_obs <- get_prior(mvgam_formula(y ~ x), data = test_data,
                          family = gaussian())
  expect_false(any(grepl("_trend$", priors_obs$class)))

  # A trend formula with a predictor and no intercept
  priors_no_int_pred <- get_prior(
    mvgam_formula(y ~ 1, trend_formula = ~ x - 1), data = test_data,
    family = gaussian()
  )
  expect_true("b_trend" %in% priors_no_int_pred$class)
  expect_false("Intercept_trend" %in% priors_no_int_pred$class)
})

test_that("combine_obs_trend_priors works correctly", {
  obs_prior <- mock_obs_prior()
  trend_prior <- mock_trend_prior()

  # Basic combination
  combined <- mvgam:::combine_obs_trend_priors(obs_prior, trend_prior)
  expect_s3_class(combined, "brmsprior")
  expect_equal(nrow(combined), 3)

  # Check parameter distribution (should have obs + trend parameters)
  obs_count <- sum(!grepl("_trend$", combined$class))
  trend_count <- sum(grepl("_trend$", combined$class))
  expect_equal(obs_count, 1)   # 1 observation parameter
  expect_equal(trend_count, 2) # 2 trend parameters
})

test_that("validate_trend_formula rejects invalid terms", {
  # Valid formulas pass
  expect_no_error(mvgam:::validate_trend_formula(~ AR()))
  expect_no_error(mvgam:::validate_trend_formula(~ x + AR()))
  expect_no_error(mvgam:::validate_trend_formula(~ s(x) + RW()))

  # Invalid addition terms rejected
  for (term in c("weights(w)", "trials(n)", "cens(c)")) {
    expect_error(
      validate_trend_formula(stats::as.formula(paste("~", term, "+ AR()"))),
      paste0("Found: '", term, "'"), fixed = TRUE
    )
  }
  # brms imputes a `mi()` predictor as a response, which a trend
  # formula cannot hold
  expect_error(mvgam:::validate_trend_formula(~ mi(x) + AR()),
               "'mi\\(\\)' is not supported")
  # Every response shares one trend. The refusal names one constructor.
  expect_error(
    mvgam:::validate_trend_formula(~ AR() + RW()),
    "Keep one constructor"
  )
})

test_that("a trend_formula routes to the mvgam methods", {
  test_data <- create_test_data()
  tf <- ~ AR(p = 1)

  # The bare spelling gives what the wrapped one gives
  bare <- get_prior(y ~ x, trend_formula = tf, data = test_data)
  wrapped <- get_prior(
    mvgam_formula(y ~ x, trend_formula = tf), data = test_data
  )
  expect_equal(as.data.frame(bare), as.data.frame(wrapped))
  expect_true(any(grepl("_trend$", bare$class)))

  # Stan code and Stan data carry the trend as well
  expect_identical(
    stancode(y ~ x, trend_formula = tf, data = test_data),
    stancode(mvgam_formula(y ~ x, trend_formula = tf), data = test_data)
  )
  expect_identical(
    standata(y ~ x, trend_formula = tf, data = test_data),
    standata(mvgam_formula(y ~ x, trend_formula = tf), data = test_data)
  )
  # A family mvgam builds takes the route without a trend. brms alone
  # would write a program missing its density and priors.
  count_data <- transform(test_data, y = rpois(nrow(test_data), 3))
  expect_identical(
    stancode(y ~ x, data = count_data, family = tweedie()),
    stancode(mvgam_formula(y ~ x), data = count_data, family = tweedie())
  )
  expect_identical(
    default_prior(y ~ x, data = count_data, family = beta_nb()),
    get_prior(mvgam_formula(y ~ x), data = count_data, family = beta_nb())
  )

  # A brmsformula takes the same route
  bf_bare <- get_prior(brms::bf(y ~ x), trend_formula = tf,
                       data = test_data)
  expect_true(any(grepl("_trend$", bf_bare$class)))

  # A call supplying no trend_formula still matches brms exactly
  expect_equal(
    as.data.frame(get_prior(y ~ x, data = test_data)),
    as.data.frame(brms::get_prior(y ~ x, data = test_data))
  )
  expect_equal(
    as.character(stancode(y ~ x, data = test_data)),
    as.character(brms::make_stancode(y ~ x, data = test_data))
  )
})

test_that("distributional models work correctly with trends", {
  test_data <- create_test_data(n = 30)
  bf_sigma <- brms::bf(y ~ x, sigma ~ x1, family = gaussian())
  priors_distrib <- get_prior(
    mvgam_formula(bf_sigma, trend_formula = ~ AR(p = 1)), data = test_data
  )
  expect_true("ar1_trend" %in% priors_distrib$class)
  expect_obs_priors_match(priors_distrib, bf_sigma, test_data,
                          label = "sigma ~ x1")

  # One Intercept row each for mu, sigma and nu
  bf_multi_param <- brms::bf(y ~ x, sigma ~ x1, nu ~ x2, family = student())
  priors_multi <- get_prior(
    mvgam_formula(bf_multi_param, trend_formula = ~ RW()), data = test_data
  )
  for (par in c("", "sigma", "nu")) {
    expect_true(any(grepl(
      "Intercept", priors_multi$class[priors_multi$dpar == par]
    )))
  }
})

test_that("multivariate responses with mvbind work correctly", {
  mv_data <- data.frame(
    count = rpois(40, 5),
    biomass = rnorm(40, 10, 2),
    x = rnorm(40),
    time = rep(1:20, 2),
    series = factor(rep(c("A", "B"), each = 20))
  )
  mv_formula <- brms::bf(count ~ x, family = poisson()) +
                brms::bf(biomass ~ x, family = gaussian())
  priors_mvbind <- get_prior(
    mvgam_formula(mv_formula, trend_formula = ~ VAR(p = 1)), data = mv_data
  )
  expect_true(any(grepl("^A.*_trend$", priors_mvbind$class)))
  expect_obs_priors_match(priors_mvbind, mv_formula, mv_data,
                          label = "mvbind")
})

test_that("non-Gaussian families work with trends", {
  test_data <- create_test_data()
  test_data$count <- rpois(nrow(test_data), 5)
  test_data$binary <- rbinom(nrow(test_data), 1, 0.3)
  family_tests <- list(
    poisson = list(formula = count ~ x, family = poisson()),
    binomial = list(formula = binary ~ x, family = binomial()),
    gamma = list(formula = y ~ x, family = Gamma(link = "log")),
    exponential = list(formula = y ~ x, family = exponential()),
    beta = list(formula = I((y + 10) / 20) ~ x, family = Beta())
  )
  for (family_name in names(family_tests)) {
    family_spec <- family_tests[[family_name]]
    priors_family <- get_prior(
      mvgam_formula(family_spec$formula, trend_formula = ~ AR(p = 1)),
      data = test_data, family = family_spec$family
    )
    expect_true("ar1_trend" %in% priors_family$class, label = family_name)
    expect_obs_priors_match(priors_family, family_spec$formula, test_data,
                            family_spec$family, family_name)
  }
})

test_that("a family embedded in bf() sets the observation family", {
  test_data <- create_test_data(n = 25)
  test_data$count <- rpois(nrow(test_data), 3)
  # An embedded zero-inflated family has to price its `zi` parameter,
  # which is the half a plain family lookup would miss.
  bf_zi <- brms::bf(count ~ x, zi ~ x1, family = zero_inflated_poisson())
  priors <- get_prior(
    mvgam_formula(bf_zi, trend_formula = ~ AR(p = 1)),
    data = test_data
  )
  expect_true(any(priors$dpar == "zi"))
  expect_true("ar1_trend" %in% priors$class)
})

# get_prior.mvgam: the table a fit was built with, the same one
# prior_summary() reports. Uses a stub mvgam object to avoid fitting
# Stan in CI (per the package's no-fits-in-testthat rule).

test_that("get_prior on a fit is the table prior_summary reports", {
  # `get_prior()` once rebuilt the table from the formula, inside a
  # caught error that fell back to the stored table. The two accessors
  # could then describe different models.
  stored <- c(
    brms::prior(normal(0, 1), class = b),
    brms::prior(exponential(2), class = sigma_trend)
  )
  stub <- structure(
    list(formula = y ~ x, trend_call = ~ AR(p = 1),
         data = create_test_data(), family = gaussian(), prior = stored),
    class = c("mvgam", "brmsfit")
  )
  expect_identical(get_prior(stub), prior_summary(stub))
  expect_identical(default_prior(stub), prior_summary(stub))
  expect_error(get_prior(stub, zzz_unknown = 1), "zzz_unknown")
})

# ---- mvgam's injected defaults must not block user priors -----------

prior_test_data <- function() {
  set.seed(1)
  d <- expand.grid(time = 1:60, series = factor(paste0("s", 1:3)))
  d$y <- rpois(nrow(d), 5)
  d
}

grouped_prior_test_data <- function() {
  set.seed(1)
  d <- expand.grid(time = 1:60, series = factor(paste0("s", 1:4)))
  d$grp <- factor(rep(c("a", "a", "b", "b"), each = 60))
  d$y <- rpois(nrow(d), 5)
  d
}

prior_code <- function(mf, fam, pr = NULL, data = prior_test_data()) {
  paste(unlist(stancode(mf, data = data, family = fam, prior = pr,
                        backend = "cmdstanr",
                        validate = FALSE)), collapse = "\n")
}

test_that("merge_default_priors() drops defaults the user has claimed", {
  # Concatenating leaves two rows for the same class. brms refuses the
  # whole set with "Duplicated prior specifications are not
  # allowed". A user overriding a default got that error back.
  defaults <- c(brms::prior("gamma(2, 0.5)", class = "shape"),
                brms::prior("gamma(2, 0.25)", class = "mtail"))
  user <- brms::prior("gamma(3, 1)", class = "shape")

  merged <- mvgam:::merge_default_priors(defaults, user)
  expect_equal(sum(merged$class == "shape"), 1L)
  expect_true("gamma(3, 1)" %in% merged$prior)
  expect_false("gamma(2, 0.5)" %in% merged$prior)
  # the default the user did not name survives
  expect_true("gamma(2, 0.25)" %in% merged$prior)
})

test_that("merge_default_priors() passes both sides through untouched", {
  defaults <- brms::prior("gamma(2, 0.5)", class = "shape")
  expect_equal(
    nrow(mvgam:::merge_default_priors(defaults, NULL)), 1L
  )
  user <- brms::prior("normal(0, 1)", class = "b")
  merged <- mvgam:::merge_default_priors(defaults, user)
  expect_identical(merged$class, c("shape", "b"))
})

test_that("each response gets the defaults of its own family", {
  # The defaults were read off the family given beside the formula,
  # so a `beta_nb()` or `com_binomial()` response named inside a
  # multivariate formula sampled under brms's fallbacks. Each default
  # now names the response whose family carries it, and a user prior
  # on one response replaces that response's default alone.
  f <- bf(y ~ 1, family = beta_nb()) +
    bf(k | trials(n) ~ 1, family = com_binomial()) +
    bf(z ~ 1, family = poisson())
  defaults <- mvgam:::response_default_priors(f, gaussian())
  expect_identical(
    paste(defaults$class, defaults$resp),
    c("shape y", "mtail y", "nu k")
  )
  user <- brms::prior("normal(0, 2)", class = "nu", resp = "k")
  merged <- mvgam:::merge_default_priors(defaults, user)
  expect_identical(merged$prior[merged$class == "nu"], "normal(0, 2)")
  expect_identical(sum(merged$class == "shape"), 1L)
})

test_that(
  "get_prior() reports a family written inside bf() as the fit uses it", {
  # A family inside `bf()` was left to brms, which reported its own
  # fallbacks while the fit sampled under mvgam's defaults, and on a
  # multivariate formula every response naming no family was reported
  # as gaussian whatever `family` said.
  d <- prior_test_data()
  d$pos <- rgamma(nrow(d), 2, 1)
  embedded <- get_prior(mvgam_formula(bf(y ~ 1, family = beta_nb())),
                        data = d)
  beside <- get_prior(mvgam_formula(y ~ 1), data = d, family = beta_nb())
  expect_identical(embedded$prior[embedded$class == "shape"],
                   beside$prior[beside$class == "shape"])
  expect_identical(embedded$prior[embedded$class == "shape"],
                   "gamma(2, 0.5)")
  expect_true("mtail" %in% beside$class)
  mv <- get_prior(
    mvgam_formula(bf(y ~ 1, family = beta_nb()) + bf(pos ~ 1) +
                    set_rescor(FALSE)),
    data = d, family = Gamma(link = "log")
  )
  shape <- mv[mv$class == "shape", ]
  expect_identical(shape$prior[shape$resp == "y"], "gamma(2, 0.5)")
  # `pos` names no family and takes the gamma given beside the
  # formula, whose shape brms defaults.
  expect_identical(shape$prior[shape$resp == "pos"], "gamma(0.01, 0.01)")
  expect_false(any(mv$class == "sigma"))
})

test_that("beta_nb() default priors can be overridden", {
  mf <- mvgam_formula(y ~ 1)
  sc_default <- prior_code(mf, beta_nb())
  expect_true(grepl("gamma_lpdf(shape | 2, 0.5)", sc_default, fixed = TRUE))
  expect_true(grepl("gamma_lpdf(mtail | 2, 0.25)", sc_default, fixed = TRUE))

  sc_shape <- prior_code(mf, beta_nb(),
                         brms::prior("gamma(3, 1)", class = "shape"))
  expect_true(grepl("gamma_lpdf(shape | 3, 1)", sc_shape, fixed = TRUE))
  expect_false(grepl("gamma_lpdf(shape | 2, 0.5)", sc_shape, fixed = TRUE))
  # the parameter the user left alone keeps its default
  expect_true(grepl("gamma_lpdf(mtail | 2, 0.25)", sc_shape, fixed = TRUE))

  sc_both <- prior_code(mf, beta_nb(),
                        c(brms::prior("gamma(3, 1)", class = "shape"),
                          brms::prior("gamma(5, 2)", class = "mtail")))
  expect_true(grepl("gamma_lpdf(shape | 3, 1)", sc_both, fixed = TRUE))
  expect_true(grepl("gamma_lpdf(mtail | 5, 2)", sc_both, fixed = TRUE))
})

test_that("com_binomial() default prior can be overridden", {
  set.seed(4)
  dat <- data.frame(
    y = rbinom(40, 10, 0.5), trials = 10L, time = 1:40,
    series = factor(rep("series1", 40))
  )
  sc <- prior_code(mvgam_formula(y | trials(trials) ~ 1), com_binomial(),
                   brms::prior("normal(2, 1)", class = "nu"), data = dat)
  expect_true(grepl("normal_lpdf(nu | 2, 1)", sc, fixed = TRUE))
  expect_false(grepl("normal_lpdf(nu | 1, 1)", sc, fixed = TRUE))

  # get_prior() must advertise the prior the model samples under.
  # brms's own fallback for `nu` is the Student-t degrees of
  # freedom `gamma(2, 0.1)`, which has positive support only and
  # would misreport a parameter declared with `lb = -5`.
  tab <- get_prior(mvgam_formula(y | trials(trials) ~ 1), data = dat,
                   family = com_binomial())
  expect_identical(tab$prior[tab$class == "nu"], "normal(1, 1)")
})

test_that("nu_trend default prior can be overridden", {
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, df = NA))
  sc_default <- prior_code(mf, poisson())
  expect_true(grepl(stan_prior_line("nu_trend", "gamma(4, 0.3)"),
                      sc_default, fixed = TRUE))

  sc_user <- prior_code(mf, poisson(),
                        brms::prior("gamma(2, 0.1)", class = "nu_trend"))
  expect_true(grepl(stan_prior_line("nu_trend", "gamma(2, 0.1)"),
                      sc_user, fixed = TRUE))
  expect_false(grepl("gamma(4, 0.3)", sc_user, fixed = TRUE))
})

test_that("nu_trend is discoverable through get_prior()", {
  # Being able to override a prior is not much use if the class name
  # only appears in a help page. It must be listed alongside
  # sigma_trend and the autoregressive coefficients.
  d <- prior_test_data()
  p_est <- get_prior(
    mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, df = NA)),
    data = d, family = poisson()
  )
  expect_true("nu_trend" %in% p_est$class)
  row <- p_est[p_est$class == "nu_trend", ]
  expect_identical(row$prior, "gamma(4, 0.3)")
  # the bound is required, not conventional: below 2 the innovations
  # have no finite variance
  expect_identical(row$lb, "2")
  for (tf in list(~ RW(df = NA), ~ ZMVN(df = NA))) {
    p <- get_prior(mvgam_formula(y ~ 1, trend_formula = tf),
                   data = d, family = poisson())
    expect_true("nu_trend" %in% p$class)
  }
})

test_that("nu_trend is absent unless the degrees of freedom are estimated", {
  d <- prior_test_data()
  p_gauss <- get_prior(
    mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1)),
    data = d, family = poisson()
  )
  expect_false("nu_trend" %in% p_gauss$class)

  # a fixed df has no parameter to place a prior on either
  p_fixed <- get_prior(
    mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, df = 7)),
    data = d, family = poisson()
  )
  expect_false("nu_trend" %in% p_fixed$class)
})

# ---- what is reported must be what is sampled -----------------------
#
# A default written in more than one place drifts. `sigma_trend` was
# sampled under `exponential(2)` while the table reported brms's
# `student_t(3, 0, 2.5)`, and because `update()` passes the stored
# table back through `mvgam()`, every update re-specified the model.
# `ar1_trend` was sampled under a prior the table did not mention at
# all, and `L_Omega_trend` was emitted as a literal, so a user prior on
# it was discarded without a word.

test_that("every shared default names a distribution", {
  # `LV` sat here with a default for a parameter the generator never
  # emits, reachable only from a function with no caller.
  for (par in names(mvgam:::common_trend_priors)) {
    expect_true(nzchar(mvgam:::get_default_trend_parameter_prior(par)$prior))
  }
})

# The trend a model is written with decides which parameters it
# samples, so a prior that goes missing goes missing for one trend
# type alone. Checking a single AR fit is what let `delta_trend` and
# the four VARMA hyperpriors sit blank in the reported table while the
# Stan sampled them under real priors. The parsing is the package's
# own `mvgam_stancode_prior_rows()`, the same reader the fitted object
# uses.
trend_formulas_by_type <- list(
  AR = list(obs = y ~ 1, trend = ~ AR(p = 1, cor = TRUE)),
  RW = list(obs = y ~ 1, trend = ~ RW()),
  VAR = list(obs = y ~ 1, trend = ~ VAR(p = 1, cor = TRUE)),
  VARMA = list(obs = y ~ 1, trend = ~ VAR(p = 1, ma = TRUE, cor = TRUE)),
  ZMVN = list(obs = y ~ 1, trend = ~ ZMVN()),
  CAR = list(obs = y ~ 1, trend = ~ CAR()),
  # PW's own offset competes with an observation intercept, which
  # warns; the no-intercept form is the identified one.
  PW = list(obs = y ~ -1, trend = ~ PW()),
  factor = list(obs = y ~ 1, trend = ~ AR(p = 1, n_lv = 2)),
  hierarchical = list(obs = y ~ 1,
                      trend = ~ AR(p = 1, gr = grp, cor = TRUE))
)

# A sampled prior missing from the reported table, as "type class"
unreported_trend_priors <- function(types) {
  missing <- character(0)
  for (type in types) {
    spec <- trend_formulas_by_type[[type]]
    dat <- if (type == "hierarchical") {
      grouped_prior_test_data()
    } else {
      prior_test_data()
    }
    mf <- mvgam_formula(spec$obs, trend_formula = spec$trend)
    tab <- as.data.frame(get_prior(mf, data = dat, family = poisson()))
    sampled <- mvgam:::mvgam_stancode_prior_rows(
      prior_code(mf, poisson(), data = dat)
    )
    expect_gt(length(sampled), 0L)
    for (row in sampled) {
      if (!row$prior %in% tab$prior[tab$class == row$class]) {
        missing <- c(missing, paste(type, row$class))
      }
    }
  }
  missing
}

test_that("the trend priors reported are the ones sampled", {
  # VARMA and PW lost priors from the table before. The factor and
  # hierarchical shapes add loadings and group-correlation priors.
  expect_identical(
    unreported_trend_priors(c("VARMA", "PW", "factor", "hierarchical")),
    character(0)
  )
})

test_that("every other trend reports the priors it samples", {
  skip_on_cran()
  expect_identical(
    unreported_trend_priors(c("AR", "RW", "VAR", "ZMVN", "CAR")),
    character(0)
  )
})

test_that("trend scale priors take the response's spread, as brms does", {
  dat <- prior_test_data()
  sigma_prior <- function(d, fam) {
    tab <- as.data.frame(get_prior(mvgam_formula(y ~ 1, ~ RW()), data = d,
                                   family = fam))
    tab$prior[tab$class == "sigma_trend"]
  }
  # A Gaussian response in its own units
  dat$y <- 100 + 40 * sin(seq_len(nrow(dat)))
  s <- round(stats::mad(dat$y), 1) / 2.5
  expect_identical(sigma_prior(dat, gaussian()),
                   paste0("exponential(", signif(2 / s, 4), ")"))
  # Counts on the log link spread by less than one
  dat$y <- rpois(nrow(dat), 5)
  expect_identical(sigma_prior(dat, poisson()), "exponential(2)")

  # A missing response is dropped. As in brms, a log link takes 0.1
  # for a zero and spread 0 from a negative value. A simplex share and
  # a binary detection also take 0: the link scale does not measure
  # them.
  expect_identical(link_scale_spread(c(0, 1, 4, NA), poisson()),
                   round(stats::mad(log(c(0.1, 1, 4))), 1))
  expect_identical(link_scale_spread(c(-1, 2, 3), Gamma(link = "log")), 0)
  expect_identical(link_scale_spread(c(0.2, 0.5, 0.3), diri()), 0)
  expect_identical(
    link_scale_spread(c(0, 1, 1, 0), nmix(type = "royle_nichols")), 0
  )
})

test_that("LLT and PW growth priors scale with the number of time points", {
  # 60 time points. A factor model measures the slope in units of its
  # level's spread, sqrt(60).
  dat <- prior_test_data()
  resolved <- function(trend) {
    tab <- as.data.frame(get_prior(mvgam_formula(y ~ 1, trend), data = dat,
                                   family = poisson()))
    stats::setNames(tab$prior, tab$class)
  }
  # The shocks at times 2 to 59 reach time 60 with weights 58 to 1,
  # and the initial slope with weight 59
  plain <- resolved(~ LLT())
  expect_identical(plain[["sigma_slope_trend"]],
                   paste0("student_t(3, 0, ",
                          signif(1.3 / sqrt(sum((1:58)^2)), 4), ")"))
  expect_identical(plain[["init_slope_trend"]],
                   paste0("normal(0, ", signif(2 / 59, 4), ")"))
  # A damped slope reaches with geometric weights at phi = 0.913
  factor <- resolved(~ LLT(n_lv = 2, damped = TRUE))
  reach <- function(m) (1 - 0.913^m) / (1 - 0.913)
  expect_identical(factor[["sigma_slope_trend"]],
                   paste0("student_t(3, 0, ",
                          signif(1.3 * sqrt(60 / sum(reach(1:58)^2)), 4), ")"))
  expect_identical(factor[["init_slope_trend"]],
                   paste0("normal(0, ", signif(2 * sqrt(60) / reach(59), 4),
                          ")"))
  expect_identical(factor[["ar1_slope_trend"]], "beta(18, 2)")

  # Two harmonics drift over 59 steps. Their starting pairs give the
  # pattern a root mean square of one unit.
  seasonal <- resolved(~ LLT(season = 12, harmonics = 2))
  expect_identical(seasonal[["sigma_season_trend"]],
                   paste0("student_t(3, 0, ", signif(1.3 / sqrt(2 * 59), 4),
                          ")"))
  expect_identical(seasonal[["init_season_trend"]],
                   paste0("normal(0, ", signif(1 / sqrt(2), 4), ")"))

  # PW's base rate takes the LLT initial slope's scale, and its
  # changepoint scale is the user's own, per step
  pw <- resolved(~ PW(changepoint_scale = 0.4))
  expect_identical(pw[["k_trend"]], plain[["init_slope_trend"]])
  expect_identical(pw[["delta_trend"]],
                   paste0("double_exponential(0, ", signif(0.4 / 60, 4), ")"))
})

test_that("a user prior reaches the parameters that reported nothing", {
  # Each of these was resolved from a literal at the emission site, so
  # the user's own value never entered the Stan.
  mf <- mvgam_formula(y ~ 1,
                      trend_formula = ~ VAR(p = 1, ma = TRUE, cor = TRUE))
  user <- brms::prior_string("normal(0, 3)", class = "Amu_trend") +
    brms::prior_string("gamma(5, 2)", class = "Domega_trend")
  code <- prior_code(mf, poisson(), pr = user)
  expect_true(grepl(stan_prior_line("Amu_trend[k]", "normal(0, 3)"),
                      code, fixed = TRUE))
  expect_true(grepl(stan_prior_line("Domega_trend[k]", "gamma(5, 2)"),
                      code, fixed = TRUE))

  pw <- mvgam_formula(y ~ -1, trend_formula = ~ PW())
  pw_user <- brms::prior_string("double_exponential(0, 1)",
                                class = "delta_trend")
  expect_true(grepl(
    stan_prior_line("to_vector(delta_trend)", "double_exponential(0, 1)"),
    prior_code(pw, poisson(), pr = pw_user), fixed = TRUE
  ))
})

test_that("a user prior on the trend correlation reaches the model", {
  # Emitted as a literal, this one ignored the user entirely.
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, cor = TRUE))
  user <- brms::prior_string("lkj_corr_cholesky(9)", class = "L_Omega_trend")
  expect_true(grepl(stan_prior_line("L_Omega_trend",
                                      "lkj_corr_cholesky(9)"),
                     prior_code(mf, poisson(), pr = user), fixed = TRUE))
})

test_that("brms's trend-side sigma is not filed under sigma_trend", {
  # The trend submodel goes to brms as a gaussian, so brms hands back
  # a `sigma` row carrying its own student_t(3, 0, 2.5). That residual
  # scale is bookkeeping; the trend's process noise is `sigma_trend`,
  # which the Stan generator samples under `common_trend_priors` and
  # which reaches the stored table from the compiled code. Suffixing
  # brms's row filed its value under mvgam's name, so a fitted model
  # reported a prior it never sampled under and `update()` fed that
  # back, re-specifying the model.
  trend_priors <- brms::prior_string("student_t(3, 0, 2.5)",
                                      class = "sigma")
  out <- add_trend_suffix_to_priors(trend_priors)
  expect_equal(nrow(out), 0L)

  # `suffix_trend_prior_classes()` is the one implementation of the
  # rule; both callers reach the same answer.
  expect_equal(nrow(suffix_trend_prior_classes(trend_priors)), 0L)
})

test_that("suffix_trend_prior_classes carries both guards", {
  # The rule was written twice and the copies drifted: one turned an
  # empty class into "_trend", the other suffixed an already-suffixed
  # class twice.
  empty_class <- brms::prior_string("normal(0, 1)", class = "b")
  empty_class$class <- ""
  expect_equal(suffix_trend_prior_classes(empty_class)$class, "")

  already <- brms::prior_string("exponential(2)", class = "sigma_trend")
  expect_equal(suffix_trend_prior_classes(already)$class, "sigma_trend")
})

test_that("the trend suffix skips classes mvgam already names", {
  # `Z` is the Stan parameter's own name, so `Z_trend` names nothing.
  cls <- c("sigma", "ar1_trend", "Z", "Psi", "theta_dist_phy", "")
  expect_equal(
    apply_trend_class_suffix(cls),
    c("sigma_trend", "ar1_trend", "Z", "Psi", "theta_dist_phy", "")
  )
})

test_that("a reported innovation scale is one the model samples", {
  # Two things remove `sigma_trend`. The table reports each.
  # `PW()` never had one: its path is a function of the
  # changepoints. Sampled loadings absorb the factors' scale, which
  # fixes it at 1. Multiplicative gamma process shrinkage scales the
  # sampled loadings. A loading fixed by `trend_map` pins that
  # scale and the model samples it again. Offering the row where the
  # model fixes the scale invites a prior the fit cannot take.
  dat <- data.frame(
    y = rpois(40, 5), time = rep(1:20, 2),
    series = factor(rep(c("a", "b"), each = 20))
  )
  cases <- list(
    list(tf = ~ PW(), args = list(), sampled = FALSE),
    list(tf = ~ RW(), args = list(), sampled = TRUE),
    list(tf = ~ AR(p = 1), args = list(), sampled = TRUE),
    list(tf = ~ ZMVN(n_lv = 2), args = list(), sampled = FALSE),
    list(tf = ~ ZMVN(n_lv = 1, trend_map = matrix(c(1, NA), 2, 1)),
         args = list(), sampled = TRUE),
    list(tf = ~ ZMVN(n_lv = 2),
         args = list(loadings_prior = list(column_shrinkage = "mgp")),
         sampled = FALSE)
  )
  for (case in cases) {
    mf <- mvgam_formula(y ~ -1, case$tf)
    common <- list(mf, data = dat, family = poisson())
    tab <- do.call(get_prior, c(common, case$args))
    sc <- do.call(stancode, c(common, case$args))
    expect_equal("sigma_trend" %in% tab$class, case$sampled)
    # Sampled in either spelling: a normalised program writes the
    # density call in place of a tilde.
    sampled <- grepl("sigma_trend ~", sc) ||
      grepl("_lpdf(sigma_trend", sc, fixed = TRUE)
    expect_equal(sampled, case$sampled)
  }
})

test_that("the registry decides whether a trend has an innovation scale", {
  pw <- PW()
  rw <- RW()
  expect_false(samples_innovation_scale(pw))
  expect_true(samples_innovation_scale(rw))
  expect_true("sigma_trend" %in% generate_monitor_params(rw))
})

test_that("loadings traits answer once for every consumer", {
  # A kernel takes precedence in the `Z` branch, but column
  # shrinkage still has to reach the program through its own
  # hyperpriors, so the two questions stay separate.
  kernel_mgp <- list(N_features_trend = 3L, column_shrinkage = "mgp")
  traits <- loadings_spec_traits(kernel_mgp)
  expect_true(traits$kernel)
  expect_true(traits$mgp)

  # A spec missing the count fields returns FALSE. It once raised on a
  # zero-length comparison.
  bare <- loadings_spec_traits(list(column_shrinkage = "iid"))
  expect_false(bare$kernel)
  expect_false(bare$mgp)
})

test_that("add_trend_suffix_to_priors leaves other classes alone", {
  trend_priors <- rbind(
    brms::prior_string("normal(0, 1)", class = "b"),
    brms::prior_string("student_t(3, 0, 2.5)", class = "sds")
  )
  out <- add_trend_suffix_to_priors(trend_priors)
  expect_equal(sort(out$class), c("b_trend", "sds_trend"))
  expect_equal(sort(out$prior),
               sort(c("normal(0, 1)", "student_t(3, 0, 2.5)")))
})

test_that("trend priors name the trend's axis as lv_axis()", {
  # brms labels the term with the column that replaced 'lv_axis()',
  # a column the user is refused in 'trend_formula'. A second smooth
  # keeps the class-wide default in use.
  dat <- create_test_data(n = 20, n_series = 3)
  f <- mvgam_formula(
    y ~ 1, trend_formula = ~ s(x, k = 4, by = lv_axis()) + s(x1, k = 4) + RW()
  )
  coef <- "s(x, k = 4, by = lv_axis())"
  expect_true(coef %in% get_prior(f, data = dat, family = gaussian())$coef)
  code <- stancode(
    f, data = dat, family = gaussian(),
    prior = brms::set_prior("inv_gamma(2, 1)", class = "sds_trend",
                            coef = coef), validate = FALSE
  )
  # The prior reaches the smooth of each of the three series
  expect_equal(lengths(regmatches(code, gregexpr(
    "inv_gamma_lpdf\\(sds_[0-9]_trend", code
  ))), 3L)
})

test_that("a coef-scoped sigma row is not mistaken for the bookkeeping one", {
  # Only the bare `sigma` row is brms's residual scale. A row scoped
  # to a coefficient belongs to a distributional sub-formula and keeps
  # whatever the user or brms set.
  scoped <- brms::prior_string("normal(0, 3)", class = "sigma",
                                coef = "x")
  out <- add_trend_suffix_to_priors(scoped)
  expect_equal(out$prior, "normal(0, 3)")
  expect_equal(out$class, "sigma_trend")
})

test_that("a class mvgam manages is routed to the trend side", {
  # Splitting a prior table on the `_trend` suffix alone files `Z`
  # as observation-side, so brms is handed a class it has no
  # parameter for and refuses it, while `default_prior()` is where
  # the user read the class name. One predicate decides.
  cls <- c("b", "Intercept", "sigma", "sigma_trend", "ar1_trend",
           "Z", "Z_free_vec", "varrho_inv", "theta_features",
           "theta_dist_phylo")
  managed <- mvgam:::is_mvgam_managed_class(cls)
  expect_equal(cls[managed],
               c("sigma_trend", "ar1_trend", "Z", "Z_free_vec",
                 "varrho_inv", "theta_features", "theta_dist_phylo"))
  expect_equal(cls[!managed], c("b", "Intercept", "sigma"))
})

test_that("a user prior on Z reaches the emitted Stan", {
  # `default_prior()` advertises the class. Setting it once failed
  # with brms' "do not correspond to any model parameter".
  set.seed(1L)
  d <- sim_mvgam(family = poisson(), n_series = 4L,
                  n_timepoints = 20L)$data_train
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, n_lv = 2))
  sc <- stancode(mf, data = d, family = poisson(),
                 prior = prior(normal(0, 3), class = Z), validate = FALSE)
  ln <- strsplit(paste(as.character(sc), collapse = "\n"), "\n")[[1]]
  expect_true(any(grepl(stan_prior_line("to_vector(Z)", "normal(0, 3)"),
                          ln, fixed = TRUE)))
})

test_that("the reported Z prior is the one the model samples", {
  # `Z` is drawn from one of three branches depending on the
  # loadings prior. The table reported the unstructured default for
  # all three, so two of them described a model the user was not
  # fitting. Both surfaces now read `loadings_z_branch()`.
  set.seed(1L)
  d <- sim_mvgam(family = poisson(), n_series = 4L,
                  n_timepoints = 20L)$data_train
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, n_lv = 2))
  traits <- data.frame(sp = factor(paste0("series_", 1:4)),
                        t1 = rnorm(4))
  agrees <- function(...) {
    p <- get_prior(mf, data = d, family = poisson(), ...)
    reported <- as.character(p$prior[p$class == "Z"])[1]
    sc <- gsub("[[:space:]]+", " ", paste(
      as.character(stancode(mf, data = d, family = poisson(), ...,
                            validate = FALSE)),
      collapse = " "
    ))
    # The table reports `dist(args)`. Each branch writes `Z` with
    # a different left-hand side, `to_vector(Z)` for two of them
    # and a column slice for the kernel, so the comparison names
    # the distribution and its arguments and leaves the left side
    # open. Either spelling of the statement counts.
    dist <- sub("\\(.*$", "", reported)
    args <- sub("\\)$", "", sub("^[^(]*\\(", "", reported))
    tilde <- grepl(paste0("~ ", reported), sc, fixed = TRUE)
    call <- grepl(paste0(dist, "_lpdf("), sc, fixed = TRUE) &&
      (!nzchar(args) || grepl(paste0("| ", args, ")"), sc,
                                fixed = TRUE))
    tilde || call
  }
  expect_true(agrees())
  expect_true(agrees(loadings_prior = list(column_shrinkage = "mgp")))
  expect_true(agrees(loadings_prior = list(features = traits)))
  expect_true(agrees(loadings_prior = list(features = traits,
                                            column_shrinkage = "mgp")))
})

test_that("the branch classifier names each loadings path once", {
  expect_equal(mvgam:::loadings_z_branch(NULL), "unstructured")
  expect_equal(
    mvgam:::loadings_z_branch(list(column_shrinkage = "mgp")), "mgp"
  )
  expect_equal(
    mvgam:::loadings_z_branch(list(N_features_trend = 2L)), "kernel"
  )
  # A kernel takes precedence: the emitted statement is the
  # multivariate normal either way.
  expect_equal(
    mvgam:::loadings_z_branch(
      list(N_features_trend = 2L, column_shrinkage = "mgp")
    ),
    "kernel"
  )
  expect_null(mvgam:::loadings_z_prior_string("unstructured"))
})

test_that("get_prior describes the loadings a trend_map leaves", {
  # A fully fixed map moves `Z` to the data block, with no prior. A
  # partial map samples its `NA` entries as `Z_free_vec` under the
  # `Z` prior. Fixed loadings pin the factors' scale, and the model
  # samples `sigma_trend`.
  set.seed(1L)
  d <- sim_mvgam(family = poisson(), n_series = 4L,
                  n_timepoints = 20L)$data_train
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1))
  fixed <- data.frame(series = factor(paste0("series_", 1:4)),
                      trend = c(1L, 1L, 2L, 2L))
  partial <- matrix(c(1, NA, 0, NA, 0, NA, 1, NA), 4, 2)
  for (tm in list(fixed, partial)) {
    tab <- get_prior(mf, data = d, family = poisson(), trend_map = tm)
    sc <- stancode(mf, data = d, family = poisson(), trend_map = tm,
                   validate = FALSE)
    expect_true("sigma_trend" %in% tab$class)
    expect_equal(stan_prior_on(sc, "sigma_trend"),
                 tab$prior[tab$class == "sigma_trend"])
    expect_equal(stan_prior_on(sc, "Z_free_vec"),
                 tab$prior[tab$class == "Z"])
  }
  # Only the partial map samples loadings.
  expect_false("Z" %in% get_prior(mf, data = d, family = poisson(),
                                   trend_map = fixed)$class)

  # A user prior on `Z` reaches the free entries of a partial map.
  sc <- stancode(mf, data = d, family = poisson(), trend_map = partial,
                 prior = prior(normal(0, 3), class = Z), validate = FALSE)
  expect_equal(stan_prior_on(sc, "Z_free_vec"), "normal(0, 3)")

  # Written on the constructor, the map gives the same table.
  mf_con <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1,
                                                      trend_map = fixed))
  expect_equal(
    get_prior(mf_con, data = d, family = poisson()),
    get_prior(mf, data = d, family = poisson(), trend_map = fixed)
  )
})

test_that("get_prior uses the axis columns the constructor names", {
  d <- data.frame(y = rpois(40, 5), week = rep(1:20, 2),
                  site = factor(rep(c("a", "b"), each = 20)))
  mf <- mvgam_formula(y ~ 1, ~ AR(time = week, series = site))
  expect_equal(
    get_prior(mf, data = d, family = poisson())$class,
    get_prior(mvgam_formula(y ~ 1, ~ AR()), family = poisson(),
              data = data.frame(y = d$y, time = d$week,
                                series = d$site))$class
  )
})

test_that("get_prior refuses a factor request the trend cannot take", {
  # The top-level `trend_map` reaches a trend that has no factor
  # form. `mvgam()` refuses it, and the prior table must agree.
  d <- data.frame(y = rpois(40, 5), time = rep(1:20, 2),
                  series = factor(rep(c("a", "b"), each = 20)))
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ CAR())
  expect_error(
    get_prior(mf, data = d, family = poisson(),
              trend_map = matrix(c(1, NA), 2, 1)),
    "Factor models are not supported for CAR trends"
  )
})

test_that("trend priors are checked against the table get_prior() shows", {
  defaults <- rbind(
    brms::set_prior("exponential(2)", class = "sigma_trend", lb = 0),
    brms::set_prior("", class = "lscale_trend"),
    brms::set_prior("inv_gamma(1.5, 0.06)", class = "lscale_trend",
                    coef = "gpz")
  )
  check <- function(p) check_trend_priors(p, defaults, "sigma_trend")
  # A class the model lacks was dropped unseen: 'nu_trend' on gaussian
  # innovations, 'sigma_trend' under PW().
  expect_error(check(brms::set_prior("gamma(2, 0.1)", class = "nu_trend")),
               "class = nu_trend")
  expect_error(
    check(brms::set_prior("normal(0, 1)", class = "lscale_trend",
                          coef = "gpx")),
    "class = lscale_trend, coef = gpx"
  )
  # The Stan declaration keeps its own bounds whatever the row says.
  expect_error(
    check(brms::set_prior("exponential(5)", class = "sigma_trend", lb = 0.5)),
    "sets the bounds"
  )
  # A fitted table restates the declared bound. update() inherits it.
  expect_silent(
    check(brms::set_prior("exponential(5)", class = "sigma_trend", lb = 0))
  )
  # brms ignores a class-wide length-scale prior, and its own warning
  # names the class with the suffix removed.
  expect_warning(
    kept <- check(brms::set_prior("inv_gamma(3, 1)", class = "lscale_trend")),
    "class = lscale_trend, coef\\s+= gpz"
  )
  expect_identical(nrow(kept), 0L)
  by_coef <- check(
    brms::set_prior("exponential(5)", class = "sigma_trend") +
      brms::set_prior("inv_gamma(3, 1)", class = "lscale_trend", coef = "gpz")
  )
  expect_identical(attr(by_coef, "constructor"), c(TRUE, FALSE))
})
