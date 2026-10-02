# `trend_registry` is package state, and a testthat worker runs
# several files in one process. The tests below clear it. A failure
# between a clear and its re-registration would hand every later file
# in that worker an empty registry. This restores the core set when
# the file ends.
withr::defer(
  {
    rm(list = ls(envir = trend_registry), envir = trend_registry)
    register_core_trends()
  },
  teardown_env()
)

test_that("the registry initialises once with every core trend", {
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  expect_false(is_registry_initialized())
  ensure_registry_initialized()
  expect_true(is_registry_initialized())
  n_trends <- length(ls(trend_registry))
  ensure_registry_initialized()
  expect_length(ls(trend_registry), n_trends)

  trends <- list_trend_types()
  expect_setequal(trends$trend_type[trends$supports_factors],
                  c("AR", "RW", "VAR", "ZMVN"))
  expect_setequal(trends$trend_type[!trends$supports_factors],
                  c("CAR", "PW"))
  expect_error(get_trend_info("NonExistentTrend"),
               "Unknown trend type.*NonExistentTrend")
})

test_that("trend properties are validated", {
  for (props in list(ar_trend_properties(), rw_trend_properties(),
                     var_trend_properties(), zmvn_trend_properties(),
                     car_trend_properties(), pw_trend_properties())) {
    expect_invisible(validate_trend_properties(props, "f"))
  }
  # Every property is declared by each trend and none has a default
  expect_error(
    validate_trend_properties(list(supports_factors = TRUE), "f"),
    "covariance_pattern"
  )
  expect_error(validate_trend_properties("x", "f"), "must be a list")

  # The values are checked where the entry is recorded
  generator <- function(trend_specs, data_info) list()
  register <- function(...) {
    args <- utils::modifyList(
      list(name = "T", supports_factors = TRUE,
           covariance_pattern = "diagonal", stationary_source = "none",
           requires_regular_intervals = TRUE,
           per_factor_coefficients = FALSE, generator_func = generator),
      list(...)
    )
    do.call(register_trend_type, args)
  }
  withr::defer(rm(list = intersect("T", ls(trend_registry)),
                  envir = trend_registry))
  expect_error(register(supports_factors = "yes"), "supports_factors")
  expect_error(register(covariance_pattern = "banded"), "covariance_pattern")
  # A trend without a factor form gives the refusal its reason
  expect_error(register(supports_factors = FALSE),
               "incompatibility_reason")
})

test_that("the registry holds each trend's covariance and time facts", {
  ensure_registry_initialized()
  patterns <- vapply(c("AR", "RW", "ZMVN", "VAR", "CAR", "PW"),
                     get_covariance_pattern, character(1))
  expect_identical(
    unname(patterns),
    c(rep("cholesky_scaled", 3L), "full_covariance", "diagonal", "none")
  )
  # PW samples no innovation, which removes `sigma_trend`
  expect_false(samples_innovation_scale(PW()))
  expect_true(samples_innovation_scale(CAR()))
  # CAR carries the elapsed gap and ZMVN is exchangeable in time
  regular <- vapply(c("AR", "RW", "VAR", "PW", "CAR", "ZMVN"), function(tt) {
    get_trend_info(tt)$requires_regular_intervals
  }, logical(1))
  expect_identical(unname(regular), c(rep(TRUE, 4L), FALSE, FALSE))
})

test_that("create_mvgam_trend checks the arguments every trend shares", {
  expect_error(AR(n_lv = 0), "n_lv")
  expect_error(RW(n_lv = 1.5), "n_lv")
  expect_error(VAR(ma = NA), "ma")
  expect_error(AR(cor = NA), "cor")
})

test_that("simplified RW constructor works correctly", {
  # Test simplified RW constructor using helper functions
  # Test basic RW constructor
  rw_trend <- RW()
  expect_s3_class(rw_trend, "mvgam_trend")
  expect_equal(rw_trend$trend, "RW")
  expect_false(rw_trend$ma)
  expect_false(rw_trend$cor)
  expect_null(rw_trend$n_lv)

  # RW indexes its lag by position, and the fitting path tests this
  # declaration to require an even grid.
  expect_identical(rw_trend$validation_rules,
                   "requires_regular_intervals")

  # Test RW with parameters
  rw_ma_trend <- RW(ma = TRUE, cor = TRUE)
  expect_true(rw_ma_trend$ma)
  expect_true(rw_ma_trend$cor)

  # Test RW with custom variables
  rw_custom <- RW(time = week, series = species, ma = TRUE)
  expect_equal(rw_custom$time, "week")
  expect_equal(rw_custom$series, "species")
  expect_true(rw_custom$ma)
})

test_that("helper functions work correctly", {
  # Test get_mvgam_trend_defaults
  defaults <- get_mvgam_trend_defaults()
  expect_type(defaults, "list")
  expect_equal(defaults$time, "time")
  expect_equal(defaults$series, "series")
  expect_equal(defaults$gr, "NA")
  expect_false(defaults$ma)
  expect_false(defaults$cor)
  expect_null(defaults$n_lv)

  # The one declaration, and the two trends exempt from it. ZMVN's
  # likelihood is `MVN(0, Sigma)` with covariance indexed by series,
  # and the time gap enters neither the Stan likelihood
  # (`to_vector(innovations_trend) ~ std_normal()`) nor
  # `propagate_zmvn()`, which takes no `time` argument. That admits a
  # gappy grid, such as the `jsdgam(unit = site)` layout whose kfold
  # refit drops a fold of sites.
  for (tt in c("RW", "AR", "VAR", "PW")) {
    expect_identical(get_default_validation_rules(tt),
                     "requires_regular_intervals")
  }
  for (tt in c("CAR", "ZMVN")) {
    expect_identical(get_default_validation_rules(tt), character(0))
  }

  # Test apply_mvgam_trend_defaults
  partial_trend <- list(trend = "RW", ma = TRUE)
  complete_trend <- apply_mvgam_trend_defaults(partial_trend)
  expect_equal(complete_trend$trend, "RW")
  expect_true(complete_trend$ma)
  expect_equal(complete_trend$time, "time")  # Filled by defaults
  expect_equal(complete_trend$series, "series")  # Filled by defaults
  expect_true(length(complete_trend$validation_rules) > 0)  # Auto-assigned

  # Test create_mvgam_trend helper
  trend_obj <- create_mvgam_trend("RW", ma = TRUE, cor = FALSE)
  expect_s3_class(trend_obj, "mvgam_trend")
  expect_equal(trend_obj$trend, "RW")
  expect_true(trend_obj$ma)
  expect_false(trend_obj$cor)
  expect_equal(trend_obj$time, "time")
  expect_equal(trend_obj$series, "series")
})

test_that("simplified AR constructor works correctly", {
  # Test simplified AR constructor using create_mvgam_trend helper
  # Test basic AR constructor
  ar_trend <- AR()
  expect_s3_class(ar_trend, "mvgam_trend")
  expect_equal(ar_trend$trend, "AR")  # Base type for dispatch
  expect_equal(ar_trend$p, 1)  # Default p = 1
  expect_false(ar_trend$ma)
  expect_false(ar_trend$cor)
  expect_null(ar_trend$n_lv)

  # Test AR with custom parameters
  ar_custom <- AR(p = 3, ma = TRUE, cor = TRUE, n_lv = 2)
  expect_equal(ar_custom$trend, "AR")  # Always base type
  expect_equal(ar_custom$p, 3)
  expect_true(ar_custom$ma)
  expect_true(ar_custom$cor)
  expect_equal(ar_custom$n_lv, 2)

  # Test AR with vector p
  ar_vector <- AR(p = c(1, 12))
  expect_equal(ar_vector$trend, "AR")  # Still base type
  expect_equal(ar_vector$p, c(1, 12))

  # Test AR with custom time/series variables
  ar_vars <- AR(time = month, series = site)
  expect_equal(ar_vars$time, "month")
  expect_equal(ar_vars$series, "site")

  # Test AR with grouping variables
  ar_grouped <- AR(gr = region, subgr = species)
  expect_equal(ar_grouped$gr, "region")
  expect_equal(ar_grouped$subgr, "species")
})

test_that("create_mvgam_trend handles all parameters consistently", {
  # Test that all parameters use dot-prefix convention
  # Use a valid trend type (RW) for this test
  trend_obj <- create_mvgam_trend(
    "RW",
    .time = quote(month),
    .series = quote(site),
    .gr = quote(region),
    .subgr = quote(species),
    custom_param = 42
  )

  expect_equal(trend_obj$trend, "RW")
  expect_equal(trend_obj$time, "month")
  expect_equal(trend_obj$series, "site")
  expect_equal(trend_obj$gr, "region")
  expect_equal(trend_obj$subgr, "species")
  expect_equal(trend_obj$custom_param, 42)
})

test_that("a trendless fit takes the \"none\" pattern", {
  # A trendless fit reports its trend type as "None". Routing it to a
  # stochastic pattern sends every prediction surface to
  # sample_process_errors() on metadata the fit lacks.
  expect_identical(get_covariance_pattern("None"), "none")
  expect_identical(trend_stationary_source("None"), "none")
  expect_error(get_covariance_pattern("AR1"), "Unknown trend type")
})

test_that("has_stochastic_trend() returns FALSE for trendless mvgam objects", {
  # Reason: a trendless fit (no trend_model, no trend_formula) carries
  # trend_type = "None". Misrouting this case as a stochastic trend
  # bombs every prediction surface that conditionally calls
  # sample_process_errors() on the missing trend_metadata.
  obj <- structure(
    list(trend_type = "None"),
    class = "mvgam"
  )
  expect_false(mvgam:::has_stochastic_trend(obj))
})

test_that("normalise_prior_arg_alias() renames 'priors' to 'prior'", {
  # Reason: the brms convention is the singular form; historic mvgam
  # and jsdgam docs use the plural. Without aliasing, the plural
  # form falls into `...` and is silently dropped by every consumer
  # downstream that takes `prior = NULL`.
  fake_prior <- structure(
    data.frame(
      prior = "normal(0, 1)", class = "b",
      coef = "", group = "", resp = "", dpar = "",
      nlpar = "a", lb = NA, ub = NA, tag = "",
      source = "user", stringsAsFactors = FALSE
    ),
    class = c("brmsprior", "data.frame")
  )

  # Plural alias renamed in place.
  out <- mvgam:::normalise_prior_arg_alias(list(priors = fake_prior))
  expect_named(out, "prior")
  expect_identical(out$prior, fake_prior)

  # Singular passthrough untouched.
  out_s <- mvgam:::normalise_prior_arg_alias(list(prior = fake_prior))
  expect_named(out_s, "prior")
  expect_identical(out_s$prior, fake_prior)

  # No prior at all -> no rename, no addition.
  expect_identical(
    mvgam:::normalise_prior_arg_alias(list(chains = 2L)),
    list(chains = 2L)
  )

  # Conflicting prior + priors with different values errors.
  other <- fake_prior
  other$prior <- "normal(0, 10)"
  expect_error(
    mvgam:::normalise_prior_arg_alias(
      list(prior = fake_prior, priors = other)
    ),
    "Both 'prior' and 'priors' supplied"
  )

  # Same value under both names is a no-op (no error).
  expect_silent(
    mvgam:::normalise_prior_arg_alias(
      list(prior = fake_prior, priors = fake_prior)
    )
  )
})


# ---- an `ma` term must bring its parameters with it ------------------
#
# The branch adding them was written out once per trend, and the copies
# drifted: `RW()` and `VAR()` listed theirs while `AR()` did not, so an
# `AR(ma = TRUE)` fit sampled `theta1_trend` under a prior nothing
# reported and nothing could override. The names now live in one table.


test_that("each trend lists the parameters its ma term samples", {
  expect_identical(
    generate_rw_monitor_params(RW(ma = TRUE)), "theta1_trend"
  )
  expect_true(
    "theta1_trend" %in% generate_ar_monitor_params(AR(p = 1, ma = TRUE))
  )
  expect_true(
    all(c("Dmu_trend", "Domega_trend") %in%
          generate_var_monitor_params(VAR(ma = TRUE)))
  )
})


test_that("no ma term means no ma parameters", {
  expect_length(generate_rw_monitor_params(RW()), 0L)
  expect_false(
    "theta1_trend" %in% generate_ar_monitor_params(AR(p = 1))
  )
  expect_false(
    any(c("Dmu_trend", "Domega_trend") %in%
          generate_var_monitor_params(VAR()))
  )
})


test_that("the ma parameter table names every trend that accepts one", {
  # A trend accepting `ma` without an entry here would silently list
  # nothing, which is how the AR gap arose.
  for (ctor in list(RW(ma = TRUE), AR(p = 1, ma = TRUE), VAR(ma = TRUE))) {
    expect_gt(length(ma_params_for(ctor, ctor$trend)), 0L)
  }
  expect_length(ma_params_for(AR(p = 1), "AR"), 0L)
})


test_that("a reported prior carries the support it is sampled on", {
  # The bounds a parameter is declared with sit in the same program
  # as its prior. Reading one and not the other reported
  # `normal(0, 0.5)` unbounded for an autoregressive coefficient held
  # inside (-1, 1), and for a continuous-time one held inside
  # (0.001, 0.999), so half the reported mass lay outside the support
  # the model samples on.
  dat <- data.frame(
    time = 1:40, series = factor(rep("s1", 40)),
    y = rpois(40, 5)
  )
  declared <- function(sc, par) {
    line <- grep(paste0("[ ]", par, ";"), strsplit(sc, "\n")[[1]],
                 value = TRUE)
    c(lb = sub(".*lower[ ]*=[ ]*([^,>]+).*", "\\1", line[1]),
      ub = if (grepl("upper", line[1])) {
        sub(".*upper[ ]*=[ ]*([^,>]+).*", "\\1", line[1])
      } else "")
  }
  for (tr in list(~ AR(p = 1), ~ CAR())) {
    fit <- mvgam(y ~ 1, trend_formula = tr, data = dat,
                 family = poisson(), run_model = FALSE, silent = 2)
    tab <- as.data.frame(prior_summary(fit))
    sc <- as.character(stancode(fit))
    for (par in c("ar1_trend", "sigma_trend")) {
      row <- tab[tab$class == par, , drop = FALSE]
      expect_identical(nrow(row), 1L)
      d <- declared(sc, par)
      expect_identical(row$lb[1], unname(d["lb"]))
      expect_identical(row$ub[1], unname(d["ub"]))
    }
  }
})


test_that("a spec's sharing mode and cap column each have one reader", {
  # The Stan generator, the monitor list and the prior table each
  # spelled their own fallback sharing mode, and the Stan data builder
  # and the forecast each spelled their own cap column.
  expect_identical(
    ar_coef_sharing(AR(p = 2, coef_sharing = "hierarchical")),
    "hierarchical"
  )
  expect_identical(ar_coef_sharing(AR()), "none")
  expect_identical(ar_coef_sharing(RW()), "none")
  expect_identical(pw_cap_var(PW(growth = "logistic", cap = K)), "K")
  expect_identical(pw_cap_var(PW(growth = "logistic")), "cap")
  # A spec nested under `$trend_model` names the same column.
  expect_identical(
    pw_cap_var(list(trend_model = PW(growth = "logistic", cap = K))), "K"
  )
  # A spec built without `PW()` records no column, and no column is
  # guessed for it.
  expect_error(
    pw_cap_var(list(trend = "PW", growth = "logistic")),
    "lacks a carrying-capacity column"
  )
})
