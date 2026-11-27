
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

test_that("combine_obs_trend_priors works correctly", {
  obs_prior <- mock_obs_prior()
  trend_prior <- mock_trend_prior()

  combined <- mvgam:::combine_obs_trend_priors(obs_prior, trend_prior)
  expect_s3_class(combined, "brmsprior")
  expect_identical(combined$class, c("Intercept", "ar1_trend", "sigma_trend"))
})

test_that("validate_trend_formula rejects invalid terms", {
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
  # A category-specific effect shifts ordinal thresholds the trend lacks.
  # brms would report only that its family does not support it.
  for (f in list(~ cs(x) + AR(), ~ (cs(1) | g) + AR())) {
    expect_error(mvgam:::validate_trend_formula(f),
                 "'cs\\(\\)' is not supported in 'trend_formula'")
  }
  # Every response shares one trend. The refusal names one constructor.
  expect_error(
    mvgam:::validate_trend_formula(~ AR() + RW()),
    "Keep one constructor"
  )
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

test_that("merge_default_priors() drops defaults the user has claimed", {
  # Concatenating leaves two rows for the same class. brms refuses the
  # whole set with "Duplicated prior specifications are not
  # allowed". A user overriding a default got that error back.
  defaults <- c(brms::prior("gamma(2, 0.5)", class = "shape"),
                brms::prior("gamma(2, 0.25)", class = "mtail"))
  user <- brms::prior("gamma(3, 1)", class = "shape")

  merged <- mvgam:::merge_default_priors(defaults, user)
  expect_identical(merged$prior[merged$class == "shape"], "gamma(3, 1)")
  # the default the user did not name survives
  expect_identical(merged$prior[merged$class == "mtail"], "gamma(2, 0.25)")
})

test_that("a class-wide user prior leaves the coefficient rows blank", {
  # brms stores it this way. A copy on each level of a by-factor gp()
  # reached brms on refit, which refuses a prior on one level.
  defaults <- rbind(
    brms::set_prior("", class = "sdgp"),
    brms::set_prior("", class = "sdgp", coef = "gpxa"),
    brms::set_prior("", class = "sdgp", coef = "gpxb")
  )
  defaults$source <- "default"
  merged <- merge_user_priors(defaults,
                              brms::prior(exponential(1), class = sdgp))
  expect_identical(merged$prior, c("exponential(1)", "", ""))
  expect_identical(merged$source, c("user", "default", "default"))
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

test_that("a trend with an innovation scale monitors it", {
  expect_true("sigma_trend" %in% generate_monitor_params(RW()))
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

test_that("get_prior refuses a factor request the trend cannot take", {
  # A `trend_map` asks a trend with no factor form for factors.
  # `mvgam()` refuses it. The prior table must refuse it too.
  d <- data.frame(y = rpois(40, 5), time = rep(1:20, 2),
                  series = factor(rep(c("a", "b"), each = 20)))
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ CAR(
    trend_map = matrix(c(1, NA), 2, 1)
  ))
  expect_error(
    get_prior(mf, data = d, family = poisson()),
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
  # The blank class row a refit inherits sets nothing to warn about.
  expect_silent(check(brms::set_prior("", class = "lscale_trend")))
  by_coef <- check(
    brms::set_prior("exponential(5)", class = "sigma_trend") +
      brms::set_prior("inv_gamma(3, 1)", class = "lscale_trend", coef = "gpz")
  )
  expect_identical(attr(by_coef, "constructor"), c(TRUE, FALSE))
})
