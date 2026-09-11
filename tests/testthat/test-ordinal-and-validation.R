# Tests for the ordinal families' predictive distribution and the
# brms-parity argument validation on loo / waic. CI-safe: synthetic
# inputs only.

# -------------------------------------------------------------------------
# Category probabilities, checked against the densities brms writes in
# Stan for each family. At eta = 0.5, thresholds (-1, 1) and disc = 2,
# the offsets are disc * (thres - eta) = (-3, 1) for the families brms
# marks "thres_minus_eta" and their negatives for "eta_minus_thres".
# -------------------------------------------------------------------------

ordinal_case <- function(family, hu = NULL) {
  ordinal_probs(
    eta = matrix(0.5), thres = matrix(c(-1, 1), nrow = 1L),
    family = family, disc = matrix(2),
    hu = if (!is.null(hu)) matrix(hu)
  )
}

test_that("ordinal_probs puts disc outside the threshold offset", {
  cdf <- stats::plogis
  # cumulative_logit_lpmf: F(disc * (thres[k] - mu)), differenced
  expect_equal(
    as.numeric(ordinal_case(brms::cumulative())),
    c(cdf(-3), cdf(1) - cdf(-3), 1 - cdf(1))
  )
  # sratio_logit_lpmf: P(y = k | y >= k) = F(disc * (thres[k] - mu))
  expect_equal(
    as.numeric(ordinal_case(brms::sratio())),
    c(cdf(-3), (1 - cdf(-3)) * cdf(1), (1 - cdf(-3)) * (1 - cdf(1)))
  )
  # cratio_logit_lpmf: P(y > k | y >= k) = F(disc * (mu - thres[k]))
  expect_equal(
    as.numeric(ordinal_case(brms::cratio())),
    c(1 - cdf(3), cdf(3) * (1 - cdf(-1)), cdf(3) * cdf(-1))
  )
  # acat_logit_lpmf: softmax of the running sums of disc * (mu - thres)
  unnorm <- exp(c(0, 3, 3 - 1))
  expect_equal(as.numeric(ordinal_case(brms::acat())),
               unnorm / sum(unnorm))
})

test_that("ordinal_probs applies each family's own link", {
  # A probit acat takes the general branch, which a logit never reaches:
  # P(Y = k + 1) / P(Y = k) = F(x_k) / (1 - F(x_k)).
  cdf <- stats::pnorm(c(3, -1))
  unnorm <- c(
    (1 - cdf[1]) * (1 - cdf[2]),
    cdf[1] * (1 - cdf[2]),
    cdf[1] * cdf[2]
  )
  expect_equal(as.numeric(ordinal_case(brms::acat("probit"))),
               unnorm / sum(unnorm))
  expect_equal(
    as.numeric(ordinal_case(brms::cumulative("cloglog"))),
    c(inv_link(-3, "cloglog"),
      inv_link(1, "cloglog") - inv_link(-3, "cloglog"),
      1 - inv_link(1, "cloglog"))
  )
})

test_that("a hurdle ordinal family puts its hurdle on a category of its own", {
  probs <- ordinal_case(brms::hurdle_cumulative(), hu = 0.2)
  expect_identical(dimnames(probs)[[3L]], c("0", "1", "2", "3"))
  expect_equal(
    as.numeric(probs),
    c(0.2, 0.8 * as.numeric(ordinal_case(brms::cumulative())))
  )
  # The expected level counts the hurdle as level 0.
  expect_equal(
    as.numeric(ordinal_category_mean(probs)),
    sum(0:3 * as.numeric(probs))
  )
})

test_that("ordinal_probs keeps every draw and observation apart", {
  set.seed(31)
  eta <- matrix(stats::rnorm(6), nrow = 2L)
  thres <- rbind(c(-1, 0.5), c(-0.3, 1.2))
  disc <- matrix(c(1, 2, 0.5, 1, 3, 2), nrow = 2L)
  probs <- ordinal_probs(eta, thres, brms::cumulative(), disc = disc)
  expect_identical(dim(probs), c(2L, 3L, 3L))
  for (d in 1:2) {
    for (i in 1:3) {
      cdf <- stats::plogis(disc[d, i] * (thres[d, ] - eta[d, i]))
      expect_equal(unname(probs[d, i, ]), diff(c(0, cdf, 1)))
    }
  }
})

test_that("ordinal_log_lik reads the observed level's probability", {
  probs <- ordinal_case(brms::hurdle_cumulative(), hu = 0.2)
  probs <- abind::abind(probs, probs, probs, along = 2L)
  ll <- ordinal_log_lik(probs, c(0, 3, NA))
  expect_equal(ll[1L, 1L], log(0.2))
  expect_equal(ll[1L, 2L], log(unname(probs[1L, 2L, "3"])))
  # A row whose response was not measured has no density.
  expect_true(is.na(ll[1L, 3L]))
  expect_error(ordinal_log_lik(probs, c(0, 4, 1)),
               "none of its levels")
})

test_that("ordinal_pit_bounds gives the interval each level fills", {
  probs <- ordinal_case(brms::hurdle_cumulative(), hu = 0.2)
  probs <- abind::abind(probs, probs, probs, along = 2L)
  p <- as.numeric(probs[1L, 1L, ])
  bounds <- ordinal_pit_bounds(probs, c(0, 2, NA))
  # Level 0 is the hurdle, the lowest category.
  expect_equal(bounds$lower[1L, 1L], 0)
  expect_equal(bounds$upper[1L, 1L], 0.2)
  # Level 2 sits above the hurdle and level 1.
  expect_equal(bounds$lower[1L, 2L], sum(p[1:2]))
  expect_equal(bounds$upper[1L, 2L], sum(p[1:3]))
  expect_true(is.na(bounds$upper[1L, 3L]))
})

test_that("ordinal_draws draws each level at its probability", {
  set.seed(2026)
  n <- 20000L
  probs <- ordinal_case(brms::hurdle_cumulative(), hu = 0.2)
  many <- probs[rep(1L, n), , , drop = FALSE]
  drawn <- ordinal_draws(many)
  expect_type(drawn, "integer")
  expect_identical(dim(drawn), c(n, 1L))
  observed <- as.numeric(prop.table(table(factor(drawn, levels = 0:3))))
  expected <- as.numeric(probs)
  expect_true(all(abs(observed - expected) <
                    3 * sqrt(expected * (1 - expected) / n)))
})

test_that("every brms ordinal family is recognised as ordinal", {
  for (f in c("cumulative", "sratio", "cratio", "acat",
              "hurdle_cumulative")) {
    expect_true(is_ordinal_family(brms::brmsfamily(f)))
  }
  expect_false(is_ordinal_family(poisson()))
  expect_false(is_ordinal_family(NULL))
})


# -------------------------------------------------------------------------
# Thresholds, read from a fit's draws
# -------------------------------------------------------------------------

ordinal_fit_stub <- function(draws, formula) {
  structure(
    list(fit = posterior::as_draws_matrix(draws), formula = formula,
         family = brms::cumulative()),
    class = "mvgam"
  )
}

test_that("thresholds cut the uncentred predictor mvgam builds", {
  # `Intercept[k]` cut the centred design brms samples on; mvgam's
  # predictor uses the uncentred one, which `b_Intercept[k]` cut. The
  # two differ by the slopes times the covariate means.
  draws <- cbind(
    `Intercept[1]` = c(-1, -1.1), `Intercept[2]` = c(1, 0.9),
    `b_Intercept[1]` = c(-4, -4.1), `b_Intercept[2]` = c(-2, -2.1),
    disc = c(1, 1)
  )
  stub <- ordinal_fit_stub(draws, brms::bf(y ~ x))
  expect_equal(ordinal_thresholds(stub), unname(draws[, 3:4]))
  expect_equal(ordinal_thresholds(stub, draw_ids = 2L),
               unname(draws[2L, 3:4, drop = FALSE]))

  eta <- matrix(c(0.2, -0.4), nrow = 2L)
  expect_equal(
    ordinal_category_probs(stub, eta, brms::cumulative()),
    ordinal_probs(eta, unname(draws[, 3:4]), brms::cumulative())
  )
})

test_that("thresholds are read in index order and per response", {
  # Ten thresholds sort `[10]` before `[2]` as text.
  thres <- stats::setNames(seq(-4.5, 4.5, length.out = 10L),
                           paste0("b_y_Intercept[", 1:10, "]"))
  shuffled <- thres[c(10, 2:9, 1)]
  draws <- rbind(c(shuffled, `b_c_Intercept[1]` = 9, disc_y = 1))
  stub <- ordinal_fit_stub(
    draws,
    brms::bf(y ~ x, family = brms::cumulative()) +
      brms::bf(c ~ x, family = brms::cumulative())
  )
  expect_equal(as.numeric(ordinal_thresholds(stub, resp = "y")),
               as.numeric(thres))
  expect_equal(as.numeric(ordinal_thresholds(stub, resp = "c")), 9)
})


# -------------------------------------------------------------------------
# Families mvgam refuses at build
# -------------------------------------------------------------------------

test_that("brms's matrix-response families point at mvgam's own", {
  # Each is refused by the special brms marks it with, which a list of
  # names had missed for `dirichlet2()` and `dirichlet_multinomial()`.
  expect_error(validate_supported_family(brms::dirichlet_multinomial()),
               "multi\\(\\)")
  expect_error(
    validate_supported_family(brms::brmsfamily("dirichlet2")),
    "diri\\(\\)"
  )
  expect_error(validate_supported_family(brms::logistic_normal()),
               "mvn\\(\\)")
  expect_error(validate_supported_family(brms::categorical()),
               "categ\\(\\)")
  expect_true(validate_supported_family(brms::cumulative()))
})

test_that("ordinal thresholds that vary by group are refused at build", {
  set.seed(5)
  d <- data.frame(
    y = sample(1:4, 40, TRUE), x = stats::rnorm(40),
    g = factor(rep(c("u", "v"), 20)),
    time = rep(1:20, 2), series = factor(rep(c("a", "b"), each = 20))
  )
  expect_error(
    stancode(mvgam_formula(y | thres(gr = g) ~ x), data = d,
             family = cumulative()),
    "vary by group"
  )
  # A shared threshold count is one set for every observation.
  expect_no_error(
    stancode(mvgam_formula(y | thres(3) ~ x), data = d,
             family = cumulative())
  )
})


# -------------------------------------------------------------------------
# Link inverses
# -------------------------------------------------------------------------

test_that("inv_link inverts every link brms offers", {
  expect_equal(inv_link(log(3), "logm1"), 4)
  expect_equal(inv_link(0, "softit"), log(2) / (1 + log(2)))
  expect_equal(inv_link(0, "squareplus"), 1)
  expect_equal(inv_link(4, "1/mu^2"), 0.5)
  expect_equal(inv_link(1, "tan_half"), pi / 2)
  expect_equal(inv_link(log(2), "softplus"), log(3))
  # `exp()` overflows past 709, where softplus is the identity.
  expect_equal(inv_link(800, "softplus"), 800)
  x <- matrix(c(-1, 0, 1, 2), nrow = 2L)
  expect_equal(inv_link(x, "logit"), stats::plogis(x))
  expect_error(inv_link(0, "magic"), "no inverse")
})


# -------------------------------------------------------------------------
# loo.mvgam / waic.mvgam — brms-parity argument validation
# -------------------------------------------------------------------------

# These tests assert the rejection paths fire before any model machinery
# touches the object, so a bare-class stub is sufficient.

test_that("loo.mvgam: pointwise = TRUE errors", {
  stub <- structure(list(), class = "mvgam")
  expect_error(loo(stub, pointwise = TRUE),
               regexp = "pointwise = TRUE")
})

test_that("loo.mvgam: moment_match = TRUE errors", {
  stub <- structure(list(), class = "mvgam")
  expect_error(loo(stub, moment_match = TRUE),
               regexp = "moment_match")
})

test_that("loo.mvgam: reloo = TRUE errors", {
  stub <- structure(list(), class = "mvgam")
  expect_error(loo(stub, reloo = TRUE),
               regexp = "reloo")
})

test_that("waic.mvgam: pointwise = TRUE errors", {
  stub <- structure(list(), class = "mvgam")
  expect_error(waic(stub, pointwise = TRUE),
               regexp = "pointwise = TRUE")
})


# -------------------------------------------------------------------------
# log_lik.mvgam — argument validation that does not require a real fit
# -------------------------------------------------------------------------

test_that("log_lik.mvgam: invalid object class errors via checkmate", {
  # Calling log_lik on a non-mvgam stub with class "mvgam" but no body
  # should fire the checkmate assertions on newdata / ndraws etc.
  # Calling the generic on a bare string fails at S3 dispatch before
  # the checkmate; both error paths are acceptable as long as no
  # silent success can happen.
  stub <- structure(list(), class = "mvgam")
  expect_error(log_lik(stub))  # no newdata, no $data
  expect_error(log_lik("not an mvgam"))
})


# -------------------------------------------------------------------------
# loo.mvgam: signature has incl_dynamics after ...
# -------------------------------------------------------------------------

test_that("loo.mvgam: incl_dynamics lives after `...` (brms convention)", {
  fmls <- names(formals(getS3method("loo", "mvgam")))
  dots_idx <- which(fmls == "...")
  inc_idx <- which(fmls == "incl_dynamics")
  expect_true(length(dots_idx) == 1L)
  expect_true(length(inc_idx) == 1L)
  expect_gt(inc_idx, dots_idx)
})

test_that("loo_compare.mvgam: criterion + incl_dynamics live after `...`", {
  fmls <- names(formals(getS3method("loo_compare", "mvgam")))
  dots_idx <- which(fmls == "...")
  expect_true(length(dots_idx) == 1L)
  expect_gt(which(fmls == "criterion"), dots_idx)
  expect_gt(which(fmls == "incl_dynamics"), dots_idx)
})

test_that("pp_check.mvgam: resp + draw_ids are accepted (brms parity)", {
  fmls <- names(formals(getS3method("pp_check", "mvgam")))
  expect_true("resp" %in% fmls)
  expect_true("draw_ids" %in% fmls)
})


# -------------------------------------------------------------------------
# extract_component_linpred / extract_trend_latent_states signature gates
# -------------------------------------------------------------------------

test_that("extract_component_linpred: incl_latent_state arg exists", {
  fmls <- names(formals(extract_component_linpred))
  expect_true("incl_latent_state" %in% fmls)
})

test_that("extract_trend_latent_states: missing trend[t,s] column errors", {
  # Hand-build a [3 x 4] draws matrix containing only trend[1,1] and
  # trend[2,1]; index a (t,s) that does not exist in the columns to
  # confirm the pre-loop validation fires with a clear message rather
  # than the cryptic 'subscript out of bounds'.
  full_draws <- matrix(
    rnorm(12),
    nrow = 3,
    dimnames = list(NULL, c("trend[1,1]", "trend[2,1]",
                            "sigma_trend[1]", "ar1_trend[1]"))
  )
  # Build a minimal mock mvgam object with the structure the helper
  # depends on: standata + trend_metadata + obs_data with a `time`
  # column. Mock get_observation_structure by passing newdata that
  # maps to t = 3 (which is missing from full_draws above).
  mock_fit <- structure(
    list(
      standata = list(
        N_time_trend = 3L,
        N_series_trend = 1L,
        times_trend = matrix(c(1L, 2L, 3L), ncol = 1L)
      ),
      trend_metadata = list(
        variables = list(time_var = "time", series_var = "series")
      ),
      obs_data = data.frame(time = 1:3),
      data = data.frame(time = 1:3)
    ),
    class = "mvgam"
  )
  attr(mock_fit$obs_data, "mvgam_series") <- factor("s1", levels = "s1")
  # newdata spans t = 1, 2, 3 so `obs_struct$unique_times` sees all
  # three positions and the last row resolves to `trend[3, 1]`,
  # which is deliberately absent from full_draws above so the
  # pre-loop validation raises its error.
  newdata <- data.frame(time = c(1L, 2L, 3L))
  expect_error(
    extract_trend_latent_states(mock_fit, newdata,
                                full_draws = full_draws),
    regexp = "Latent trend state column missing from posterior draws"
  )
})

test_that("ordinal_category_mean: expectation over the ordered levels", {
  # Two draws, three observations, three categories.
  epred <- array(0, dim = c(2L, 3L, 3L), dimnames = list(NULL, NULL, 1:3))
  epred[, , 1L] <- matrix(c(1, 0, 0.5, 0.25, 0, 0.2), nrow = 2L)
  epred[, , 2L] <- matrix(c(0, 1, 0.5, 0.25, 0.5, 0.3), nrow = 2L)
  epred[, , 3L] <- matrix(c(0, 0, 0, 0.5, 0.5, 0.5), nrow = 2L)

  expect_equal(
    ordinal_category_mean(epred),
    matrix(c(1, 2, 1.5, 2.25, 2.5, 2.3), nrow = 2L)
  )
})

test_that("ordinal_category_variance: zero on a point mass, and E[k^2]-E[k]^2", {
  epred <- array(0, dim = c(1L, 2L, 3L), dimnames = list(NULL, NULL, 1:3))
  # First observation puts all mass on level 2; second splits 1 and 3.
  epred[1L, 1L, ] <- c(0, 1, 0)
  epred[1L, 2L, ] <- c(0.5, 0, 0.5)

  v <- ordinal_category_variance(epred)
  expect_equal(v[1L, 1L], 0)
  # 0.5*1 + 0.5*9 - 2^2
  expect_equal(v[1L, 2L], 1)
})

test_that("ordinal_category_variance: matches the variance of the levels drawn", {
  set.seed(11)
  probs <- c(0.2, 0.5, 0.3)
  epred <- array(probs, dim = c(1L, 1L, 3L),
                 dimnames = list(NULL, NULL, 1:3))
  analytic <- ordinal_category_variance(epred)[1L, 1L]
  drawn <- sample(seq_along(probs), 2e5, replace = TRUE, prob = probs)
  expect_equal(analytic, stats::var(drawn), tolerance = 0.02)
})

test_that("summarize_predictions: keeps the category margin of an ordinal epred", {
  set.seed(12)
  draws <- array(stats::runif(50 * 4 * 3), dim = c(50L, 4L, 3L),
                 dimnames = list(NULL, paste0("obs", 1:4), c("lo", "mid", "hi")))
  out <- summarize_predictions(draws, probs = c(0.025, 0.975), robust = FALSE)

  # brms summarises each category separately and stacks along dim 3.
  expect_equal(dim(out), c(4L, 4L, 3L))
  expect_equal(dimnames(out)[[1L]], paste0("obs", 1:4))
  expect_equal(dimnames(out)[[2L]], c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  expect_equal(dimnames(out)[[3L]], c("lo", "mid", "hi"))
  # Each slice equals the summary of that category on its own.
  expect_equal(
    out[, , 2L],
    summarize_predictions(draws[, , 2L], probs = c(0.025, 0.975),
                          robust = FALSE)
  )
})

test_that("summarize_predictions: a single-observation category slice stays a matrix", {
  draws <- array(stats::rnorm(10 * 1 * 2), dim = c(10L, 1L, 2L))
  out <- summarize_predictions(draws, probs = 0.5, robust = TRUE)
  expect_equal(dim(out), c(1L, 3L, 2L))
})


test_that("a single-series ZMVN is flagged only when it is confounded", {
  # `ZMVN()` gives the latent state no temporal structure, so on one
  # series its innovations and the observation residuals are both iid
  # and only their variance sum is identified. Two series break the
  # tie through the cross-series covariance, and a family with no
  # residual scale never had the problem: a Poisson has no `sigma`
  # for the trend to trade against, and a negative binomial's `shape`
  # is a dispersion rather than an additive scale.
  mk <- function(n) {
    data.frame(
      y = rnorm(30 * n), time = rep(seq_len(30), n),
      series = factor(rep(paste0("s", seq_len(n)), each = 30))
    )
  }
  # The build hands the check a validated family, which carries its
  # distributional parameters.
  flagged <- function(tf, n, fam) {
    zmvn_scale_confounded(
      parse_multivariate_trends(y ~ 1, tf), validate_family(fam), mk(n)
    )
  }

  expect_true(flagged(~ ZMVN(), 1L, gaussian()))
  expect_true(flagged(~ ZMVN(), 1L, brms::student()))

  # More than one series identifies the split.
  expect_false(flagged(~ ZMVN(), 3L, gaussian()))
  # No residual scale to trade against.
  expect_false(flagged(~ ZMVN(), 1L, poisson()))
  expect_false(flagged(~ ZMVN(), 1L, brms::negbinomial()))
  # Temporal structure identifies the trend on its own.
  expect_false(flagged(~ AR(p = 1), 1L, gaussian()))
  expect_false(flagged(~ RW(), 1L, gaussian()))
})


test_that("every integer family has a response support to check against", {
  # `mvgam_response_support` is what refuses a negative count or a
  # fractional one, and a family absent from it is validated against
  # nothing: the response passes whatever it holds. The integer
  # families are enumerated once, by `family_uses_integers()`, so the
  # table is checked against that rather than against a second list.
  #
  # Ordinal responses are levels rather than points on an interval,
  # and brms checks them against the category count it derives from
  # the data, so they are excluded here for the reason the table's
  # own comment gives.
  ordinal_like <- c("hurdle_cumulative", "cumulative", "sratio",
                    "cratio", "acat")
  candidates <- setdiff(
    Filter(mvgam:::family_uses_integers, c(
      "poisson", "negbinomial", "negbinomial2", "geometric",
      "binomial", "beta_binomial", "bernoulli",
      "zero_inflated_poisson", "zero_inflated_negbinomial",
      "zero_inflated_binomial", "zero_inflated_beta_binomial",
      "hurdle_poisson", "hurdle_negbinomial", "hurdle_cumulative",
      "discrete_weibull", "com_poisson", "beta_nb", "com_binomial"
    )),
    ordinal_like
  )
  # An empty set would satisfy the comparison without checking one.
  expect_gt(length(candidates), 10L)
  expect_identical(
    setdiff(candidates, names(mvgam:::mvgam_response_support)),
    character(0)
  )
})


test_that("the support table and the integer predicate agree", {
  # Two records of the same fact, so they are compared rather than
  # trusted: a family the table calls integer that the predicate
  # does not is a family whose truncation bounds and whose response
  # check disagree about what its support is.
  tbl <- mvgam:::mvgam_response_support
  disagreed <- Filter(function(nm) {
    !identical(isTRUE(tbl[[nm]]$integer),
               mvgam:::family_uses_integers(nm))
  }, names(tbl))
  expect_identical(disagreed, character(0))
  expect_gt(length(tbl), 30L)
})
