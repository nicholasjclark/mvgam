# The draws accessors, the `variable` keywords and the summary and
# diagnostic methods, on stubs whose `$fit` is a draws array.


# A stub holding draws, data, formula and prior table. Its empty
# `trend_formula` makes the keywords resolve on the observation side.
make_mvgam_stub <- function(varnames = c(
                              "b_Intercept", "b_x", "b_trend[1]",
                              "b_x_trend", "Intercept", "Intercept_trend",
                              "sigma", "phi",
                              "sd_1[1]", "sds_sx_1", "sds_sz_1_trend",
                              "ar1_trend[1]", "sigma_trend[1]",
                              "innovations_trend[1,1]",
                              "trend[1,1]", "Y_pred[1]",
                              "lp__", "lprior"),
                            n_iter = 50L, n_chains = 2L,
                            mv = FALSE) {
  set.seed(1L)
  arr <- array(
    rnorm(n_iter * n_chains * length(varnames)),
    dim = c(n_iter, n_chains, length(varnames)),
    dimnames = list(NULL, NULL, varnames)
  )
  drws <- posterior::as_draws_array(arr)
  formula <- if (mv) {
    brms::bf(brms::mvbind(y1, y2) ~ x)
  } else {
    brms::bf(y ~ x)
  }
  data <- if (mv) {
    data.frame(y1 = rnorm(10), y2 = rnorm(10), x = rnorm(10))
  } else {
    data.frame(y = rnorm(10), x = rnorm(10))
  }
  structure(
    list(
      fit = drws,
      formula = formula,
      trend_formula = NULL,
      data = data,
      prior = data.frame(prior = "(flat)", class = "b"),
      call = call("mvgam", formula = formula)
    ),
    class = "mvgam"
  )
}


# ---- as_draws_*.mvgam runtime paths ----------------------------------

test_that("as.matrix.mvgam(variable = NULL) returns all parameters", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub)
  # A plain matrix, as `brms::as.matrix.brmsfit()` returns. The draws
  # class survives subsetting and arithmetic.
  expect_true(is.matrix(out))
  expect_false(inherits(out, "draws"))
  # The stub declares 18 columns, two of them Stan's own `lp__` and
  # `lprior`. A user-facing matrix carries the other 16.
  expect_equal(ncol(out), 16L)
  # The draws object is still reachable by asking for it.
  expect_s3_class(posterior::as_draws_matrix(stub), "draws_matrix")
})

test_that("as.matrix.mvgam(variable = 'betas') extracts b_* only", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "betas")
  # The columns keep the order of the draws
  expect_identical(colnames(out), c("b_Intercept", "b_x"))
  # The set `brms::fixef()` reports: the unpenalised smooth part is
  # in, the monotonic simplex is not
  smooth_stub <- make_mvgam_stub(c("b_Intercept", "bs_sx_1", "simo_mox1[1]"))
  expect_identical(rownames(fixef(smooth_stub)), c("Intercept", "sx_1"))
})

test_that("as.matrix.mvgam(variable = 'trend_betas') extracts b_trend[", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "trend_betas")
  # Both positional `b_trend[k]` and the brms-aliased
  # `b_<term>_trend` count as trend-side fixed effects.
  expect_identical(colnames(out), c("b_trend[1]", "b_x_trend"))
})

test_that("as.matrix.mvgam(variable = 'obs_params') excludes _trend", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "obs_params")
  expect_identical(colnames(out), c("sigma", "phi"))
})

test_that("as.matrix.mvgam(variable = 'smooth_params') excludes _trend", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "smooth_params")
  expect_equal(colnames(out), "sds_sx_1")
})

test_that("as.matrix.mvgam(variable = 'trend_smooth_params') matches _trend", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "trend_smooth_params")
  expect_equal(colnames(out), "sds_sz_1_trend")
})

test_that("trend_params on trend-formula fit picks _trend block", {
  stub <- make_mvgam_stub()
  stub$trend_formula <- ~AR(p = 1)
  out <- as.matrix(stub, variable = "trend_params")
  expect_true("ar1_trend[1]" %in% colnames(out))
  expect_true("sigma_trend[1]" %in% colnames(out))
  # Regression coefficients on the trend side (positional, brms-
  # aliased, or centred intercept) must NOT leak into the latent-
  # dynamics keyword.
  expect_false("b_trend[1]" %in% colnames(out))
  expect_false("b_x_trend" %in% colnames(out))
  expect_false("Intercept_trend" %in% colnames(out))
  expect_false("innovations_trend[1,1]" %in% colnames(out))
  # The observation family's scale belongs to the other side
  expect_false("sigma" %in% colnames(out))
  expect_false("sds_sz_1_trend" %in% colnames(out))
})

test_that("variable = mix of keyword and regex composes", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = c("betas", "^sd_"), regex = TRUE)
  expect_identical(colnames(out), c("b_Intercept", "b_x", "sd_1[1]"))
})

test_that("variable = explicit name vector works (no regex)", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = c("b_Intercept", "sigma"))
  expect_identical(colnames(out), c("b_Intercept", "sigma"))
  # A bare vector name selects its elements, as posterior does
  out <- as.matrix(stub, variable = "sigma_trend")
  expect_identical(colnames(out), "sigma_trend[1]")
})

test_that("free pattern that matches nothing errors informatively", {
  stub <- make_mvgam_stub()
  expect_error(
    as.matrix(stub, variable = "totally_nonexistent_par"),
    regexp = "No parameters matched"
  )
})

test_that("keyword that resolves to empty (e.g. obs_params on Poisson) is OK", {
  stub <- make_mvgam_stub(varnames = c("b_Intercept", "ar1_trend[1]",
                                        "sigma_trend[1]", "lp__"))
  out <- as.matrix(stub, variable = "obs_params")
  expect_equal(ncol(out), 0L)
})

test_that("as.data.frame.mvgam returns a data.frame", {
  stub <- make_mvgam_stub()
  out <- as.data.frame(stub, variable = "betas")
  expect_s3_class(out, "data.frame")
  expect_true("b_Intercept" %in% names(out))
  # A draw index counts across chains: draw 51 opens the second chain
  full <- as.matrix(stub, variable = "betas")
  expect_equal(unname(as.matrix(stub, variable = "betas", draw = c(1, 51))),
               unname(full[c(1, 51), ]))
  expect_equal(as.data.frame(stub, variable = "betas", draw = 51)$b_x,
               full[51, "b_x"])
  # The stub's draws hold no warmup
  expect_error(as_draws_df(stub, inc_warmup = TRUE), "save_warmup = TRUE")
})

test_that("as.array.mvgam returns a plain array", {
  stub <- make_mvgam_stub()
  out <- as.array(stub, variable = "betas")
  # A plain array, as `brms::as.array.brmsfit()` returns
  expect_true(is.array(out))
  expect_false(inherits(out, "draws"))
  expect_s3_class(posterior::as_draws_array(stub, variable = "betas"),
                  "draws_array")
})

test_that("as_draws_matrix / df / array / list / rvars all dispatch", {
  stub <- make_mvgam_stub()
  expect_s3_class(as_draws_matrix(stub, variable = "betas"),
                  "draws_matrix")
  expect_s3_class(as_draws_df(stub, variable = "betas"), "draws_df")
  expect_s3_class(as_draws_array(stub, variable = "betas"),
                  "draws_array")
  expect_s3_class(as_draws_list(stub, variable = "betas"),
                  "draws_list")
  expect_s3_class(as_draws_rvars(stub, variable = "betas"),
                  "draws_rvars")
})


# ---- bayesplot-ecosystem wrappers ------------------------------------

test_that("coef and fixef summarise the population coefficients", {
  stub <- make_mvgam_stub()
  betas <- unclass(posterior::as_draws_matrix(stub$fit))[
    , c("b_Intercept", "b_x")
  ]
  expect_equal(coef(stub), colMeans(betas))
  draws <- coef(stub, summary = FALSE)
  expect_identical(colnames(draws), colnames(betas))
  expect_equal(as.vector(draws), as.vector(betas))
  fe <- fixef(stub)
  expect_identical(dimnames(fe), list(c("Intercept", "x"),
                   c("Estimate", "Est.Error", "Q2.5", "Q97.5")))
  expect_equal(unname(fe[, "Est.Error"]), unname(apply(betas, 2L, sd)))
  expect_identical(colnames(fixef(stub, summary = FALSE)),
                   c("Intercept", "x"))
})

test_that("fixef.mvgam refuses a 'pars' entry the model does not have", {
  # A misspelt term is refused by name. Dropping it would leave a
  # table with fewer rows than were asked for and no reason given.
  stub <- make_mvgam_stub()
  expect_error(fixef(stub, pars = c("x", "xx")), "'xx'")
  expect_identical(rownames(fixef(stub, pars = "x")), "x")
})

test_that("trend_effects reports the trend formula's coefficients", {
  # `fixef()`, `coef()` and `vcov()` reach the trend formula's terms
  stub <- make_mvgam_stub(c("b_Intercept", "b_x", "b_x_trend", "lp__"))
  expect_error(fixef(stub, trend_effects = TRUE), "'trend_formula'")
  stub$trend_formula <- ~ x
  # The stub's one trend coefficient, against which each accessor's
  # numbers are checked
  draws <- as.vector(posterior::subset_draws(stub$fit, "b_x_trend"))
  fe <- fixef(stub, trend_effects = TRUE)
  expect_identical(rownames(fe), "x_trend")
  expect_equal(unname(fe[, "Estimate"]), mean(draws))
  expect_equal(unname(coef(stub, trend_effects = TRUE)), mean(draws))
  expect_equal(vcov(stub, trend_effects = TRUE),
               matrix(var(draws), 1, 1,
                      dimnames = list("x_trend", "x_trend")))
})

test_that("rhat and neff_ratio follow posterior and brms", {
  stub <- make_mvgam_stub()
  # One entry per parameter, leaving out `lp__` and `lprior`
  expect_length(rhat(stub), 16L)
  sigma <- posterior::extract_variable_matrix(stub$fit, "sigma")
  expect_equal(rhat(stub, pars = c("b_Intercept", "sigma"))[["sigma"]],
               posterior::rhat(sigma))
  # The smaller of the bulk and tail ESS over the number of draws
  expect_equal(
    neff_ratio(stub, pars = "sigma")[["sigma"]],
    min(posterior::ess_bulk(sigma), posterior::ess_tail(sigma)) / 100
  )
})

test_that("compute_all_summaries() columns and measures match the draws", {
  stub <- make_mvgam_stub()
  draws <- posterior::as_draws_df(stub)
  v <- as.numeric(draws[["b_x"]])

  pars <- variables(stub)
  robust_out <- compute_all_summaries(
    stub, probs = c(0.025, 0.975), robust = TRUE, variable = pars
  )
  # The columns `summary()` prints
  expect_equal(colnames(robust_out),
               c("Estimate", "Est.Error", "l-95% CI", "u-95% CI",
                 "Rhat", "Bulk_ESS", "Tail_ESS"))
  expect_true("b_x" %in% rownames(robust_out))

  # Computed from the draws directly. A measure resolving to a
  # different function would change these two numbers.
  expect_equal(robust_out["b_x", "Estimate"], stats::median(v))
  expect_equal(robust_out["b_x", "Est.Error"], stats::mad(v))

  plain_out <- compute_all_summaries(
    stub, probs = c(0.025, 0.975), robust = FALSE, variable = pars
  )
  expect_equal(colnames(plain_out), colnames(robust_out))
  expect_equal(plain_out["b_x", "Estimate"], base::mean(v))
  expect_equal(plain_out["b_x", "Est.Error"], stats::sd(v))

  # The interval columns take their label from `probs`.
  narrow <- compute_all_summaries(
    stub, probs = c(0.1, 0.9), robust = FALSE, variable = pars
  )
  expect_equal(colnames(narrow)[3:4], c("l-80% CI", "u-80% CI"))
})


test_that("posterior_summary.mvgam returns a brms-shaped matrix", {
  stub <- make_mvgam_stub()
  out <- posterior_summary(stub, pars = c("b_Intercept", "sigma"))
  expect_true(is.matrix(out))
  expect_identical(colnames(out), c("Estimate", "Est.Error",
                                    "Q2.5", "Q97.5"))
  # brms names the selection argument 'variable'.
  expect_identical(
    posterior_summary(stub, variable = c("b_Intercept", "sigma")), out
  )
})

test_that("a lost chain is named against the count that was asked for", {
  stub <- make_mvgam_stub(n_chains = 3L)
  expect_equal(realised_chain_count(stub$fit), 3L)

  # Stan hands back only the chains that finished and the request is
  # recorded nowhere on the fit, which is what let a lost chain pass
  # without notice.
  expect_warning(
    check_chains_finished(stub$fit, requested = 4L, algorithm = "sampling"),
    "Fewer chains finished"
  )
  # The notice names both counts, which is what a reader needs to tell
  # a 4-chain request apart from a 3-chain posterior.
  expect_warning(
    check_chains_finished(stub$fit, requested = 4L, algorithm = "sampling"),
    "Asked for 4 chains and 3 finished"
  )
  expect_silent(check_chains_finished(stub$fit, 3L, "sampling"))
  expect_silent(check_chains_finished(stub$fit, 2L, "sampling"))

  # The approximations have no chains. "pathfinder" uses the same
  # argument for its number of paths.
  expect_silent(check_chains_finished(stub$fit, 4L, "pathfinder"))
  expect_silent(check_chains_finished(stub$fit, 4L, "meanfield"))
  expect_silent(check_chains_finished(stub$fit, 4L, "laplace"))

  # Without a count there is no warning
  expect_identical(realised_chain_count(NULL), NA_integer_)
  expect_silent(check_chains_finished(NULL, 4L, "sampling"))
})


test_that("ndraws / nchains / niterations / nvariables work", {
  stub <- make_mvgam_stub(n_iter = 50L, n_chains = 2L)
  expect_equal(ndraws(stub), 100L)
  expect_equal(nchains(stub), 2L)
  expect_equal(niterations(stub), 50L)
  # 18 columns declared, less `lp__` and `lprior`.
  expect_equal(nvariables(stub), 16L)
})

test_that("getCall.mvgam returns the stored call", {
  stub <- make_mvgam_stub()
  expect_identical(getCall(stub), stub$call)
})

test_that("prior_summary.mvgam returns the prior table", {
  stub <- make_mvgam_stub()
  expect_identical(prior_summary(stub), stub$prior)
})

test_that(
  "extract_prior_from_setup merges a partial user prior into the table", {
  # The stored table holds every parameter class of the model. The
  # user row is tagged 'user'. The defaults keep the tag 'default'.
  dat <- data.frame(
    y      = rpois(40, 3),
    x      = rnorm(40),
    series = factor(rep("a", 40)),
    time   = seq_len(40)
  )
  setup <- list(
    formula = y ~ x,
    data    = dat,
    family  = poisson(),
    data2   = NULL,
    prior   = brms::prior(normal(0, 2), class = "b", coef = "x")
  )
  merged <- mvgam:::extract_prior_from_setup(setup)
  expect_s3_class(merged, "brmsprior")
  # The user row should be present and tagged as user-supplied.
  user_row <- merged[
    merged$class == "b" & merged$coef == "x", ,
    drop = FALSE
  ]
  expect_equal(nrow(user_row), 1L)
  expect_equal(user_row$prior, "normal(0, 2)")
  expect_equal(user_row$source, "user")
  # The Intercept default brms emits is kept with the user row
  intercept_row <- merged[
    merged$class == "Intercept", ,
    drop = FALSE
  ]
  expect_gte(nrow(intercept_row), 1L)
  expect_true(any(intercept_row$source == "default"))
})

test_that(
  "extract_prior_from_setup returns the default table verbatim", {
  # NULL user prior -> full default table, with every row tagged
  # default or (vectorized).
  dat <- data.frame(
    y      = rpois(40, 3),
    x      = rnorm(40),
    series = factor(rep("a", 40)),
    time   = seq_len(40)
  )
  setup <- list(
    formula = y ~ x,
    data    = dat,
    family  = poisson(),
    data2   = NULL,
    prior   = NULL
  )
  out <- mvgam:::extract_prior_from_setup(setup)
  expect_s3_class(out, "brmsprior")
  expect_true(all(out$source %in%
                    c("default", "(vectorized)")))
})

# lift_mvgam_stanvar_priors() on hand-written Stan fragments. Each
# holds one prior pattern. The helper appends a row tagged
# source = "mvgam" to an empty brmsprior table.

empty_brmsprior <- function() {
  brms::validate_prior(
    prior   = NULL,
    formula = y ~ 1,
    data    = data.frame(y = rnorm(20)),
    family  = gaussian()
  )[0L, , drop = FALSE]
}

test_that("lift detects Z_free_vec partial-Z loadings prior", {
  sc <- "model { Z_free_vec ~ student_t(3, 0, 1); }"
  out <- mvgam:::lift_mvgam_stanvar_priors(empty_brmsprior(), sc)
  expect_s3_class(out, "brmsprior")
  expect_equal(nrow(out), 1L)
  expect_equal(out$class, "Z_free_vec")
  expect_equal(out$prior, "student_t(3, 0, 1)")
  expect_equal(out$source, "mvgam")
})

test_that("lift strips comments before matching priors and bounds", {
  # A comment in the program can look like a prior or a bounded
  # declaration. Each comment here contradicts the code.
  sc <- paste(
    "parameters {",
    "  // vector<lower=0, upper=5>[N] ar1_trend;",
    "  vector<lower=-1, upper=1>[N] ar1_trend;",
    "}",
    "model {",
    "  /* ar1_trend ~ normal(0, 9); */",
    "  // ar1_trend ~ cauchy(0, 1);",
    "  ar1_trend ~ normal(0, 0.5);",
    "}",
    sep = "\n"
  )
  out <- mvgam:::lift_mvgam_stanvar_priors(empty_brmsprior(), sc)
  expect_equal(nrow(out), 1L)
  expect_equal(out$prior, "normal(0, 0.5)")
  expect_equal(c(out$lb, out$ub), c("-1", "1"))
})

test_that("lift detects a length-scale prior in either spelling", {
  # brms writes `target += dist_lpdf(x | args)`. mvgam writes
  # `x ~ dist(args)`. The scan for `prior_summary()` matches both.
  emitted <- "  theta_features ~ inv_gamma(3, 2);"
  row <- mvgam:::mvgam_stancode_prior_rows(emitted)[[1L]]
  expect_equal(row$class, "theta_features")
  expect_equal(row$prior, "inv_gamma(3, 2)")

  sc <- "  target += lognormal_lpdf(theta_features | 0, 1);"
  out <- mvgam:::lift_mvgam_stanvar_priors(empty_brmsprior(), sc)
  expect_equal(out$class, "theta_features")
  expect_equal(out$prior, "lognormal(0, 1)")
  expect_equal(out$source, "mvgam")
})

test_that("lift detects per-distance theta_dist_<NAME> kernel priors", {
  sc <- paste(
    "target += lognormal_lpdf(theta_dist_phylo | 0, 1);",
    "target += lognormal_lpdf(theta_dist_geo | 0, 1);",
    sep = "\n"
  )
  out <- mvgam:::lift_mvgam_stanvar_priors(empty_brmsprior(), sc)
  expect_equal(nrow(out), 2L)
  expect_identical(out$class, c("theta_dist_phylo", "theta_dist_geo"))
  expect_true(all(out$source == "mvgam"))
})

test_that("lift detects both MGP varrho_inv priors with distinct coefs", {
  sc <- paste(
    "model {",
    "  varrho_inv[1] ~ inv_gamma(mgp_a1, 1);",
    "  if (N_lv_trend > 1) {",
    "    varrho_inv[2:N_lv_trend] ~ inv_gamma(mgp_a2, 1);",
    "  }",
    "}",
    sep = "\n"
  )
  out <- mvgam:::lift_mvgam_stanvar_priors(empty_brmsprior(), sc)
  expect_equal(nrow(out), 2L)
  expect_true(all(out$class == "varrho_inv"))
  expect_identical(out$coef, c("1", "2:N_lv_trend"))
  expect_identical(
    out$prior,
    c("inv_gamma(mgp_a1, 1)", "inv_gamma(mgp_a2, 1)")
  )
})

test_that("lift detects closure-unit Psi exponential prior", {
  sc <- "model {\n  Psi ~ exponential(1);\n}"
  out <- mvgam:::lift_mvgam_stanvar_priors(empty_brmsprior(), sc)
  expect_equal(out$class, "Psi")
  expect_equal(out$prior, "exponential(1)")
})

test_that("lift keeps every latent state out of the prior table", {
  # The latent states have sampling statements of their own. Their
  # names end in `_trend`, as the parameters' do. Every name in
  # `MVGAM_STATE_PARAMS` is excluded.
  sc <- paste(
    "model {",
    paste0("  ", mvgam:::MVGAM_STATE_PARAMS, " ~ normal(0, 1);",
           collapse = "\n"),
    "}",
    sep = "\n"
  )
  out <- mvgam:::lift_mvgam_stanvar_priors(empty_brmsprior(), sc)
  expect_equal(nrow(out), 0L)
})


test_that("lift keeps one row per parameter the table holds", {
  # The trend's brms model writes a user's `b_trend` prior into the
  # program. The stored table already holds that row.
  base <- brms::prior_string("std_normal()", class = "b_trend")
  base$source <- "user"
  sc <- "lprior += std_normal_lpdf(b_trend);"
  expect_identical(nrow(mvgam:::lift_mvgam_stanvar_priors(base, sc)), 1L)
  # A row brms left flat takes the prior the program applies.
  base$prior <- ""
  expect_identical(
    mvgam:::lift_mvgam_stanvar_priors(base, sc)$prior, "std_normal()"
  )
})


test_that("lift returns prior unchanged when stancode has no matches", {
  sc <- "model { Intercept ~ student_t(3, 0, 2.5); }"
  base <- empty_brmsprior()
  out <- mvgam:::lift_mvgam_stanvar_priors(base, sc)
  expect_equal(nrow(out), 0L)
})

test_that("lift returns prior unchanged when stancode is NULL or empty", {
  base <- empty_brmsprior()
  expect_identical(
    mvgam:::lift_mvgam_stanvar_priors(base, NULL), base
  )
  expect_identical(
    mvgam:::lift_mvgam_stanvar_priors(base, ""), base
  )
})

test_that("lift preserves brmsprior columns when appending mvgam rows", {
  base <- brms::validate_prior(
    prior   = brms::prior(normal(0, 2), class = "b", coef = "x"),
    formula = y ~ x,
    data    = data.frame(y = rnorm(20), x = rnorm(20)),
    family  = gaussian()
  )
  sc <- paste(
    "Z_free_vec ~ student_t(3, 0, 1);",
    "Psi ~ exponential(1);",
    sep = "\n"
  )
  out <- mvgam:::lift_mvgam_stanvar_priors(base, sc)
  # Columns of the union are preserved.
  expect_identical(names(out), names(base))
  # User row still there and tagged "user".
  user_row <- out[out$class == "b" & out$coef == "x", , drop = FALSE]
  expect_equal(user_row$source, "user")
  # Two mvgam rows added.
  mvgam_rows <- out[out$source == "mvgam", , drop = FALSE]
  expect_equal(nrow(mvgam_rows), 2L)
  expect_identical(mvgam_rows$class, c("Z_free_vec", "Psi"))
})

test_that("lift rejects non-brmsprior prior input", {
  expect_error(
    mvgam:::lift_mvgam_stanvar_priors(
      data.frame(prior = "x"), "Psi ~ exponential(1);"
    ),
    "brmsprior"
  )
})

test_that("bayes_R2 compares the expectation with the observed rows", {
  stub <- make_mvgam_stub()
  stub$data$y[3L] <- NA
  epred <- matrix(seq(0.1, 4, length.out = 40L), 4L, 10L)
  local_mocked_bindings(posterior_epred.mvgam = function(object, ...) epred)
  r2 <- bayes_R2(stub, summary = FALSE)
  expect_identical(colnames(r2), "R2")
  # Gelman et al. (2019), over the rows with a response
  obs <- -3L
  resid <- sweep(epred[, obs], 2L, stub$data$y[obs])
  want <- apply(epred[, obs], 1L, var) /
    (apply(epred[, obs], 1L, var) + apply(resid, 1L, var))
  expect_equal(r2[, 1L], want)
  expect_identical(rownames(bayes_R2(stub)), "R2")
  expect_error(bayes_R2(make_mvgam_stub(mv = TRUE)),
               "Choose a response with 'resp'")
})

test_that(
  "hidden_par_pattern hides rotation-indeterminate params", {
  # Free-Z factor fit: Z_tilde[ present means raw Z, raw lv_trend,
  # innovations and the latent-factor variance block all get the
  # rotation-indeterminacy hide pattern applied together.
  pars_free <- c(
    "Intercept", "b_x", "shape", "sigma_trend[1]", "sigma_trend[2]",
    "L_Omega_trend[1,1]", "L_Omega_trend[2,1]", "Sigma_trend[1,1]",
    "Z[1,1]", "Z[2,1]", "Z_tilde[1,1]", "Z_tilde[2,1]",
    "lv_trend[1,1]", "lv_trend_tilde[1,1]",
    "innovations_trend[1,1]", "scaled_innovations_trend[1,1]",
    "b_trend[1]", "b_x:.trend1_trend"
  )
  pat <- mvgam:::hidden_par_pattern(pars_free)
  hidden <- pars_free[grepl(pat, pars_free)]
  surviving <- pars_free[!grepl(pat, pars_free)]
  # All raw / rotation-indeterminate params are hidden.
  expect_identical(
    hidden,
    c("sigma_trend[1]", "sigma_trend[2]",
      "L_Omega_trend[1,1]", "L_Omega_trend[2,1]", "Sigma_trend[1,1]",
      "Z[1,1]", "Z[2,1]",
      "lv_trend[1,1]", "innovations_trend[1,1]",
      "scaled_innovations_trend[1,1]", "b_trend[1]")
  )
  # Identified counterparts + obs-side params survive.
  expect_identical(
    surviving,
    c("Intercept", "b_x", "shape",
      "Z_tilde[1,1]", "Z_tilde[2,1]", "lv_trend_tilde[1,1]",
      "b_x:.trend1_trend")
  )
})

test_that(
  "hidden_par_pattern returns NULL for a non-factor fit", {
  # Non-factor fit: no Z_tilde, no lv_trend_tilde, no Phi_trend_tilde.
  # The variance block must survive because it is properly identified
  # in non-factor trend fits.
  pars_nonfactor <- c(
    "Intercept", "b_x", "sigma_trend[1]", "L_Omega_trend[1,1]",
    "Sigma_trend[1,1]"
  )
  pat <- mvgam:::hidden_par_pattern(pars_nonfactor)
  expect_null(pat)
  # filter_hidden_pars returns the input unchanged.
  expect_identical(
    mvgam:::filter_hidden_pars(pars_nonfactor), pars_nonfactor
  )
})

test_that("hidden_par_pattern adds Phi_trend hide on VAR factor fits", {
  pars_var <- c(
    "Z_tilde[1,1]", "Phi_trend[1][1,1]", "Phi_trend_tilde[1][1,1]"
  )
  pat <- mvgam:::hidden_par_pattern(pars_var)
  expect_true(grepl("\\^Phi_trend\\\\\\[", pat))
  expect_true(grepl("Phi_trend\\[1\\]\\[1,1\\]",
                    grep(pat, pars_var, value = TRUE)[1]))
})

test_that("the funnel advisor checks only an MCMC run", {
  # Divergences and R-hat belong to a sampler's run. An object that
  # records no algorithm, or an approximation, carries neither.
  stub <- make_mvgam_stub()
  stub$trend_metadata <- list(
    has_by_lv = TRUE, n_lv_for_grain = 2L,
    axes = list(series = list(n = 2L))
  )
  expect_false(mvgam:::flag_by_lv_full_rank_funnel(stub))
  stub$algorithm <- "meanfield"
  expect_false(mvgam:::flag_by_lv_full_rank_funnel(stub))
})

test_that("compiled_model() requires the backend the fit recorded", {
  # A fit without a recorded backend is refused
  stub <- make_mvgam_stub()
  expect_error(mvgam:::compiled_model(stub), "x\\$backend")
})

