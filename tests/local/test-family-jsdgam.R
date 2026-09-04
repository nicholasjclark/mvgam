# One factor model, seven observation families.
#
# A `jsdgam` puts the same latent structure under every family it
# supports: K species load on N_lv factors through Z, each site
# draws its own factor scores, and the observation family is the
# only thing that changes. So the questions worth asking of such a
# fit are the same seven times over -- is the species axis the one
# the user declared, does each row read the latent cell the sampler
# gave it, do the hindcast arms and the loadings and the residual
# correlation all agree about which species is which -- and only
# the response scale, the recovered nuisance parameter and the
# identification constraint differ.
#
# Those shared questions live in `jsdgam_battery()` and are asked
# once per family. What belongs to one family alone is written out
# below it: Beta's precision, the negative binomial's shape, the
# multivariate normal and Student-t scale and tail, the sum-to-zero
# constraint the softmax families need, and the simplex or trial
# total each composition has to respect.
#
# The simulations are not shared. Each family draws its truth in
# its own order -- the negative binomial takes its intercepts from
# `runif` where the others take theirs from `rnorm`, the
# multinomial draws its per-site totals before the loop, the
# multivariate normal never draws latent scores at all because it
# uses the marginal form -- so one simulator parameterised over
# seven families would put every family on a different draw from
# the one its cached fit was built on. Each is kept verbatim, under
# its own seed, so the caches stay valid and a rerun loads rather
# than samples.
#
# Two claims that only the multivariate-normal file used to make
# are asked of every family here, because both are structural
# rather than family-specific: that a frame maps to trend cells
# with no posterior in hand, and that the rotated loadings are the
# ones the diagnostics report.
#
# Three kinds of assertion are deliberately absent, because each
# passes on the failure it appears to guard. A method is never held
# only to its class, since `plot()` and `pp_check()` return a
# ggplot whether or not anything was drawn on it; `expect_drawn()`
# builds the object and requires a layer with rows. A tidier is
# never held only to `is.data.frame()`, which a frame of no rows
# satisfies. And a summarising method is never held only to its
# shape: `fitted()` and `residuals()` are tied by value to the
# draws they summarise, so a method reaching a different surface
# fails rather than returning the right rectangle of wrong numbers.
#
# Not asserted, and deliberately: `forecast(type = "response")` on
# the three softmax families. A composition drawn one species at a
# time is not a composition, and what such an arm should return is
# a question for the families rather than for the axis.
#
# Fits cache under fixtures/val_mvgam_jsdgam_mv_<family>.rds.
# Delete one to refit it. Full cold run ~30-45 min.
#
# Run with:
#   Rscript -e "devtools::load_all('.'); testthat::test_file('tests/local/test-family-jsdgam.R')"

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(tidyr)
  library(posterior)
  library(testthat)
})


# testthat runs from tests/local/ and Rscript from the package root,
# so the branch asks which of those this is rather than whether a
# cache is already there: on a clean tree the latter picks the root
# path while already inside tests/local.
jsdm_cache <- function(name) {
  dir <- if (dir.exists(file.path("tests", "local"))) {
    file.path("tests", "local", "fixtures")
  } else {
    "fixtures"
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, paste0("val_mvgam_jsdgam_mv_", name, ".rds"))
}


# A ggplot is returned whether or not a layer received any data, so
# asserting the class passes on the empty panel it looks like it is
# guarding. Building the plot is what forces the layers to resolve.
expect_drawn <- function(p) {
  expect_s3_class(p, "ggplot")
  layers <- ggplot2::ggplot_build(p)$data
  expect_gt(sum(vapply(layers, nrow, integer(1L))), 0L)
  invisible(layers)
}


# Fold a wide site-by-species matrix into the long frame a jsdgam
# reads, keyed by site and species. Every simulation below ends
# here, so the frame's shape is stated once.
as_long_jsdm <- function(Y_wide, env, species_levels, time_offset = 0L,
                         integer_response = FALSE) {
  wide <- as.data.frame(Y_wide)
  wide$site <- seq_len(nrow(Y_wide))
  wide$env <- env
  out <- pivot_longer(
    wide, all_of(species_levels),
    names_to = "series", values_to = "y"
  ) |>
    mutate(
      series = factor(series, levels = species_levels),
      time = site + time_offset
    ) |>
    arrange(time, series)
  if (integer_response) out$y <- as.integer(out$y)
  as.data.frame(out)
}


# ---- Simulations -----------------------------------------------------
#
# Each reproduces its own file's draw exactly: same seed, same
# random calls in the same order. Changing any of them invalidates
# that family's cached fit.

sim_nb <- function() {
  set.seed(608L)
  K <- 5L
  N_lv <- 2L
  n_sites <- 60L
  phi_true <- 4
  species_levels <- paste0("sp", seq_len(K))

  Z_true <- matrix(rnorm(K * N_lv, sd = 0.8), nrow = K, ncol = N_lv)
  env <- rnorm(n_sites)
  mu_intercept <- runif(K, 0.4, 1.4)
  mu_env_slope <- rnorm(K, sd = 0.4)
  lv_sim <- matrix(rnorm(n_sites * N_lv), nrow = n_sites, ncol = N_lv)

  Y_wide <- matrix(NA_integer_, nrow = n_sites, ncol = K)
  for (i in seq_len(n_sites)) {
    eta_i <- mu_intercept + mu_env_slope * env[i] +
      as.numeric(Z_true %*% lv_sim[i, ])
    Y_wide[i, ] <- rnbinom(K, mu = exp(eta_i), size = phi_true)
  }
  colnames(Y_wide) <- species_levels

  sigma_true_cov <- tcrossprod(Z_true)
  list(
    K = K, N_lv = N_lv, species_levels = species_levels,
    Z_true = Z_true, phi_true = phi_true, lv_sim = lv_sim,
    sigma_true_cov = sigma_true_cov,
    sigma_true_cor = cov2cor(sigma_true_cov + diag(1e-8, K)),
    mu_intercept = mu_intercept, mu_env_slope = mu_env_slope, env = env,
    long_dat = as_long_jsdm(Y_wide, env, species_levels,
                            integer_response = TRUE)
  )
}


sim_mvt <- function() {
  set.seed(605L)
  K <- 4L
  N_lv <- 2L
  n_sites <- 30L
  nu_true <- 5
  species_levels <- paste0("y", seq_len(K))

  Z_true <- matrix(rnorm(K * N_lv, sd = 0.7), nrow = K, ncol = N_lv)
  psi_true <- rep(0.5, K)
  sigma_true_cov <- tcrossprod(Z_true) +
    diag(psi_true^2 * nu_true / (nu_true - 2))

  env <- rnorm(n_sites)
  mu_intercept <- rnorm(K)
  mu_env_slope <- rnorm(K)

  # Conditional form: each site draws latent scores, then the
  # per-row residual is scalar Student-t at scale psi[k] and
  # shared nu.
  lv_sim <- matrix(rnorm(n_sites * N_lv), nrow = n_sites, ncol = N_lv)
  Y_wide <- matrix(NA_real_, nrow = n_sites, ncol = K)
  for (i in seq_len(n_sites)) {
    mu_i <- mu_intercept + mu_env_slope * env[i] +
      as.numeric(Z_true %*% lv_sim[i, ])
    Y_wide[i, ] <- mu_i + psi_true * rt(K, df = nu_true)
  }
  colnames(Y_wide) <- species_levels

  list(
    K = K, N_lv = N_lv, species_levels = species_levels,
    Z_true = Z_true, psi_true = psi_true, nu_true = nu_true,
    lv_sim = lv_sim, sigma_true_cov = sigma_true_cov,
    sigma_true_cor = cov2cor(sigma_true_cov),
    mu_intercept = mu_intercept, mu_env_slope = mu_env_slope, env = env,
    long_dat = as_long_jsdm(Y_wide, env, species_levels)
  )
}


# The three softmax families share a shape: centre the true
# loadings into the identified subspace the sum_to_zero_vector
# parameterisation explores, then emit from softmax(eta).
centred_loadings <- function(K, N_lv, sd = 0.7) {
  Z <- matrix(rnorm(K * N_lv, sd = sd), nrow = K, ncol = N_lv)
  Z <- scale(Z, center = TRUE, scale = FALSE)
  attr(Z, "scaled:center") <- NULL
  Z
}


sim_diri <- function() {
  set.seed(601L)
  K <- 4L
  N_lv <- 2L
  n_sites <- 30L
  phi_true <- 30
  species_levels <- paste0("y", seq_len(K))

  Z_true <- centred_loadings(K, N_lv)
  env <- rnorm(n_sites)
  mu_intercept <- rnorm(K)
  mu_env_slope <- rnorm(K)

  Y_wide <- matrix(NA_real_, nrow = n_sites, ncol = K)
  for (i in seq_len(n_sites)) {
    lv_i <- rnorm(N_lv)
    eta_i <- mu_intercept + mu_env_slope * env[i] +
      as.numeric(Z_true %*% lv_i)
    p_i <- exp(eta_i) / sum(exp(eta_i))
    gam <- rgamma(K, shape = p_i * phi_true, rate = 1)
    Y_wide[i, ] <- gam / sum(gam)
  }
  colnames(Y_wide) <- species_levels

  sigma_true_cov <- tcrossprod(Z_true)
  list(
    K = K, N_lv = N_lv, species_levels = species_levels,
    Z_true = Z_true, phi_true = phi_true,
    sigma_true_cov = sigma_true_cov,
    sigma_true_cor = cov2cor(sigma_true_cov + diag(1e-8, K)),
    mu_intercept = mu_intercept, mu_env_slope = mu_env_slope, env = env,
    long_dat = as_long_jsdm(Y_wide, env, species_levels)
  )
}


# ---- Family specifications -------------------------------------------
#
# `epred_ok` and `predict_ok` state the response scale each family
# answers on, which is the one thing a shape check cannot see: a
# draw taken on the linear-predictor scale has the right dimensions
# and finite values and is not a proportion, a count or a category.
#
# `optional_methods` names the post-fit methods this family is held
# to beyond the shared set. A method absent from the vector is one
# no existing fixture drove; it is a gap recorded rather than a
# claim made.

SPECS <- list(
  nb = list(
    label = "negative binomial", family = quote(brms::negbinomial()),
    sim = sim_nb,
    threshold_cor = 0.6, mae_max = 0.6, na_response = NA_integer_,
    has_psi = FALSE,
    epred_ok = function(x) all(x > 0),
    predict_ok = function(x) all(x >= 0) && all(x == floor(x)),
    fc_types = c("link", "expected", "trend", "response"),
    fc_response_ok = function(x) all(x >= 0) && all(x == floor(x)),
    pp_check_extra = "rootogram",
    plot_types = c("residuals", "trend", "factors"),
    optional_methods = c("linpred", "residuals", "mcmc_plot",
                         "marginaleffects"),
    ce_response_ok = NULL,
    # A count family's expectation is continuous even though its
    # draws are whole, so an all-integer estimate is a draw being
    # reported as the mean and a tolerance alone would hide it.
    me_integer_tell = TRUE
  ),
  mvt = list(
    label = "multivariate Student-t", family = quote(mvt()), sim = sim_mvt,
    identity_link = TRUE,
    threshold_cor = 0.7, mae_max = 0.5, na_response = NA_real_,
    has_psi = TRUE,
    epred_ok = NULL, predict_ok = NULL,
    fc_types = c("link", "expected", "trend"),
    fc_response_ok = NULL,
    pp_check_extra = "resid_qq",
    plot_types = c("residuals", "trend", "factors"),
    optional_methods = c("linpred", "residuals", "mcmc_plot",
                         "marginaleffects",
                         "forecast_response_agrees"),
    ce_response_ok = NULL,
    me_integer_tell = FALSE
  ),
  diri = list(
    label = "Dirichlet", family = quote(diri()), sim = sim_diri,
    threshold_cor = 0.7, mae_max = 0.5, na_response = NA_real_,
    has_psi = FALSE,
    epred_ok = function(x) all(x >= 0 & x <= 1),
    predict_ok = function(x) all(x >= 0 & x <= 1),
    fc_types = c("link", "expected", "trend"),
    fc_response_ok = NULL,
    pp_check_extra = NULL,
    plot_types = c("trend", "factors"),
    optional_methods = character(0),
    ce_response_ok = NULL,
    me_integer_tell = FALSE
  )
)


# ---- Fit ------------------------------------------------------------

fit_jsdm <- function(nm, spec, sim) {
  cache <- jsdm_cache(nm)
  if (file.exists(cache)) {
    fit <- readRDS(cache)
  } else {
    fit <- jsdgam(
      formula = y ~ env * series,
      factor_formula = ~ -1,
      data = sim$long_dat,
      unit = time, species = series,
      family = eval(spec$family),
      n_lv = sim$N_lv,
      chains = 2L,
      iter = 1000L, warmup = 500L,
      silent = 2,
      backend = "cmdstanr"
    )
  }
  truth <- sim[setdiff(names(sim), "long_dat")]
  if (!identical(attr(fit, "sim_truth"), truth)) {
    attr(fit, "sim_truth") <- truth
    saveRDS(fit, cache)
  }
  fit
}


# ---- The shared battery ----------------------------------------------

jsdgam_battery <- function(nm, spec, sim, fit) {
  d <- sim$long_dat
  K <- sim$K
  N_lv <- sim$N_lv
  lev <- sim$species_levels
  n_obs <- nrow(d)
  says <- function(claim) paste0(nm, ": ", claim)

  dm <- as_draws_matrix(fit$fit)
  post_cor <- residual_cor(fit)$cor
  true_off <- sim$sigma_true_cor[upper.tri(sim$sigma_true_cor)]
  post_off <- post_cor[upper.tri(post_cor)]
  cor_off <- stats::cor(true_off, post_off)
  mae_off <- mean(abs(true_off - post_off))
  Z_mean <- apply(
    mvgam:::extract_Z_loadings(dm, n_obs_series = K, n_lv = N_lv),
    c(2L, 3L), mean
  )

  test_that(says("the residual correlation recovers the simulated one"), {
    # The off-diagonals are compared as a set, so this number is
    # reached just as well by a fit that hands every species another
    # species' latent column. The structural claims below are what
    # separate the two cases.
    expect_gt(cor_off, spec$threshold_cor)
    expect_lt(mae_off, spec$mae_max)
  })

  test_that(says("the reported correlation is the one the loadings imply"), {
    # `residual_cor()` and the loadings are two accounts of the same
    # quantity reached by different code, so they have to agree. The
    # sharper claim is the second: permuting the loadings' rows has
    # to make the agreement worse. If a permuted Z matches the
    # reported matrix as well as the true ordering does, then neither
    # surface carries any information about which species is which,
    # and the recovery number above is measuring nothing.
    implied_cov <- tcrossprod(Z_mean)
    if (isTRUE(spec$has_psi)) {
      psi_cols <- grep("^Psi\\[", colnames(dm), value = TRUE)
      implied_cov <- implied_cov +
        diag(colMeans(dm[, psi_cols, drop = FALSE])^2)
    }
    agreement <- function(M) {
      cc <- cov2cor(M + diag(1e-8, K))
      stats::cor(cc[upper.tri(cc)], post_off)
    }
    base <- agreement(implied_cov)
    # The floor is the family's own recovery threshold rather than a
    # flat number: the gap between the mean of the correlation and
    # the correlation of the mean loadings widens as the posterior
    # does, so a single-trial categorical agrees less than a
    # multinomial for the same reason it recovers less. The claim
    # that carries the weight is the permutation one below, which
    # holds for every family whatever the level of agreement.
    expect_gt(base, spec$threshold_cor)
    for (perm in list(c(2:K, 1L), rev(seq_len(K)))) {
      shuffled <- tcrossprod(Z_mean[perm, , drop = FALSE])
      if (isTRUE(spec$has_psi)) {
        shuffled <- shuffled + diag(diag(implied_cov - tcrossprod(Z_mean)))
      }
      expect_gt(base, agreement(shuffled))
    }
  })

  test_that(says("the species axis is the one the frame declared"), {
    axes <- mvgam:::mvgam_axes(fit)
    expect_identical(as.character(axes$series$levels), lev)
    expect_identical(as.integer(axes$series$n), K)
    expect_identical(as.integer(fit$standata$N_lv_trend), N_lv)
    expect_identical(as.integer(fit$standata$N_series_trend), K)
  })

  test_that(says("every prediction surface answers for every row"), {
    ep <- posterior_epred(fit, draw_ids = 1:20)
    pp <- posterior_predict(fit, draw_ids = 1:20)
    expect_identical(dim(ep), c(20L, n_obs))
    expect_identical(dim(pp), c(20L, n_obs))
    expect_true(all(is.finite(ep)))
    expect_true(all(is.finite(pp)))
    if (!is.null(spec$epred_ok)) expect_true(spec$epred_ok(ep))
    if (!is.null(spec$predict_ok)) expect_true(spec$predict_ok(pp))

    # `fitted()` summarises the draws `posterior_epred()` returns, so
    # its Estimate column is their column mean exactly. Asserting
    # only its row count passes a method that reached a different
    # surface and came back with the right rectangle of wrong
    # numbers.
    ft <- fitted(fit, draw_ids = 1:20)
    expect_identical(nrow(ft), n_obs)
    expect_equal(unname(ft[, "Estimate"]), unname(colMeans(ep)),
                 tolerance = 1e-8)
    expect_true(all(ft[, "Q2.5"] <= ft[, "Estimate"]))
    expect_true(all(ft[, "Estimate"] <= ft[, "Q97.5"]))

    pd <- predict(fit, draw_ids = 1:20)
    expect_identical(nrow(pd), n_obs)

    if ("linpred" %in% spec$optional_methods) {
      lp <- posterior_linpred(fit, draw_ids = 1:20)
      expect_identical(dim(lp), c(20L, n_obs))
      expect_true(all(is.finite(lp)))
      # Under a non-identity link the two scales are different
      # quantities, so a linpred that echoed the expectation would
      # pass every shape check and fail here. `mvn` and `mvt` carry
      # the identity link, where the two agreeing is the contract
      # rather than a fault, so they are asked for that instead.
      if (isTRUE(spec$identity_link)) {
        expect_equal(unname(lp), unname(ep))
      } else {
        expect_false(isTRUE(all.equal(unname(lp), unname(ep))))
      }
    }
    if ("residuals" %in% spec$optional_methods) {
      rs <- residuals(fit, draw_ids = 1:20)
      expect_identical(nrow(rs), n_obs)
      expect_true(all(is.finite(rs[, "Estimate"])))
      expect_true(all(rs[, "Q2.5"] <= rs[, "Estimate"]))
      expect_true(all(rs[, "Estimate"] <= rs[, "Q97.5"]))
      # The roxygen promises the matrix returned for each draw
      # "carries the full posterior uncertainty in the residual
      # distribution". A column with no spread is a residual that did
      # not move with the draw it came from, and the interval check
      # above is satisfied by a constant.
      expect_gt(min(rs[, "Est.Error"]), 0)
    }

    # Column j of every surface above is row j of the frame, so the
    # species the fit resolves for each row has to be the species the
    # frame states there. A permutation keeps every dimension, leaves
    # every value finite and uses every species exactly once.
    os <- mvgam:::get_observation_structure(fit, newdata = d)
    expect_identical(as.character(os$series), as.character(d$series))
    expect_identical(os$series_levels, lev)
    expect_identical(as.integer(os$series_int),
                     match(as.character(d$series), lev))
    # And the time index runs in the frame's own order, not the order
    # the rows happened to be assembled in.
    expect_identical(as.integer(os$time),
                     match(d$time, sort(unique(d$time))))
  })

  test_that(says("the axis maps a frame without touching the draws"), {
    # Which species a row belongs to, and which occasions a frame
    # supplies, are settled from the axis record alone. Nothing here
    # reads a posterior, so a failure is structural rather than a
    # sampling one. The forecast grid is included because that is
    # where the raw-column read lived.
    ids <- mvgam:::axis_row_series(fit, d)
    expect_identical(levels(ids), lev)
    expect_identical(as.character(ids), as.character(d$series))

    # Answered row by row, not by position.
    set.seed(31L)
    perm <- sample(nrow(d))
    expect_identical(
      as.character(mvgam:::axis_row_series(fit, d[perm, , drop = FALSE])),
      as.character(d$series)[perm]
    )

    # A species the model never had is refused, and named.
    bad <- d
    bad$series <- as.character(bad$series)
    bad$series[bad$series == lev[2L]] <- "y_ghost"
    err <- expect_error(
      mvgam:::validate_prediction_factor_levels(bad, fit$trend_metadata)
    )
    expect_match(conditionMessage(err), "y_ghost", fixed = TRUE)

    # Each training arm carries the user's own time values, in order,
    # and that species' own observations.
    training <- mvgam:::build_training_arms(fit, lev)
    expect_identical(names(training$times), lev)
    user_times <- sort(unique(as.integer(d$time)))
    for (s in lev) {
      expect_identical(as.integer(training$times[[s]]), user_times)
      expect_equal(training$observations[[s]],
                   d$y[as.character(d$series) == s])
    }

    # A frame past the training grid yields exactly the new
    # occasions, per species, in the user's numbering; one inside it
    # yields none.
    future_times <- max(user_times) + seq_len(4L)
    future <- expand.grid(
      time = future_times,
      series = factor(lev, levels = lev),
      stringsAsFactors = FALSE
    )
    future$env <- 0
    grid <- mvgam:::resolve_forecast_grid(fit, future, training, lev)
    expect_false(is.null(grid))
    expect_identical(names(grid$times), lev)
    for (s in lev) {
      expect_identical(as.integer(grid$times[[s]]), future_times)
    }
    expect_null(mvgam:::resolve_forecast_grid(fit, d, training, lev))
  })

  test_that(says("each row reads the latent cell the sampler drew"), {
    # The one comparison here that does not ask two derivations
    # whether they agree with each other. `obs_trend_time` and
    # `obs_trend_series` are the cell this fit gave each row and
    # `trend[t, s]` is what it sampled there, so a species reading
    # another's column returns a real state of the right shape and
    # only a value comparison sees it.
    t_rec <- as.integer(fit$standata$obs_trend_time)
    s_rec <- as.integer(fit$standata$obs_trend_series)
    expect_length(t_rec, n_obs)
    want <- vapply(paste0("trend[", t_rec, ",", s_rec, "]"),
                   function(k) mean(dm[, k]), numeric(1))
    got <- colMeans(
      mvgam:::extract_trend_latent_states(fit, newdata = d,
                                          full_draws = dm)
    )
    expect_equal(unname(got), unname(want))
    # And the states differ across cells, so a route returning one
    # constant for every row cannot satisfy the comparison above by
    # matching a constant `want`.
    expect_gt(stats::sd(want), 1e-6)
  })

  test_that(says("a shuffled frame answers the same, in the new order"), {
    # Every other check here hands back the training frame in the
    # order it was built. A prediction that places rows by position
    # rather than by content agrees with all of them, and disagrees
    # here.
    set.seed(13L)
    perm <- sample(nrow(d))
    base <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                            incl_autocor = TRUE)
    shuf <- posterior_epred(fit, newdata = d[perm, , drop = FALSE],
                            draw_ids = 1:10, incl_autocor = TRUE)
    expect_equal(unname(base[, perm, drop = FALSE]), unname(shuf))
    # A frame whose columns were all equal would satisfy that under
    # any permutation, so the values have to move across rows.
    expect_gt(stats::sd(colMeans(base)), 1e-8)
  })

  test_that(says("a subset of the frame reads the same cells"), {
    # The shape every per-series arm passes down, and the shape that
    # exposed the defect this battery guards: the species index was
    # taken from the levels the frame carried rather than the levels
    # the fit was built on, so a subset numbered its species 1
    # whatever it was and read the first species' latent column.
    #
    # The subset is whole sites rather than one species, which the
    # single-species form was until measurement showed it is not a
    # question the composition families can answer. Dirichlet,
    # multinomial and categorical share a softmax normaliser across
    # the species at a site, so a frame carrying one of them has a
    # different denominator and must give a different number; asking
    # them to agree is asking them to ignore the constraint that
    # makes them compositions. Whole sites keep the normaliser
    # intact, so the claim holds for all seven families and still
    # fails on a prediction placed by position.
    full <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                            incl_autocor = TRUE)
    ts <- sort(unique(d$time))
    rows <- which(d$time %in% ts[seq(1L, length(ts), by = 2L)])
    sub <- d[rows, , drop = FALSE]
    expect_setequal(as.character(unique(sub$series)), lev)
    got <- posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                           incl_autocor = TRUE)
    expect_equal(unname(got), unname(full[, rows, drop = FALSE]))
    # The species have to differ, or reading the wrong one would
    # satisfy every comparison above.
    per_species <- vapply(lev, function(s) {
      mean(full[, as.character(d$series) == s])
    }, numeric(1))
    expect_equal(length(unique(round(per_species, 10L))), length(lev))
  })

  test_that(says("levels declared in another order still map right"), {
    # The mapping is by label, so re-declaring the same species in a
    # different order must not move the answers. Taking the axis from
    # the frame's own `levels()` instead makes them move.
    base <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                            incl_autocor = TRUE)
    nd <- d
    nd$series <- factor(as.character(nd$series), levels = rev(lev))
    got <- posterior_epred(fit, newdata = nd, draw_ids = 1:10,
                           incl_autocor = TRUE)
    expect_equal(unname(got), unname(base))
  })

  test_that(says("a frame naming an unknown species is refused"), {
    nd <- d
    nd$series <- factor(
      ifelse(seq_len(nrow(nd)) == 1L, "y_unseen", as.character(nd$series)),
      levels = c(lev, "y_unseen")
    )
    err <- expect_error(
      posterior_epred(fit, newdata = nd, draw_ids = 1:5),
      "Series levels in newdata not found in training data"
    )
    # A refusal that does not name the offending level, or list the
    # ones that would have worked, leaves the user to find which of
    # their species the model has never seen.
    expect_match(conditionMessage(err), "y_unseen", fixed = TRUE)
    for (s in lev) {
      expect_match(conditionMessage(err), s, fixed = TRUE)
    }
  })

  test_that(says("hindcast arms are the species, in order, and distinct"), {
    arms <- hindcast(fit, ndraws = 20L)$hindcasts
    expect_identical(names(arms), lev)
    expect_true(all(vapply(arms, function(a) NROW(a) > 0L, logical(1))))
    # Every pair. Checking only the opening pair passes a fit that
    # gave the last two species one latent column.
    same <- character(0)
    for (i in seq_along(arms)) {
      for (j in seq_along(arms)) {
        if (j <= i) next
        if (isTRUE(all.equal(arms[[i]], arms[[j]]))) {
          same <- c(same, paste(names(arms)[i], names(arms)[j], sep = "="))
        }
      }
    }
    expect_identical(same, character(0))
    # Each arm holds that species' own occasions, so a hindcast cut
    # by the wrong axis has the wrong width.
    n_times <- length(unique(d$time))
    for (s in lev) expect_identical(ncol(arms[[s]]), n_times)
  })

  test_that(says("forecast is keyed by the species axis"), {
    # `unit = time` makes the sites this fit's grid, so extending it
    # is structurally a forecast even though a further site is not a
    # later occasion. The keying and the width are what a wrongly
    # resolved axis gets wrong.
    h <- 4L
    nd <- expand.grid(
      time = max(d$time) + seq_len(h),
      series = factor(lev, levels = lev),
      stringsAsFactors = FALSE
    )
    nd$env <- 0
    nd$y <- spec$na_response
    for (ty in spec$fc_types) {
      fc <- forecast(fit, newdata = nd, ndraws = 20L, type = ty)
      expect_s3_class(fc, "mvgam_forecast")
      expect_identical(names(fc$forecasts), lev)
      for (s in lev) {
        expect_identical(dim(fc$forecasts[[s]]), c(20L, h))
        expect_true(all(is.finite(fc$forecasts[[s]])))
      }
      # The species have to be told apart on every scale. An arm
      # list of the right names holding one shared matrix passes
      # every dimension check above.
      arm_means <- vapply(fc$forecasts, mean, numeric(1))
      expect_equal(length(unique(round(arm_means, 10L))), length(lev))
    }
    # `type = "expected"` is the family's mean, so it lands on the
    # scale `posterior_epred()` occupies over the training grid: the
    # two are one quantity, reached over the grid and over its
    # extension. A forecast arm outside that scale is the inverse
    # link having been skipped, and `is.finite()` cannot see it.
    if (!is.null(spec$epred_ok)) {
      fc_exp <- do.call(cbind, forecast(
        fit, newdata = nd, ndraws = 20L, type = "expected"
      )$forecasts)
      expect_true(spec$epred_ok(fc_exp))
    }
    if (!is.null(spec$fc_response_ok)) {
      # A drawn value has to be on the family's own scale; the
      # expectation under another name would pass every check above.
      drawn <- do.call(cbind, forecast(
        fit, newdata = nd, ndraws = 20L, type = "response"
      )$forecasts)
      expect_true(spec$fc_response_ok(drawn))
    }
    if ("forecast_response_agrees" %in% spec$optional_methods) {
      # `hindcast(type = "response")` answers for this family and
      # `forecast(type = "response")` refuses it. They are the same
      # quantity reached over the training grid and over its
      # extension, so one of the two is wrong: either the family can
      # be drawn from and the forecast arm should not refuse, or it
      # cannot and the hindcast arm is calling something a response
      # draw that is not one. This asserts they agree rather than
      # picking a side.
      expect_s3_class(hindcast(fit, ndraws = 20L, type = "response"),
                      "mvgam_forecast")
      fc_resp <- forecast(fit, newdata = nd, ndraws = 20L,
                          type = "response")
      expect_identical(names(fc_resp$forecasts), lev)
    }
  })

  test_that(says("residual_cor is labelled by the species axis"), {
    # The matrix the recovery number above is read off. A correct
    # matrix under the wrong labels reads, to anyone using it,
    # exactly like a wrong matrix, and the correlation is identical
    # either way.
    expect_identical(rownames(post_cor), lev)
    expect_identical(colnames(post_cor), lev)
    expect_equal(unname(diag(post_cor)), rep(1, K))
    # A correlation matrix is symmetric; an asymmetry means the two
    # indices were resolved by different routes.
    expect_equal(unname(post_cor), unname(t(post_cor)))
    expect_true(all(post_off >= -1 & post_off <= 1))
    # An identity matrix satisfies every line above and says the
    # species share nothing, which is not the model that was fitted.
    expect_gt(max(abs(post_off)), 0.05)
  })

  test_that(says("the factor methods report the fit's own loadings"), {
    # These are the methods a jsdgam exists for, and each reads the
    # loadings against the species axis. A wrong axis gives every one
    # of them a real answer about the wrong species.
    af <- active_factors(fit)
    expect_s3_class(af, "mvgam_active_factors")
    expect_identical(as.integer(af$n_lv), N_lv)
    # One row per latent factor, not per species: reading this at the
    # series grain is how a factor method comes to report K factors
    # for an N_lv-factor fit.
    expect_identical(nrow(af$per_factor), N_lv)

    sv <- shared_variation(fit)
    expect_s3_class(sv, "mvgam_shared_variation")
    # The names this is read by, in the order the loadings run. A
    # correct decomposition under permuted names attributes each
    # species' shared variance to another species.
    expect_identical(as.character(sv$series_names), lev)
    expect_identical(as.integer(sv$n_series), K)
    expect_identical(as.integer(sv$n_lv), N_lv)
    # Delta is a covariance, so it is symmetric with positive
    # variances on its diagonal. A frame of finite numbers is not.
    expect_identical(dim(sv$delta), c(K, K))
    expect_equal(unname(sv$delta), unname(t(sv$delta)))
    expect_true(all(diag(sv$delta) > 0))

    # The biplot puts one point per species; a plot drawn off a
    # collapsed axis carries fewer.
    ord_layers <- expect_drawn(ordinate(fit))
    expect_true(any(vapply(ord_layers, function(x) nrow(x) == K,
                           logical(1))))

    # Both fits have to reach the panel. The title names them and the
    # point layer carries one loading per species per fit, so a
    # comparison that silently dropped one is a plot of the right
    # shape holding half the data.
    cl <- compare_loadings(fit, fit, labels = c("first", "second"))
    pts <- expect_drawn(cl)
    expect_match(cl$labels$title, "first", fixed = TRUE)
    expect_match(cl$labels$title, "second", fixed = TRUE)
    expect_true(any(vapply(pts, function(x) nrow(x) == 2L * K,
                           logical(1))))
    # Facetting is a different layout rather than the same plot
    # returned twice, which is what `facet` is asked for.
    faceted <- compare_loadings(fit, fit,
                                labels = c("first", "second"),
                                facet = TRUE)
    expect_drawn(faceted)
    expect_false(identical(class(faceted$facet)[1L],
                           class(cl$facet)[1L]))

    expect_identical(dim(Z_mean), c(K, N_lv))
    # No two species share a loading vector, which is what a
    # collapsed axis produces while leaving every dimension correct.
    same <- character(0)
    for (i in seq_len(K)) {
      for (j in seq_len(K)) {
        if (j <= i) next
        if (isTRUE(all.equal(Z_mean[i, ], Z_mean[j, ]))) {
          same <- c(same, paste(lev[i], lev[j], sep = "="))
        }
      }
    }
    expect_identical(same, character(0))
    # And the loadings are not all near zero, which would make the
    # distinctness above hold on numerical noise alone.
    expect_gt(max(abs(Z_mean)), 0.1)
  })

  test_that(says("the rotated loadings are what the diagnostics report"), {
    # A factor model's raw `Z` is rotation-indeterminate, so its
    # draws describe nothing a user should read: any rotation of the
    # loadings with the matching counter-rotation of the factors
    # gives the same likelihood, and the chains wander between them.
    # Once `Z_tilde` is in the posterior it is the identified
    # surface, and the diagnostic entry points hide the raw block
    # rather than reporting an r-hat on a quantity with no fixed
    # value.
    v <- variables(fit)
    expect_true(any(grepl("^Z_tilde\\[", v)))
    expect_false(any(grepl("^Z\\[", v)))
    # The trend scale and correlation factors are absorbed into the
    # loadings here, so they are hidden on the same grounds.
    expect_false(any(grepl("^sigma_trend\\[", v)))
    expect_false(any(grepl("^L_Omega_trend\\[", v)))

    # Every entry point agrees, not just `variables()`. An r-hat
    # computed on the unrotated block is the misleading number this
    # prevents.
    expect_false(any(grepl("^Z\\[", rownames(posterior_summary(fit)))))
    expect_false(any(grepl("^Z\\[", names(rhat(fit)))))

    # The hiding is a default, not a removal: naming the block is the
    # documented way to reach the raw draws, and both halves of that
    # contract are asserted because either one alone passes on the
    # other's failure. A selection that returned only `Z_tilde` would
    # satisfy "some Z came back" while the escape hatch was gone.
    # The softmax families carry a third block under the same
    # prefix, so the two named ones are counted rather than the
    # total.
    cols <- posterior::variables(
      as_draws_array(fit, variable = "Z", regex = TRUE)
    )
    expect_identical(sum(grepl("^Z\\[", cols)), K * N_lv)
    expect_identical(sum(grepl("^Z_tilde\\[", cols)), K * N_lv)
  })

  test_that(says("summary and the criticism methods run on this fit"), {
    txt <- capture.output(summary(fit))
    # The series count and the species names are what a reader checks
    # the axis against.
    expect_true(any(grepl(paste0("Series:\\s*", K), txt)))
    expect_true(any(grepl("env", txt, fixed = TRUE)))

    ll <- log_lik(fit, draw_ids = 1:20)
    expect_identical(dim(ll), c(20L, n_obs))
    expect_true(all(is.finite(ll)))
    # A density that came back constant across rows is not this
    # model's, whatever its shape.
    expect_gt(stats::sd(colMeans(ll)), 1e-8)

    # `loo()` warns when a Pareto-k crosses its threshold, which is
    # the one diagnostic saying whether the approximation can be
    # trusted. Capturing it rather than suppressing it keeps the
    # signal: an unrelated notice fails instead of passing unseen.
    seen <- character(0)
    ic <- withCallingHandlers(loo(fit), warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    expect_s3_class(ic, "loo")
    expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
    # The diagnostics have to be there and cover every observation
    # the density answered for, or the estimate above is summarising
    # a different set of points.
    expect_identical(length(ic$diagnostics$pareto_k), ncol(ll))
    expect_true(all(is.finite(ic$diagnostics$pareto_k)))
    expect_true(all(grepl("Pareto", seen)))
  })

  test_that(says("pp_check, plotting and conditional_effects render"), {
    expect_drawn(pp_check(fit, ndraws = 20L))
    if (!is.null(spec$pp_check_extra)) {
      expect_drawn(pp_check(fit, type = spec$pp_check_extra,
                            ndraws = 20L))
    }
    for (ty in spec$plot_types) {
      expect_drawn(plot(fit, type = ty))
    }
    if ("mcmc_plot" %in% spec$optional_methods) {
      expect_drawn(mcmc_plot(fit))
    }

    ce <- conditional_effects(fit)
    expect_s3_class(ce, "mvgam_conditional_effects")
    # The observation formula is `y ~ env * series`, so the covariate
    # has to be offered. A default list that lost it draws a correct
    # panel of the wrong thing.
    env_panels <- grep("env", names(ce), value = TRUE)
    expect_gt(length(env_panels), 0L)
    for (eff in names(ce)) {
      cd <- ce[[eff]]$data
      expect_true(all(c("estimate", "conf.low", "conf.high") %in%
                        names(cd)))
      expect_true(all(is.finite(cd$estimate)))
      # An interval drawn the wrong way round renders as a ribbon of
      # the right shape around the right line.
      expect_true(all(cd$conf.low <= cd$estimate))
      expect_true(all(cd$estimate <= cd$conf.high))
      # A zero-width interval means the draws were collapsed before
      # the panel was built.
      expect_gt(max(cd$conf.high - cd$conf.low), 0)
      # Where a panel is cut by species it is cut by the model's axis.
      if ("series" %in% names(cd)) {
        expect_true(all(as.character(unique(cd$series)) %in% lev))
      }
      if (!is.null(spec$ce_response_ok)) {
        expect_true(spec$ce_response_ok(cd$estimate))
      }
    }
    # A covariate panel whose estimate does not move is a predictor
    # that reached the plot without reaching the linear predictor.
    for (eff in env_panels) {
      expect_gt(stats::sd(ce[[eff]]$data$estimate), 1e-8)
    }
  })

  test_that(says("every time-axis panel draws the occasions supplied"), {
    # The trend panel, the series panel and a hindcast all label
    # their x axis `Time` and mean the occasions the user gave. The
    # factor panel draws the occasion's rank under the same label. On
    # a grid numbered from one the two coincide and nothing shows,
    # which is why the multivariate-normal frame here is numbered
    # from three: a reader comparing a factor trajectory against a
    # series trajectory is otherwise off by the offset between them.
    ut <- range(as.numeric(d$time))
    panel_x <- function(p) {
      xs <- unlist(lapply(
        ggplot2::ggplot_build(p)$data,
        function(l) if ("x" %in% names(l)) l$x else NULL
      ))
      range(xs[is.finite(xs)])
    }
    for (ty in intersect(spec$plot_types, c("trend", "factors"))) {
      expect_equal(panel_x(plot(fit, type = ty)), ut)
    }
    expect_equal(panel_x(plot(hindcast(fit, ndraws = 20L), series = 1L)),
                 ut)
  })

  test_that(says("the draws and the tidiers keep this fit's row order"), {
    v <- variables(fit)
    expect_true(any(grepl("^Z(_tilde)?\\[", v)))

    td <- tidy(fit)
    expect_true(is.data.frame(td))
    expect_gt(nrow(td), 0L)
    expect_true(all(c("term", "type", "estimate") %in% names(td)))
    expect_true(all(is.finite(td$estimate)))
    # `effects = "all"` is documented to return every parameter, and
    # the loadings are what a jsdgam is fitted to estimate, so a
    # table without them is not a tidy summary of this fit.
    expect_true(any(grepl("^Z_tilde", td$term)))
    # The same table must not report what the diagnostics hide.
    # `variables()`, `posterior_summary()` and `rhat()` all drop
    # `L_Omega_trend` on a factor fit because it is absorbed into the
    # loadings and has no fixed value under rotation; a tidier that
    # reports it hands the user a number the package refuses to put
    # an r-hat on.
    expect_true(all(td$term %in% v))

    gl <- glance(fit)
    expect_true(is.data.frame(gl))
    expect_identical(nrow(gl), 1L)

    aug <- augment(fit)
    expect_true(is.data.frame(aug))
    expect_identical(nrow(aug), n_obs)
    # Row for row, in the frame's own order. A tidier that re-sorts
    # its output pairs each fitted value with another row's
    # observation while every column keeps the right length.
    expect_identical(as.character(aug$series), as.character(d$series))
    expect_equal(as.numeric(aug$time), as.numeric(d$time))
    expect_equal(as.numeric(aug$.observed), as.numeric(d$y))
  })

  if ("marginaleffects" %in% spec$optional_methods) {
    test_that(says("marginaleffects reaches this fit and separates species"), {
      withr::local_options(marginaleffects_model_classes = "mvgam")
      grid <- expand.grid(
        env = c(-1, 1), series = factor(lev, levels = lev),
        stringsAsFactors = FALSE
      )
      grid$time <- min(d$time)
      grid$y <- spec$na_response
      # mvgam parts company with brms and marginaleffects on what
      # `type` means, and does so deliberately: `"expected"` is the
      # family's mean and `"response"` samples the family, so the
      # latter reports a predictive median. Each type is pinned to
      # its own quantity, because comparing one against the other's
      # is what made a whole number read as a defect once already.
      pr_e <- marginaleffects::predictions(fit, newdata = grid,
                                           type = "expected")
      expect_identical(nrow(pr_e), nrow(grid))
      expect_true(all(is.finite(pr_e$estimate)))
      if (!is.null(spec$epred_ok)) {
        expect_true(spec$epred_ok(pr_e$estimate))
      }
      # The expectation reached through marginaleffects is the one
      # `posterior_epred()` returns. Both average the whole
      # posterior, so the tolerance is about the two routes agreeing
      # rather than about how many draws each took.
      ep_grid <- colMeans(posterior_epred(fit, newdata = grid,
                                          ndraws = NULL))
      expect_equal(as.numeric(pr_e$estimate), as.numeric(ep_grid),
                   tolerance = 0.02)

      pr_r <- marginaleffects::predictions(fit, newdata = grid,
                                           type = "response")
      expect_identical(nrow(pr_r), nrow(grid))
      if (isTRUE(spec$me_integer_tell)) {
        # A count family sampled rather than averaged returns whole
        # numbers, which is what separates the two types here.
        expect_true(all(pr_r$estimate == floor(pr_r$estimate)))
        expect_false(all(pr_e$estimate == floor(pr_e$estimate)))
      }
      # `env * series` gives each species its own slope; a design
      # that dropped the interaction returns one slope shared by all.
      # A threshold near machine zero would pass on numerical noise,
      # so the claim is that the species differ by an amount
      # comparable to the slopes themselves.
      slopes <- vapply(lev, function(s) {
        e <- pr_e$estimate[grid$series == s]
        e[2L] - e[1L]
      }, numeric(1))
      expect_gt(stats::sd(slopes), 0.05 * mean(abs(slopes)))
    })
  }

  invisible(NULL)
}


# ---- Run the battery once per family ---------------------------------

built <- new.env(parent = emptyenv())

for (nm in names(SPECS)) {
  spec <- SPECS[[nm]]
  sim <- spec$sim()
  fit <- fit_jsdm(nm, spec, sim)
  assign(nm, list(spec = spec, sim = sim, fit = fit), envir = built)
  jsdgam_battery(nm, spec, sim, fit)
}


# ---- What belongs to one family alone --------------------------------


test_that("nb: the shape parameter recovers the simulated dispersion", {
  # The shape sets how far counts scatter around their mean, so a
  # shape off by an order of magnitude leaves the covariance
  # recovery intact and makes every predictive interval wrong.
  obj <- get("nb", envir = built)
  dm <- as_draws_matrix(obj$fit$fit)
  phi_cols <- grep("^shape$|^phi$", colnames(dm), value = TRUE)
  expect_gt(length(phi_cols), 0L)
  phi_post <- as.numeric(dm[, phi_cols[1L]])
  expect_true(all(phi_post > 0))
  expect_lt(abs(mean(phi_post) - obj$sim$phi_true) / obj$sim$phi_true,
            0.75)
})


test_that("mvt: Psi and nu recover the simulated residual law", {
  # A tail parameter that had collapsed to its floor or run off to
  # the prior median leaves every other number in this file intact.
  obj <- get("mvt", envir = built)
  dm <- as_draws_matrix(obj$fit$fit)
  psi_cols <- grep("^Psi\\[", colnames(dm), value = TRUE)
  expect_length(psi_cols, obj$sim$K)
  expect_lt(
    max(abs(colMeans(dm[, psi_cols, drop = FALSE]) - obj$sim$psi_true)),
    0.5
  )

  expect_true("nu" %in% colnames(dm))
  nu_draws <- as.numeric(dm[, "nu"])
  # The Student-t degrees of freedom have a hard floor at 2; a
  # posterior with mass below it means the constraint is not applied.
  expect_true(all(nu_draws >= 2))
  # Loose by design: at K = 4 and 30 sites there are ~120
  # observations, too few to identify the tail tightly against a
  # gamma(2, 0.1) prior whose median sits near 14. What the interval
  # must not do is sit on the prior with the data saying nothing, so
  # the posterior also has to be narrower than that prior.
  expect_lt(abs(mean(nu_draws) - obj$sim$nu_true), 2 * sd(nu_draws))
  expect_lt(sd(nu_draws), sqrt(2) / 0.1)
  expect_true("nu" %in% variables(obj$fit))
})


test_that("diri: the concentration parameter recovers phi", {
  # phi sets how tightly the compositions concentrate around their
  # expectation, so a phi off by an order of magnitude leaves the
  # correlation recovery intact while every interval is the wrong
  # width.
  obj <- get("diri", envir = built)
  dm <- as_draws_matrix(obj$fit$fit)
  phi_cols <- grep("^phi$|^b_phi_Intercept$|^Intercept_phi$",
                   colnames(dm), value = TRUE)
  expect_gt(length(phi_cols), 0L)
  phi_post <- as.numeric(dm[, phi_cols[1L]])
  phi_resp <- if (grepl("Intercept", phi_cols[1L])) {
    exp(phi_post)
  } else {
    phi_post
  }
  expect_true(all(phi_resp > 0))
  expect_lt(abs(mean(phi_resp) - obj$sim$phi_true) / obj$sim$phi_true,
            0.6)
})


test_that("diri: the loadings are pinned to sum to zero over species", {
  # A softmax is invariant to a constant added across species, so an
  # unpinned Z leaves the model identified only up to that shift and
  # the loadings wander between chains. The constraint is shared by
  # every softmax family, so pinning it here pins it for all of them.
  obj <- get("diri", envir = built)
  Z_arr <- mvgam:::extract_Z_loadings(
    as_draws_matrix(obj$fit$fit),
    n_obs_series = obj$sim$K, n_lv = obj$sim$N_lv
  )
  Z_mean <- apply(Z_arr, c(2L, 3L), mean)
  expect_lt(max(abs(colSums(Z_mean))), 1e-6)
  # The columns sum to zero because they are constrained, not
  # because they are empty.
  expect_gt(max(abs(Z_mean)), 0.1)
  # The constraint holds draw by draw, not only on the average. A
  # posterior mean can sum to zero from a pair of draws that shift
  # in opposite directions while neither is itself pinned.
  per_draw <- apply(Z_arr, 1L, function(z) max(abs(colSums(z))))
  expect_lt(max(per_draw), 1e-6)
})


test_that("diri: the data and the expectation are both compositions", {
  # The fixture's own premise, checked rather than assumed: if the
  # simulation drifted off the simplex, every number above describes
  # a different model. And a per-species inverse link applied
  # without the shared normaliser gives four independent proportions
  # that do not sum to one, every one of them still inside the unit
  # interval.
  obj <- get("diri", envir = built)
  d <- obj$sim$long_dat
  per_site <- tapply(d$y, d$time, sum)
  expect_equal(as.numeric(per_site), rep(1, length(per_site)),
               tolerance = 1e-8)
  expect_true(all(d$y > 0 & d$y < 1))

  ep <- posterior_epred(obj$fit, draw_ids = 1:20)
  site_sums <- tapply(colMeans(ep), d$time, sum)
  expect_equal(as.numeric(site_sums), rep(1, length(site_sums)),
               tolerance = 1e-6)
  # And a draw is a composition too, not merely bounded.
  pp <- posterior_predict(obj$fit, draw_ids = 1:20)
  for (i in seq_len(nrow(pp))) {
    expect_equal(as.numeric(tapply(pp[i, ], d$time, sum)),
                 rep(1, length(per_site)), tolerance = 1e-6)
  }
})
