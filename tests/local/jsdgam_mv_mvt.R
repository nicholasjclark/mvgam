# Recovery fixture for the mvt() Multivariate Student-t family.
#
# Simulates K = 4 species at n_sites = 30 sites with a known
# low-rank residual covariance. The conditional gllvm
# parameterisation routes `lv ~ N(0, I)` through the trend
# pipeline; the per-row residual is scalar Student-t with scale
# Psi[k] and shared nu, so the marginal residual covariance is
# approximately Sigma_true = Z_true %*% t(Z_true) +
# diag(Psi_true^2 * nu_true / (nu_true - 2)).
#
# Primary go/no-go:
#   cor(off_diag(true_cor), off_diag(post_cor)) > 0.7
#   AND |posterior_mean(nu) - 5| < 2 * posterior_sd(nu)
#
# The nu check is loose because heavy-tail recovery is
# data-hungry: at K = 4 and n_sites = 30 there are only ~120
# observations, which is too few to identify the tail parameter
# tightly against the gamma(2, 0.1) prior (median ~14). The
# threshold accepts any nu posterior that is consistent with
# the truth at 2 posterior standard deviations.
#
# Diagnostics reported:
#   - MAE on off-diagonals
#   - per-species Psi recovery (posterior mean vs truth)
#   - nu posterior summary + 95% interval
#   - divergent transitions + max-treedepth saturation
#   - min / max bulk ESS on Z entries
#
# Cached at tests/local/fixtures/val_mvgam_jsdgam_mv_mvt.rds.
# Delete to refit. Runtime ~5-8 min.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(tidyr)
  library(posterior)
})

# testthat sets the working directory to tests/local/ when it runs a
# file, while Rscript runs it from the package root. Reach the shared
# fixture helpers by whichever of the two paths exists.
source(if (file.exists("concordance_helpers.R")) {
  "concordance_helpers.R"
} else {
  file.path("tests", "local", "concordance_helpers.R")
})

set.seed(605L)

K <- 4L
N_lv <- 2L
n_sites <- 30L
threshold_cor <- 0.7
nu_true <- 5
species_levels <- paste0("y", seq_len(K))

Z_true <- matrix(rnorm(K * N_lv, sd = 0.7), nrow = K, ncol = N_lv)
psi_true <- rep(0.5, K)
sigma_true_cov <- tcrossprod(Z_true) +
                    diag(psi_true^2 * nu_true / (nu_true - 2))
sigma_true_cor <- cov2cor(sigma_true_cov)

env <- rnorm(n_sites)
mu_intercept <- rnorm(K)
mu_env_slope <- rnorm(K)

# Conditional gllvm sim: each site draws a latent lv ~ N(0, I),
# then mu_unit_i = mu_fixed_i + Z * lv_i, then per-row residuals
# are independent Student-t with scale psi[k] and shared nu.
lv_sim <- matrix(rnorm(n_sites * N_lv), nrow = n_sites, ncol = N_lv)
Y_wide <- matrix(NA_real_, nrow = n_sites, ncol = K)
for (i in seq_len(n_sites)) {
  mu_i <- mu_intercept + mu_env_slope * env[i] +
            as.numeric(Z_true %*% lv_sim[i, ])
  resid_i <- psi_true * rt(K, df = nu_true)
  Y_wide[i, ] <- mu_i + resid_i
}
colnames(Y_wide) <- species_levels

wide_dat <- as.data.frame(Y_wide)
wide_dat$site <- seq_len(n_sites)
wide_dat$env  <- env
long_dat <- pivot_longer(
  wide_dat, all_of(species_levels),
  names_to = "series", values_to = "y"
) |>
  mutate(
    series = factor(series, levels = species_levels),
    time   = site
  ) |>
  arrange(time, series)

cat("Simulated", n_sites, "sites x", K, "species.",
    "True nu =", nu_true,
    "| true cor off-diag range: [",
    round(min(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), ",",
    round(max(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), "].\n")

# The generative truth rides on the saved fit so a separate test can
# assert recovery against it without repeating the simulation.
sim_truth <- list(
  K = K, N_lv = N_lv, species_levels = species_levels,
  Z_true = Z_true, psi_true = psi_true, nu_true = nu_true,
  sigma_true_cov = sigma_true_cov, sigma_true_cor = sigma_true_cor,
  mu_intercept = mu_intercept, mu_env_slope = mu_env_slope,
  lv_sim = lv_sim, env = env
)

cache <- local_fixture_path("val_mvgam_jsdgam_mv_mvt.rds")
if (file.exists(cache)) {
  cat("[cache] Loading mvt recovery fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] jsdgam(mvt(), n_lv = 2, ", n_sites, " sites)\n", sep = "")
  fit <- jsdgam(
    formula = y ~ env * series,
    factor_formula = ~ -1,
    data = as.data.frame(long_dat),
    unit = time, species = series,
    family = mvt(),
    n_lv = N_lv,
    chains = 2L,
    iter = 1000L, warmup = 500L,
    silent = 2,
    backend = "cmdstanr"
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}

cat("\n=== Primary recovery (correlation off-diagonals) ===\n")
res_cor <- residual_cor(fit)
post_cor <- res_cor$cor
true_off <- sigma_true_cor[upper.tri(sigma_true_cor)]
post_off <- post_cor[upper.tri(post_cor)]
cor_off <- stats::cor(true_off, post_off)
mae_off <- mean(abs(true_off - post_off))
cat(sprintf("cor(true, posterior) = %.4f  (threshold > %.2f)\n",
            cor_off, threshold_cor))
cat(sprintf("MAE                  = %.4f\n", mae_off))

# Both go/no-go lines in this file printed a verdict and checked
# nothing, so it reported PASS whatever it found. They are asserted
# below, together with the structure a recovery correlation cannot
# see: the off-diagonals are compared as a set, so a fit that hands
# each species another species' latent column scores exactly as well.

library(testthat)

test_that("the residual correlation recovers the simulated one", {
  expect_gt(cor_off, threshold_cor)
  expect_lt(mae_off, 0.5)
})


test_that("the species axis is the four simulated species, in order", {
  axes <- mvgam:::mvgam_axes(fit)
  expect_identical(as.character(axes$series$levels), species_levels)
  expect_identical(as.integer(axes$series$n), K)
  expect_identical(as.integer(fit$standata$N_lv_trend), N_lv)
  expect_identical(as.integer(fit$standata$N_series_trend), K)
})


test_that("every prediction surface answers for every row", {
  n_obs <- nrow(long_dat)
  ep <- posterior_epred(fit, ndraws = 20L)
  pp <- posterior_predict(fit, ndraws = 20L)
  lp <- posterior_linpred(fit, ndraws = 20L)
  expect_identical(dim(ep), c(20L, n_obs))
  expect_identical(dim(pp), c(20L, n_obs))
  expect_identical(dim(lp), c(20L, n_obs))
  # Student-t draws are heavy-tailed but finite; an infinite draw
  # means nu reached its floor or the scale collapsed.
  expect_true(all(is.finite(ep)))
  expect_true(all(is.finite(pp)))
  expect_true(all(is.finite(lp)))
  expect_identical(nrow(predict(fit, ndraws = 20L)), n_obs)
  expect_identical(nrow(fitted(fit, ndraws = 20L)), n_obs)
  expect_identical(nrow(residuals(fit, ndraws = 20L)), n_obs)

  # Column j of every surface above is row j of the frame, so the
  # species the fit resolves for each row must be the species the
  # frame states there. A permutation keeps every dimension, leaves
  # every value finite and uses every species exactly once.
  d <- as.data.frame(long_dat)
  os <- mvgam:::get_observation_structure(fit, newdata = d)
  expect_identical(as.character(os$series), as.character(d$series))
  expect_identical(os$series_levels, species_levels)
  expect_identical(as.integer(os$series_int),
                   match(as.character(d$series), species_levels))
  expect_identical(as.integer(os$time),
                   match(d$time, sort(unique(d$time))))
})


test_that("each row reads the latent cell the sampler drew for it", {
  # The only comparison here that does not ask two derivations
  # whether they agree with each other: `obs_trend_time` and
  # `obs_trend_series` are the cell this fit gave each row, and
  # `trend[t, s]` is what it sampled there. A species reading
  # another's column returns a real state of the right shape.
  d <- as.data.frame(long_dat)
  dm <- posterior::as_draws_matrix(fit$fit)
  t_rec <- as.integer(fit$standata$obs_trend_time)
  s_rec <- as.integer(fit$standata$obs_trend_series)
  expect_length(t_rec, nrow(d))
  want <- vapply(paste0("trend[", t_rec, ",", s_rec, "]"),
                 function(k) mean(dm[, k]), numeric(1))
  got <- colMeans(
    mvgam:::extract_trend_latent_states(fit, newdata = d, full_draws = dm)
  )
  expect_equal(unname(got), unname(want))
})


test_that("a shuffled newdata answers the same, in the new order", {
  # A prediction placing rows by position rather than by content
  # agrees with every check that hands back the training frame in its
  # own order, and disagrees here.
  d <- as.data.frame(long_dat)
  set.seed(14L)
  perm <- sample(nrow(d))
  base <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)
  shuf <- posterior_epred(fit, newdata = d[perm, , drop = FALSE],
                          draw_ids = 1:10, incl_autocor = TRUE)
  expect_equal(unname(base[, perm, drop = FALSE]), unname(shuf))
})


test_that("a newdata holding one species reads that species' state", {
  # `droplevels()` leaves the frame carrying only its own species,
  # which is what a real subset does. A species index taken from the
  # levels the frame carries numbers that species 1 whatever it is,
  # and it then reads the first species' latent column.
  d <- as.data.frame(long_dat)
  full <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)
  for (s in species_levels) {
    rows <- which(as.character(d$series) == s)
    sub <- d[rows, , drop = FALSE]
    sub$series <- droplevels(sub$series)
    expect_identical(levels(sub$series), s)
    got <- posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                           incl_autocor = TRUE)
    expect_equal(unname(got), unname(full[, rows, drop = FALSE]))
  }
})


test_that("a newdata declaring its levels in another order maps right", {
  # The mapping is by label, so re-declaring the same species in a
  # different order must not move the answers.
  d <- as.data.frame(long_dat)
  base <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)
  nd <- d
  nd$series <- factor(as.character(nd$series),
                      levels = rev(species_levels))
  got <- posterior_epred(fit, newdata = nd, draw_ids = 1:10,
                         incl_autocor = TRUE)
  expect_equal(unname(got), unname(base))
})


test_that("a newdata naming an unknown species is refused", {
  d <- as.data.frame(long_dat)
  nd <- d
  nd$series <- factor(
    ifelse(seq_len(nrow(nd)) == 1L, "y_unseen", as.character(nd$series)),
    levels = c(species_levels, "y_unseen")
  )
  err <- expect_error(
    posterior_epred(fit, newdata = nd, draw_ids = 1:5),
    "Series levels in newdata not found in training data"
  )
  # A refusal that does not name the offending level, or list the
  # ones that would have worked, leaves the user to find which of
  # their species the model has never seen.
  expect_match(conditionMessage(err), "y_unseen", fixed = TRUE)
  for (s in species_levels) {
    expect_match(conditionMessage(err), s, fixed = TRUE)
  }
})


test_that("hindcast arms are the species, in order, and distinct", {
  arms <- hindcast(fit, ndraws = 20L)$hindcasts
  expect_identical(names(arms), species_levels)
  expect_true(all(vapply(arms, function(a) NROW(a) > 0L, logical(1))))
  # Every pair. The opening pair alone passes a fit that gave the
  # last two species one latent column.
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
})


test_that("forecast is keyed by the species axis", {
  h <- 4L
  last_t <- max(long_dat$time)
  nd <- expand.grid(
    time = (last_t + 1L):(last_t + h),
    series = factor(species_levels, levels = species_levels),
    stringsAsFactors = FALSE
  )
  nd$env <- 0
  nd$y <- NA_real_
  for (ty in c("link", "expected", "trend")) {
    fc <- forecast(fit, newdata = nd, ndraws = 20L, type = ty)
    expect_s3_class(fc, "mvgam_forecast")
    expect_identical(names(fc$forecasts), species_levels)
    for (s in species_levels) {
      expect_identical(dim(fc$forecasts[[s]]), c(20L, h))
      expect_true(all(is.finite(fc$forecasts[[s]])))
    }
  }

  # `hindcast(type = "response")` answers for this family and
  # `forecast(type = "response")` refuses it. They are the same
  # quantity reached over the training grid and over the extension of
  # it, so one of the two is wrong: either the family can be drawn
  # from and the forecast arm should not refuse, or it cannot and the
  # hindcast arm is calling something a response draw that is not
  # one. This asserts they agree rather than picking a side.
  expect_s3_class(hindcast(fit, ndraws = 20L, type = "response"),
                  "mvgam_forecast")
  fc_resp <- forecast(fit, newdata = nd, ndraws = 20L, type = "response")
  expect_identical(names(fc_resp$forecasts), species_levels)
})


test_that("residual_cor is labelled by the species axis", {
  # The matrix the recovery number above is read off. A correct
  # matrix under the wrong labels reads, to anyone using it, exactly
  # like a wrong matrix, and the correlation is identical either way.
  expect_identical(rownames(post_cor), species_levels)
  expect_identical(colnames(post_cor), species_levels)
  expect_equal(unname(diag(post_cor)), rep(1, K))
  # A correlation matrix is symmetric; an asymmetry means the two
  # indices were resolved by different routes.
  expect_equal(unname(post_cor), unname(t(post_cor)))
})


test_that("summary reports the fit's own structure", {
  txt <- capture.output(summary(fit))
  expect_gt(length(txt), 10L)
  expect_true(any(grepl("Series:\\s*4", txt)))
  expect_true(any(grepl("env", txt, fixed = TRUE)))
})


test_that("Psi and nu recover the simulated residual law", {
  # The nu verdict was printed as PASS and checked by nobody, so a
  # tail parameter that had collapsed to its floor or run off to the
  # prior median would have been reported as a success.
  dm <- as_draws_matrix(fit$fit)
  psi_cols <- grep("^Psi\\[", colnames(dm), value = TRUE)
  expect_length(psi_cols, K)
  expect_lt(max(abs(colMeans(dm[, psi_cols, drop = FALSE]) - psi_true)), 0.5)

  expect_true("nu" %in% colnames(dm))
  nu_draws <- as.numeric(dm[, "nu"])
  # The Student-t degrees of freedom have a hard floor at 2; a
  # posterior with mass below it means the constraint is not applied.
  expect_true(all(nu_draws >= 2))
  expect_lt(abs(mean(nu_draws) - nu_true), 2 * sd(nu_draws))
})


test_that("log_lik answers per row and feeds loo", {
  n_obs <- nrow(long_dat)
  ll <- log_lik(fit, ndraws = 20L)
  expect_identical(dim(ll), c(20L, n_obs))
  expect_true(all(is.finite(ll)))
  ic <- loo(fit)
  expect_s3_class(ic, "loo")
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
})


test_that("pp_check draws this family", {
  expect_s3_class(pp_check(fit, ndraws = 20L), "ggplot")
  expect_s3_class(pp_check(fit, type = "resid_qq", ndraws = 20L), "ggplot")
})


test_that("conditional_effects is drawn per species", {
  ce <- conditional_effects(fit)
  expect_s3_class(ce, "mvgam_conditional_effects")
  expect_gt(length(ce), 0L)
  for (eff in names(ce)) {
    d <- ce[[eff]]$data
    expect_true(all(c("estimate", "conf.low", "conf.high") %in% names(d)))
    expect_true(all(is.finite(d$estimate)))
    expect_true(all(d$conf.low <= d$estimate))
    expect_true(all(d$estimate <= d$conf.high))
    if ("series" %in% names(d)) {
      expect_true(all(as.character(unique(d$series)) %in% species_levels))
    }
  }
})


test_that("the factor methods report two factors over four species", {
  af <- active_factors(fit)
  expect_s3_class(af, "mvgam_active_factors")
  expect_identical(as.integer(af$n_lv), N_lv)
  # One row per latent factor, not per species.
  expect_identical(nrow(af$per_factor), N_lv)

  sv <- shared_variation(fit)
  expect_s3_class(sv, "mvgam_shared_variation")
  # The names this is read by, in the order the loadings run. A
  # correct decomposition under permuted names hands each species'
  # shared variance to another species.
  expect_identical(as.character(sv$series_names), species_levels)
  expect_identical(as.integer(sv$n_series), K)
  expect_identical(as.integer(sv$n_lv), N_lv)
  expect_true(all(is.finite(as.numeric(sv$delta))))

  expect_s3_class(ordinate(fit), "ggplot")
  expect_false(is.null(compare_loadings(fit, fit)))

  Z_arr <- mvgam:::extract_Z_loadings(
    posterior::as_draws_matrix(fit$fit), n_obs_series = K, n_lv = N_lv
  )
  expect_identical(dim(Z_arr)[2:3], c(K, N_lv))
  # No two species share a loading vector; a collapsed axis gives
  # them one while every dimension stays correct.
  Z_mean <- apply(Z_arr, c(2L, 3L), mean)
  same <- character(0)
  for (i in seq_len(K)) {
    for (j in seq_len(K)) {
      if (j <= i) next
      if (isTRUE(all.equal(Z_mean[i, ], Z_mean[j, ]))) {
        same <- c(same, paste(species_levels[i], species_levels[j],
                              sep = "="))
      }
    }
  }
  expect_identical(same, character(0))
})


test_that("the plotting methods render for this fit", {
  for (ty in c("residuals", "trend", "factors")) {
    # `plot()` returns a ggplot, so that is what is asserted. The
    # alternation this replaced ended in `is.list(p)`, which an empty
    # list satisfies: any method returning `list()` passed it.
    p <- plot(fit, type = ty)
    expect_s3_class(p, "ggplot")
  }
  expect_s3_class(mcmc_plot(fit), "ggplot")
})


test_that("the draws and the tidiers name this fit's parameters", {
  vars <- variables(fit)
  expect_true(any(grepl("^Z(_tilde)?\\[", vars)))
  expect_true(any(grepl("^Psi\\[", vars)))
  expect_true("nu" %in% vars)
  td <- tidy(fit)
  expect_true(is.data.frame(td))
  expect_gt(nrow(td), 0L)
  expect_true(is.data.frame(glance(fit)))
  aug <- augment(fit)
  expect_true(is.data.frame(aug))
  expect_identical(nrow(aug), nrow(long_dat))
  # Row for row, in the frame's own order. A tidier that re-sorts its
  # output pairs each fitted value with another row's observation
  # while every column keeps the right length.
  expect_identical(as.character(aug$series),
                   as.character(long_dat$series))
  expect_equal(as.numeric(aug$time), as.numeric(long_dat$time))
  expect_equal(as.numeric(aug$.observed), as.numeric(long_dat$y))
})


test_that("marginaleffects reaches this fit and separates the species", {
  withr::local_options(marginaleffects_model_classes = "mvgam")
  grid <- expand.grid(
    env = c(-1, 1),
    series = factor(species_levels, levels = species_levels),
    stringsAsFactors = FALSE
  )
  grid$time <- 1L
  grid$y <- NA_real_
  pr <- marginaleffects::predictions(fit, newdata = grid, type = "response")
  expect_identical(nrow(pr), nrow(grid))
  expect_true(all(is.finite(pr$estimate)))
  # `env * series` gives each species its own slope. A design that
  # dropped the interaction returns one slope shared by all four.
  slopes <- vapply(species_levels, function(s) {
    e <- pr$estimate[grid$series == s]
    e[2L] - e[1L]
  }, numeric(1))
  expect_gt(stats::sd(slopes), 1e-6)
})

cat("\n=== Psi recovery (per-species residual SD) ===\n")
draws <- as_draws_matrix(fit$fit)
psi_cols <- grep("^Psi\\[", colnames(draws), value = TRUE)
if (length(psi_cols) == K) {
  psi_mean <- colMeans(draws[, psi_cols, drop = FALSE])
  cat("Psi posterior mean:",
      paste(round(psi_mean, 3), collapse = " "), "\n")
  cat("Psi truth         :",
      paste(round(psi_true, 3), collapse = " "), "\n")
  cat(sprintf("max abs error     : %.3f\n",
              max(abs(psi_mean - psi_true))))
} else {
  cat("WARN: expected", K, "Psi entries, found",
      length(psi_cols), "\n")
}

cat("\n=== nu recovery (Student-t degrees of freedom) ===\n")
if ("nu" %in% colnames(draws)) {
  nu_draws <- as.numeric(draws[, "nu"])
  nu_mean <- mean(nu_draws)
  nu_sd   <- sd(nu_draws)
  cat(sprintf("nu posterior mean   = %.2f\n", nu_mean))
  cat(sprintf("nu posterior median = %.2f\n",
              median(nu_draws)))
  cat(sprintf("nu posterior Q2.5   = %.2f\n",
              quantile(nu_draws, 0.025)))
  cat(sprintf("nu posterior Q97.5  = %.2f\n",
              quantile(nu_draws, 0.975)))
  cat(sprintf("nu truth            = %d\n", nu_true))
  nu_consistent <- abs(nu_mean - nu_true) < 2 * nu_sd
  cat(sprintf("PASS (nu within 2 posterior SD of truth): %s\n",
              isTRUE(nu_consistent)))
  cat(sprintf("P(nu >= 2 | data)   = %.3f (hard floor; expect 1)\n",
              mean(nu_draws >= 2)))
} else {
  cat("WARN: nu parameter not present in draws.\n")
}

cat("\n=== Sampler diagnostics ===\n")
diag_df <- nuts_params(fit$fit)
n_div <- sum(subset(diag_df, Parameter == "divergent__")$Value)
n_treedepth <- sum(
  subset(diag_df, Parameter == "treedepth__")$Value >= 10L
)
n_total_trans <- nrow(subset(diag_df, Parameter == "divergent__"))
cat(sprintf("Divergent transitions:    %d / %d (%.2f%%)\n",
            n_div, n_total_trans,
            100 * n_div / n_total_trans))
cat(sprintf("Max-treedepth saturation: %d / %d (%.2f%%)\n",
            n_treedepth, n_total_trans,
            100 * n_treedepth / n_total_trans))

cat("\n=== Z-entry bulk ESS ===\n")
z_pat <- if (any(grepl("^Z_tilde\\[", colnames(draws)))) {
  "^Z_tilde\\["
} else {
  "^Z\\["
}
z_cols <- grep(z_pat, colnames(draws), value = TRUE)
z_summary <- summarise_draws(draws[, z_cols, drop = FALSE], "ess_bulk")
cat(sprintf("min ess_bulk = %.0f\n",
            min(z_summary$ess_bulk, na.rm = TRUE)))
cat(sprintf("max ess_bulk = %.0f\n",
            max(z_summary$ess_bulk, na.rm = TRUE)))

cat("\nDone.\n")
