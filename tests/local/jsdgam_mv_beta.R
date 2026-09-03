# Recovery fixture for Beta-distributed proportions under a jsdgam.
#
# Simulates K = 5 species at n_sites = 60 sites with a known rank-2
# loading matrix Z_true. Each site draws lv ~ N(0, I), the species'
# logit-means are mu_fixed + Z %*% lv, and the responses are Beta at
# precision phi_true, so the species covary on the logit scale while
# each observation stays strictly inside the unit interval.
#
# Primary go/no-go:
#   cor(off_diag(true_cor), off_diag(post_cor)) > 0.6
#
# where true_cor = cov2cor(Z_true %*% t(Z_true)) and post_cor is
# residual_cor(fit)$cor. A bounded response carries less information
# about the latent covariance than an unbounded one, so the threshold
# sits below the mvn value.
#
# Cached at tests/local/fixtures/val_mvgam_jsdgam_mv_beta.rds. Delete
# to refit. Runtime ~4-6 min.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
  library(tidyr)
  library(posterior)
  library(testthat)
})

# testthat sets the working directory to tests/local/ when it runs a
# file, while Rscript runs it from the package root. Reach the shared
# fixture helpers by whichever of the two paths exists.
source(if (file.exists("concordance_helpers.R")) {
  "concordance_helpers.R"
} else {
  file.path("tests", "local", "concordance_helpers.R")
})

set.seed(609L)

K <- 5L
N_lv <- 2L
n_sites <- 60L
phi_true <- 8
threshold_cor <- 0.6
species_levels <- paste0("sp", seq_len(K))

Z_true <- matrix(rnorm(K * N_lv, sd = 0.8), nrow = K, ncol = N_lv)
sigma_true_cov <- tcrossprod(Z_true)
sigma_true_cor <- cov2cor(sigma_true_cov + diag(1e-8, K))

env <- rnorm(n_sites)
mu_intercept <- rnorm(K, mean = 0, sd = 0.5)
mu_env_slope <- rnorm(K, sd = 0.4)

lv_sim <- matrix(rnorm(n_sites * N_lv), nrow = n_sites, ncol = N_lv)
Y_wide <- matrix(NA_real_, nrow = n_sites, ncol = K)
for (i in seq_len(n_sites)) {
  eta_i <- mu_intercept + mu_env_slope * env[i] +
    as.numeric(Z_true %*% lv_sim[i, ])
  mu_i <- 1 / (1 + exp(-eta_i))
  Y_wide[i, ] <- rbeta(K, shape1 = mu_i * phi_true,
                       shape2 = (1 - mu_i) * phi_true)
}
# Beta support is open at both ends; nudge off the boundary so the
# likelihood is finite for every observation.
Y_wide <- pmin(pmax(Y_wide, 1e-4), 1 - 1e-4)
colnames(Y_wide) <- species_levels

wide_dat <- as.data.frame(Y_wide)
wide_dat$site <- seq_len(n_sites)
wide_dat$env <- env
long_dat <- pivot_longer(
  wide_dat, all_of(species_levels),
  names_to = "series", values_to = "y"
) |>
  mutate(
    series = factor(series, levels = species_levels),
    time = site
  ) |>
  arrange(time, series)

cat("Simulated", n_sites, "sites x", K, "species. y range [",
    round(min(long_dat$y), 4), ",", round(max(long_dat$y), 4), "].",
    "True cor off-diag range: [",
    round(min(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), ",",
    round(max(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), "].\n")

# The generative truth rides on the saved fit so a separate test can
# assert recovery against it without repeating the simulation.
sim_truth <- list(
  K = K, N_lv = N_lv, species_levels = species_levels,
  Z_true = Z_true, phi_true = phi_true, lv_sim = lv_sim,
  sigma_true_cov = sigma_true_cov, sigma_true_cor = sigma_true_cor,
  mu_intercept = mu_intercept, mu_env_slope = mu_env_slope, env = env
)

cache <- local_fixture_path("val_mvgam_jsdgam_mv_beta.rds")
if (file.exists(cache)) {
  cat("[cache] Loading Beta recovery fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] jsdgam(Beta(), n_lv = 2, ", n_sites, " sites)\n", sep = "")
  fit <- jsdgam(
    formula = y ~ env * series,
    factor_formula = ~ -1,
    data = as.data.frame(long_dat),
    unit = time, species = series,
    family = Beta(),
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

cat("\n=== Primary recovery (cor on Z Z' off-diagonals) ===\n")
res_cor <- residual_cor(fit)
post_cor <- res_cor$cor
true_off <- sigma_true_cor[upper.tri(sigma_true_cor)]
post_off <- post_cor[upper.tri(post_cor)]
cor_off <- stats::cor(true_off, post_off)
mae_off <- mean(abs(true_off - post_off))
cat(sprintf("cor(true, posterior) = %.4f  (threshold > %.2f)\n",
            cor_off, threshold_cor))
cat(sprintf("MAE                  = %.4f\n", mae_off))


test_that("the residual correlation recovers the simulated one", {
  # The off-diagonals are compared as a set, so this number is
  # reached just as well by a fit that hands every species another
  # species' latent column.
  expect_gt(cor_off, threshold_cor)
  expect_lt(mae_off, 0.6)
})


test_that("the precision parameter recovers the simulated phi", {
  # phi sets how tightly the proportions concentrate around their
  # mean. A phi off by an order of magnitude leaves the latent
  # covariance recovery intact and makes every interval the wrong
  # width.
  dm <- as_draws_matrix(fit$fit)
  phi_cols <- grep("^phi$|^b_phi_Intercept$", colnames(dm), value = TRUE)
  expect_gt(length(phi_cols), 0L)
  phi_post <- as.numeric(dm[, phi_cols[1L]])
  phi_resp <- if (grepl("Intercept", phi_cols[1L])) {
    exp(phi_post)
  } else {
    phi_post
  }
  expect_true(all(phi_resp > 0))
  expect_lt(abs(mean(phi_resp) - phi_true) / phi_true, 0.75)
})


test_that("the species axis is the five simulated species, in order", {
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
  expect_true(all(is.finite(lp)))
  # Proportions: strictly inside the unit interval, which a draw
  # taken on the linear-predictor scale would not be.
  expect_true(all(ep > 0 & ep < 1))
  expect_true(all(pp > 0 & pp < 1))
  expect_identical(nrow(predict(fit, ndraws = 20L)), n_obs)
  expect_identical(nrow(fitted(fit, ndraws = 20L)), n_obs)
  expect_identical(nrow(residuals(fit, ndraws = 20L)), n_obs)

  # Column j is row j of the frame, so the species the fit resolves
  # for each row is the species the frame states there.
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
  # `obs_trend_time` and `obs_trend_series` name the cell this fit
  # gave each row, and `trend[t, s]` is what it sampled there. A
  # species reading another's column returns a real state of the
  # right shape, so only a value comparison sees it.
  d <- as.data.frame(long_dat)
  dm <- as_draws_matrix(fit$fit)
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
  # agrees with every check that hands back the training frame in
  # its own order, and disagrees here.
  d <- as.data.frame(long_dat)
  set.seed(13L)
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
  # levels the frame carries numbers that species 1 whatever it is.
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
    ifelse(seq_len(nrow(nd)) == 1L, "sp_unseen", as.character(nd$series)),
    levels = c(species_levels, "sp_unseen")
  )
  err <- expect_error(
    posterior_epred(fit, newdata = nd, draw_ids = 1:5),
    "Series levels in newdata not found in training data"
  )
  # A refusal that does not name the offending level, or list the
  # ones that would have worked, leaves the user to find which of
  # their species the model has never seen.
  expect_match(conditionMessage(err), "sp_unseen", fixed = TRUE)
  for (s in species_levels) {
    expect_match(conditionMessage(err), s, fixed = TRUE)
  }
})


test_that("hindcast arms are the species, in order, and distinct", {
  arms <- hindcast(fit, ndraws = 20L)$hindcasts
  expect_identical(names(arms), species_levels)
  expect_true(all(vapply(arms, function(a) NROW(a) > 0L, logical(1))))
  # Every pair: an axis that gave the last two species one column
  # leaves the opening pair distinct.
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
  # A hindcast of proportions stays inside the unit interval.
  expect_true(all(do.call(cbind, arms) > 0))
  expect_true(all(do.call(cbind, arms) < 1))
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
  for (ty in c("link", "expected", "trend", "response")) {
    fc <- forecast(fit, newdata = nd, ndraws = 20L, type = ty)
    expect_s3_class(fc, "mvgam_forecast")
    expect_identical(names(fc$forecasts), species_levels)
    for (s in species_levels) {
      expect_identical(dim(fc$forecasts[[s]]), c(20L, h))
      expect_true(all(is.finite(fc$forecasts[[s]])))
    }
  }
  # A drawn proportion is a proportion; the expectation under
  # another name would be too, so the link scale is checked apart.
  drawn <- do.call(cbind,
                   forecast(fit, newdata = nd, ndraws = 20L,
                            type = "response")$forecasts)
  expect_true(all(drawn > 0 & drawn < 1))
})


test_that("residual_cor is labelled by the species axis", {
  expect_identical(rownames(post_cor), species_levels)
  expect_identical(colnames(post_cor), species_levels)
  expect_equal(unname(diag(post_cor)), rep(1, K))
  expect_equal(unname(post_cor), unname(t(post_cor)))
})


test_that("the factor methods report two factors over five species", {
  af <- active_factors(fit)
  expect_s3_class(af, "mvgam_active_factors")
  expect_identical(as.integer(af$n_lv), N_lv)
  expect_identical(nrow(af$per_factor), N_lv)

  sv <- shared_variation(fit)
  expect_s3_class(sv, "mvgam_shared_variation")
  # The names this is read by, in the order the loadings run.
  expect_identical(as.character(sv$series_names), species_levels)
  expect_identical(as.integer(sv$n_series), K)
  expect_identical(as.integer(sv$n_lv), N_lv)

  expect_s3_class(ordinate(fit), "ggplot")
  expect_false(is.null(compare_loadings(fit, fit)))

  Z_arr <- mvgam:::extract_Z_loadings(
    as_draws_matrix(fit$fit), n_obs_series = K, n_lv = N_lv
  )
  expect_identical(dim(Z_arr)[2:3], c(K, N_lv))
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


test_that("summary and the criticism methods run on this fit", {
  txt <- capture.output(summary(fit))
  expect_gt(length(txt), 10L)
  expect_true(any(grepl("Series:\\s*5", txt)))
  expect_true(any(grepl("env", txt, fixed = TRUE)))

  ll <- log_lik(fit, ndraws = 20L)
  expect_identical(dim(ll), c(20L, nrow(long_dat)))
  expect_true(all(is.finite(ll)))
  ic <- suppressWarnings(loo(fit))
  expect_s3_class(ic, "loo")
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
})


test_that("pp_check, plotting and conditional_effects render", {
  expect_s3_class(pp_check(fit, ndraws = 20L), "ggplot")
  expect_s3_class(pp_check(fit, type = "ecdf_overlay", ndraws = 20L),
                  "ggplot")
  for (ty in c("residuals", "trend", "factors")) {
    # `plot()` returns a ggplot, so that is what is asserted. The
    # alternation this replaced ended in `is.list(p)`, which an empty
    # list satisfies: any method returning `list()` passed it.
    p <- plot(fit, type = ty)
    expect_s3_class(p, "ggplot")
  }
  expect_s3_class(mcmc_plot(fit), "ggplot")

  ce <- conditional_effects(fit)
  expect_s3_class(ce, "mvgam_conditional_effects")
  expect_gt(length(ce), 0L)
  for (eff in names(ce)) {
    d <- ce[[eff]]$data
    expect_true(all(is.finite(d$estimate)))
    # An interval drawn the wrong way round renders as a ribbon of
    # the right shape around the right line.
    expect_true(all(d$conf.low <= d$estimate))
    expect_true(all(d$estimate <= d$conf.high))
    # A conditional effect on the response scale is a proportion.
    expect_true(all(d$estimate > 0 & d$estimate < 1))
  }
})


test_that("the draws and the tidiers keep this fit's row order", {
  vars <- variables(fit)
  expect_true(any(grepl("^Z(_tilde)?\\[", vars)))
  expect_true(is.data.frame(tidy(fit)))
  expect_true(is.data.frame(glance(fit)))
  aug <- augment(fit)
  expect_identical(nrow(aug), nrow(long_dat))
  # Row for row: a tidier that re-sorts pairs each fitted value with
  # another row's observation while every column keeps its length.
  expect_identical(as.character(aug$series),
                   as.character(long_dat$series))
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
  pr <- marginaleffects::predictions(fit, newdata = grid,
                                     type = "response")
  expect_identical(nrow(pr), nrow(grid))
  expect_true(all(pr$estimate > 0 & pr$estimate < 1))

  # `predictions(type = "response")` reports the expected response,
  # which is what `posterior_epred()` returns. What comes back is the
  # median of the posterior predictive instead: on a count family
  # that is a whole number and obvious, and on a bounded continuous
  # one it is a plausible proportion a little away from the mean,
  # which is why only a comparison catches it here.
  #
  # Both sides average the whole posterior rather than a subsample,
  # so the tolerance is about the two routes agreeing rather than
  # about how many draws each happened to take.
  ep <- colMeans(posterior_epred(fit, newdata = grid))
  expect_equal(as.numeric(pr$estimate), as.numeric(ep),
               tolerance = 0.01)
  # `env * series` gives each species its own slope; a design that
  # dropped the interaction returns one slope shared by all five.
  slopes <- vapply(species_levels, function(s) {
    e <- pr$estimate[grid$series == s]
    e[2L] - e[1L]
  }, numeric(1))
  expect_gt(stats::sd(slopes), 1e-6)
})

cat("\n=== Sampler diagnostics ===\n")
diag_df <- nuts_params(fit$fit)
n_div <- sum(subset(diag_df, Parameter == "divergent__")$Value)
n_total <- nrow(subset(diag_df, Parameter == "divergent__"))
cat(sprintf("Divergent transitions: %d / %d (%.2f%%)\n",
            n_div, n_total, 100 * n_div / n_total))

cat("\nDone.\n")
