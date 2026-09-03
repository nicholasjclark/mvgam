# Recovery fixture for the multi() Multinomial family.
#
# Simulates K = 4 species at n_sites = 30 sites under a known
# rank-2 factor loading Z_true (column-centred to match Stan's
# sum_to_zero_vector identification), softmax-transforms to
# category probabilities per site, then draws Multinomial counts
# with per-site total N_i sampled in [40, 80].
#
# Primary go/no-go:
#   cor(off_diag(true_cor), off_diag(post_cor)) > 0.7
#
# where true_cor = cov2cor(Z_true %*% t(Z_true)) and post_cor
# is residual_cor(fit)$cor from the factor-loadings branch.
#
# Diagnostics reported:
#   - MAE on off-diagonals
#   - max|colSums(posterior_mean(Z))| (mode-1 health check)
#   - divergent transitions + max-treedepth saturation
#   - min / max bulk ESS on Z entries
#
# Cached at tests/local/fixtures/val_mvgam_jsdgam_mv_multi.rds.
# Delete to refit. Runtime ~3-5 min.

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

set.seed(602L)

K <- 4L
N_lv <- 2L
n_sites <- 30L
threshold_cor <- 0.7
species_levels <- paste0("y", seq_len(K))

Z_true <- matrix(rnorm(K * N_lv, sd = 0.7), nrow = K, ncol = N_lv)
Z_true <- scale(Z_true, center = TRUE, scale = FALSE)
attr(Z_true, "scaled:center") <- NULL

sigma_true_cov <- tcrossprod(Z_true)
sigma_true_cor <- cov2cor(sigma_true_cov + diag(1e-8, K))

env <- rnorm(n_sites)
mu_intercept <- rnorm(K)
mu_env_slope <- rnorm(K)

Y_wide <- matrix(NA_integer_, nrow = n_sites, ncol = K)
N_per_site <- sample(40:80, n_sites, replace = TRUE)
for (i in seq_len(n_sites)) {
  lv_i <- rnorm(N_lv)
  eta_i <- mu_intercept + mu_env_slope * env[i] +
             as.numeric(Z_true %*% lv_i)
  p_i <- exp(eta_i) / sum(exp(eta_i))
  Y_wide[i, ] <- as.vector(rmultinom(1L, N_per_site[i], p_i))
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
    time   = site,
    y      = as.integer(y)
  ) |>
  arrange(time, series)

cat("Simulated", n_sites, "sites x", K, "species. True cor off-diag",
    "range: [",
    round(min(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), ",",
    round(max(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), "].\n")

# The generative truth rides on the saved fit so a separate test can
# assert recovery against it without repeating the simulation.
sim_truth <- list(
  K = K, N_lv = N_lv, species_levels = species_levels,
  Z_true = Z_true, N_per_site = N_per_site,
  sigma_true_cov = sigma_true_cov, sigma_true_cor = sigma_true_cor,
  mu_intercept = mu_intercept, mu_env_slope = mu_env_slope, env = env
)

cache <- local_fixture_path("val_mvgam_jsdgam_mv_multi.rds")
if (file.exists(cache)) {
  cat("[cache] Loading multi recovery fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] jsdgam(multi(), n_lv = 2, ", n_sites, " sites)\n", sep = "")
  fit <- jsdgam(
    formula = y ~ env * series,
    factor_formula = ~ -1,
    data = as.data.frame(long_dat),
    unit = time, species = series,
    family = multi(),
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

cat("\n=== Mode-1 diagnostic (Z column sums) ===\n")
draws <- as_draws_matrix(fit$fit)
Z_arr <- mvgam:::extract_Z_loadings(
  draws, n_obs_series = K, n_lv = N_lv
)
Z_mean <- apply(Z_arr, c(2L, 3L), mean)
col_sums_abs <- abs(colSums(Z_mean))
cat(sprintf("max|colSums(posterior_mean(Z))| = %.4f\n",
            max(col_sums_abs)))
cat("(Hard sum_to_zero_vector pins this to ~0 by construction.)\n")

# The go/no-go printed above was checked by nobody. It is asserted
# here, with the structure a recovery correlation cannot see: the
# off-diagonals are compared as a set, so a fit handing each species
# another species' latent column scores exactly the same. The
# multinomial total is the sharper claim, and nothing checked it.

library(testthat)

test_that("the residual correlation recovers the simulated one", {
  expect_gt(cor_off, threshold_cor)
  expect_lt(mae_off, 0.5)
})


test_that("the loadings obey the sum-to-zero identification", {
  # The header calls this pinned by construction, and a softmax is
  # invariant to a constant added across species, so an unpinned Z
  # leaves the model identified only up to that shift.
  expect_lt(max(col_sums_abs), 1e-6)
})


test_that("the species axis is the four simulated species, in order", {
  axes <- mvgam:::mvgam_axes(fit)
  expect_identical(as.character(axes$series$levels), species_levels)
  expect_identical(as.integer(axes$series$n), K)
  expect_identical(as.integer(fit$standata$N_lv_trend), N_lv)
  expect_identical(as.integer(fit$standata$N_series_trend), K)
})


test_that("the simulated counts really are multinomial per site", {
  # The fixture's own premise, checked rather than assumed.
  per_site <- tapply(long_dat$y, long_dat$time, sum)
  expect_identical(as.integer(per_site), as.integer(N_per_site))
  expect_true(all(long_dat$y >= 0L))
})


test_that("every prediction surface answers for every row", {
  n_obs <- nrow(long_dat)
  ep <- posterior_epred(fit, ndraws = 20L)
  pp <- posterior_predict(fit, ndraws = 20L)
  expect_identical(dim(ep), c(20L, n_obs))
  expect_identical(dim(pp), c(20L, n_obs))
  expect_true(all(is.finite(ep)))
  expect_true(all(is.finite(pp)))
  # Counts, so non-negative whole numbers.
  expect_true(all(pp >= 0))
  expect_true(all(pp == floor(pp)))
  expect_true(all(ep >= 0))
  expect_identical(nrow(predict(fit, ndraws = 20L)), n_obs)
  expect_identical(nrow(fitted(fit, ndraws = 20L)), n_obs)

  # Column j is row j of the frame, so the species the fit resolves
  # for each row must be the species the frame states there.
  d <- as.data.frame(long_dat)
  os <- mvgam:::get_observation_structure(fit, newdata = d)
  expect_identical(as.character(os$series), as.character(d$series))
  expect_identical(os$series_levels, species_levels)
  expect_identical(as.integer(os$series_int),
                   match(as.character(d$series), species_levels))
})


test_that("a predicted composition keeps each site's own total", {
  # The claim that separates a multinomial from four independent
  # count models: the species at a site share one trial total, so a
  # draw has to sum to that site's total exactly. Modelled as
  # separate Poissons every draw is still a non-negative whole
  # number of the right shape, and only this sum notices.
  d <- as.data.frame(long_dat)
  pp <- posterior_predict(fit, ndraws = 20L)
  for (i in seq_len(nrow(pp))) {
    draw_sums <- tapply(pp[i, ], d$time, sum)
    expect_identical(as.integer(draw_sums), as.integer(N_per_site))
  }
  # And the expectation carries the same total.
  ep <- posterior_epred(fit, ndraws = 20L)
  ep_sums <- tapply(colMeans(ep), d$time, sum)
  expect_equal(as.numeric(ep_sums), as.numeric(N_per_site),
               tolerance = 1e-6)
})


test_that("each row reads the latent cell the sampler drew for it", {
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
  # agrees with every check that hands back the training frame in its
  # own order, and disagrees here.
  d <- as.data.frame(long_dat)
  set.seed(17L)
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
  nd$y <- NA_integer_
  for (ty in c("link", "expected", "trend")) {
    fc <- forecast(fit, newdata = nd, ndraws = 20L, type = ty)
    expect_s3_class(fc, "mvgam_forecast")
    expect_identical(names(fc$forecasts), species_levels)
    for (s in species_levels) {
      expect_identical(dim(fc$forecasts[[s]]), c(20L, h))
      expect_true(all(is.finite(fc$forecasts[[s]])))
    }
  }
})


test_that("residual_cor is labelled by the species axis", {
  expect_identical(rownames(post_cor), species_levels)
  expect_identical(colnames(post_cor), species_levels)
  expect_equal(unname(diag(post_cor)), rep(1, K))
  expect_equal(unname(post_cor), unname(t(post_cor)))
})


test_that("the factor methods report two factors over four species", {
  af <- active_factors(fit)
  expect_s3_class(af, "mvgam_active_factors")
  expect_identical(as.integer(af$n_lv), N_lv)
  expect_identical(nrow(af$per_factor), N_lv)

  sv <- shared_variation(fit)
  expect_s3_class(sv, "mvgam_shared_variation")
  expect_identical(as.character(sv$series_names), species_levels)
  expect_identical(as.integer(sv$n_series), K)
  expect_identical(as.integer(sv$n_lv), N_lv)

  expect_identical(dim(Z_arr)[2:3], c(K, N_lv))
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
  expect_true(any(grepl("Series:\\s*4", txt)))
  ll <- log_lik(fit, ndraws = 20L)
  expect_true(all(is.finite(ll)))
  ic <- loo(fit)
  expect_s3_class(ic, "loo")
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
})


test_that("pp_check, plotting and conditional_effects render", {
  expect_s3_class(pp_check(fit, ndraws = 20L), "ggplot")
  for (ty in c("trend", "factors")) {
    # `plot()` returns a ggplot, so that is what is asserted. The
    # alternation this replaced ended in `is.list(p)`, which an empty
    # list satisfies: any method returning `list()` passed it.
    p <- plot(fit, type = ty)
    expect_s3_class(p, "ggplot")
  }
  ce <- conditional_effects(fit)
  expect_s3_class(ce, "mvgam_conditional_effects")
  for (eff in names(ce)) {
    d <- ce[[eff]]$data
    expect_true(all(is.finite(d$estimate)))
    expect_true(all(d$conf.low <= d$estimate))
    expect_true(all(d$estimate <= d$conf.high))
  }
})


test_that("the tidiers keep this fit's row order", {
  aug <- augment(fit)
  expect_identical(nrow(aug), nrow(long_dat))
  expect_identical(as.character(aug$series),
                   as.character(long_dat$series))
  expect_equal(as.numeric(aug$.observed), as.numeric(long_dat$y))
  expect_true(is.data.frame(tidy(fit)))
  expect_true(is.data.frame(glance(fit)))
})

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
