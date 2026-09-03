# Recovery fixture for occ() + factor-model JSDM (imperfect-
# detection track). Mirrors jsdgam_mv_nmix.R but with the single-
# season Bernoulli-binomial occupancy likelihood, where the latent
# state is binary occupancy z[s, i] instead of count N[s, i]:
#
#   lv[i, :] = s(env[i], by = lv_axis())      # env-tracking factors
#                ↓ via Z[s, :]
#   logit_psi[s, i] = b_int[s] + Z %*% lv     # latent log-odds of
#                                             # occupancy
#                ↓ via per-(species, site) Bernoulli marginalisation
#   y[s, i, v] ~ Bernoulli(z[s, i] * p)       # observed detections
#                                             # (z marginalised out)
#
# Site count bumped to 50 (vs the 30 used by nmix). Single-bit
# detection per visit carries less information per closure unit than
# a Poisson count, so the recovery threshold on cor is lowered to
# 0.5, mirroring the categorical fixture's reasoning.
#
# Primary go/no-go: cor(off_diag(true_ZZ'), off_diag(post_ZZ')) > 0.5
# Diagnostics:
#   - MAE on Z Z' off-diagonals
#   - max|colSums(posterior_mean(Z))| (free Z under Heaps QR)
#   - posterior mean detection probability vs truth
#   - posterior mean occupancy probability vs truth
#   - divergent transitions + max-treedepth saturation
#   - min / max bulk ESS on Z entries
#
# Cached at tests/local/fixtures/val_mvgam_jsdgam_mv_occ.rds. Delete
# to refit. Runtime ~5-8 min.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
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

set.seed(607L)

K <- 4L
N_lv <- 2L
n_sites <- 50L
n_visits <- 3L
threshold_cor <- 0.5
species_levels <- paste0("sp", seq_len(K))

env <- sort(runif(n_sites, -2, 2))

lv_true <- cbind(sin(env), env^2 - mean(env^2))
lv_true <- scale(lv_true, center = TRUE, scale = apply(lv_true, 2L, sd))

Z_true <- matrix(rnorm(K * N_lv, sd = 1.0), nrow = K, ncol = N_lv)
Z_true <- scale(Z_true, center = TRUE, scale = FALSE)
attr(Z_true, "scaled:center") <- NULL

# Per-species occupancy intercept on the logit scale. Centred around
# 0 so each species occupies roughly half the sites on average.
b_int <- rnorm(K, mean = 0, sd = 0.5)

# Truth on the latent occupancy: logit_psi[s, i] = b_int[s] + Z %*% lv.
logit_psi <- matrix(NA_real_, nrow = K, ncol = n_sites)
for (s in seq_len(K)) {
  for (i in seq_len(n_sites)) {
    logit_psi[s, i] <- b_int[s] +
      sum(Z_true[s, ] * lv_true[i, ])
  }
}
psi_true <- 1 / (1 + exp(-logit_psi))
z_latent <- matrix(rbinom(K * n_sites, 1L, psi_true),
                   nrow = K, ncol = n_sites)

p_true <- 0.6

# Sites are numbered from 3, so a site's identifier never equals its
# own rank. `unit = site` makes these the fit's occasions, and on a
# `1..n` numbering a unit index and a unit label are indistinguishable.
site_ids <- seq_len(n_sites) + 2L

rows <- list()
for (s in seq_len(K)) {
  for (i in seq_len(n_sites)) {
    for (v in seq_len(n_visits)) {
      rows[[length(rows) + 1L]] <- data.frame(
        species = species_levels[s],
        site    = site_ids[i],
        env     = env[i],
        visit   = v,
        y       = if (z_latent[s, i] == 1L) {
          rbinom(1L, 1L, p_true)
        } else {
          0L
        }
      )
    }
  }
}
dat <- do.call(rbind, rows)
dat$species <- factor(dat$species, levels = species_levels)

sigma_true_cov <- tcrossprod(Z_true)
sigma_true_cor <- cov2cor(sigma_true_cov + diag(1e-8, K))

cat("Simulated", nrow(dat), "rows: K =", K, "species,",
    n_sites, "sites,", n_visits, "visits.\n")
cat("Closure units = species x sites =", K * n_sites, ".\n")
cat("Mean true psi =", round(mean(psi_true), 3),
    "| naive detection rate =",
    round(mean(dat$y), 3), ".\n")
cat("True cor off-diag range: [",
    round(min(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), ",",
    round(max(sigma_true_cor[upper.tri(sigma_true_cor)]), 3), "].\n")

# The generative truth rides on the saved fit so a separate test can
# assert recovery against it without repeating the simulation.
sim_truth <- list(
  K = K, N_lv = N_lv, species_levels = species_levels,
  Z_true = Z_true, lv_true = lv_true, b_int = b_int,
  psi_true = psi_true, p_true = p_true, z_latent = z_latent,
  sigma_true_cov = sigma_true_cov, sigma_true_cor = sigma_true_cor,
  env = env
)

cache <- local_fixture_path("val_mvgam_jsdgam_mv_occ.rds")
if (file.exists(cache)) {
  cat("[cache] Loading occ recovery fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] jsdgam(occ(), n_lv = 2, env-driven factors,",
      n_sites, "sites)\n", sep = " ")
  fit <- jsdgam(
    formula        = y ~ species,
    factor_formula = ~ s(env, by = lv_axis(), k = 5) - 1,
    data           = dat,
    unit           = site, species = species,
    family         = occ(),
    n_lv           = N_lv,
    chains         = 2L,
    iter           = 1000L, warmup = 500L,
    silent         = 2,
    backend        = "cmdstanr"
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

# The go/no-go above printed a verdict and checked nothing. It is
# asserted below, together with the structure the recovery number
# cannot see: the off-diagonals are compared as a set, so a fit that
# hands each species another species' latent column scores the same.
# This fixture is the closure-unit case -- a (species, site) cell with
# three visits -- so the grain the answers come back at is asserted
# too, since a per-visit answer and a per-unit answer are both
# plausible shapes and only one is right.

library(testthat)

test_that("the residual correlation recovers the simulated one", {
  expect_gt(cor_off, threshold_cor)
  expect_lt(mae_off, 0.6)
})


test_that("the species axis is the four simulated species, in order", {
  axes <- mvgam:::mvgam_axes(fit)
  expect_identical(as.character(axes$series$levels), species_levels)
  expect_identical(as.integer(axes$series$n), K)
  expect_identical(as.integer(fit$standata$N_lv_trend), N_lv)
  # `by = lv_axis()` puts the trend design on the factor axis rather
  # than the series axis, and that is what `times_trend` is indexed
  # by; reading it at the series grain is a silent mis-index.
  expect_identical(axes$grain, "lv")
})


test_that("the closure units are the species-site cells", {
  # A closure unit is a (species, site) cell with three visits, not a
  # species and not a row. The fit has to count them the way the
  # simulation built them, or the detection marginalisation is being
  # taken over the wrong grouping.
  expect_identical(as.integer(fit$standata$N_unit), K * n_sites)
  units <- unique(dat[, c("species", "site")])
  expect_identical(nrow(units), K * n_sites)
  expect_identical(as.integer(fit$standata$N), nrow(dat))
})


test_that("the trend design is split by factor, on one shared basis", {
  # This fixture is the second `by = lv_axis()` case, so the same
  # fingerprint has to hold here as on a plain gaussian panel: the
  # design is block-complementary across the factors, evaluated at
  # one shared basis. Read off the data Stan is handed rather than
  # off the program text, since a design built on the series axis and
  # relabelled produces correct dimensions and wrong content.
  sd <- fit$standata
  expect_identical(as.integer(sd$N_trend), n_sites * N_lv)
  expect_identical(dim(sd$times_trend), c(n_sites, N_lv))
  r1 <- as.integer(sd$times_trend[, 1L])
  r2 <- as.integer(sd$times_trend[, 2L])
  expect_length(intersect(r1, r2), 0L)
  expect_identical(sort(c(r1, r2)), seq_len(n_sites * N_lv))

  X <- sd$Xs_trend
  expect_identical(nrow(X), n_sites * N_lv)
  expect_identical(ncol(X), N_lv)
  expect_true(all(X[r1, 2L] == 0))
  expect_true(all(X[r2, 1L] == 0))
  # One smooth split two ways: the same covariate value reaches both
  # factors, in their own columns.
  expect_equal(unname(X[r1, 1L]), unname(X[r2, 2L]))

  zs <- grep("^Zs_[0-9]+_[0-9]+_trend$", names(sd), value = TRUE)
  expect_length(zs, N_lv)
  Z1 <- sd$Zs_1_1_trend
  Z2 <- sd$Zs_2_1_trend
  expect_identical(dim(Z1), dim(Z2))
  expect_true(all(Z1[r2, ] == 0))
  expect_true(all(Z2[r1, ] == 0))
  expect_equal(unname(Z1[r1, ]), unname(Z2[r2, ]))
  expect_equal(as.integer(sd$knots_1_trend),
               as.integer(sd$knots_2_trend))
})


test_that("an observation still reads a species cell, not a factor", {
  # `times_trend` moved to the factor axis, but `trend[t, s]` stays
  # species-grained because the program folds through `Z`. So
  # `obs_trend_series` runs over the four species and not the two
  # factors. Running it over factors stays in range, samples, and
  # silently gives four species two states.
  s_rec <- as.integer(fit$standata$obs_trend_series)
  expect_identical(sort(unique(s_rec)), seq_len(K))
  train <- as.data.frame(fit$obs_data)
  expect_identical(s_rec,
                   match(as.character(train$species), species_levels))
  # Every species carries the same number of rows: sites x visits.
  expect_identical(as.integer(table(s_rec)),
                   rep(n_sites * n_visits, K))
})


test_that("the axis maps a newdata frame without touching the draws", {
  # Draw-free throughout: which species a row belongs to, and which
  # occasions a frame supplies, come from the record alone.
  train <- as.data.frame(fit$obs_data)
  ids <- mvgam:::axis_row_series(fit, train)
  expect_identical(levels(ids), species_levels)
  expect_identical(as.character(ids), as.character(train$species))

  set.seed(41L)
  perm <- sample(nrow(train))
  expect_identical(
    as.character(
      mvgam:::axis_row_series(fit, train[perm, , drop = FALSE])
    ),
    as.character(train$species)[perm]
  )

  bad <- train
  bad$series <- as.character(bad$series)
  bad$series[bad$series == species_levels[2L]] <- "sp_ghost"
  err <- expect_error(
    mvgam:::validate_prediction_factor_levels(bad, fit$trend_metadata)
  )
  expect_match(conditionMessage(err), "sp_ghost", fixed = TRUE)

  # The occasions are the sites, and they are numbered from 3. An
  # arm handing back ranks reads 1..50 and is caught here.
  training <- mvgam:::build_training_arms(fit, species_levels)
  expect_identical(names(training$times), species_levels)
  unit_times <- sort(unique(as.integer(train$time)))
  expect_identical(unit_times, site_ids)
  for (s in species_levels) {
    expect_identical(as.integer(training$times[[s]]), unit_times)
  }
  # And the record agrees with the arms about what an occasion is.
  ax <- mvgam:::mvgam_axes(fit)
  expect_identical(as.integer(ax$time$values), unit_times)
  expect_identical(as.integer(ax$time$n), n_sites)
})


test_that("every prediction surface answers at the row grain", {
  n_obs <- nrow(dat)
  ep <- posterior_epred(fit, ndraws = 20L)
  pp <- posterior_predict(fit, ndraws = 20L)
  expect_identical(dim(ep), c(20L, n_obs))
  expect_identical(dim(pp), c(20L, n_obs))
  expect_true(all(is.finite(ep)))
  expect_true(all(is.finite(pp)))
  # Detections are Bernoulli, so a draw is 0 or 1 and nothing else.
  expect_true(all(pp %in% c(0, 1)))
  expect_identical(nrow(predict(fit, ndraws = 20L)), n_obs)
  expect_identical(nrow(fitted(fit, ndraws = 20L)), n_obs)

  # Column j is row j of the frame, so the species the fit resolves
  # for each row must be the species the frame states there.
  #
  # `unit = site, species = species` makes the fit carry its own
  # `time` and `series` columns alongside the ones the simulation
  # wrote, and the prediction path reads those. The training frame
  # the fit kept is therefore what a newdata check has to be given;
  # handing back the frame the user built is refused for want of a
  # `time` column the user never wrote.
  train <- as.data.frame(fit$obs_data)
  os <- mvgam:::get_observation_structure(fit, newdata = train)
  expect_identical(as.character(os$series), as.character(train$species))
  expect_identical(os$series_levels, species_levels)
  expect_identical(as.integer(os$series_int),
                   match(as.character(train$species), species_levels))
  # The unit index runs over sites, and every site is reached.
  expect_identical(sort(unique(as.integer(os$time))), seq_len(n_sites))
})


test_that("the closure-unit prediction types answer per unit", {
  # `latent_state` and `detection` are the two quantities this family
  # exists to separate: occupancy z and detection p. They answer per
  # closure unit, not per visit, and confusing the two grains is what
  # makes an occupancy estimate look like a detection rate.
  ls <- predict(fit, type = "latent_state", ndraws = 20L)
  det <- predict(fit, type = "detection", ndraws = 20L)
  expect_false(is.null(ls))
  expect_false(is.null(det))
  # Occupancy and detection are both probabilities here.
  expect_true(all(as.numeric(det[, "Estimate"]) >= 0 &
                    as.numeric(det[, "Estimate"]) <= 1))
  expect_true(all(as.numeric(ls[, "Estimate"]) >= 0 &
                    as.numeric(ls[, "Estimate"]) <= 1))
})


test_that("each row reads the latent cell the sampler drew for it", {
  # The one comparison that does not ask two derivations whether they
  # agree with each other. `obs_trend_time` and `obs_trend_series` are
  # the cell this fit gave each row; `trend[t, s]` is what it sampled
  # there. A species reading another's column returns a real state of
  # the right shape, so only a value comparison sees it.
  #
  # `unit = site` makes the fit synthesise its own `time` column, so
  # the frame handed back has to be the one the fit kept. Passing the
  # frame the simulation built raises for want of a `time` column the
  # user never wrote, which left this assertion erroring rather than
  # comparing.
  train <- as.data.frame(fit$obs_data)
  dm <- posterior::as_draws_matrix(fit$fit)
  t_rec <- as.integer(fit$standata$obs_trend_time)
  s_rec <- as.integer(fit$standata$obs_trend_series)
  expect_length(t_rec, nrow(train))
  want <- vapply(paste0("trend[", t_rec, ",", s_rec, "]"),
                 function(k) mean(dm[, k]), numeric(1))
  got <- colMeans(
    mvgam:::extract_trend_latent_states(fit, newdata = train,
                                        full_draws = dm)
  )
  expect_equal(unname(got), unname(want))
})


test_that("a shuffled newdata answers the same, in the new order", {
  # A prediction placing rows by position rather than by content
  # agrees with every check that hands back the training frame in its
  # own order, and disagrees here.
  train <- as.data.frame(fit$obs_data)
  set.seed(18L)
  perm <- sample(nrow(train))
  base <- posterior_epred(fit, newdata = train, draw_ids = 1:10,
                          incl_autocor = TRUE)
  shuf <- posterior_epred(fit, newdata = train[perm, , drop = FALSE],
                          draw_ids = 1:10, incl_autocor = TRUE)
  expect_equal(unname(base[, perm, drop = FALSE]), unname(shuf))
})


test_that("a newdata holding one species reads that species' state", {
  # Taking every row of one species keeps whole closure units, so the
  # subset is a legal frame. `droplevels()` leaves it carrying only
  # its own species, which is what a real subset does; a species
  # index read off the levels present then numbers it 1 whatever it
  # is, and it reads the first species' latent column.
  train <- as.data.frame(fit$obs_data)
  full <- posterior_epred(fit, newdata = train, draw_ids = 1:10,
                          incl_autocor = TRUE)
  for (s in species_levels) {
    rows <- which(as.character(train$species) == s)
    sub <- train[rows, , drop = FALSE]
    sub$species <- droplevels(factor(sub$species))
    sub$series <- droplevels(factor(sub$series))
    expect_identical(levels(sub$series), s)
    got <- posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                           incl_autocor = TRUE)
    expect_equal(unname(got), unname(full[, rows, drop = FALSE]))
  }
})


test_that("a newdata naming an unknown species is refused", {
  train <- as.data.frame(fit$obs_data)
  nd <- train
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
  # The latent-state hindcast answers at the closure-unit grain
  # rather than per series, so it carries a draw matrix and a unit
  # index instead of an arm per species. There are as many units as
  # (species, site) cells, and the labels have to name them: a unit
  # index that is right while its labels are permuted attributes
  # every occupancy estimate to another cell.
  ls <- hindcast(fit, ndraws = 20L, type = "latent_state")
  expect_true(all(c("draws", "unit") %in% names(ls)))
  expect_identical(NROW(ls$unit), K * n_sites)
  expect_identical(ncol(ls$draws), K * n_sites)
  expect_setequal(as.character(unique(ls$unit$series)), species_levels)
})


test_that("the detection probability recovers the simulated one", {
  # Printed and unchecked, so a detection probability that had run to
  # 0 or 1 would have been reported without comment.
  draws <- as_draws_matrix(fit$fit)
  p_cols <- grep("^b_p_Intercept$|^Intercept_p$|^p$", colnames(draws),
                 value = TRUE)
  expect_gt(length(p_cols), 0L)
  p_post <- as.numeric(draws[, p_cols[1L]])
  p_resp <- if (grepl("^b_|^Intercept", p_cols[1L])) {
    1 / (1 + exp(-p_post))
  } else {
    p_post
  }
  expect_true(all(p_resp > 0 & p_resp < 1))
  expect_lt(abs(mean(p_resp) - p_true), 0.2)
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


test_that("the env smooth is drawn once per latent factor", {
  # `s(env, by = lv_axis())` is the only smooth here, and it is
  # indexed by latent factor rather than by species. Checking merely
  # that the call returns leaves the two ways it can be wrong
  # untouched: an empty grid, which comes back correctly named with
  # no rows in it, and a grid built over the observation frame, which
  # carries no `.trend` column and cannot separate the factors.
  sm <- smooths(fit)
  expect_length(sm, 1L)
  expect_match(sm[1L], ".trend", fixed = TRUE)

  # Evaluated over the trend grid: one row per (unit, factor).
  ps <- posterior_smooths(fit, smooth = sm[1L], ndraws = 20L)
  expect_identical(dim(ps), c(20L, n_sites * N_lv))
  expect_true(all(is.finite(ps)))

  cs <- conditional_smooths(fit)
  expect_length(cs, 1L)
  # The frame comes back directly rather than under `$data`, which is
  # where `conditional_effects()` puts it.
  d <- cs[[1L]]
  expect_s3_class(d, "data.frame")
  expect_gt(nrow(d), 0L)
  expect_true(all(is.finite(d$estimate__)))
  expect_true(all(d$lower__ <= d$estimate__))
  expect_true(all(d$estimate__ <= d$upper__))

  # One curve per latent factor, and the two differ: a design that
  # collapsed the factor axis draws one shape twice.
  curves <- split(d$estimate__, d$cond__)
  expect_length(curves, N_lv)
  expect_false(isTRUE(all.equal(curves[[1L]], curves[[2L]])))

  # Drawn over the covariate the trend side actually saw.
  trend_env <- fit$trend_model$data$env
  expect_gte(min(d$effect1__), min(trend_env) - 1e-8)
  expect_lte(max(d$effect1__), max(trend_env) + 1e-8)
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


test_that("pp_check and the plotting methods render", {
  expect_s3_class(pp_check(fit, ndraws = 20L), "ggplot")
  expect_s3_class(pp_check(fit, type = "resid_qq", ndraws = 20L), "ggplot")
  for (ty in c("trend", "factors")) {
    # `plot()` returns a ggplot, so that is what is asserted. The
    # alternation this replaced ended in `is.list(p)`, which an empty
    # list satisfies: any method returning `list()` passed it.
    p <- plot(fit, type = ty)
    expect_s3_class(p, "ggplot")
  }
  expect_s3_class(mcmc_plot(fit), "ggplot")
})


test_that("conditional_effects is drawn with ordered intervals", {
  ce <- conditional_effects(fit)
  expect_s3_class(ce, "mvgam_conditional_effects")
  expect_gt(length(ce), 0L)
  for (eff in names(ce)) {
    d <- ce[[eff]]$data
    expect_true(all(c("estimate", "conf.low", "conf.high") %in% names(d)))
    expect_true(all(is.finite(d$estimate)))
    expect_true(all(d$conf.low <= d$estimate))
    expect_true(all(d$estimate <= d$conf.high))
  }
})


test_that("marginaleffects reaches this closure-unit fit", {
  withr::local_options(marginaleffects_model_classes = "mvgam")
  ap <- marginaleffects::avg_predictions(fit)
  expect_true(all(is.finite(ap$estimate)))
})

cat("\n=== Mode-1 diagnostic (Z column sums) ===\n")
draws <- as_draws_matrix(fit$fit)
Z_arr <- mvgam:::extract_Z_loadings(
  draws, n_obs_series = K, n_lv = N_lv
)
Z_mean <- apply(Z_arr, c(2L, 3L), mean)
col_sums_abs <- abs(colSums(Z_mean))
cat(sprintf("max|colSums(posterior_mean(Z))| = %.4f\n",
            max(col_sums_abs)))
cat("(Free Z under Heaps QR; small but not pinned to 0.)\n")

cat("\n=== Detection probability recovery ===\n")
p_cols <- grep("^b_p_Intercept$|^Intercept_p$|^p$", colnames(draws),
               value = TRUE)
if (length(p_cols) > 0L) {
  p_post <- as.numeric(draws[, p_cols[1L]])
  p_post_resp <- if (grepl("^b_", p_cols[1L]) ||
                       grepl("^Intercept", p_cols[1L])) {
    1 / (1 + exp(-p_post))
  } else {
    p_post
  }
  cat(sprintf("p posterior mean = %.3f  (truth = %.2f)\n",
              mean(p_post_resp), p_true))
} else {
  cat("WARN: detection probability column not located in posterior.\n")
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
