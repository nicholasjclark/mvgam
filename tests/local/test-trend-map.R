# Recovery and post-fit coverage for a fixed-loading `trend_map`,
# fitted in this file.
#
# `trend_map` is the only route on which the loadings reach Stan as
# data rather than as a parameter. Everywhere else `Z` is sampled, so
# its rows carry no labels until a fit exists and the row order can
# only be checked against declaration text. Here it can be checked on
# values, which makes this the one place two claims are answerable:
#
#   row `k` of `Z` holds the loadings the user wrote for the series
#   that occupies trend column `k`, and
#
#   `trend[t, s]` really is `Z[s, ] . lv_trend[t, ]`.
#
# The second is the axis chain itself. Every other file has to take
# it on trust because `Z` is a parameter; here both sides are in the
# draws and the identity holds exactly or not at all.
#
# The prefit forms a `trend_map` can take -- a frame, a matrix, a
# partial matrix, the "shared" and "identity" spellings, and the
# refusals -- are covered across ten cells of
# `tests/testthat/test-axis-ordering.R`, which needs no sampling.
# This file does not repeat them.
#
#   truth: 4 series on 50 occasions loading 2 latent AR(1) factors
#          through a known Z with four distinct rows, poisson
#   model: y ~ 1, trend_formula = ~ -1 + AR(p = 1, trend_map = Z)
#
# Series are declared out of alphabetical order, because the map is
# reordered by the series levels before it reaches Stan: if that
# reorder disagrees with the axis `obs_trend_series` indexes, every
# series loads on another's factors and nothing raises.
#
# Run with:
#   testthat::test_file("tests/local/test-trend-map.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

# This file fits its own model and caches it beside itself, so it
# depends on no shared fixture and no build step.
# Resolved from where this file is running rather than from what is
# already on disk. testthat sets the working directory to the test
# file's own, so asking whether `fixtures` exists picks the wrong
# branch on a clean tree and writes tests/local/tests/local/fixtures.
cache_path <- function(name) {
  dir <- if (dir.exists(file.path("tests", "local"))) {
    file.path("tests", "local", "fixtures")
  } else {
    "fixtures"
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}

set.seed(3301L)

n_time <- 50L
time_vals <- seq_len(n_time) + 2L
series_levels <- c("delta", "alpha", "charlie", "bravo")
stopifnot(!identical(series_levels, sort(series_levels)))
n_series <- length(series_levels)
n_lv <- 2L

# Four distinct rows, so no two series load alike and a permutation
# of the rows is a different model rather than a relabelling. Written
# in the order the frame declares its series.
Z_true <- matrix(
  c(1, 0,
    0, 1,
    1, 1,
    1, -1),
  nrow = n_series, byrow = TRUE,
  dimnames = list(series_levels, NULL)
)
stopifnot(nrow(unique(Z_true)) == n_series)

ar_true <- c(0.75, 0.35)
lv_true <- matrix(0, nrow = n_time, ncol = n_lv)
for (t in 2:n_time) {
  lv_true[t, ] <- ar_true * lv_true[t - 1L, ] + rnorm(n_lv, 0, 0.35)
}
latent <- lv_true %*% t(Z_true)

dat <- data.frame(
  time = rep(time_vals, times = n_series),
  series = factor(rep(series_levels, each = n_time),
                  levels = series_levels)
)
dat$y <- rpois(nrow(dat), exp(2.2 + as.numeric(latent)))

obs_formula <- y ~ 1

sim_truth <- list(
  n_time = n_time, n_series = n_series, n_lv = n_lv,
  series_levels = series_levels, time_vals = time_vals,
  Z_true = Z_true, ar_true = ar_true, lv_true = lv_true
)

make_future <- function(h) {
  data.frame(
    time = rep(max(time_vals) + seq_len(h), times = n_series),
    series = factor(rep(series_levels, each = h),
                    levels = series_levels),
    y = NA_integer_
  )
}


# -- Prefit: Z as data, and the permutation that proves it is read ---

prefit <- mvgam(
  formula = obs_formula,
  trend_formula = ~ -1 + AR(p = 1, trend_map = Z_true),
  data = dat, family = poisson(), run_model = FALSE, silent = 2
)


test_that("Z reaches Stan as data, sized by the axis it is read on", {
  sd <- prefit$standata
  z_data <- sd$Z %||% sd$Z_template
  expect_false(is.null(z_data))
  expect_identical(dim(z_data), c(n_series, n_lv))
  expect_identical(as.integer(sd$N_series_trend), n_series)
  expect_identical(as.integer(sd$N_lv_trend), n_lv)

  code <- paste(as.character(stancode(prefit)), collapse = "\n")
  # Declared in the data block rather than the parameter block. The
  # sampled route spells it `Z_raw`, so its absence is what says this
  # model took the fixed path at all.
  expect_true(grepl("matrix[N_series_trend, N_lv_trend] Z;", code,
                    fixed = TRUE))
  expect_false(grepl("Z_raw", code, fixed = TRUE))
})


test_that("each row of Z loads the series occupying its column", {
  # The claim this route exists to make answerable. `Z` arriving
  # value-for-value intact says the matrix survived the trip; it does
  # not say row `k` belongs to the series Stan puts in column `k`.
  # A permutation between those two orders keeps every loading
  # present and every dimension right, and each series simply loads
  # on another's factors.
  #
  # The occupant of a column is read from `obs_trend_series`, which
  # is what the sampler indexes, and the intended row is found by the
  # series' own label. Neither side comes from the normaliser being
  # checked, and the series levels are not in alphabetical order, so
  # a map reordered by sorted labels differs from this.
  sd <- prefit$standata
  z_data <- sd$Z %||% sd$Z_template
  s_idx <- as.integer(sd$obs_trend_series)
  labels <- as.character(dat$series)

  occupant <- vapply(seq_len(n_series), function(k) {
    held <- unique(labels[s_idx == k])
    if (length(held) == 1L) held else NA_character_
  }, character(1))
  expect_false(anyNA(occupant))
  expect_setequal(occupant, series_levels)

  for (k in seq_len(n_series)) {
    expect_equal(
      unname(z_data[k, ]),
      unname(Z_true[occupant[k], ]),
      tolerance = 1e-12
    )
  }
})


test_that("permuting the map's rows emits a different Z", {
  # The mutation that makes the check above mean something. Moving
  # the loadings between series has to move them in the program: a
  # fit that reordered the map back to some canonical order, or that
  # keyed the rows positionally, emits the same `Z` for both maps and
  # every assertion above passes on either.
  Z_swapped <- Z_true[c(2L, 1L, 3L, 4L), , drop = FALSE]
  rownames(Z_swapped) <- series_levels
  swapped <- mvgam(
    formula = obs_formula,
    trend_formula = ~ -1 + AR(p = 1, trend_map = Z_swapped),
    data = dat, family = poisson(), run_model = FALSE, silent = 2
  )
  base_z <- prefit$standata$Z %||% prefit$standata$Z_template
  swap_z <- swapped$standata$Z %||% swapped$standata$Z_template
  expect_identical(dim(swap_z), dim(base_z))
  expect_false(isTRUE(all.equal(unname(base_z), unname(swap_z))))

  # And it differs in exactly the two rows that were exchanged, so a
  # program that scrambled the whole matrix also fails.
  differs <- vapply(seq_len(n_series), function(k) {
    !isTRUE(all.equal(unname(base_z[k, ]), unname(swap_z[k, ])))
  }, logical(1))
  expect_identical(which(differs), c(1L, 2L))
})


test_that("a matrix map keys its rows by the names the user gave", {
  # A matrix carries rownames, and a user who writes them is saying
  # which series each row of loadings belongs to. Reading the rows
  # positionally instead assigns row one to whichever series the
  # frame happens to declare first.
  #
  # This fails today. Rownames are dropped and the rows are taken in
  # position order, and the emitted matrix is then relabelled with
  # the declared levels, so the object asserts the assignment the
  # user asked for while holding another series' numbers. Every
  # series loads on another's factors, nothing raises, and the labels
  # say otherwise. Recorded as finding 16.
  #
  # The same map with its rows named in a different order is a
  # different model, so it has to emit a different `Z`.
  Z_named <- Z_true[c(2L, 1L, 4L, 3L), , drop = FALSE]
  rownames(Z_named) <- series_levels[c(2L, 1L, 4L, 3L)]
  renamed <- mvgam(
    formula = obs_formula,
    trend_formula = ~ -1 + AR(p = 1, trend_map = Z_named),
    data = dat, family = poisson(), run_model = FALSE, silent = 2
  )
  base_z <- prefit$standata$Z %||% prefit$standata$Z_template
  named_z <- renamed$standata$Z %||% renamed$standata$Z_template

  # Row order in the call differs from row order in `Z_true`, but the
  # names say each row still belongs to the same series, so the two
  # programs describe one model and their `Z` must agree.
  expect_equal(unname(named_z), unname(base_z), tolerance = 1e-12)

  # And a rowname the frame never had is a mistake worth reporting
  # rather than dropping.
  Z_stranger <- Z_true
  rownames(Z_stranger) <- c("delta", "alpha", "charlie", "hazel")
  expect_error(
    mvgam(
      formula = obs_formula,
      trend_formula = ~ -1 + AR(p = 1, trend_map = Z_stranger),
      data = dat, family = poisson(), run_model = FALSE, silent = 2
    ),
    "training data"
  )
})


# -- Fit --------------------------------------------------------------

cache <- cache_path("val_mvgam_trend_map.rds")
if (file.exists(cache)) {
  cat("[cache] Loading trend_map fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(AR(p = 1, trend_map = Z), 4 series on 2 factors)\n")
  fit <- mvgam(
    formula = obs_formula,
    trend_formula = ~ -1 + AR(p = 1, trend_map = Z_true),
    data = dat, family = poisson(),
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  )
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}

dm <- posterior::as_draws_matrix(fit$fit)


test_that("the trend is the loadings times the factors, draw by draw", {
  # The axis chain, checked on values. `trend[t, s]` is declared as
  # `Z[s, ] . lv_trend[t, ]`, and because `Z` is data here both sides
  # of that identity are available: the loadings from `standata` and
  # the factors from the draws. Every other file has to take this on
  # trust, since a sampled `Z` puts the thing being checked on both
  # sides of the comparison.
  #
  # A transposed `Z`, a column read as a row, or a factor axis
  # indexed against the series axis all keep every value finite and
  # every dimension right, and all of them break this identity.
  z_data <- fit$standata$Z %||% fit$standata$Z_template
  ks <- unique(round(seq(1, nrow(dm), length.out = 15L)))

  for (k in ks) {
    lv <- matrix(NA_real_, n_time, n_lv)
    for (t in seq_len(n_time)) {
      for (j in seq_len(n_lv)) {
        lv[t, j] <- dm[k, paste0("lv_trend[", t, ",", j, "]")]
      }
    }
    tr <- matrix(NA_real_, n_time, n_series)
    for (t in seq_len(n_time)) {
      for (s in seq_len(n_series)) {
        tr[t, s] <- dm[k, paste0("trend[", t, ",", s, "]")]
      }
    }
    # The draws are stored to finite precision, so the identity is
    # checked to the precision they carry rather than to machine
    # epsilon. Every violation this test exists for -- a transpose, a
    # row read as a column, the wrong axis -- moves entries by order
    # one, not by a billionth.
    expect_equal(unname(tr), unname(lv %*% t(z_data)),
                 tolerance = 1e-6)
  }
})


test_that("the factor axis is narrower than the series axis", {
  # Two factors carrying four series is the whole point of a map, so
  # the draws have to hold one `lv_trend` column per factor and one
  # `trend` column per series. A model that quietly gave every series
  # its own factor fits at least as well and differs only here.
  expect_length(grep("^lv_trend\\[", colnames(dm)), n_time * n_lv)
  expect_length(grep("^trend\\[", colnames(dm)), n_time * n_series)
  expect_length(grep("^ar1_trend\\[", colnames(dm)), n_lv)
})


test_that("Z is not a posterior parameter on the fixed route", {
  expect_length(grep("^Z_raw", colnames(dm)), 0L)
  expect_length(grep("^Z\\[", colnames(dm)), 0L)
  # And the record keeps the matrix the user supplied, so post-fit
  # can project without rebuilding it.
  expect_false(is.null(fit$trend_metadata$fixed_Z))
  expect_equal(unname(fit$trend_metadata$fixed_Z), unname(Z_true),
               tolerance = 1e-12)
  expect_identical(as.integer(fit$trend_metadata$n_lv), n_lv)
})


test_that("each row reads the latent cell the sampler drew for it", {
  t_rec <- as.integer(fit$standata$obs_trend_time)
  s_rec <- as.integer(fit$standata$obs_trend_series)
  expect_length(t_rec, nrow(dat))
  want <- vapply(paste0("trend[", t_rec, ",", s_rec, "]"),
                 function(k) mean(dm[, k]), numeric(1))
  got <- colMeans(
    mvgam:::extract_trend_latent_states(fit, newdata = dat,
                                        full_draws = dm)
  )
  expect_equal(unname(got), unname(want))
})


test_that("series sharing no factor weight do not share a state", {
  # `delta` loads (1, 0) and `alpha` loads (0, 1), so their states
  # are different linear combinations and cannot coincide. A collapsed
  # axis gives every series one column and passes every shape check.
  state_of <- function(s) {
    k <- match(s, series_levels)
    vapply(seq_len(n_time), function(t) {
      mean(dm[, paste0("trend[", t, ",", k, "]")])
    }, numeric(1))
  }
  states <- lapply(series_levels, state_of)
  names(states) <- series_levels
  for (i in seq_along(states)) {
    for (j in seq_along(states)) {
      if (j <= i) next
      expect_false(isTRUE(all.equal(states[[i]], states[[j]])))
    }
  }
  # `charlie` is the sum of the two factors and `bravo` their
  # difference, so those two states sum to twice `delta`'s.
  expect_equal(states[["charlie"]] + states[["bravo"]],
               2 * states[["delta"]], tolerance = 1e-6)
})


test_that("every prediction surface answers for every row", {
  n_obs <- nrow(dat)
  ep <- posterior_epred(fit, ndraws = 20L)
  pp <- posterior_predict(fit, ndraws = 20L)
  expect_identical(dim(ep), c(20L, n_obs))
  expect_identical(dim(pp), c(20L, n_obs))
  expect_true(all(is.finite(ep)) && all(ep > 0))
  expect_true(all(pp >= 0) && all(pp == floor(pp)))
  expect_identical(nrow(fitted(fit, ndraws = 20L)), n_obs)
  expect_identical(nrow(residuals(fit, ndraws = 20L)), n_obs)

  os <- mvgam:::get_observation_structure(fit, newdata = dat)
  expect_identical(as.character(os$series), as.character(dat$series))
  expect_identical(os$series_levels, series_levels)
})


test_that("the hindcast reads the same cells as the conditional epred", {
  hc <- hindcast(fit, type = "expected")
  blocks <- hc$hindcasts
  expect_identical(names(blocks), series_levels)
  cells <- unlist(lapply(names(blocks), function(s) {
    rows <- which(as.character(dat$series) == s)
    rows[order(dat$time[rows])]
  }))
  ep <- posterior_epred(fit, incl_autocor = TRUE)
  expect_equal(unname(ep[, cells, drop = FALSE]),
               unname(do.call(cbind, blocks)))
})


test_that("the forecast steps in factor space and projects through Z", {
  # The recursion runs on `lv_trend`, which has two columns, and the
  # result reaches four series through `Z`. So the one-step forecasts
  # inherit the loadings' structure: `charlie` and `bravo` still sum
  # to twice `delta`, whatever the innovations did.
  h <- 3L
  fc <- forecast(fit, newdata = make_future(h), ndraws = NULL,
                 type = "trend")
  expect_identical(names(fc$forecasts), series_levels)
  for (s in series_levels) {
    expect_identical(dim(fc$forecasts[[s]])[2L], h)
    expect_true(all(is.finite(fc$forecasts[[s]])))
  }
  first <- vapply(series_levels, function(s) {
    mean(fc$forecasts[[s]][, 1L])
  }, numeric(1))
  expect_equal(first[["charlie"]] + first[["bravo"]],
               2 * first[["delta"]], tolerance = 0.1)

  # And a four-series forecast built from two factors cannot have
  # four independent trajectories: `delta` and `alpha` fix the other
  # two entirely.
  expect_equal(first[["charlie"]] - first[["bravo"]],
               2 * first[["alpha"]], tolerance = 0.1)
})


test_that("the forecast object is keyed, ordered and in user units", {
  h <- 4L
  future_times <- max(time_vals) + seq_len(h)
  fc <- forecast(fit, newdata = make_future(h), ndraws = 20L,
                 type = "link")
  expect_identical(as.character(fc$series_names), series_levels)
  for (s in series_levels) {
    expect_identical(as.integer(fc$test_times[[s]]), future_times)
    expect_identical(as.integer(fc$train_times[[s]]), time_vals)
    rows <- which(as.character(dat$series) == s)
    rows <- rows[order(dat$time[rows])]
    expect_equal(as.numeric(fc$train_observations[[s]]),
                 as.numeric(dat$y[rows]))
  }
})


test_that("residual_cor is labelled by the series axis", {
  rc <- residual_cor(fit)
  expect_identical(rownames(rc$cor), series_levels)
  expect_identical(colnames(rc$cor), series_levels)
  expect_equal(unname(diag(rc$cor)), rep(1, n_series))
  expect_equal(unname(rc$cor), unname(t(rc$cor)))

  # Two factors behind four series is a rank-deficient correlation,
  # which is what a factor model produces and a per-series trend does
  # not.
  ev <- eigen(rc$cor, only.values = TRUE)$values
  expect_gt(sum(ev > 1e-6), 0L)
})


test_that("criticism and plotting run on a fixed-Z fit", {
  loo_warnings <- character(0)
  ic <- withCallingHandlers(
    loo(fit),
    warning = function(w) {
      loo_warnings <<- c(loo_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  pareto_k <- ic$diagnostics$pareto_k
  expect_true(all(is.finite(pareto_k)))
  expect_identical(
    any(pareto_k > 0.7),
    any(grepl("Pareto k", loo_warnings))
  )
  expect_true(all(grepl("Pareto k", loo_warnings)))

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_s3_class(pp_check(fit, ndraws = 20L), "ggplot")
  for (ty in c("residuals", "trend", "factors")) {
    expect_s3_class(plot(fit, type = ty), "ggplot")
  }
  expect_s3_class(mcmc_plot(fit), "ggplot")

  expect_true(is.data.frame(tidy(fit)))
  aug <- augment(fit)
  expect_identical(nrow(aug), nrow(dat))
  expect_identical(as.character(aug$series), as.character(dat$series))
})


test_that("active_factors, shared_variation and ordinate answer here", {
  # These three exist only for a factor model, and this is the one
  # fit in the local suite whose loadings are known, so what they
  # report can be checked against the map rather than merely
  # rendered.
  af <- active_factors(fit)
  expect_s3_class(af, "mvgam_active_factors")
  expect_identical(as.integer(af$n_lv), n_lv)
  # One row per column of the map, in order.
  expect_identical(nrow(af$per_factor), n_lv)
  expect_identical(as.integer(af$per_factor$factor), seq_len(n_lv))
  # Every series loads on both factors here, so neither column is
  # redundant and both have to come back active. A ceiling wider than
  # the map would show up as an inactive column, and the count the
  # object reports has to agree with the per-column verdicts rather
  # than being a second opinion.
  expect_true(all(af$per_factor$is_active))
  expect_identical(as.integer(sum(af$per_factor$is_active)), n_lv)
  expect_true(all(af$per_factor$prob_active > 0.5))
  expect_true(all(af$per_factor$median_norm_sq > af$threshold[[3L]]))

  sv <- shared_variation(fit)
  expect_s3_class(sv, "mvgam_shared_variation")
  expect_identical(as.integer(sv$n_lv), n_lv)
  expect_identical(as.integer(sv$n_series), n_series)
  expect_identical(as.character(sv$series_names), series_levels)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_s3_class(ordinate(fit), "ggplot")
})


test_that("every plot draws the occasions the user supplied", {
  # A latent state is indexed by a rank internally and reported at
  # the time the user gave it. A plot that draws the rank is off by
  # the offset between the two, silently, and looks entirely
  # reasonable: this frame is numbered from three, so the axis runs
  # 1..50 instead of 3..52.
  #
  # `type = "factors"` fails today while `trend`, `series` and the
  # hindcast all pass, which is what isolates it. Recorded as
  # finding 17.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  drawn_x <- function(p) {
    b <- ggplot2::ggplot_build(p)
    xs <- unlist(lapply(b$data, function(d) if ("x" %in% names(d)) d$x))
    range(xs, na.rm = TRUE)
  }
  want <- as.numeric(range(time_vals))
  for (ty in c("trend", "series", "factors")) {
    expect_equal(drawn_x(plot(fit, type = ty)), want, tolerance = 0.02)
  }
  expect_equal(drawn_x(plot(hindcast(fit, ndraws = 20L), series = 1L)),
               want, tolerance = 0.02)
})


test_that("a hindcast summarises to one row per cell, in user units", {
  # The table a reader actually takes away. Keyed by the series and
  # the occasion, so a row that lost its label or took a rank for a
  # time is what this catches, and `truth` is the observation itself
  # rather than anything the model produced.
  hc <- hindcast(fit, ndraws = 50L)
  sm <- summary(hc)
  expect_true(is.data.frame(sm))
  expect_identical(nrow(sm), nrow(dat))
  expect_true(all(c("series", "time", "predQ50", "predQ2.5",
                    "predQ97.5", "truth") %in% names(sm)))
  expect_setequal(as.character(sm$series), series_levels)
  expect_setequal(as.integer(sm$time), time_vals)
  expect_true(all(sm$predQ2.5 <= sm$predQ50))
  expect_true(all(sm$predQ50 <= sm$predQ97.5))

  # Each cell's truth is that cell's own observation.
  key <- paste(sm$series, sm$time)
  want <- dat$y[match(key, paste(dat$series, dat$time))]
  expect_equal(as.numeric(sm$truth), as.numeric(want))
})


test_that("a forecast is scored per series, and named by them", {
  # A per-series score under permuted names is finding 8 in the place
  # a user is most likely to act on it, so the keying is the claim.
  # Scoring needs a truth to score against, so the future frame
  # carries held-out observations rather than `NA`.
  h <- 4L
  future <- make_future(h)
  future$y <- rpois(nrow(future), exp(2.2))
  fc <- forecast(fit, newdata = future, ndraws = 100L)

  sm <- summary(fc)
  expect_true(is.data.frame(sm))
  expect_setequal(as.character(sm$series), series_levels)
  expect_true(all(time_vals %in% as.integer(sm$time)))
  expect_true(all(max(time_vals) + seq_len(h) %in% as.integer(sm$time)))

  sc <- score(fc)
  expect_identical(names(sc), c(series_levels, "all_series"))
  for (sname in series_levels) {
    arm <- sc[[sname]]
    expect_true(is.data.frame(arm))
    expect_identical(nrow(arm), h)
    expect_identical(as.integer(arm$eval_horizon), seq_len(h))
    expect_true(all(is.finite(arm$score)))
  }

  # A hindcast holds no held-out cell, so it is turned away rather
  # than scored against the data it was fitted on.
  expect_error(score(hindcast(fit, ndraws = 20L)),
               "no held-out forecasts")
})


test_that("the variance surface is the family's own variance", {
  # `type = "variance"` is the only prediction type that names a
  # moment rather than a scale, and for a poisson the variance is the
  # mean exactly. A surface returning the standard deviation, or the
  # variance of the linear predictor, is positive, finite and
  # correctly shaped, and fails this.
  set.seed(17L)
  v <- predict(fit, type = "variance", ndraws = 200L)
  set.seed(17L)
  e <- predict(fit, type = "expected", ndraws = 200L)
  expect_identical(dim(v), dim(e))
  expect_true(all(v > 0))
  expect_equal(as.numeric(v[, 1L]), as.numeric(e[, 1L]),
               tolerance = 1e-6)
})


test_that("the prediction types this family has no answer for are refused", {
  # Each names the family in its refusal rather than returning an
  # empty result, and `terms` points at the surface that does answer.
  expect_error(predict(fit, type = "latent_state", ndraws = 5L),
               "not available for this family")
  expect_error(predict(fit, type = "detection", ndraws = 5L),
               "not available for this family")
  expect_error(predict(fit, type = "terms", ndraws = 5L),
               "posterior_smooths")
})


# -- The newdata battery ----------------------------------------------

ref_epred <- posterior_epred(fit, newdata = dat, draw_ids = 1:10,
                             incl_autocor = TRUE)

test_that("the reference the battery compares against actually varies", {
  colm <- colMeans(ref_epred)
  expect_gt(stats::sd(colm), 0)
  by_series <- tapply(colm, as.character(dat$series), mean)
  expect_length(by_series, n_series)
  expect_gt(stats::sd(as.numeric(by_series)), 0)
})


test_that("a shuffled newdata answers the same, in the new order", {
  set.seed(44L)
  perm <- sample(nrow(dat))
  expect_equal(
    unname(posterior_epred(fit, newdata = dat[perm, , drop = FALSE],
                           draw_ids = 1:10, incl_autocor = TRUE)),
    unname(ref_epred[, perm, drop = FALSE])
  )
})


test_that("a newdata holding one series reads that series' state", {
  for (s in series_levels) {
    rows <- which(as.character(dat$series) == s)
    sub <- dat[rows, , drop = FALSE]
    sub$series <- droplevels(sub$series)
    expect_identical(levels(sub$series), s)
    expect_equal(
      unname(posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                             incl_autocor = TRUE)),
      unname(ref_epred[, rows, drop = FALSE])
    )
  }
})


test_that("an unknown series is refused, and named", {
  nd <- dat
  nd$series <- factor(
    ifelse(seq_len(nrow(nd)) == 1L, "hazel", as.character(nd$series)),
    levels = c(series_levels, "hazel")
  )
  err <- expect_error(
    posterior_epred(fit, newdata = nd, draw_ids = 1:5),
    "Series levels in newdata not found in training data"
  )
  expect_match(conditionMessage(err), "hazel", fixed = TRUE)
})


cat("\nDone.\n")
