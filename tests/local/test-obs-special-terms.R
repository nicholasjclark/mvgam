# Monotonic, measurement-error and imputed predictors under a latent
# trend.
#
# Six fits combine the special terms, each with an AR(1) trend. All but
# the fourth cover two series. In the fourth the responses are the
# series, in a frame of one row per occasion:
#
#   A  poisson   y ~ mo(dose) + mo(dose):z + mo(dose):hab +
#                    (mo(dose) | site)
#   B  gaussian  yg ~ me(xo, sdx):mo(dose) + me(go, sdg, gr = site),
#                trend_formula = ~ me(wo, sdw) + AR(p = 1),
#                saving the latent values
#   C  gaussian  bf(yc ~ mi(xm) + z) + bf(xm | mi() ~ w)
#   D  gaussian  bf(ya ~ mi(xa) + z) + bf(xa | mi() ~ w), wide
#   E  gaussian  bf(yc ~ mi(xm) + mi(xm):z + me(xo, sdx) + me(xo, sdx):z) +
#                bf(xm | mi() ~ w), saving the latent values
#   F  gaussian  bf(yg ~ mi(xm)) + bf(xm | mi(sdx) ~ w), saving the latent
#                values
#
# A special coefficient multiplies the product of its term's parts. In
# A each of the three coefficients has a simplex of its own. `mo(dose)
# :hab` has one column per level of `hab` after the first. The varying
# slope adds each site's deviation to the first coefficient. A predictor
# that reused one simplex, dropped the covariate or missed the slope
# stays finite and correctly shaped.
#
# In B the noise-free values belong to the rows the model was fitted
# to. Predictions there take the saved draws. Predictions at new rows
# draw them from the observed values and their known SDs. The grouped
# term has one value per site. The trend formula carries a term of its
# own.
#
# In C `xm` is a response that imputes its missing values and a
# predictor of `yc`. Both responses then cover the same rows: those
# where `yc` was observed. The likelihood of `xm` scores the imputed
# vector `Yl_xm`. The trend joins that likelihood and `yc`'s predictor.
#
# D gives each response a trend of its own. A value missing at a new row
# then takes the trend of `xa`. `ya`'s trend gives a different value.
#
# E uses each latent value in two terms. Both terms take one draw. Two
# draws of a value at a new row would stay finite and correctly shaped.
#
# F measures `xm` with known noise, which makes every value latent.
#
# The reference for each predictor is brms's own, computed from the
# same draws. The Stan code and data are held against the frame.
#
# Run with:
#   TESTTHAT_MAX_FAILS=1000 Rscript -e "devtools::load_all('.');
#     testthat::test_file('tests/local/test-obs-special-terms.R')"

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})


# -- Data ---------------------------------------------------------------

set.seed(2611L)

series_levels <- c("south", "north")
n_time <- 60L
n <- 2L * n_time
latent <- matrix(0, n_time, 2L)
for (s in 1:2) {
  latent[1L, s] <- rnorm(1L, 0, 0.4)
  for (t in 2:n_time) {
    latent[t, s] <- 0.6 * latent[t - 1L, s] + rnorm(1L, 0, 0.3)
  }
}
trend <- as.numeric(latent)

# `mo(dose)` is three steps at the top level
dose_levels <- c("none", "low", "mid", "high")
site_levels <- paste0("st", 1:6)
dat <- data.frame(
  time = rep(seq_len(n_time), times = 2L),
  series = factor(rep(series_levels, each = n_time), levels = series_levels),
  dose = factor(sample(dose_levels, n, replace = TRUE),
                levels = dose_levels, ordered = TRUE),
  z = rnorm(n),
  w = rnorm(n),
  hab = factor(sample(c("closed", "open"), n, replace = TRUE)),
  site = factor(sample(site_levels, n, replace = TRUE), levels = site_levels)
)
mo_true <- 3 * c(0, 0.2, 0.5, 1)[as.integer(dat$dose)]
site <- as.integer(dat$site)

# A: a varying intercept and a varying monotonic slope by site
dat$y <- rpois(n, exp(
  0.8 + trend + (0.25 + rnorm(6L, 0, 0.1)[site]) * mo_true +
    0.15 * mo_true * dat$z + 0.2 * mo_true * (dat$hab == "open") +
    rnorm(6L, 0, 0.4)[site]
))

# B: covariates measured with known error. `go` is measured once per
# site. `wo` enters the trend.
x_true <- rnorm(n)
g_true <- rnorm(6L)
w_true <- rnorm(n)
dat$sdx <- 0.3
dat$xo <- x_true + rnorm(n, 0, dat$sdx)
dat$sdg <- 0.2
dat$go <- (g_true + rnorm(6L, 0, 0.2))[site]
dat$sdw <- 0.2
dat$wo <- w_true + rnorm(n, 0, dat$sdw)
dat$yg <- 0.4 * x_true * mo_true + 0.6 * g_true[site] + trend +
  0.5 * w_true + rnorm(n, 0, 0.3)

# C: `xm` loses twelve values and `yc` three others
dat$xm <- 0.7 * dat$w + trend + rnorm(n, 0, 0.3)
dat$yc <- 0.5 + 0.6 * dat$xm + 0.3 * dat$z + trend + rnorm(n, 0, 0.3)
na_xm <- sort(sample(n, 12L))
dat$xm[na_xm] <- NA
dat$yc[sort(sample(setdiff(seq_len(n), na_xm), 3L))] <- NA

# D: one row per occasion. `xa` and `ya` follow their own trends.
wide <- data.frame(time = seq_len(n_time), z = rnorm(n_time),
                   w = rnorm(n_time))
wide$xa <- 0.7 * wide$w + latent[, 1L] + rnorm(n_time, 0, 0.3)
wide$ya <- 0.5 + 0.6 * wide$xa + 0.3 * wide$z + latent[, 2L] +
  rnorm(n_time, 0, 0.3)
wide$xa[sort(sample(n_time, 8L))] <- NA

na_notice <- "Rows containing NAs"

models <- list(
  A = list(formula = y ~ mo(dose) + mo(dose):z + mo(dose):hab +
             (mo(dose) | site),
           trend_formula = ~ AR(p = 1), family = poisson()),
  B = list(formula = bf(yg ~ me(xo, sdx):mo(dose) + me(go, sdg, gr = site)),
           trend_formula = ~ me(wo, sdw) + AR(p = 1), family = gaussian(),
           save_pars = save_pars(latent = TRUE)),
  C = list(formula = bf(yc ~ mi(xm) + z) + bf(xm | mi() ~ w) +
             set_rescor(FALSE),
           trend_formula = ~ AR(p = 1), family = gaussian()),
  D = list(formula = bf(ya ~ mi(xa) + z) + bf(xa | mi() ~ w) +
             set_rescor(FALSE),
           trend_formula = ~ AR(p = 1), family = gaussian()),
  E = list(formula = bf(yc ~ mi(xm) + mi(xm):z + me(xo, sdx) +
                          me(xo, sdx):z) + bf(xm | mi() ~ w) +
             set_rescor(FALSE),
           trend_formula = ~ AR(p = 1), family = gaussian(),
           save_pars = save_pars(latent = TRUE)),
  F = list(formula = bf(yg ~ mi(xm)) + bf(xm | mi(sdx) ~ w) +
             set_rescor(FALSE),
           trend_formula = ~ AR(p = 1), family = gaussian(),
           save_pars = save_pars(latent = TRUE))
)
frames <- list(D = wide)

# C and E drop rows, those where `yc` is missing
build <- function(name, ...) {
  call <- function() {
    do.call(mvgam, c(models[[name]],
                     list(data = frames[[name]] %||% dat, silent = 2, ...)))
  }
  if (name %in% c("C", "E")) {
    expect_warning(out <- call(), na_notice)
  } else {
    out <- call()
  }
  out
}

prefits <- lapply(stats::setNames(nm = names(models)), build,
                  run_model = FALSE)

fits <- lapply(stats::setNames(nm = names(models)), function(name) {
  cached_fit(paste0("val_obs_special_", name, ".rds"), function() {
    build(name, chains = 2L, iter = 1000L, warmup = 500L,
          backend = "cmdstanr", seed = 1L)
  }, key = list(dat = frames[[name]] %||% dat, model = models[[name]]))
})

ids <- 1:50
code <- function(name) paste(as.character(stancode(prefits[[name]])),
                             collapse = "\n")
# Each statement present, each before the next
expect_in_order <- function(text, lines) {
  at <- vapply(lines, function(l) regexpr(l, text, fixed = TRUE),
               integer(1L))
  expect_true(all(at > 0L))
  expect_false(is.unsorted(at))
}


# -- The program and its data --------------------------------------------

test_that("A: each special coefficient takes its own simplex and part", {
  expect_in_order(code("A"), c(
    "mu[n] += (bsp[1] + r_1_2[J_1[n]]) * mo(simo_1, Xmo_1[n])",
    "+ (bsp[2]) * mo(simo_2, Xmo_2[n]) * Csp_1[n]",
    "+ (bsp[3]) * mo(simo_3, Xmo_3[n]) * Csp_2[n]",
    "mu += to_vector(trend)[obs_trend_idx];",
    "target += poisson_log_lpmf(Y | mu);"
  ))
  sd <- prefits$A$standata
  d <- prefits$A$data
  # brms codes an ordered level as its position from zero
  for (k in 1:3) {
    expect_identical(as.integer(sd[[paste0("Xmo_", k)]]),
                     as.integer(d$dose) - 1L)
  }
  expect_identical(as.integer(sd$Jmo), rep(3L, 3L))
  expect_equal(as.numeric(sd$Csp_1), d$z)
  expect_equal(as.numeric(sd$Csp_2), as.numeric(d$hab == "open"))
  expect_identical(as.integer(sd$J_1), as.integer(d$site))
})


test_that("B: each noise-free covariate takes its own rows", {
  expect_in_order(code("B"), c(
    "mu[n] += (bsp[1]) * Xme_1[Jme_1[n]]",
    "+ (bsp[2]) * Xme_2[n] * mo(simo_1, Xmo_1[n]);",
    "mu += to_vector(trend)[obs_trend_idx];"
  ))
  for (line in c(
    "mu_trend[n] += (bsp_trend[1]) * Xme_1_trend[n];",
    "target += normal_lpdf(Xn_1_trend | Xme_1_trend, noise_1_trend);",
    "target += normal_lpdf(Xn_1 | Xme_1, noise_1);",
    "target += normal_lpdf(Xn_2 | Xme_2, noise_2);"
  )) {
    expect_true(grepl(line, code("B"), fixed = TRUE))
  }
  sd <- prefits$B$standata
  d <- prefits$B$data
  # The grouped term holds one value per site, in level order
  expect_identical(as.integer(sd$Jme_1), as.integer(d$site))
  expect_equal(as.numeric(sd$Xn_1),
               unname(vapply(split(d$go, d$site), `[`, 0, 1L)))
  expect_equal(as.numeric(sd$noise_1), rep(0.2, 6L))
  expect_equal(as.numeric(sd$Xn_2), d$xo)
  expect_equal(as.numeric(sd$noise_2), d$sdx)
  # The trend's term holds one value per trend cell, in the order of
  # the trend data
  expect_equal(as.numeric(sd$Xn_1_trend), prefits$B$trend_model$data$wo)
})


test_that("C: both responses cover the rows where yc was observed", {
  expect_in_order(code("C"), c(
    "vector[N_xm] Yl_xm = Y_xm;",
    "Yl_xm[Jmi_xm] = Ymi_xm;",
    "mu_yc[n] += (bsp_yc[1]) * Yl_xm[n];",
    "mu_yc += to_vector(trend)[obs_trend_idx_yc];",
    "target += normal_id_glm_lpdf(Y_yc | Xc_yc, mu_yc, b_yc, sigma_yc);",
    "target += normal_id_glm_lpdf(Yl_xm | Xc_xm, Intercept_xm",
    "+ to_vector(trend)[obs_trend_idx_xm], b_xm, sigma_xm);"
  ))
  sd <- prefits$C$standata
  d <- prefits$C$data
  keep <- !is.na(d$yc)
  # The missing values of `xm` are imputed at their positions among
  # the covered rows
  expect_identical(as.integer(sd$Jmi_xm), which(is.na(d$xm[keep])))
  expect_equal(as.numeric(sd$Y_xm[-sd$Jmi_xm]), d$xm[keep & !is.na(d$xm)])
  expect_equal(as.numeric(sd$Y_yc), d$yc[keep])
  # Row n of one response is row n of the other, at one trend cell
  expect_identical(as.integer(sd$obs_trend_time_yc),
                   match(d$time[keep], sort(unique(d$time))))
  expect_identical(sd$obs_trend_time_xm, sd$obs_trend_time_yc)
  expect_identical(as.integer(sd$obs_trend_series_yc),
                   as.integer(d$series[keep]))
  expect_identical(sd$obs_trend_series_xm, sd$obs_trend_series_yc)
})


test_that("each fit ran the program the package writes today", {
  # A cached fit from an older program would test that program
  for (name in names(fits)) {
    expect_identical(as.character(stancode(fits[[name]])),
                     as.character(stancode(prefits[[name]])))
  }
})


# -- Predictors against brms ---------------------------------------------

test_that("each predictor is brms's own at the fitted rows", {
  # The fitted rows take the saved noise-free values and the imputed
  # draws, which brms uses only when given no new data
  # brms fitted C to the rows where `yc` was observed.
  for (fit in fits) {
    rows <- fit$data[!is.na(fit$data[[response_columns(fit)[[1L]]]]), ]
    for (re_formula in list(NULL, NA)) {
      expect_lt(max(predictor_gap(fit, rows, re_formula)), 1e-8)
    }
  }
})


test_that("new rows take each special term's value from their own data", {
  # A: the monotonic transform of each new row
  d <- fits$A$data
  expect_lt(max(predictor_gap(fits$A, d[c(90L, 3L, 41L, 7L), ])), 1e-8)

  # B: new rows draw the noise-free values from the observed ones. With
  # known SDs near zero the draws equal the observed values, which
  # brms then uses as well.
  d <- fits$B$data[c(90L, 3L, 41L, 7L), ]
  d$sdx <- 1e-10
  d$sdg <- 1e-10
  expect_lt(max(predictor_gap(fits$B, d)), 1e-6)

  # C: a new row with `xm` observed takes it as it is
  d <- fits$C$data
  d <- d[which(!is.na(d$xm) & !is.na(d$yc))[c(5L, 1L, 30L)], ]
  expect_lt(max(predictor_gap(fits$C, d)), 1e-8)
})


test_that("C: a value missing at a new row comes from its own model", {
  # Each row here holds `xm` in the fit. The frame drops it. The value
  # `yc` then implies is `xm`'s predictor, the trend `xm`'s likelihood
  # takes and one error at its SD. Leaving the trend out gives the
  # errors an SD near 1.8.
  fit <- fits$C
  rows <- fit$data[!is.na(fit$data$yc) & !is.na(fit$data$xm), ][1:30, ]
  rows$xm <- NA_real_
  draws <- as_plain_matrix(posterior::as_draws_matrix(fit$fit))
  trend <- trend_state_rows(fit, rows, seq_len(nrow(draws)), resp = "yc")
  eta <- get_combined_linpred(fit, rows, trend_state = "conditional",
                              resp = "yc")
  xm <- (eta - trend - draws[, "b_yc_Intercept"] -
           outer(draws[, "b_yc[1]"], rows$z)) / draws[, "bsp_yc[1]"]
  error <- (xm - trend - draws[, "b_xm_Intercept"] -
              outer(draws[, "b_xm[1]"], rows$w)) / draws[, "sigma_xm"]
  expect_lt(abs(mean(error)), 0.05)
  expect_lt(abs(sd(error) - 1), 0.05)
})


test_that("D: a missing value takes its own response's trend", {
  fit <- fits$D
  rows <- fit$data[!is.na(fit$data$xa), ][1:20, ]
  rows$xa <- NA_real_
  draws <- as_plain_matrix(posterior::as_draws_matrix(fit$fit))
  state <- function(resp) {
    trend_state_rows(fit, rows, seq_len(nrow(draws)), resp = resp)
  }
  eta <- get_combined_linpred(fit, rows, trend_state = "conditional",
                              resp = "ya")
  xa <- (eta - state("ya") - draws[, "b_ya_Intercept"] -
           outer(draws[, "b_ya[1]"], rows$z)) / draws[, "bsp_ya[1]"]
  error <- function(trend) {
    (xa - trend - draws[, "b_xa_Intercept"] -
       outer(draws[, "b_xa[1]"], rows$w)) / draws[, "sigma_xa"]
  }
  expect_lt(abs(mean(error(state("xa")))), 0.05)
  expect_lt(abs(sd(error(state("xa"))) - 1), 0.05)
  # The other response's trend does not fit these values
  expect_gt(sd(error(state("ya"))), 1.2)
})


test_that("D: each imputed value takes the name of its row", {
  # Row names overlap the Stan positions, which a renaming in turn
  # would collapse
  gaps <- which(is.na(fits$D$data$xa))
  named <- posterior::as_draws_matrix(fits$D, variable = "^Ymi_xa",
                                      regex = TRUE)
  raw <- posterior::as_draws_matrix(fits$D$fit)
  expect_equal(unname(named[, paste0("Ymi_xa[", gaps[3L], "]")]),
               unname(raw[, "Ymi_xa[3]"]), ignore_attr = TRUE)
})


test_that("D: a frame without the column misses every value", {
  rows <- data.frame(time = n_time + 1:3, z = 0, w = 0, ya = NA_real_)
  draw <- function(frame) {
    set.seed(9L)
    posterior_epred(fits$D, newdata = frame, resp = "ya", draw_ids = ids)
  }
  expect_equal(draw(rows), draw(transform(rows, xa = NA_real_)))
})


test_that("E: every term of a latent value takes one draw", {
  # Each row holds the same `xo` and a new `xm`. With both latent values
  # out, every row holds the intercept alone, whatever its `z`.
  rows <- data.frame(time = n_time + 1:3,
                     series = factor("south", series_levels),
                     z = c(0, 1, 2), w = 0, xo = 0.5, sdx = 0.3,
                     xm = NA_real_, yc = NA_real_, yg = 0, site = "st1")
  prep <- prepare_component(fits$E, rows, "obs", "yc", ids, NULL, FALSE,
                            "uncertainty", function(var) {
                              matrix(0, length(ids), nrow(rows))
                            })$prep
  eta <- extract_linpred_from_prep(prep, resp = "yc")
  b <- as_plain_matrix(prep$draws)[, paste0("bsp_yc[", 1:4, "]")]
  xm <- prep$latents$Yl_xm
  xo <- prep$latents$Xme_1
  left <- eta - b[, 1L] * xm - b[, 2L] * xo -
    b[, 3L] * sweep(xm, 2L, rows$z, `*`) -
    b[, 4L] * sweep(xo, 2L, rows$z, `*`)
  expect_lt(max(apply(left, 1L, function(row) diff(range(row)))), 1e-10)
})


test_that("F: a value measured with noise keeps its fitted draw", {
  rows <- fits$F$data[1:6, ]
  rows$xm <- NA_real_
  prep <- prepare_component(fits$F, rows, "obs", "yg", ids, NULL, FALSE,
                            "uncertainty", NULL)$prep
  extract_linpred_from_prep(prep, resp = "yg")
  expect_identical(
    unname(prep$latents$Yl_xm),
    unname(as_plain_matrix(prep$draws)[, paste0("Yl_xm[", 1:6, "]")])
  )
})


test_that("F: a value missing at a new row comes from its own model", {
  rows <- data.frame(time = n_time + 1:3,
                     series = factor("south", series_levels),
                     w = c(-1, 0, 1), sdx = 0.3, xm = NA_real_, yg = 0)
  ids_all <- seq_len(ndraws(fits$F))
  prep <- prepare_component(fits$F, rows, "obs", "yg", ids_all, NULL, FALSE,
                            "uncertainty", function(var) {
                              matrix(0, length(ids_all), nrow(rows))
                            })$prep
  extract_linpred_from_prep(prep, resp = "yg")
  draws <- as_plain_matrix(prep$draws)
  error <- prep$latents$Yl_xm - (draws[, "b_xm_Intercept"] +
    outer(draws[, "b_xm[1]"], rows$w))
  z <- as.vector(error) / draws[, "sigma_xm"]
  expect_lt(abs(mean(z)), 0.1)
  expect_lt(abs(sd(z) - 1), 0.1)
})


test_that("D: the forecast's parts hold the imputed value's own trend", {
  rows <- future_rows(fits$D)
  rows$xa <- NA_real_
  expect_forecast_parts_sum(fits$D, "state", rows)
})


test_that("D: an imputed value's innovations are its own driver", {
  # By hand: a step of `xa`'s trend adds b^2 sigma^2 to `ya`
  ids <- 1:200
  known <- future_rows(fits$D, 1L)
  imputed <- known
  imputed$xa <- NA_real_
  split <- function(rows) {
    forecast_uncertainty(fits$D, newdata = rows, resp = "ya",
                         n_replicates = 50L, n_boot = 2L, draw_ids = ids)$sources
  }
  of <- function(x, source) x$variance[x$source == source]
  at_imputed <- split(imputed)
  draws <- as_draws_matrix(fits$D)[ids, ]
  by_hand <- mean(as.vector(draws[, "bsp_ya_mixa"])^2 *
                    as.vector(draws[, "sigma_trend[xa]"])^2)
  expect_equal(of(at_imputed, "imputed covariates"), by_hand, tolerance = 0.25)
  # What remains is the process of `ya` itself
  expect_equal(of(at_imputed, "process error"),
               of(split(known), "process error"), tolerance = 0.25)
})


test_that("D: each response of a wide frame is one series in the equations", {
  text <- methods_md(fits$D)
  expect_match(text, "\\mathrm{ya}_{t} &\\sim", fixed = TRUE)
  expect_match(text, "\\eta_{\\mathrm{xa},t}", fixed = TRUE)
})


test_that("F: the equations hold the measurement of its noisy response", {
  expect_match(methods_md(fits$F), paste0(
    "\\mathrm{xm}^{\\text{obs}}_{i,t} &\\sim \\text{Normal}\\!\\left(",
    "\\mathrm{xm}_{i,t}, \\mathrm{sdx}_{i,t}\\right)"
  ), fixed = TRUE)
})


test_that("B's trend predictor is brms's own, latent values and all", {
  fit <- fits$B
  full <- subset_draws_rows(posterior::as_draws_matrix(fit$fit),
                            draw_ids = ids)
  mine <- extract_linpred_from_prep(prepare_linpred_data(
    side_draws(full, side_parameters(fit, "trend"), "trend"),
    fit$trend_model, newdata = fit$trend_model$data
  ))
  theirs <- brms::posterior_linpred(as_brms(fit, "trend"), draw_ids = ids,
                                    incl_autocor = FALSE)
  expect_lt(max(abs(unname(mine) - unname(theirs))), 1e-8)
})


test_that("every special parameter carries brms's name", {
  # The draws of each side take the names brms gives the same model
  for (fit in fits) {
    expect_contains(variables(fit), grep(
      "^(bsp|simo|meanme|sdme|Xme|Ymi)_", variables(as_brms(fit)),
      value = TRUE
    ))
  }
  trend <- grep("^(bsp|meanme|sdme|Xme)_",
                variables(as_brms(fits$B, "trend")), value = TRUE)
  expect_contains(variables(fits$B),
                  sub("^([^[]+)", "\\1_trend", trend))
})


test_that("row units place each fit's rows on every kind of axis", {
  fixtures <- c("hier_trend", "jsdgam_mv_nb", "mvbf_wide", "by_lv_axis",
                "car_own_times", "trend_map", "llt_grouped", "occ_units")
  for (name in fixtures) {
    fit <- readRDS(test_path("fixtures", paste0("val_mvgam_", name, ".rds")))
    obs <- obs_row_units(fit, fit$data)
    rows <- seq_len(nrow(obs$fitted))
    # Stacked copies of the frame each take the fitted row
    expect_identical(
      fitted_row_index(rbind(obs$fitted, obs$fitted), obs$fitted),
      c(rows, rows)
    )
    if (is.null(fit$trend_model)) next
    # The trend's rows run in the order Stan indexes its cells
    if (isTRUE(fit$trend_metadata$has_by_lv)) {
      frame <- expand_factor_axis(fit$trend_metadata$n_lv_for_grain,
                                  time = unique(fit$data$time))
      cells <- trend_row_units(fit, as.data.frame(frame))
      expect_identical(as.character(cells$fitted$.trend),
                       as.character(fit$trend_model$data$.trend))
    } else if (!is_response_keyed(fit)) {
      sd <- fit$standata
      kept <- Reduce(`&`, response_rows(fit, fit$data))
      cells <- trend_row_units(fit, fit$data[kept, ])
      expect_identical(
        fitted_row_index(cells$new, cells$fitted),
        as.integer((sd$obs_trend_time - 1L) * sd$N_series_trend +
                     sd$obs_trend_series)
      )
    }
  }
})


# -- Methods that compute from the predictor ------------------------------

test_that("the parts and the expectation follow the predictor", {
  # The parts sum to the predictor the trend joins. The expectation is
  # the family's mean of that predictor.
  for (fit in fits) {
    # A row outside the fit draws its latent values afresh in each call
    expect_parts_sum(fit, "state", fit$data[
      Reduce(`&`, response_rows(fit, fit$data)),
    ])
  }
  expect_epred_parts(fits$A, exp)
  expect_epred_parts(fits$B, identity)
  expect_epred_parts(fits$C, identity)
})


test_that("the site grouping reports the varying monotonic slope", {
  expect_identical(dimnames(ranef(fits$A)$site)[[3L]],
                   c("Intercept", "modose"))
})


cat("\nDone.\n")
