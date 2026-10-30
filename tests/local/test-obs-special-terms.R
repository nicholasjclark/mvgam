# Monotonic, measurement-error and imputed predictors under a latent
# trend.
#
# Three fits combine the special terms, each over two series with an
# AR(1) trend:
#
#   A  poisson   y ~ mo(dose) + mo(dose):z + mo(dose):hab +
#                    (mo(dose) | site)
#   B  gaussian  yg ~ me(xo, sdx):mo(dose) + me(go, sdg, gr = site),
#                trend_formula = ~ me(wo, sdw) + AR(p = 1),
#                saving the latent values
#   C  gaussian  bf(yc ~ mi(xm) + z) + bf(xm | mi() ~ w)
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
           trend_formula = ~ AR(p = 1), family = gaussian())
)

# Only C drops rows, those where `yc` is missing
build <- function(name, ...) {
  call <- function() {
    do.call(mvgam, c(models[[name]], list(data = dat, silent = 2, ...)))
  }
  if (identical(name, "C")) {
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
  }, key = list(dat = dat, model = models[[name]]))
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
  expect_identical(as.integer(sd$N_yc), sum(keep))
  expect_identical(as.integer(sd$N_xm), sum(keep))
  # The missing values of `xm` are imputed at their positions among
  # the covered rows
  expect_identical(as.integer(sd$Jmi_xm), which(is.na(d$xm[keep])))
  expect_equal(as.numeric(sd$Y_xm[-sd$Jmi_xm]), d$xm[keep & !is.na(d$xm)])
  expect_equal(as.numeric(sd$Y_yc), d$yc[keep])
  # Row n of one response is row n of the other, at one trend cell
  expect_identical(as.integer(sd$obs_trend_time_yc),
                   match(d$time[keep], sort(unique(d$time))))
  expect_identical(sd$obs_trend_time_xm, sd$obs_trend_time_yc)
  expect_length(sd$obs_trend_series_yc, sum(keep))
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
    theirs <- grep("^(bsp|simo|meanme|sdme|Xme|Ymi)_",
                   variables(as_brms(fit)), value = TRUE)
    expect_gt(length(theirs), 0L)
    expect_identical(setdiff(theirs, variables(fit)), character(0))
  }
  trend <- grep("^(bsp|meanme|sdme|Xme)_",
                variables(as_brms(fits$B, "trend")), value = TRUE)
  expect_gt(length(trend), 0L)
  expect_identical(setdiff(paste0(trend, "_trend"), variables(fits$B)),
                   character(0))
})


# -- Methods that compute from the predictor ------------------------------

test_that("the parts and the expectation follow the predictor", {
  # The parts sum to the predictor the trend joins. The expectation is
  # the family's mean of that predictor.
  for (fit in fits) {
    expect_parts_sum(fit, "state")
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
