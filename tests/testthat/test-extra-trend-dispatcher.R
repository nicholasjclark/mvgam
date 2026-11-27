# The tests of test-trend-dispatcher.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("LLT refuses a short series and warns on a time term", {
  dat <- expand.grid(time = 1:12, series = factor(c("a", "b")))
  dat$y <- rpois(nrow(dat), 4)
  short <- function(n, ...) {
    get_prior(mvgam_formula(y ~ 1, ...), data = dat[dat$time <= n, ],
              family = poisson())
  }
  expect_error(short(4, ~ LLT()), "needs at least 5 time points")
  expect_warning(short(12, ~ LLT()), "barely informs 'sigma_slope_trend'")
  expect_warning(
    get_prior(mvgam_formula(y ~ s(time, k = 4), ~ LLT()),
              data = expand.grid(time = 1:30, series = factor("a"),
                                 y = 1L),
              family = poisson()),
    "'time' appears in a formula with 'LLT\\(\\)'"
  )
})

test_that("LLT takes seasonal periods it can identify", {
  # The default is the most harmonics below half the period, up to 10
  expect_identical(LLT(season = 12)$harmonics, 5L)
  expect_identical(LLT(season = c(7, 365.25))$harmonics, c(3L, 10L))
  expect_identical(LLT(season = 2.5)$harmonics, 1L)
  expect_error(LLT(harmonics = 2), "'harmonics' requires 'season'")
  expect_error(LLT(season = 2), "must exceed 2 time steps")
  expect_error(LLT(season = 12, harmonics = 6), "Period 12 allows 5")
  expect_error(LLT(season = c(7, 12), harmonics = 2),
               "one value per 'season' period")
  # Harmonic 2 of period 12 turns as fast as harmonic 1 of period 6
  expect_error(LLT(season = c(6, 12), harmonics = c(2L, 2L)),
               "share a harmonic")
  dat <- expand.grid(time = 1:30, series = factor("a"), y = 1L)
  expect_warning(
    get_prior(mvgam_formula(y ~ 1, ~ LLT(season = 40)), data = dat,
              family = poisson()),
    "period of 40 exceeds the 30 time points"
  )
})

test_that("a seasonal LLT turns each harmonic in vectorised Stan code", {
  dat <- expand.grid(time = 1:30, series = factor(paste0("s", 1:3)))
  dat$y <- rpois(nrow(dat), 4)
  build <- function(trend) {
    mvgam(y ~ 1, trend_formula = trend, data = dat, family = poisson(),
          run_model = FALSE, silent = 2)
  }
  mod <- build(~ LLT(season = 12, harmonics = 2))
  expect_identical(standata(mod)$freq_season_trend, 2 * pi * (1:2) / 12)
  code <- stancode(mod)
  expect_match(code, "re[ : , c] = cumulative_sum(re[ : , c]);",
               fixed = TRUE)
  expect_match(code, "lv_trend += season_trend;", fixed = TRUE)
  # A factor model rotates the season and its starting pairs with the
  # factors
  fac <- stancode(build(~ LLT(n_lv = 2, season = 12, harmonics = 2)))
  expect_match(fac, "season_trend_tilde = season_trend * Q';",
               fixed = TRUE)
  expect_match(fac, "init_season_trend_tilde = init_season_trend * Q';",
               fixed = TRUE)
  expect_no_match(stancode(build(~ LLT())), "season", fixed = TRUE)
})

test_that("Factor validation error messages are consistent", {
  # Check that all factor-incompatible trends give similar error structure
  # CAR()'s signature omits n_lv, and the error is raised at the
  # stanvars level

  expect_error(PW(n_lv = 1), "Factor models.*not supported")

  # Check they mention specific alternatives (AR is factor-compatible)
  expect_error(PW(n_lv = 1), "use a trend with a factor form")

  # Check they have specific reasons
  expect_error(PW(n_lv = 1), "changepoint")

  # Sampled loadings absorb any correlation among the factors, which the
  # program pins at the identity. An explicit request is refused, as a
  # jsdgam() factor request is. Fixed loadings keep it.
  d <- data.frame(time = rep(1:12, 3), series = factor(rep(1:3, each = 12)),
                  y = rnorm(36))
  for (tf in list(~ LLT(n_lv = 2, cor = TRUE), ~ RW(n_lv = 2, cor = TRUE))) {
    expect_error(stancode(mvgam_formula(y ~ 1, tf), data = d),
                 "'cor = TRUE' is not supported in a factor model")
  }
  expect_error(
    jsdgam(y ~ 1, factor_formula = ~ AR(cor = TRUE), data = d,
           unit = time, species = series, n_lv = 2, family = gaussian(),
           run_model = FALSE),
    "'cor = TRUE' is not supported in a factor model"
  )
  tm <- data.frame(series = factor(1:3), trend = c(1, 1, 2))
  expect_match(
    stancode(mvgam_formula(y ~ 1, ~ RW(cor = TRUE, trend_map = tm)),
             data = d),
    "target += lkj_corr_cholesky_lpdf(L_Omega_trend", fixed = TRUE
  )
})

test_that("a grouped trend takes its subgroups from the series column", {
  # `gr` without `subgr` names the series column as the subgroup. A
  # series column called `site` gives the subgroups that column holds.
  d <- expand.grid(t = 1:20, site = paste0("s", 1:4))
  d$region <- factor(ifelse(d$site %in% c("s1", "s2"), "A", "B"))
  d$site <- factor(d$site)
  d$y <- seq_len(nrow(d)) / 10
  m <- mvgam(y ~ 1,
             trend_formula = ~ ZMVN(time = t, series = site, gr = region),
             data = d, family = gaussian(), run_model = FALSE, silent = 2,
             validate = FALSE)
  sd <- standata(m)
  expect_identical(as.integer(sd$group_inds_trend), c(1L, 1L, 2L, 2L))
  expect_identical(unname(sd$group_members_trend),
                   matrix(1:4, 2L, byrow = TRUE))
})
