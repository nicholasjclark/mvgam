# The prior a model is reported to use must be the prior it samples
# under, for every trend kernel and not just the common one.
#
# Five defects sat behind that sentence. `sigma_trend` reported one
# distribution and sampled another, which `update()` then fed back so
# that every refit re-specified the model. `ar1_trend` was sampled
# under a prior nothing reported. `L_Omega_trend` was written into the
# Stan code as a literal on both the AR and VAR paths, so a user prior
# on a correlated trend was discarded silently. `LV` carried a default
# for a parameter the generator never emits. `Z` was stated three ways
# across five places.
#
# These live here rather than in CI because each case generates Stan
# code for a different trend kernel, which is too slow for the CI
# budget, and because the hierarchical cases need a grouped design.
#
# Run with:
#   Rscript -e "devtools::load_all('.'); testthat::test_file('tests/local/test-prior-surface.R')"

source("setup_tests_local.R")


prior_surface_data <- function(grouped = FALSE) {
  set.seed(1)
  d <- if (grouped) {
    expand.grid(time = 1:25, gr = factor(c("g1", "g2")),
                subgr = factor(c("a", "b")))
  } else {
    expand.grid(time = 1:25, series = factor(c("a", "b", "c")))
  }
  d$y <- rpois(nrow(d), 5)
  d$x <- rnorm(nrow(d))
  d
}


# Every `X ~ dist;` the generator emits for a trend parameter, paired
# with what the prior table says about the same parameter.
sampled_vs_reported <- function(trend_formula, data) {
  mf <- mvgam_formula(y ~ x, trend_formula = trend_formula)
  code <- stancode(mf, data = data, family = poisson())
  tab <- as.data.frame(get_prior(mf, data = data, family = poisson()))
  lines <- trimws(grep("^\\s*[A-Za-z_0-9]+_trend\\s*~",
                        strsplit(code, "\n")[[1]], value = TRUE))
  # `init_trend` is not a prior a user can set. A stationary VAR draws
  # its first state from the distribution its own coefficients and
  # innovation covariance imply, so the statement is structural.
  lines <- lines[!grepl("^init_trend\\s*~", lines)]
  lapply(lines, function(line) {
    par <- sub("\\s*~.*", "", line)
    list(par = par,
         sampled = trimws(sub(";.*", "", sub(".*~\\s*", "", line))),
         reported = tab$prior[tab$class == par])
  })
}


test_that("an AR trend reports the priors it samples", {
  for (pair in sampled_vs_reported(~ AR(p = 1, cor = TRUE),
                                    prior_surface_data())) {
    expect_true(length(pair$reported) > 0L)
    expect_true(pair$sampled %in% pair$reported)
  }
})


test_that("a VAR trend reports the priors it samples", {
  for (pair in sampled_vs_reported(~ VAR(cor = TRUE),
                                    prior_surface_data())) {
    expect_true(length(pair$reported) > 0L)
    expect_true(pair$sampled %in% pair$reported)
  }
})


test_that("a moving-average trend reports the priors it samples", {
  for (pair in sampled_vs_reported(~ AR(p = 1, ma = TRUE),
                                    prior_surface_data())) {
    expect_true(length(pair$reported) > 0L)
    expect_true(pair$sampled %in% pair$reported)
  }
})


test_that("a factor trend reports the priors it samples", {
  for (pair in sampled_vs_reported(~ AR(p = 1, n_lv = 2),
                                    prior_surface_data())) {
    expect_true(length(pair$reported) > 0L)
    expect_true(pair$sampled %in% pair$reported)
  }
})


test_that("a user prior reaches the Stan code on every settable trend", {
  d <- prior_surface_data()
  cases <- list(
    list(tf = ~ AR(p = 1), class = "sigma_trend", new = "exponential(7)"),
    list(tf = ~ AR(p = 1), class = "ar1_trend", new = "normal(0, 0.11)"),
    list(tf = ~ AR(p = 1, cor = TRUE), class = "L_Omega_trend",
         new = "lkj_corr_cholesky(9)"),
    list(tf = ~ VAR(cor = TRUE), class = "L_Omega_trend",
         new = "lkj_corr_cholesky(8)")
  )
  for (cs in cases) {
    mf <- mvgam_formula(y ~ x, trend_formula = cs$tf)
    user <- brms::prior_string(cs$new, class = cs$class)
    code <- stancode(mf, data = d, family = poisson(), prior = user)
    expect_true(grepl(paste0(cs$class, " ~ ", cs$new), code, fixed = TRUE))
  }
})


test_that("a custom prior survives the update round-trip", {
  # `update()` rebuilds the call from the fit's stored prior table. A
  # table that disagreed with the model silently re-specified it here.
  d <- prior_surface_data()
  user <- brms::prior_string("exponential(7)", class = "sigma_trend")
  fit <- SM(SW(mvgam(y ~ x, trend_formula = ~ AR(p = 1), data = d,
                      family = poisson(), prior = user, chains = 1,
                      iter = 400, warmup = 200, refresh = 0, silent = 2,
                      backend = "cmdstanr")))
  expect_true(grepl("sigma_trend ~ exponential(7)", fit$stancode,
                     fixed = TRUE))
  tab <- as.data.frame(fit$prior)
  expect_true("exponential(7)" %in% tab$prior[tab$class == "sigma_trend"])
  regenerated <- mvgam:::mvgam_dry_stancode(
    mvgam:::mvgam_update_call(fit, NULL, NULL, list())
  )
  expect_identical(mvgam:::mvgam_normalise_stancode(fit$stancode),
                    mvgam:::mvgam_normalise_stancode(regenerated))
})


test_that("every predict type honours a draw count", {
  # `predict()` resolves `ndraws` into indices and then nulls the
  # count, so a branch forwarding the count rather than the indices
  # answered with the whole posterior. Only `type = "response"` was
  # forwarding them, so `predict(type = "expected", ndraws = 50)`
  # silently returned every draw.
  fit <- readRDS(file.path("fixtures", "val_mvgam_ar1_fx.rds"))
  for (ty in c("response", "link", "expected")) {
    got <- predict(fit, type = ty, ndraws = 25L, summary = FALSE)
    expect_identical(nrow(got), 25L)
  }
})
