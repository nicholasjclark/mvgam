# Group-level terms beyond one ordinary grouping factor, under a latent
# trend.
#
# Three fits, each over two series with an AR(1) trend:
#
#   M  gaussian  y ~ x + (1 + x | mm(g1, g2, weights = cbind(w1, w2))),
#                trend_formula = ~ (1 | mm(g1, g2)) + AR(p = 1)
#   V  gaussian  bf(y1 ~ x + (1 + mmc(x1, x2) | mm(g1, g2))) +
#                bf(y2 ~ x)
#   G  gaussian  y ~ x + (1 + x | g1) + (1 | g2),
#                trend_formula = ~ (1 | g1) + AR(p = 1)
#
# A row of a multi-membership term sums its coefficient over each of its
# groups, each with its own weight and design. Dropping a membership or
# a weight stays finite and correctly shaped.
#
# G holds two groups. A formula keeping one renumbers it in the data
# while its draws keep their numbers. A new level takes the coefficients
# of fitted levels, which only real draws can compare.
#
# The reference for each predictor is brms's own, computed from the
# same draws.
#
# Run with:
#   TESTTHAT_MAX_FAILS=1000 Rscript -e "devtools::load_all('.');
#     testthat::test_file('tests/local/test-obs-group-terms.R')"

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})


set.seed(1502L)

series_levels <- c("south", "north")
n_time <- 50L
n <- 2L * n_time
latent <- matrix(0, n_time, 2L)
for (s in 1:2) {
  latent[1L, s] <- rnorm(1L, 0, 0.4)
  for (t in 2:n_time) {
    latent[t, s] <- 0.6 * latent[t - 1L, s] + rnorm(1L, 0, 0.3)
  }
}
trend <- as.numeric(latent)

levs <- paste0("m", 1:6)
dat <- data.frame(
  time = rep(seq_len(n_time), times = 2L),
  series = factor(rep(series_levels, each = n_time), levels = series_levels),
  g1 = factor(sample(levs, n, replace = TRUE), levels = levs),
  g2 = factor(sample(levs, n, replace = TRUE), levels = levs),
  x = rnorm(n),
  x1 = rnorm(n),
  x2 = rnorm(n),
  w1 = runif(n, 0.2, 1),
  w2 = runif(n, 0.2, 1)
)
share <- dat$w1 / (dat$w1 + dat$w2)
u <- rnorm(6L, 0, 0.6)
v <- rnorm(6L, 0, 0.3)
i1 <- as.integer(dat$g1)
i2 <- as.integer(dat$g2)
dat$y <- 0.5 + 0.4 * dat$x + share * (u[i1] + v[i1] * dat$x) +
  (1 - share) * (u[i2] + v[i2] * dat$x) + trend + rnorm(n, 0, 0.3)
dat$y1 <- 0.3 * dat$x + 0.5 * (u[i1] + u[i2]) +
  0.5 * (v[i1] * dat$x1 + v[i2] * dat$x2) + trend + rnorm(n, 0, 0.3)
dat$y2 <- 0.6 * dat$x + trend + rnorm(n, 0, 0.3)

models <- list(
  M = list(formula = y ~ x + (1 + x | mm(g1, g2, weights = cbind(w1, w2))),
           trend_formula = ~ (1 | mm(g1, g2)) + AR(p = 1),
           family = gaussian()),
  V = list(formula = bf(y1 ~ x + (1 + mmc(x1, x2) | mm(g1, g2))) +
             bf(y2 ~ x) + set_rescor(FALSE),
           trend_formula = ~ AR(p = 1), family = gaussian()),
  G = list(formula = y ~ x + (1 + x | g1) + (1 | g2),
           trend_formula = ~ (1 | g1) + AR(p = 1), family = gaussian())
)

build <- function(name, ...) {
  do.call(mvgam, c(models[[name]], list(data = dat, silent = 2, ...)))
}

prefits <- lapply(stats::setNames(nm = names(models)), build,
                  run_model = FALSE)

fits <- lapply(stats::setNames(nm = names(models)), function(name) {
  cached_fit(paste0("val_obs_group_", name, ".rds"), function() {
    build(name, chains = 2L, iter = 1000L, warmup = 500L,
          backend = "cmdstanr", seed = 1L)
  }, key = list(dat = dat, model = models[[name]]))
})

ids <- 1:50


test_that("each fit ran the program the package writes today", {
  # A cached fit from an older program would test that program
  for (name in names(fits)) {
    expect_identical(as.character(stancode(fits[[name]])),
                     as.character(stancode(prefits[[name]])))
  }
})


test_that("M's trend takes each cell's memberships in trend order", {
  # The trend's terms run over cells sorted by time, then by series
  expect_match(
    paste(as.character(stancode(prefits$M)), collapse = "\n"),
    "W_1_2_trend[n] * r_1_1_trend[J_1_2_trend[n]]", fixed = TRUE
  )
  sd <- standata(prefits$M)
  cells <- order(dat$time, dat$series)
  expect_identical(as.integer(sd$J_1_1_trend), i1[cells])
  expect_identical(as.integer(sd$J_1_2_trend), i2[cells])
})


test_that("every observation predictor is brms's own", {
  # A reversed frame puts each row's memberships through `newdata`
  for (fit in fits) {
    expect_lt(max(predictor_gap(fit, dat[n:1, ])), 1e-8)
  }
})


test_that("M's trend predictor is brms's own", {
  fit <- fits$M
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


test_that("the parts name each term and sum to the predictor", {
  parts <- get_linpred_parts(fits$M, dat, draw_ids = ids)
  expect_identical(names(parts$obs),
                   c("Intercept", "x",
                     "(1 + x | mm(g1, g2, weights = cbind(w1, w2)))"))
  expect_identical(names(parts$trend), "(1 | mm(g1, g2))")
  expect_parts_sum(fits$M, "state", dat)
  expect_forecast_parts_sum(fits$M, "state")
})


test_that("re_formula refuses a term no formula holds", {
  for (rf in list(~ (1 | g3), ~ (1 + x | g2))) {
    expect_error(posterior_linpred(fits$G, newdata = dat[1:2, ],
                                   re_formula = rf, draw_ids = 1:2),
                 "outside the model")
  }
})


test_that("re_formula keeps the terms it names", {
  # Each kept term takes its own draws
  expect_lt(max(predictor_gap(fits$G, dat, re_formula = ~ (1 | g2))), 1e-8)
  expect_lt(max(predictor_gap(fits$G, dat, re_formula = ~ (1 + x | g1))),
            1e-8)
})


test_that("a term keeps each model's own grouping", {
  # The trend's term has no weights. Its data has no weight column.
  rf <- ~ (1 | mm(g1, g2, weights = cbind(w1, w2)))
  expect_lt(max(predictor_gap(fits$M, dat, re_formula = rf)), 1e-8)
  expect_equal(
    extract_component_linpred(fits$M, dat, component = "trend",
                              draw_ids = ids, re_formula = rf),
    extract_component_linpred(fits$M, dat, component = "trend",
                              draw_ids = ids)
  )
})


test_that("a forecast takes a new level in both formulas", {
  # `g1` enters the observation and the trend formulas. Each predictor
  # needs the argument.
  future <- dat[dat$time == n_time, ]
  future$time <- n_time + 1L
  future$g1 <- "unseen"
  expect_error(forecast(fits$G, newdata = future, ndraws = 5L), "unseen")
  expect_no_error(forecast(fits$G, newdata = future, ndraws = 5L,
                           allow_new_levels = TRUE))
})


test_that("a new level takes its coefficients from one fitted level", {
  new <- dat[1:2, ]
  new$g1 <- "unseen"
  args <- list(fits$G, newdata = new, draw_ids = ids, component = "obs",
               allow_new_levels = TRUE, sample_new_levels = "old_levels")
  part <- do.call(extract_component_parts, args)[["(1 + x | g1)"]]
  draws <- as_plain_matrix(subset_draws_rows(
    posterior::as_draws_matrix(fits$G$fit), draw_ids = ids
  ))
  term <- function(level, row) {
    draws[, paste0("r_1_1[", level, "]")] +
      draws[, paste0("r_1_2[", level, "]")] * new$x[row]
  }
  gaps <- vapply(1:6, function(level) {
    max(abs(part[, 2L] - term(level, 2L)))
  }, numeric(1L))
  expect_lt(min(gaps), 1e-8)
  expect_equal(unname(part[, 1L]),
               unname(term(which.min(gaps), 1L)))
})


cat("\nDone.\n")
