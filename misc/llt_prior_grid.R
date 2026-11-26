# LLT shock-scale priors: the defaults against two recalibrations
#
# Three priors:
#   default  the package defaults. sigma_trend takes exponential(2).
#            The season's drift has a prior median of one link unit
#            over the series.
#   t        half-Student-t(3) on sigma_trend whose median moves the
#            level by one link unit over the series. The season's drift
#            has a prior median of a quarter of a link unit, a quarter
#            of the starting season's prior root mean square.
#   gamma    gamma(2, .) on all three shock scales, with the medians
#            of the t priors (the slope keeps its default median). Its
#            density rises linearly from zero.
#   inv_gamma  inverse gamma with shape 3 and the same medians. Its
#            density is near zero below a third of the median.
#
# Three Poisson or negative binomial datasets:
#   air     AirPassengers, 120 + 24 months, regular season
#   deaths  UK lung deaths, male and female, 60 + 12 months
#   sim     a simulated LLT whose season drifts, 96 + 24 months, with
#           known shock scales
#
# Each fit reports divergences, tree depth saturation, the posterior
# medians of the shock scales and the holdout CRPS. Fits run one after
# another on four cores and are cached.
#
# Run from the package root after misc/llt_benchmarks.R, whose default
# fits of air and deaths this script reuses:
# Rscript misc/llt_prior_grid.R

source("misc/llt_bench_helpers.R")

# A link unit is one for all three datasets: their spread on the log
# scale is below 2.5
prior_scales <- function(n_time, n_harmonics) {
  slope <- llt_prior_scales(n_time, FALSE, FALSE, 1, n_harmonics)$sigma
  list(
    level = signif(1.3 / sqrt(n_time - 1), 4),
    slope = slope,
    season = signif(0.25 * 1.3 / sqrt(n_harmonics * (n_time - 1)), 4)
  )
}

# gamma(2, rate) has median 1.678 / rate. A half-t(3) with scale c has
# median 0.7649 c.
gamma_rate <- function(t_scale) signif(1.678 / (0.7649 * t_scale), 4)

# inv_gamma(3, b) has median b / 2.674
inv_gamma_scale <- function(t_scale) signif(2.674 * 0.7649 * t_scale, 4)

grid_priors <- function(n_time, n_harmonics) {
  sc <- prior_scales(n_time, n_harmonics)
  list(
    default = NULL,
    t = c(
      brms::prior_string(paste0("student_t(3, 0, ", sc$level, ")"),
                         class = "sigma_trend"),
      brms::prior_string(paste0("student_t(3, 0, ", sc$season, ")"),
                         class = "sigma_season_trend")
    ),
    gamma = c(
      brms::prior_string(paste0("gamma(2, ", gamma_rate(sc$level), ")"),
                         class = "sigma_trend"),
      brms::prior_string(paste0("gamma(2, ", gamma_rate(sc$slope), ")"),
                         class = "sigma_slope_trend"),
      brms::prior_string(paste0("gamma(2, ", gamma_rate(sc$season), ")"),
                         class = "sigma_season_trend")
    ),
    inv_gamma = c(
      brms::prior_string(
        paste0("inv_gamma(3, ", inv_gamma_scale(sc$level), ")"),
        class = "sigma_trend"
      ),
      brms::prior_string(
        paste0("inv_gamma(3, ", inv_gamma_scale(sc$slope), ")"),
        class = "sigma_slope_trend"
      ),
      brms::prior_string(
        paste0("inv_gamma(3, ", inv_gamma_scale(sc$season), ")"),
        class = "sigma_season_trend"
      )
    )
  )
}

# Divergences, tree depth saturation, shock scale medians and CRPS of
# one fit
grid_row <- function(fit, test, dataset, prior) {
  np <- brms::nuts_params(fit)
  draws <- posterior::as_draws_df(fit)
  med <- function(stem) {
    cols <- grep(paste0("^", stem, "\\["), names(draws), value = TRUE)
    stats::median(as.matrix(draws[, cols]))
  }
  data.frame(
    dataset = dataset, prior = prior,
    divergent = mean(np$Value[np$Parameter == "divergent__"]),
    max_depth = mean(np$Value[np$Parameter == "treedepth__"] >= 10),
    sigma_trend = med("sigma_trend"),
    sigma_slope = med("sigma_slope_trend"),
    sigma_season = med("sigma_season_trend"),
    crps = mean(mvgam_scores(fit, test, prior)$crps),
    secs = attr(fit, "elapsed") %||% NA_real_
  )
}

# ---------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------
air <- series_to_mvgam(AirPassengers)
air_train <- air[air$time <= 120, ]
air_test <- air[air$time > 120, ]

deaths <- series_to_mvgam(cbind(male = mdeaths, female = fdeaths))
deaths_train <- deaths[deaths$time <= 60, ]
deaths_test <- deaths[deaths$time > 60, ]

# A level with a slow slope and a two-harmonic season that drifts by
# about 70% of its own size over the training months
sim_truth <- c(sigma_trend = 0.03, sigma_slope = 0.002,
               sigma_season = 0.02)
set.seed(12)
n_t <- 120
lambda <- 2 * pi * (1:2) / 12
g <- matrix(stats::rnorm(4, 0, 0.3), 2, 2)
season <- numeric(n_t)
level <- numeric(n_t)
slope <- 0.01
level[1] <- log(50)
for (t in seq_len(n_t)) {
  if (t > 1) {
    level[t] <- level[t - 1] + slope + stats::rnorm(1, 0, 0.03)
    slope <- slope + stats::rnorm(1, 0, 0.002)
    g <- cbind(cos(lambda) * g[, 1] + sin(lambda) * g[, 2],
               cos(lambda) * g[, 2] - sin(lambda) * g[, 1]) +
      matrix(stats::rnorm(4, 0, 0.02), 2, 2)
  }
  season[t] <- sum(g[, 1])
}
sim <- data.frame(time = seq_len(n_t), series = factor("sim"),
                  y = stats::rpois(n_t, exp(level + season)))
sim_train <- sim[sim$time <= 96, ]
sim_test <- sim[sim$time > 96, ]

# Formulas live at the top level. The fit cache hashes them with their
# environment.
f_one <- y ~ 1
f_series <- y ~ series
f_llt <- ~ LLT(season = 12)
f_llt_k2 <- ~ LLT(season = 12, harmonics = 2)

# ---------------------------------------------------------------------
# Fits. The default calls match misc/llt_benchmarks.R, whose cached
# fits they reuse.
# ---------------------------------------------------------------------
specs <- list(
  air = list(name = "air_llt", formula = f_one, trend_formula = f_llt,
             family = poisson(), data = air_train, test = air_test,
             priors = grid_priors(120, 5)),
  deaths = list(name = "deaths_llt", formula = f_series,
                trend_formula = f_llt, family = negbinomial(),
                data = deaths_train, test = deaths_test,
                priors = grid_priors(60, 5)),
  sim = list(name = "sim_llt", formula = f_one, trend_formula = f_llt_k2,
             family = poisson(), data = sim_train, test = sim_test,
             priors = grid_priors(96, 2))
)

rows <- list()
for (ds in names(specs)) {
  sp <- specs[[ds]]
  for (pr in names(sp$priors)) {
    args <- list(formula = sp$formula, trend_formula = sp$trend_formula,
                 family = sp$family, data = sp$data)
    if (!is.null(sp$priors[[pr]])) args$prior <- sp$priors[[pr]]
    name <- if (pr == "default") sp$name else paste0(sp$name, "_", pr)
    fit <- do.call(fit_cached, c(list(name = name), args))
    rows[[length(rows) + 1L]] <- grid_row(fit, sp$test, ds, pr)
    cat("done:", ds, pr, "\n")
  }
}

results <- do.call(rbind, rows)
cat("\nSimulated truth:\n")
print(sim_truth)
cat("\n")
print(results, row.names = FALSE, digits = 3)
saveRDS(results, file.path(cache_dir, "prior_grid_results.rds"))
