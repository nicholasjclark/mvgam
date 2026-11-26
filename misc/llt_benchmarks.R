# Seasonal LLT forecasts against standard comparators
#
# Two monthly count datasets. The mvgam models take a Poisson or
# negative binomial observation model:
#
# 1. AirPassengers (1949-1960). One series with growth and a seasonal
#    swing that widens with the level. The last 24 months are held out.
# 2. UK lung deaths, male and female (1974-1979). Two series that share
#    a season. The last 12 months are held out. Here an LLT with
#    correlated level and slope shocks meets one with independent
#    shocks. Seasonal shocks stay independent in both.
#
# Each forecast is scored on the count scale by CRPS, by the RMSE of
# its median and by the coverage of its 90% interval. ETS, the airline
# ARIMA and a seasonal naive forecast from the forecast package are the
# comparators. They model the log counts. Their predictive distribution
# on the count scale is lognormal.
#
# Fits are cached under the R user cache directory, keyed by a hash of
# each call. A rerun refits only the models whose call changed.
#
# Run from the package root: Rscript misc/llt_benchmarks.R

source("misc/llt_bench_helpers.R")

# ---------------------------------------------------------------------
# AirPassengers
# ---------------------------------------------------------------------
air <- series_to_mvgam(AirPassengers)
air_train <- air[air$time <= 120, ]
air_test <- air[air$time > 120, ]

air_fits <- list(
  `LLT + season` = fit_cached(
    "air_llt", formula = y ~ 1,
    trend_formula = ~ LLT(season = 12),
    family = poisson(), data = air_train
  ),
  `LLT + season, NB` = fit_cached(
    "air_llt_nb", formula = y ~ 1,
    trend_formula = ~ LLT(season = 12),
    family = negbinomial(), data = air_train
  ),
  `Damped LLT + season` = fit_cached(
    "air_llt_damped", formula = y ~ 1,
    trend_formula = ~ LLT(damped = TRUE, season = 12),
    family = poisson(), data = air_train
  ),
  `LLT + 2 harmonics` = fit_cached(
    "air_llt_k2", formula = y ~ 1,
    trend_formula = ~ LLT(season = 12, harmonics = 2),
    family = poisson(), data = air_train
  ),
  # A cyclic smooth whose shape changes with time, on a random walk.
  # t2() holds the cyclic main effect, the time main effect and their
  # interaction.
  `GAM + RW` = fit_cached(
    "air_gam_rw",
    formula = y ~ t2(season, time, bs = c("cc", "tp"), k = c(12, 5)),
    trend_formula = ~ RW(),
    knots = list(season = c(0.5, 12.5)),
    family = poisson(), data = air_train
  )
)

air_scores <- rbind(
  do.call(rbind, lapply(names(air_fits), function(m) {
    mvgam_scores(air_fits[[m]], air_test, m)[, -2L]
  })),
  benchmark_scores(stats::window(log(AirPassengers), end = c(1958, 12)),
                   air_test$y)
)

cat("\nAirPassengers, 24 months held out\n")
print(summarise_scores(air_scores), row.names = FALSE, digits = 3)
cat("\nSampling time (s):\n")
print(vapply(air_fits, function(f) attr(f, "elapsed") %||% NA_real_,
             numeric(1L)))

ggplot2::ggsave(file.path(cache_dir, "air_crps.png"),
                plot_crps(air_scores, "AirPassengers"),
                width = 7, height = 4, dpi = 100)
ggplot2::ggsave(
  file.path(cache_dir, "air_llt_forecast.png"),
  plot(forecast(air_fits[["LLT + season"]], newdata = air_test)),
  width = 7, height = 4, dpi = 100
)
ggplot2::ggsave(
  file.path(cache_dir, "air_llt_season.png"),
  plot(forecast(air_fits[["LLT + season"]], newdata = air_test,
                type = "trend", component = "season")),
  width = 7, height = 4, dpi = 100
)

# ---------------------------------------------------------------------
# UK lung deaths, two series sharing a season
# ---------------------------------------------------------------------
deaths_ts <- cbind(male = mdeaths, female = fdeaths)
deaths <- series_to_mvgam(deaths_ts)
deaths_train <- deaths[deaths$time <= 60, ]
deaths_test <- deaths[deaths$time > 60, ]

deaths_fits <- list(
  `LLT + season, independent` = fit_cached(
    "deaths_llt", formula = y ~ series,
    trend_formula = ~ LLT(season = 12),
    family = negbinomial(), data = deaths_train
  ),
  `LLT + season, correlated` = fit_cached(
    "deaths_llt_cor", formula = y ~ series,
    trend_formula = ~ LLT(season = 12, cor = TRUE),
    family = negbinomial(), data = deaths_train
  )
)

deaths_scores <- rbind(
  do.call(rbind, lapply(names(deaths_fits), function(m) {
    mvgam_scores(deaths_fits[[m]], deaths_test, m)
  })),
  do.call(rbind, lapply(colnames(deaths_ts), function(s) {
    train_ts <- stats::window(log(deaths_ts[, s]), end = c(1978, 12))
    out <- benchmark_scores(train_ts, deaths_test$y[deaths_test$series == s])
    cbind(out["model"], series = s, out[-1L])
  }))
)

cat("\nUK lung deaths, 12 months held out, both series\n")
print(summarise_scores(deaths_scores), row.names = FALSE, digits = 3)
cat("\nLevel shock correlation between the series:\n")
print(residual_cor(deaths_fits[["LLT + season, correlated"]]))

ggplot2::ggsave(file.path(cache_dir, "deaths_crps.png"),
                plot_crps(deaths_scores, "UK lung deaths"),
                width = 7, height = 4, dpi = 100)

cat("\nFigures and fits in", cache_dir, "\n")
