# Helpers shared by the LLT benchmark scripts: a fit cache, forecast
# scores on the count scale and a CRPS plot. Source from the package
# root.

devtools::load_all(quiet = TRUE)

cache_dir <- file.path(tools::R_user_dir("mvgam", "cache"), "llt_benchmarks")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# Fit once per distinct call. The elapsed sampling time travels with
# the fit as an attribute.
fit_cached <- function(name, ...) {
  args <- list(...)
  path <- file.path(
    cache_dir, paste0(name, "_", substr(rlang::hash(args), 1L, 8L), ".rds")
  )
  if (file.exists(path)) {
    return(readRDS(path))
  }
  secs <- system.time(
    fit <- do.call(mvgam, c(args, chains = 4, cores = 4, seed = 1959,
                            silent = 2))
  )[["elapsed"]]
  attr(fit, "elapsed") <- secs
  saveRDS(fit, path)
  fit
}

# Scores of a matrix of predictive count samples, one column per
# horizon
score_draws <- function(draws, y) {
  checkmate::assert_matrix(draws, ncols = length(y))
  lo <- apply(draws, 2L, stats::quantile, 0.05)
  hi <- apply(draws, 2L, stats::quantile, 0.95)
  data.frame(
    horizon = seq_along(y),
    crps = scoringRules::crps_sample(y, t(draws)),
    sq_err = (apply(draws, 2L, stats::median) - y)^2,
    covered = y >= lo & y <= hi
  )
}

# Scores of a forecast package forecast of the log counts. Its
# predictive distribution is normal on the log scale. The 95% interval
# gives its standard deviation.
score_lognormal <- function(fc, y) {
  mu <- as.numeric(fc$mean)
  sd <- (as.numeric(fc$upper[, "95%"]) - mu) / stats::qnorm(0.975)
  data.frame(
    horizon = seq_along(y),
    crps = scoringRules::crps_lnorm(y, mu, sd),
    sq_err = (exp(mu) - y)^2,
    covered = abs(log(y) - mu) <= stats::qnorm(0.95) * sd
  )
}

# The forecast package comparators of one training series of log
# counts
benchmark_scores <- function(train_ts, y) {
  h <- length(y)
  fcs <- list(
    ETS = forecast::forecast(forecast::ets(train_ts), h = h,
                             level = 95),
    `Airline ARIMA` = forecast::forecast(
      forecast::Arima(train_ts, order = c(0, 1, 1),
                      seasonal = c(0, 1, 1)),
      h = h, level = 95
    ),
    `Seasonal naive` = forecast::snaive(train_ts, h = h, level = 95)
  )
  do.call(rbind, lapply(names(fcs), function(m) {
    cbind(model = m, score_lognormal(fcs[[m]], y))
  }))
}

# Scores of one mvgam fit, series by series
mvgam_scores <- function(fit, test, model) {
  fc <- forecast(fit, newdata = test)
  do.call(rbind, lapply(names(fc$forecasts), function(s) {
    cbind(model = model, series = s,
          score_draws(fc$forecasts[[s]], test$y[test$series == s]))
  }))
}

summarise_scores <- function(scores) {
  out <- stats::aggregate(
    cbind(crps, sq_err, covered) ~ model, data = scores, FUN = mean
  )
  out$rmse <- sqrt(out$sq_err)
  out$sq_err <- NULL
  out[order(out$crps), ]
}

plot_crps <- function(scores, title) {
  by_h <- stats::aggregate(crps ~ model + horizon, data = scores,
                           FUN = mean)
  n_models <- length(unique(by_h$model))
  ggplot2::ggplot(by_h, ggplot2::aes(horizon, crps, colour = model)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_manual(
      values = unname(grDevices::palette.colors(n_models + 1L,
                                                "Okabe-Ito")[-1L])
    ) +
    ggplot2::labs(title = title, x = "Forecast horizon (months)",
                  y = "CRPS", colour = NULL) +
    mvgam_theme()
}
