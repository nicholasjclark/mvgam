#' Forecast uncertainty by source
#'
#' @description
#' Shows where a forecast's uncertainty comes from and how each source
#' grows with the horizon. The variance of the forecast at each horizon splits among the
#' model's terms, its random effects, the latent dynamics, process error
#' and, on the response scale, observation noise.
#'
#' @param object A fitted `mvgam` object.
#' @param newdata Data frame of future times, as [forecast.mvgam()] takes
#'   it. `NULL` uses the data held out at fit time.
#' @param ... Unused. Anything passed here is refused.
#' @param scale `"link"` (the default) splits the predictor, where its
#'   parts add and the split is exact. `"expected"` splits the expected
#'   value. Its variance grows with the mean, which lets a covariate's
#'   swings move it between horizons. `"response"` adds observation noise,
#'   giving the variance of new observations.
#' @param n_replicates Number of times each draw is propagated, at least
#'   two. More replicates steady the process error.
#' @param n_boot Number of bootstrap resamples of the draws, which give
#'   each share its interval.
#' @param ndraws,draw_ids The posterior draws to use, as
#'   [hindcast.mvgam()] takes them. `NULL` for both uses every draw.
#' @param re_formula,allow_new_levels,sample_new_levels How group-level
#'   terms enter, as [forecast.mvgam()] takes them. A level the fit never
#'   saw adds the spread among levels to `random effects`.
#' @param resp The response to report, needed on a model with several.
#'
#' @details
#' With \eqn{\theta} the parameters and \eqn{y_h} the forecast at horizon
#' \eqn{h}, the law of total variance gives
#' \deqn{\mathrm{Var}(y_h) = \mathrm{Var}(\mathrm{E}[y_h \mid \theta]) +
#'   \mathrm{E}(\mathrm{Var}[y_h \mid \theta]).}
#' Each draw is propagated `n_replicates` times with fresh innovations.
#' The variance across a draw's replicates, averaged over the draws, is
#' the `process error`. The variance of the replicates' means is the
#' parameter uncertainty, less its known excess of the process error over
#' `n_replicates`. This holds exactly on every scale. Monte Carlo error
#' can leave a small estimate negative. No estimate is clipped.
#'
#' *Sources*. Parameter uncertainty splits by source, each of one kind:
#' \describe{
#'   \item{`coefficients`}{each population-level term, smooth or `gp()`.
#'     This uncertainty falls with more data.}
#'   \item{`random effects`}{each group-level term}
#'   \item{`latent dynamics`}{the latent state's mean path, holding the
#'     uncertain last state and trend parameters}
#'   \item{`process error`}{what the trend's innovations add. It persists
#'     however much data arrives.}
#'   \item{`observation`}{noise around the expected value, under
#'     `scale = "response"` alone}
#' }
#' Terms of the trend formula are labelled `(trend)`. A term that is zero
#' at every forecast row is left out.
#'
#' *Contributions*. Parts of a model share a posterior: an intercept and
#' a covariate's effect can each be uncertain while their sum is not. A
#' source's own `variance` can then far exceed the forecast's. Its
#' `contribution` is its covariance with the whole forecast, which counts
#' what reaches the forecast. The contributions sum to the forecast's
#' variance. A source offsetting another takes a negative one. The
#' contribution is the source's Shapley value in the forecast's variance.
#'
#' Terms that overlap, such as `s(x)` with `t2(x, z)`, can take large
#' contributions of opposite sign. The sum over a kind stays stable, which
#' is why the overview and the default plot work by kind.
#'
#' On the link scale the contributions are exact. On the other scales a
#' term's values are the expected value less the expected value without
#' that term, as in [variance_partition()], rescaled to sum to the
#' parameter uncertainty. The terms then interact, which makes their
#' split an approximation. The split between parameter uncertainty,
#' process error and observation stays exact. On a skewed scale a few
#' draws can drive a variance, which makes the link scale steadier.
#'
#' The intervals resample the draws. They measure the Monte Carlo error
#' of a share, which falls with `ndraws` and `n_replicates`.
#'
#' @return An object of class `mvgam_forecast_uncertainty`. `print()`
#'   gives each series' overview. `summary()` returns a list of three
#'   tibbles: `overview`, one row per series, with the forecast's SD at
#'   its first and last horizons, the
#'   `dominant` kind of source at the last horizon with the bootstrap
#'   `support` for it (the fraction of resamples agreeing) and the
#'   horizon `changes_at` which the dominant kind first differs from the
#'   first horizon's;
#'   `horizons`, the forecast's variance at each horizon; and `sources`, each source's `variance`, `contribution` and
#'   `share` at each horizon with their interval. `plot()` stacks the
#'   sources over the horizon. A negative contribution is drawn as zero.
#'
#' @references Dietze MC (2017). Prediction in ecology: a first-principles
#'   framework. Ecological Applications 27(7), 2048-2060.
#'
#' @seealso [forecast.mvgam()] for `coef_uncertainty` and
#'   `trend_uncertainty`, [fevd()] for the sources of a VAR's forecast
#'   errors, [variance_partition()] for the fitted predictor. The
#'   `r mvgam_article("forecast_evaluation", "forecast evaluation article")`
#'   runs `forecast_uncertainty()` on rodent captures.
#'
#' @family forecasting
#' @examples
#' \dontrun{
#' set.seed(21)
#' simdat <- sim_mvgam(family = poisson(), n_series = 2L,
#'                     n_timepoints = 120L, trend_model = AR(),
#'                     proportional_train = 0.8)
#' mod <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ s(x, k = 5) + AR(p = 1),
#'   data = simdat$data_train,
#'   family = poisson(),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # The kind of source that dominates by horizon
#' fu <- forecast_uncertainty(mod, newdata = simdat$data_test, ndraws = 200)
#' fu
#' plot(fu)
#'
#' # Fractions of the total, by term
#' plot(fu, type = "share", by = "source")
#'
#' # Include observation noise
#' forecast_uncertainty(mod, newdata = simdat$data_test, ndraws = 200,
#'                      scale = "response")
#' }
#'
#' @author Nicholas J Clark
#' @export
forecast_uncertainty <- function(object, ...) {
  UseMethod("forecast_uncertainty")
}


#' @rdname forecast_uncertainty
#' @method forecast_uncertainty mvgam
#' @export
forecast_uncertainty.mvgam <- function(object, newdata = NULL, ...,
                                       scale = c("link", "expected",
                                                 "response"),
                                       n_replicates = 20L, n_boot = 200L,
                                       ndraws = NULL, draw_ids = NULL,
                                       re_formula = NULL,
                                       allow_new_levels = FALSE,
                                       sample_new_levels = "uncertainty",
                                       resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  rlang::check_dots_empty()
  scale <- match.arg(scale)
  checkmate::assert_int(n_replicates, lower = 2L)
  checkmate::assert_count(n_boot, positive = TRUE)
  require_fitted_model(object, "forecast_uncertainty")
  require_series_axis(object, "forecast_uncertainty")
  resolve_resp(object, resp, required = TRUE,
               caller = "forecast_uncertainty")
  setup <- forecast_setup(object, newdata, "link", resp, ndraws, draw_ids,
                          re_formula, allow_new_levels, sample_new_levels)
  parts <- forecast_parts_of(object, setup, TRUE, TRUE, resp,
                             replicates = as.integer(n_replicates))
  cells <- forecast_variance_cells(object, parts, setup, scale, resp)
  reported <- Filter(function(series) length(setup$fc_grid$times[[series]]),
                     setup$reported)
  tables <- lapply(reported, function(series) {
    series_variance_tables(cells(series), series,
                           setup$fc_grid$times[[series]], n_boot)
  })
  table_of <- function(name) {
    tibble::as_tibble(do.call(rbind, lapply(tables, `[[`, name)))
  }
  structure(
    list(
      sources = table_of("sources"),
      horizons = table_of("horizons"),
      boot = stats::setNames(lapply(tables, `[[`, "boot"), reported),
      series = reported,
      scale = scale,
      n_replicates = length(parts$latent),
      draw_ids = setup$draw_idx
    ),
    class = "mvgam_forecast_uncertainty"
  )
}


#' A term's label when it comes from the trend formula
#'
#' @param term Character term labels
#' @return Character labels
#' @noRd
trend_term_label <- function(term) {
  sprintf("%s (trend)", term)
}


#' Every group-level term of a fit, as its forecast parts name them
#'
#' @param object A fitted `mvgam` object
#' @return Character labels
#' @noRd
group_term_labels <- function(object) {
  labels <- function(side) {
    ranef <- side_model(object, side)$ranef
    if (!NROW(ranef)) {
      return(character())
    }
    vapply(split(ranef, ranef$id), group_block_label, character(1L),
           USE.NAMES = FALSE)
  }
  c(labels("obs"), trend_term_label(labels("trend")))
}


#' The matrices a forecast's variance is split from
#'
#' Everything is computed once on the requested scale, over every
#' series.
#'
#' @param object A fitted `mvgam` object
#' @param parts What `forecast_parts_of()` gives
#' @param setup What `forecast_setup()` gives
#' @param scale `"link"`, `"expected"` or `"response"`
#' @param resp A response name, or NULL
#' @return A function of a series, giving `series_variance_tables()`
#'   its list of `[ndraws x h]` matrices: `mean`, `within` and `noise`,
#'   with `effects`, one matrix per source
#' @noRd
forecast_variance_cells <- function(object, parts, setup, scale, resp) {
  sources <- c(parts$obs, stats::setNames(
    parts$trend, trend_term_label(names(parts$trend))
  ))
  fixed <- Reduce(`+`, sources, 0)
  sources <- Filter(function(source) any(source != 0), sources)
  kinds <- ifelse(names(sources) %in% group_term_labels(object),
                  "random effects", "coefficients")
  names(kinds) <- names(sources)
  paths <- lapply(parts$latent, function(latent) {
    Reduce(`+`, latent, fixed)
  })
  n <- length(paths)
  centre <- Reduce(`+`, paths) / n
  if (length(parts$latent[[1L]])) {
    sources[["latent dynamics"]] <- centre - fixed
    kinds[["latent dynamics"]] <- "latent dynamics"
  }
  expect <- if (scale == "link") {
    identity
  } else {
    do.call(epred_map, c(
      list(object, parts$frame, setup$draw_idx, resp, dim(centre),
           ordinal = "mean"),
      setup$group_level
    ))
  }
  on_scale <- lapply(paths, expect)
  whole <- Reduce(`+`, on_scale) / n
  within <- if (n > 1L) {
    Reduce(`+`, lapply(on_scale, function(m) (m - whole)^2)) / (n - 1L)
  } else {
    0 * whole
  }
  noise <- if (scale == "response") {
    parts$slice(Reduce(`+`, Map(function(path, mu) {
      (do.call(draw_observations, c(
        list(object, path, parts$frame, setup$draw_idx, resp = resp),
        setup$group_level
      )) - mu)^2
    }, paths, on_scale)) / n)
  }
  # A source's effect is what the forecast loses without it. On the link
  # scale that is the source itself.
  with_all <- expect(centre)
  effects <- lapply(sources, function(source) {
    parts$slice(with_all - expect(centre - source))
  })
  whole <- parts$slice(whole)
  within <- parts$slice(within)
  function(series) {
    list(mean = whole[[series]], within = within[[series]],
         noise = noise[[series]], effects = lapply(effects, `[[`, series),
         kinds = kinds, replicates = n)
  }
}


#' Each source's covariance with a matrix, over some draws
#'
#' The latent state's mean path holds the replicates' Monte Carlo error,
#' a known excess of the process error over the replicates.
#'
#' @param cell What the function from `forecast_variance_cells()` gives
#' @param rows The draws
#' @param with `[draws x h]` matrix, or `NULL` for each source's own
#'   variance
#' @return `[h x n_sources]` matrix, columns named by source
#' @noRd
effect_covariance <- function(cell, rows, with = NULL) {
  h <- ncol(cell$mean)
  covariances <- vapply(cell$effects, function(effect) {
    column_cov(effect[rows, , drop = FALSE], with)
  }, numeric(h))
  covariances <- matrix(covariances, nrow = h,
                        dimnames = list(NULL, names(cell$effects)))
  if ("latent dynamics" %in% colnames(covariances)) {
    covariances[, "latent dynamics"] <- covariances[, "latent dynamics"] -
      colMeans(cell$within[rows, , drop = FALSE]) / cell$replicates
  }
  covariances
}


#' Each source's contribution to the forecast's variance
#'
#' @inheritParams effect_covariance
#' @return `[h x n_sources]` matrix summing by row to the forecast's
#'   variance, columns named by source
#' @noRd
variance_split <- function(cell, rows = seq_len(nrow(cell$mean))) {
  whole <- cell$mean[rows, , drop = FALSE]
  process <- colMeans(cell$within[rows, , drop = FALSE])
  parameter <- column_cov(whole) - process / cell$replicates
  sources <- effect_covariance(cell, rows, whole)
  cbind(
    sources * parameter / rowSums(sources),
    `process error` = process,
    observation = if (!is.null(cell$noise)) {
      colMeans(cell$noise[rows, , drop = FALSE])
    }
  )
}


#' The variance tables of one series
#'
#' @param cell What the function from `forecast_variance_cells()` gives
#' @param series The series
#' @param times The series' forecast times
#' @param n_boot Number of resamples of the draws
#' @return List of `sources` and `horizons`, data frames, with `boot`, an
#'   `[n_boot x n_cells]` matrix of shares in the rows of `sources`
#' @noRd
series_variance_tables <- function(cell, series, times, n_boot) {
  contributions <- variance_split(cell)
  n_draws <- nrow(cell$mean)
  h <- nrow(contributions)
  source <- colnames(contributions)
  boot <- vapply(seq_len(n_boot), function(b) {
    resampled <- variance_split(cell, sample.int(n_draws, replace = TRUE))
    as.vector(resampled / rowSums(resampled))
  }, numeric(length(contributions)))
  own <- contributions
  own[, names(cell$effects)] <- effect_covariance(cell, seq_len(n_draws))
  total <- rowSums(contributions)
  kinds <- c(cell$kinds, `process error` = "process error",
             observation = "observation")
  list(
    sources = data.frame(
      series = series, time = rep(as.numeric(times), length(source)),
      horizon = rep(seq_len(h), length(source)),
      source = rep(source, each = h), kind = rep(kinds[source], each = h),
      variance = as.vector(own), contribution = as.vector(contributions),
      share = as.vector(contributions / total), row.names = NULL
    ),
    horizons = data.frame(series = series, time = as.numeric(times),
                          horizon = seq_len(h), variance = total),
    boot = t(boot)
  )
}


#' One row per series of what a forecast's split shows
#'
#' @param x An `mvgam_forecast_uncertainty` object
#' @return Data frame, as `summary()` documents it
#' @noRd
forecast_overview <- function(x) {
  do.call(rbind, lapply(x$series, function(series) {
    rows <- x$sources[x$sources$series == series, ]
    variance <- x$horizons$variance[x$horizons$series == series]
    leaders <- vapply(split(rows, rows$horizon), function(step) {
      by_kind <- tapply(step$share, step$kind, sum)
      names(by_kind)[which.max(by_kind)]
    }, character(1L))
    top <- leaders[[length(leaders)]]
    last <- rows$horizon == max(rows$horizon)
    resampled <- t(rowsum(t(x$boot[[series]][, last, drop = FALSE]),
                          rows$kind[last]))
    data.frame(
      series = series,
      sd_first = sqrt(variance[1L]),
      sd_last = sqrt(variance[length(variance)]),
      dominant = top,
      support = mean(colnames(resampled)[max.col(resampled, "first")] == top),
      changes_at = unname(which(leaders != leaders[1L]))[1L],
      row.names = NULL
    )
  }))
}


#' @param probs Interval probabilities of the table. Defaults to
#'   `c(0.025, 0.975)`.
#' @rdname forecast_uncertainty
#' @method summary mvgam_forecast_uncertainty
#' @export
summary.mvgam_forecast_uncertainty <- function(object,
                                               probs = c(0.025, 0.975),
                                               ...) {
  rlang::check_dots_empty()
  intervals <- do.call(rbind, lapply(object$boot, median_interval_cols,
                                     probs = probs, prefix = "share_"))
  list(
    overview = tibble::as_tibble(forecast_overview(object)),
    horizons = object$horizons,
    sources = tibble::as_tibble(cbind(object$sources, intervals))
  )
}


#' @param digits Number of decimal places to print. Defaults to `2`.
#' @rdname forecast_uncertainty
#' @method print mvgam_forecast_uncertainty
#' @export
print.mvgam_forecast_uncertainty <- function(x, digits = 2L, ...) {
  checkmate::assert_count(digits)
  overview <- forecast_overview(x)
  cat("Forecast uncertainty by source, ", variance_scale_label(x$scale),
      ", ", length(x$draw_ids), " draws, ", x$n_replicates, " replicates\n\n",
      sep = "")
  numbers <- vapply(overview, is.double, logical(1L))
  overview[numbers] <- lapply(overview[numbers], round, digits = digits)
  print(tibble::as_tibble(overview))
  cat("\nsummary() gives every source at every horizon.\n")
  invisible(x)
}


#' @param x An `mvgam_forecast_uncertainty` object from
#'   [forecast_uncertainty()].
#' @param type `"variance"` (the default) stacks each source's absolute
#'   variance.
#'   `"share"` stacks its fraction of the total.
#' @param by `"kind"` (the default) stacks each kind of source.
#'   `"source"` stacks each term.
#' @param series The series to plot, by name or position. `NULL` plots
#'   every series.
#' @rdname forecast_uncertainty
#' @method plot mvgam_forecast_uncertainty
#' @export
plot.mvgam_forecast_uncertainty <- function(x, type = c("variance", "share"),
                                            by = c("kind", "source"),
                                            series = NULL, ...) {
  type <- match.arg(type)
  by <- match.arg(by)
  rlang::check_dots_empty()
  selected <- x$series[resolve_series(series, x$series)]
  stacked <- as.data.frame(x$sources)
  stacked <- stacked[stacked$series %in% selected, ]
  groups <- unique(stacked[[by]][stacked$contribution != 0])
  stacked <- stacked[stacked[[by]] %in% groups, ]
  stacked$group <- factor(stacked[[by]], levels = groups)
  stacked$series <- factor(stacked$series, levels = selected)
  stacked <- stats::aggregate(contribution ~ series + horizon + group,
                              stacked, sum)
  stacked$total <- stats::ave(stacked$contribution, stacked$series,
                              stacked$horizon, FUN = sum)
  # A bar cannot stack below zero
  stacked$shown <- pmax(stacked$contribution, 0)
  stacked$shown <- stacked$shown /
    stats::ave(stacked$shown, stacked$series, stacked$horizon, FUN = sum)
  by_variance <- type == "variance"
  if (by_variance) stacked$shown <- stacked$shown * stacked$total
  label <- variance_scale_label(x$scale)
  set_color_scheme_local()
  p <- ggplot2::ggplot(
    stacked,
    ggplot2::aes(x = .data$horizon, y = .data$shown, fill = .data$group)
  ) +
    share_bars(length(groups), reverse = TRUE) +
    scale_x_steps("Forecast horizon") +
    mvgam_theme()
  p <- if (by_variance) {
    p + ggplot2::labs(y = paste0("Variance of ", label))
  } else {
    p + ggplot2::scale_y_continuous(labels = percent_labels) +
      ggplot2::labs(y = paste0("Share of ", label, " variance"))
  }
  if (length(selected) > 1L) {
    p <- p + mvgam_facet_series(scales = if (by_variance) "free_y" else "fixed")
  }
  p
}
