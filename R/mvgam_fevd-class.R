#' `mvgam_fevd` object description
#'
#' The objects returned by [fevd()]. Run
#' `methods(class = "mvgam_fevd")` and
#' `methods(class = "mvgam_fevd_summary")` to see an overview of
#' available methods.
#'
#' @details A forecast error variance decomposition quantifies how much
#'   of the forecast uncertainty in one series of a Vector
#'   Autoregression is attributable to each of the others. It is built
#'   from the orthogonalised impulse response coefficient matrices
#'   \eqn{\Psi_h}, which give the contribution of series \eqn{j} to the
#'   h-step forecast error variance of series \eqn{k}:
#'   \deqn{
#'   \sigma_k^2(h) = \sum_{j=1}^K(\psi_{kj, 0}^2 + \ldots + \psi_{kj,
#'   h-1}^2) \quad
#'   }
#'   Dividing the orthogonalised responses \eqn{(\psi_{kj, 0}^2 +
#'   \ldots + \psi_{kj, h-1}^2)} by the forecast error variance
#'   \eqn{\sigma_k^2(h)} gives the proportion of the forecast error
#'   variance for \eqn{k} explained by an exogenous shock to \eqn{j},
#'   and that proportion is what these objects carry.
#'
#'   A decomposition is a `K` by `K` matrix per horizon per posterior
#'   draw, which on a wide panel runs to hundreds of megabytes for a
#'   quantity read as a median and an interval. [fevd()] therefore
#'   returns the summary, and the two forms carry different classes:
#'
#'   - `mvgam_fevd_summary`, the default. A long-format `tibble`
#'     inheriting from `mvgam_var_surface_summary`, with one row per
#'     shock-response pair per horizon. `shock` names the pair as
#'     `"Process_j -> Process_k"`, `horizon` is the step ahead, and
#'     three further columns carry the posterior median and the
#'     interval bounds, named for the percentiles asked for:
#'     `fevdQ50`, and by default `fevdQ2.5` and `fevdQ97.5`.
#'
#'   - `mvgam_fevd`, returned under `summary = FALSE`. A `list` with
#'     one element per posterior draw, each holding the decomposition
#'     matrices for that draw. Pass it to `summary()` to reach the form
#'     above.
#'
#'   Both forms take `ndraws` and `draw_ids`, so a subset of the
#'   posterior answers faster where the full one is not needed.
#'
#' @seealso [mvgam()], [VAR()], [fevd()], [irf()]
#'
#' @references Lütkepohl, H (2006). New Introduction to Multiple Time
#'   Series Analysis. Springer, New York.
#'
#' @author Nicholas J Clark
#'
#' @name mvgam_fevd-class
NULL

#' @title Posterior summary of forecast error variance decompositions
#'
#' @description This function takes an \code{mvgam_fevd} object and calculates
#'   a posterior summary of the error variance decompositions of each series,
#'   at all horizons
#'
#' @param object an object of class `mvgam_fevd` obtained using the
#'   \code{fevd()} function. This object will contain draws from the posterior
#'   distribution of the forecast error variance decompositions.
#'
#' @param probs The upper and lower percentiles to be computed by the
#'   `quantile` function, in addition to the median
#'
#' @param ... ignored
#'
#' @return A long-format `tibble` / `data.frame` reporting the posterior median,
#'   upper and lower percentiles of the error variance decompositions of each
#'   series at all horizons.
#'
#' @method summary mvgam_fevd
#'
#' @seealso \code{\link{fevd}}, \code{\link{plot.mvgam_fevd}}
#'
#' @author Nicholas J Clark
#'
#' @export
summary.mvgam_fevd = function(object, probs = c(0.025, 0.975), ...) {
  checkmate::assert_class(object, "mvgam_fevd")
  checkmate::assert_numeric(probs, len = 2L, lower = 0, upper = 1,
                            any.missing = FALSE, sorted = TRUE)
  validate_proportional(min(probs))
  validate_proportional(max(probs))

  # Calculate posterior quantiles of error variance contributions
  ynames <- names(object[[1]])
  out <- do.call(
    rbind,
    lapply(seq_len(length(object)), function(draw) {
      fevd_df(object[[draw]], ynames = ynames) %>%
        dplyr::mutate(draw = draw)
    })
  ) %>%
    dplyr::group_by(horizon, target, Series) %>%
    dplyr::mutate(
      fevdQ50 = median(evd),
      fevd_Qlower = quantile(evd, min(probs)),
      fevd_Qupper = quantile(evd, max(probs))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      shock = gsub('process', 'Process', paste0(Series, ' -> ', target))
    ) %>%
    dplyr::select(shock, horizon, fevdQ50, fevd_Qlower, fevd_Qupper) %>%
    dplyr::distinct()
  colnames(out) <- c(
    'shock',
    'horizon',
    'fevdQ50',
    paste0('fevdQ', 100 * min(probs)),
    paste0('fevdQ', 100 * max(probs))
  )

  return(out)
}

#'Plot forecast error variance decompositions from an `mvgam_fevd` object
#'
#'This function takes an \code{mvgam_fevd} object and produces
#'a plot of the posterior median contributions to forecast variance for each series
#'in the fitted Vector Autoregression
#'
#'@importFrom ggplot2 ggplot aes geom_bar facet_wrap labs
#'
#'@param x \code{list} object of class \code{mvgam_fevd}. See [fevd()]
#'@param series Optional integer vector selecting which
#'  target processes should be shown as facets. Useful on
#'  hierarchical VAR fits where the raw K x K panel would run to
#'  dozens of panels. Defaults to all processes.
#'@param contributing Optional integer vector selecting which
#'  source processes should appear as stacked contributions to
#'  each target. Sources not in this set are dropped before
#'  computing shares, so the bars re-normalise across the
#'  retained sources; this makes it easy to zoom in on
#'  within-country dependencies on hierarchical VARs. Defaults to
#'  all processes.
#'
#'@param ... ignored
#'
#'@return A \code{\link[ggplot2]{ggplot}} object,
#'  which can be further customized using the \pkg{ggplot2} package
#'
#'@seealso [fevd()], [irf()], [stability()],
#'  [plot.mvgam_irf()], [plot.mvgam_stability()],
#'  [plot.mvgam_forecast()]
#'
#'@author Nicholas J Clark
#'
#'@export
plot.mvgam_fevd = function(x, series = NULL, contributing = NULL, ...) {
  checkmate::assert_class(x, "mvgam_fevd")
  # Calculate posterior median error variance contributions
  ynames <- names(x[[1]])
  n_proc <- length(ynames)
  target_keep <- validate_var_plot_ids(series, n_proc, "series")
  source_keep <- validate_var_plot_ids(
    contributing, n_proc, "contributing"
  )
  target_names <- ynames[target_keep]
  source_names <- paste0("process_", source_keep)

  do.call(
    rbind,
    lapply(seq_len(length(x)), function(draw) {
      fevd_df(x[[draw]], ynames = ynames)
    })
  ) %>%
    dplyr::filter(
      target %in% target_names,
      Series %in% source_names
    ) %>%
    dplyr::group_by(horizon, target, Series) %>%
    dplyr::summarise(mean_evd = median(evd), .groups = "drop") %>%
    # When `contributing` drops sources, the retained shares no
    # longer sum to 1; re-normalise per (target, horizon) so the
    # stacked bars still read as a proper variance decomposition.
    dplyr::group_by(target, horizon) %>%
    dplyr::mutate(
      mean_evd = mean_evd / sum(mean_evd)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Series = gsub('process', 'Process', Series),
      target = gsub('process', 'Process', target)
    ) -> mean_evds

  # FEVD bars encode a categorical partition (which series
  # contributed how much of the forecast variance), so use the
  # colour-blind-safe Okabe-Ito qualitative palette rather than
  # the single-hue bayesplot scheme used for the ribbon plots.
  # `mvgam_categorical_palette()` recycles when N > 8.
  series_levels <- sort(unique(mean_evds$Series))
  fill_values <- mvgam_categorical_palette(length(series_levels))
  names(fill_values) <- series_levels

  ggplot2::ggplot(
    mean_evds,
    ggplot2::aes(fill = Series, y = mean_evd, x = horizon)
  ) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::scale_fill_manual(values = fill_values) +
    ggplot2::facet_wrap(~target) +
    mvgam_theme() +
    ggplot2::labs(
      x = "Forecast horizon",
      y = "Median contribution to forecast variance"
    )
}

#'@noRd
fevd_df = function(x, ynames) {
  do.call(
    rbind,
    lapply(seq_len(length(x)), function(process) {
      data.frame(
        horizon = 1:NROW(x[[process]]),
        evd = as.vector(x[[process]]),
        Series = paste0(
          'process_',
          sort(rep(
            1:length(ynames),
            NROW(x[[process]])
          ))
        ),
        target = ynames[process]
      )
    })
  )
}
