#' `mvgam_fevd` object description
#'
#' A \code{mvgam_fevd} object returned by function \code{\link{fevd}}.
#' Run `methods(class = "mvgam_fevd")` to see an overview of available methods.
#' @details A `mvgam_fevd` object contains a `list` of posterior forecast
#' error variance decompositions, each stored as
#' its own `list`
#' @seealso [mvgam], [VAR]
#' @author Nicholas J Clark
#' @name mvgam_fevd-class
NULL

#' @title Posterior summary of forecast error variance decompositions
#'
#' @description This function takes an \code{mvgam_fevd} object and
#' calculates a posterior summary of the error variance decompositions of each
#' series, at all horizons
#'
#' @param object an object of class `mvgam_fevd` obtained using the
#' \code{fevd()} function. This object will contain draws from the posterior
#' distribution of the forecast error variance decompositions.
#' @param probs The upper and lower percentiles to be computed by the `quantile` function,
#' in addition to the median
#' @param ... ignored
#'
#' @return A long-format `tibble` reporting the posterior median,
#' upper and lower percentiles of the error variance decompositions of
#' each series at all horizons.
#'
#' @method summary mvgam_fevd
#'
#' @seealso \code{\link{fevd}}, \code{\link{plot.mvgam_fevd}}
#'
#' @author Nicholas J Clark
#'
#' @export
summary.mvgam_fevd = function(object, probs = c(0.025, 0.975), ...) {

  # Calculate posterior quantiles of error variance contributions
  ynames <- names(object[[1]])
  out <- do.call(
    rbind,
    lapply(seq_len(length(object)), function(draw) {
      fevd_df(object[[draw]], ynames = ynames) %>%
        dplyr::mutate(draw = draw)
    })
  ) %>%
    dplyr::group_by(horizon, target, draw) %>%
    dplyr::mutate(total_evd = sum(evd)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(evd = evd / total_evd) %>%
    dplyr::group_by(horizon, target, Series) %>%
    dplyr::mutate(
      fevd_median = median(evd),
      fevd_Qlower = quantile(evd, min(probs)),
      fevd_Qupper = quantile(evd, max(probs))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(shock = gsub('process', 'Process',
                               paste0(Series, ' -> ', target))) %>%
    dplyr::select(shock, horizon,
                  fevd_median, fevd_Qlower, fevd_Qupper) %>%
    dplyr::distinct()

  return(out)
}

#'Plot forecast error variance decompositions from an `mvgam_fevd` object
#'
#'This function takes an \code{mvgam_fevd} object and produces
#'a plot of the posterior median contributions to forecast variance for each series
#'in the fitted Vector Autoregression
#'
#'@importFrom ggplot2 ggplot aes geom_bar facet_wrap labs
#'@param x \code{list} object of class \code{mvgam_fevd}. See [fevd()]
#'@param ... ignored
#'@return A \code{\link[ggplot2]{ggplot}} object,
#'which can be further customized using the \pkg{ggplot2} package
#'@author Nicholas J Clark
#'@export
plot.mvgam_fevd = function(x, ...) {

  # Calculate posterior median error variance contributions
  ynames <- names(x[[1]])
  do.call(
    rbind,
    lapply(seq_len(length(x)), function(draw) {
      fevd_df(x[[draw]], ynames = ynames)
    })
  ) %>%
    dplyr::group_by(horizon, target, Series) %>%
    dplyr::summarise(mean_evd = median(evd)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(horizon, target) %>%
    dplyr::mutate(
      total_evd = sum(mean_evd),
      mean_evd = mean_evd / total_evd
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Series = gsub('process', 'Process', Series),
                  target = gsub('process', 'Process', target)) -> mean_evds

  # Plot as a ggplot object
  ggplot2::ggplot(
    mean_evds,
    ggplot2::aes(fill = Series, y = mean_evd, x = horizon)
  ) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::facet_wrap(~target) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = 'Forecast horizon',
      y = 'Median contribution to forecast variance'
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
        Series = sort(rep(colnames(x[[process]]), NROW(x[[process]]))),
        target = ynames[process]
      )
    })
  )
}
