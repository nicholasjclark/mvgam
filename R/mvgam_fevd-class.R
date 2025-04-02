#' `mvgam_fevd` object description
#'
#' A \code{mvgam_fevd} object returned by function [fevd()].
#' Run `methods(class = "mvgam_fevd")` to see an overview of available methods.
#' @details A forecast error variance decomposition is useful for quantifying the amount
#' of information each series that in a Vector Autoregression contributes to the forecast
#' distributions of the other series in the autoregression. This object contains
#' the forecast error variance decomposition using the
#' orthogonalised impulse response coefficient matrices \eqn{\Psi_h}, which can be used to
#' quantify the contribution of series \eqn{j} to the
#' h-step forecast error variance of series \eqn{k}:
#' \deqn{
#' \sigma_k^2(h) = \sum_{j=1}^K(\psi_{kj, 0}^2 + \ldots + \psi_{kj,
#' h-1}^2) \quad
#' }
#' If the orthogonalised impulse reponses \eqn{(\psi_{kj, 0}^2 + \ldots + \psi_{kj, h-1}^2)}
#' are divided by the variance of the forecast error \eqn{\sigma_k^2(h)},
#' this yields an interpretable percentage representing how much of the
#' forecast error variance for \eqn{k} can be explained by an exogenous shock to \eqn{j}.
#' This percentage is what is calculated and returned in objects of class `mvgam_fevd`,
#' where the posterior distribution of variance decompositions for each variable in the
#' original model is contained in a separate slot within the returned `list` object
#' @seealso [mvgam()], [VAR()]
#' @references Lütkepohl, H (2006).
#' New Introduction to Multiple Time Series Analysis. Springer, New York.
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
#' @return A long-format `tibble` / `data.frame` reporting the posterior median,
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
  if (length(probs) != 2L) {
    stop("argument 'probs' must be a vector of length 2", call. = FALSE)
  }
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
    dplyr::group_by(horizon, target, draw) %>%
    dplyr::mutate(total_evd = sum(evd)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(evd = evd / total_evd) %>%
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
    dplyr::mutate(
      Series = gsub('process', 'Process', Series),
      target = gsub('process', 'Process', target)
    ) -> mean_evds

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
