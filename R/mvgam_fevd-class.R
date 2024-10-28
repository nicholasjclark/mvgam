#' `mvgam_fevd` object description
#'
#' A \code{mvgam_fevd} object returned by function \code{\link{fevd}}.
#' Run `methods(class = "mvgam_fevd")` to see an overview of available methods.
#' @details A `mvgam_fevd` object contains a list of posterior forecast
#' error variance decompositions, each stored as
#' its own list
#' @seealso [mvgam], [VAR]
#' @author Nicholas J Clark
#' @name mvgam_fevd-class
NULL

#'Plot forecast error variance decompositions from an mvgam_fevd object

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
plot.mvgam_fevd = function(x, ...){

  ynames <- names(x[[1]])
  fevd_df = function(x){
    do.call(rbind, lapply(seq_len(length(x)), function(process){
      data.frame(horizon = 1:NROW(x[[process]]),
                 evd = as.vector(x[[process]]),
                 Series = sort(rep(colnames(x[[process]]),
                                   NROW(x[[process]]))),
                 target = ynames[process])
    }))
  }

  # Calculate posterior median error variance contributions
  fevd_dfs <- do.call(rbind, lapply(seq_len(length(x)),
                                    function(draw){
                                      fevd_df(x[[draw]])
                                    })) %>%
    dplyr::group_by(horizon, target, Series) %>%
    dplyr::summarise(mean_evd = median(evd)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(horizon, target) %>%
    dplyr::mutate(total_evd = sum(mean_evd),
                  mean_evd = mean_evd / total_evd) %>%
    dplyr::ungroup() -> mean_evds

  # Plot as a ggplot object
  ggplot2::ggplot(mean_evds,
                  ggplot2::aes(fill = Series,
                               y = mean_evd,
                               x = horizon)) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::facet_wrap(~target) +
    ggplot2::labs(x = 'Forecast horizon',
                  y = 'Median contribution to forecast variance')

}
