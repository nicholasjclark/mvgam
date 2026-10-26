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
#'   variance for \eqn{k} explained by an exogenous shock to \eqn{j}.
#'   These objects hold that proportion.
#'
#'   The orthogonalisation takes the lower Cholesky factor of the
#'   innovation covariance. The decomposition then depends on the order
#'   of the series. The first series takes all of its own one-step
#'   forecast error variance. Each later series shares its one-step
#'   variance with the series before it. The order is that of the
#'   levels of the series factor.
#'
#'   A decomposition is a `K` by `K` matrix per horizon per posterior
#'   draw, which on a wide panel runs to hundreds of megabytes. A reader
#'   needs only a median and an interval from them. [fevd()] returns
#'   that summary. The two forms have different classes:
#'
#'   - `mvgam_fevd_summary`, the default. A long-format `tibble`
#'     inheriting from `mvgam_var_surface_summary`, with one row per
#'     shock-response pair per horizon. `shock` names the pair as
#'     `"<from> -> <to>"`, taking the model's own series names where
#'     the latent processes are the series and `"Process_k"` where a
#'     factor VAR's processes are latent factors instead. `horizon`
#'     counts the steps from the forecast origin. Three columns hold
#'     the posterior median and the interval bounds, named for the
#'     percentiles asked for: `fevdQ50`, `fevdQ2.5` and `fevdQ97.5` by
#'     default.
#'
#'   - `mvgam_fevd`, returned under `summary = FALSE`. A `list` with
#'     one element per posterior draw, each holding the decomposition
#'     matrices for that draw. Pass it to `summary()` to reach the form
#'     above.
#'
#'   Both forms take `ndraws` and `draw_ids`. A subset of the posterior
#'   computes faster.
#'
#' @seealso [mvgam()], [VAR()], [fevd()], [irf()]
#'
#' @references Lutkepohl H (2005). New Introduction to Multiple Time Series
#'   Analysis. Springer, Berlin. \doi{10.1007/978-3-540-27752-1}
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
#' @inheritParams summary.mvgam_irf
#' @param object an object of class `mvgam_fevd` obtained using the
#'   \code{fevd()} function. This object will contain draws from the posterior
#'   distribution of the forecast error variance decompositions.
#'
#'
#' @param ... Unused. Anything passed here is refused.
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
summary.mvgam_fevd <- function(object, probs = c(0.025, 0.975), ...) {
  checkmate::assert_class(object, "mvgam_fevd")
  rlang::check_dots_empty()
  surface <- fevd_draws(object)
  tibble::tibble(
    shock = paste0(surface$keys$Series, " -> ", surface$keys$target),
    horizon = surface$keys$horizon,
    median_interval_cols(surface$draws, probs, "fevd")
  )
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
#'@param series Optional positions or names of the target processes
#'  shown as facets. Useful on hierarchical VAR fits where the raw
#'  K x K panel would run to dozens of panels. Defaults to all
#'  processes.
#'@param contributing Optional positions or names of the source
#'  processes stacked as contributions to each target.
#'  The shares of the sources drawn are renormalised to sum to one. On
#'  a hierarchical VAR this isolates the dependencies within one group.
#'  Defaults to all processes.
#'
#'@param ... Unused. Anything passed here is refused.
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
plot.mvgam_fevd <- function(x, series = NULL, contributing = NULL, ...) {
  checkmate::assert_class(x, "mvgam_fevd")
  rlang::check_dots_empty()
  ynames <- names(x[[1]])
  target_keep <- validate_var_plot_ids(series, ynames, "series")
  source_keep <- validate_var_plot_ids(
    contributing, ynames, "contributing"
  )
  surface <- fevd_draws(x)
  shares <- surface$keys
  shares$share <- apply(surface$draws, 2L, stats::median)
  plot_fevd_shares(shares, ynames[target_keep], ynames[source_keep])
}

#' Stacked median shares of forecast error variance, a panel per target
#'
#' The shares of the sources drawn are renormalised within each target
#' and horizon. The stack of each bar then sums to one when
#' `contributing` drops sources. The processes keep the order the VAR
#' numbers them in, which is the fit's series axis.
#'
#' @param shares Frame of `horizon`, the source `Series`, the `target`
#'   and `share`, the median share of each pair
#' @param target_names Targets drawn as panels, in the VAR's order
#' @param source_names Sources stacked in each bar, in the VAR's order
#' @return A `ggplot` object
#' @noRd
plot_fevd_shares <- function(shares, target_names, source_names) {
  checkmate::assert_data_frame(shares)
  checkmate::assert_names(names(shares),
                          must.include = c("horizon", "Series", "target",
                                           "share"))
  shares <- shares[shares$target %in% target_names &
                     shares$Series %in% source_names, , drop = FALSE]
  shares$share <- shares$share /
    stats::ave(shares$share, shares$target, shares$horizon, FUN = sum)
  shares$Series <- factor(shares$Series, levels = source_names)
  shares$target <- factor(shares$target, levels = target_names)

  # The bars partition a variance among sources. Up to three sources
  # take the shades of the red scheme the irf and stability plots draw
  # with. More sources take the Okabe-Ito hues.
  set_color_scheme_local()
  ggplot2::ggplot(
    shares,
    ggplot2::aes(fill = .data$Series, y = .data$share, x = .data$horizon)
  ) +
    ggplot2::geom_bar(position = "stack", stat = "identity",
                      colour = "white", linewidth = 0.2) +
    mvgam_model_colour_scale(length(source_names), name = "Source",
                             aesthetics = "fill") +
    ggplot2::facet_wrap(~ target) +
    mvgam_theme() +
    scale_x_steps("Forecast horizon") +
    ggplot2::labs(y = "Median contribution to forecast variance",
                  fill = "Source")
}

#' The draws of every variance share, one column per cell
#'
#' @param x An `mvgam_fevd` object
#' @return List of two elements. `keys` is a frame naming each cell's
#'   `horizon`, source `Series` and `target`. `draws` is an
#'   `(ndraws x n_cells)` matrix of shares in the same order.
#' @noRd
fevd_draws <- function(x) {
  # Each target's shares are an `(h x K)` matrix, horizons down the
  # rows and source processes across the columns.
  ynames <- names(x[[1L]])
  h <- NROW(x[[1L]][[1L]])
  n <- length(ynames)
  keys <- data.frame(
    horizon = rep(seq_len(h), times = n * n),
    Series = rep(rep(ynames, each = h), times = n),
    target = rep(ynames, each = h * n)
  )
  draws <- t(vapply(x, function(draw) {
    unlist(lapply(draw, as.vector), use.names = FALSE)
  }, numeric(nrow(keys))))
  list(keys = keys, draws = draws)
}
