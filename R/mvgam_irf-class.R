#' `mvgam_irf` object description
#'
#' The objects returned by [irf()]. Run `methods(class = "mvgam_irf")`
#' and `methods(class = "mvgam_irf_summary")` to see an overview of
#' available methods.
#'
#' @details Generalized or Orthogonalized Impulse Response Functions
#'   are computed from the posterior estimates of Vector Autoregressive
#'   parameters. [irf()] applies a positive shock of one standard
#'   deviation to one process and calculates how each process in the
#'   latent VAR responds over `h` horizons, for every process in turn.
#'   Horizon 1 is the time step of the shock. [stability()] computes
#'   community-level metrics of stability from the same parameters.
#'
#'   A shock response is a `K` by `K` matrix per horizon per posterior
#'   draw. On a wide panel the draws run to hundreds of megabytes,
#'   where a median and an interval are what a reader uses. [irf()]
#'   returns the summary by default. The two forms carry different
#'   classes:
#'
#'   - `mvgam_irf_summary`, the default. A long-format `tibble`
#'     inheriting from `mvgam_var_surface_summary`, with one row per
#'     shock-response pair per horizon. `shock` names the pair as
#'     `"<from> -> <to>"`, taking the model's own series names where
#'     the latent processes are the series and `"Process_k"` where a
#'     factor VAR's processes are latent factors instead. `horizon`
#'     counts from 1, the time step of the shock. Three further
#'     columns hold the posterior median and the interval bounds,
#'     named for the percentiles asked for: `irfQ50`, `irfQ2.5` and
#'     `irfQ97.5` by default. An
#'     `irf_type` attribute records whether the responses are
#'     orthogonalised.
#'
#'   - `mvgam_irf`, returned under `summary = FALSE`. A `list` with one
#'     element per posterior draw, each itself a `list` holding one
#'     `h` by `K` matrix of responses per shocked process. Pass it to
#'     `summary()` to reach the form above.
#'
#'   Both forms take `ndraws` and `draw_ids`. A subset of the posterior
#'   shortens the computation.
#'
#' @seealso [mvgam], [VAR], [irf], [stability]
#'
#' @references Pesaran HH and Shin Y (1998). Generalized impulse
#'   response analysis in linear multivariate models. Economics Letters
#'   58, 17-29. \doi{10.1016/S0165-1765(97)00214-0}
#'
#' @author Nicholas J Clark
#'
#' @name mvgam_irf-class
NULL

#' @title Posterior summary of impulse responses
#'
#' @description This function takes an \code{mvgam_irf} object and
#'   calculates a posterior summary of the impulse responses of each
#'   series to shocks from each of the other series, at all horizons
#'
#' @param object an object of class `mvgam_irf` obtained using the
#'   \code{irf()} function. This object will contain draws from the posterior
#'   distribution of the impulse responses.
#'
#' @param probs The upper and lower percentiles to be computed by the
#'   `quantile` function, in addition to the median
#'
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A long-format `tibble` / `data.frame` reporting the posterior median,
#'   upper and lower percentiles of the impulse responses of each series to
#'   shocks from each of the other series at all horizons.
#'
#' @method summary mvgam_irf
#'
#' @seealso \code{\link{irf}}, \code{\link{plot.mvgam_irf}}
#'
#' @author Nicholas J Clark
#'
#' @export
summary.mvgam_irf <- function(object, probs = c(0.025, 0.975), ...) {
  checkmate::assert_class(object, "mvgam_irf")
  rlang::check_dots_empty()
  labels <- names(object[[1L]])
  n_processes <- length(labels)
  h <- NROW(object[[1L]][[1L]])
  do.call(rbind, lapply(seq_len(n_processes), function(series) {
    out <- tibble::tibble(
      shock = paste0(labels[series], " -> ",
                     rep(labels, each = h)),
      horizon = rep(seq_len(h), times = n_processes),
      median_interval_cols(
        irf_draws(object, series, seq_len(n_processes)), probs, "irf"
      )
    )
    out
  }))
}

#' The draws of the responses to one shock
#'
#' @param x An `mvgam_irf` object
#' @param series The process the shock originates in
#' @param resp_ids The responding processes
#' @return An `(ndraws x (h * length(resp_ids)))` matrix, with the
#'   horizons of each response in consecutive columns
#' @noRd
irf_draws <- function(x, series, resp_ids) {
  h <- NROW(x[[1L]][[1L]])
  do.call(cbind, lapply(resp_ids, function(resp) {
    t(vapply(x, function(draw) draw[[series]][, resp], numeric(h)))
  }))
}

#' Plot impulse responses from an `mvgam_irf` object
#'
#' This function takes an \code{mvgam_irf} object and produces plots of
#' Impulse Response Functions
#'
#' @param x \code{list} object of class \code{mvgam_irf}. See [irf()]
#'
#' @param series Position or name of the process given the shock
#' @param responses Optional positions or names of the processes drawn
#'   as response panels. Useful for hierarchical VAR fits where the
#'   raw K-panel grid is unreadable. Defaults to all processes.
#'
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `ggplot` object showing the expected response of each latent time
#'   series to a shock of the focal `series`
#'
#' @seealso [irf()], [fevd()], [stability()],
#'   [plot.mvgam_fevd()], [plot.mvgam_stability()],
#'   [plot.mvgam_forecast()]
#'
#' @author Nicholas J Clark
#'
#' @export
plot.mvgam_irf <- function(x, series = 1, responses = NULL, ...) {
  checkmate::assert_class(x, "mvgam_irf")
  checkmate::assert_vector(series, len = 1L, any.missing = FALSE)
  rlang::check_dots_empty()
  set_color_scheme_local()
  labels <- names(x[[1]])
  series <- validate_var_plot_ids(series, labels, "series")
  resp_ids <- validate_var_plot_ids(responses, labels, "responses")
  h <- dim(x[[1]][[1]])[1]

  # One panel per response, labelled as `summary()` labels the pair and
  # ordered as the VAR numbers its processes
  resp_keys <- paste0(labels[series], " -> ", labels[resp_ids])
  draws_mat <- irf_draws(x, series, resp_ids)
  times <- rep(seq_len(h), times = length(resp_ids))
  group <- factor(rep(resp_keys, each = h), levels = resp_keys)

  band_probs <- c(0.2, 0.4, 0.6, 0.8)
  ggplot2::ggplot() +
    mvgam_band_layer(
      draws_mat, times = times, probs = band_probs, group = group
    ) +
    mvgam_median_layer(draws_mat, times = times, group = group) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "dashed", colour = "black"
    ) +
    ggplot2::facet_wrap(~series, scales = "free_y",
                        labeller = irf_panel_labeller()) +
    scale_x_steps() +
    ggplot2::labs(y = irf_axis_label(attr(x, "irf_type"))) +
    mvgam_theme()
}

#' Panel titles of an impulse response plot
#'
#' A title names the pair as `"<from> -> <to>"`. On a hierarchical VAR
#' each name joins a group and a subgroup. A title that outgrows its
#' panel breaks after the arrow.
#'
#' @return A ggplot2 labeller
#' @noRd
irf_panel_labeller <- function() {
  ggplot2::label_wrap_gen(width = 24)
}

#' Axis title of an impulse response plot
#'
#' @param type The `irf_type` attribute, `"Generalized"` or
#'   `"Orthogonalized"`, or `NULL`
#' @return Character string
#' @noRd
irf_axis_label <- function(type) {
  checkmate::assert_string(type, null.ok = TRUE)
  if (is.null(type)) "Impulse response" else paste(type, "impulse response")
}
