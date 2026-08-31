#' `mvgam_irf` object description
#'
#' The objects returned by [irf()]. Run `methods(class = "mvgam_irf")`
#' and `methods(class = "mvgam_irf_summary")` to see an overview of
#' available methods.
#'
#' @details Generalized or Orthogonalized Impulse Response Functions
#'   are computed from the posterior estimates of Vector Autoregressive
#'   parameters. [irf()] applies a positive shock to one process at
#'   time `t = 0` and calculates how each of the remaining processes in
#'   the latent VAR responds over the horizon `h`, for every process in
#'   turn. To inspect community-level metrics of stability from the
#'   same parameters, use [stability()].
#'
#'   A shock response is a `K` by `K` matrix per horizon per posterior
#'   draw, which on a wide panel runs to hundreds of megabytes for a
#'   quantity read as a median and an interval. [irf()] therefore
#'   returns the summary, and the two forms carry different classes:
#'
#'   - `mvgam_irf_summary`, the default. A long-format `tibble`
#'     inheriting from `mvgam_var_surface_summary`, with one row per
#'     shock-response pair per horizon. `shock` names the pair as
#'     `"Process_j -> Process_k"`, `horizon` is the step ahead, and
#'     three further columns carry the posterior median and the
#'     interval bounds, named for the percentiles asked for:
#'     `irfQ50`, and by default `irfQ2.5` and `irfQ97.5`. An
#'     `irf_type` attribute records whether the responses are
#'     orthogonalised.
#'
#'   - `mvgam_irf`, returned under `summary = FALSE`. A `list` with one
#'     element per posterior draw, each itself a `list` holding one
#'     `h` by `K` matrix of responses per shocked process. Pass it to
#'     `summary()` to reach the form above.
#'
#'   Both forms take `ndraws` and `draw_ids`, so a subset of the
#'   posterior answers faster where the full one is not needed.
#'
#' @seealso [mvgam], [VAR], [irf], [stability]
#'
#' @references PH Pesaran & Shin Yongcheol (1998).
#'   Generalized impulse response analysis in linear multivariate models.
#'   Economics Letters 58: 17–29.
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
#' @param ... ignored
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
summary.mvgam_irf = function(object, probs = c(0.025, 0.975), ...) {
  checkmate::assert_class(object, "mvgam_irf")
  checkmate::assert_numeric(probs, len = 2L, lower = 0, upper = 1,
                            any.missing = FALSE, sorted = TRUE)
  validate_proportional(min(probs))
  validate_proportional(max(probs))

  n_processes <- dim(object[[1]][[1]])[2]
  h <- dim(object[[1]][[1]])[1]
  n_draws <- length(object)

  out <- do.call(
    rbind,
    lapply(1:n_processes, function(series) {
      # Extract IRFs for the specific series
      impulse_responses <- lapply(seq_along(object), function(j) {
        object[[j]][series]
      })

      responses <- do.call(
        rbind,
        lapply(seq_along(impulse_responses), function(j) {
          data.frame(
            horizon = 1:h,
            imp_resp = as.vector(impulse_responses[[j]][[1]]),
            resp_var = paste0(
              'Process_',
              sort(rep(
                1:n_processes,
                NROW(impulse_responses[[j]][[1]])
              ))
            )
          )
        })
      ) %>%
        dplyr::mutate(shock = paste0('Process_', series, ' -> ', resp_var)) %>%

        # Calculate posterior empirical quantiles of impulse responses
        dplyr::group_by(shock, horizon) %>%
        dplyr::summarise(
          irfQ50 = median(imp_resp),
          irfQlower = quantile(imp_resp, min(probs)),
          irfQupper = quantile(imp_resp, max(probs)),
          .groups = 'keep'
        ) %>%
        dplyr::ungroup()
      colnames(responses) <- c(
        'shock',
        'horizon',
        'irfQ50',
        paste0('irfQ', 100 * min(probs)),
        paste0('irfQ', 100 * max(probs))
      )
      responses
    })
  )

  return(out)
}

#' Plot impulse responses from an `mvgam_irf` object
#'
#' This function takes an \code{mvgam_irf} object and produces plots of
#' Impulse Response Functions
#'
#' @param x \code{list} object of class \code{mvgam_irf}. See [irf()]
#'
#' @param series \code{integer} specifying which process series should be
#'   given the shock
#' @param responses Optional integer vector of process indices to
#'   display as response panels. Useful for hierarchical VAR fits
#'   where the raw K-panel grid is unreadable. Defaults to all
#'   processes.
#'
#' @param ... ignored
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
plot.mvgam_irf = function(x, series = 1, responses = NULL, ...) {
  checkmate::assert_class(x, "mvgam_irf")
  validate_pos_integer(series)
  # Lock the bayesplot scheme to the house red for the duration
  # of this call so IRFs share the visual identity of forecast(),
  # fevd() and stability() plots.
  set_color_scheme_local("red")
  n_processes <- dim(x[[1]][[1]])[2]
  if (series > n_processes) {
    stop(insight::format_error(c(
      paste0("'series' must be at most ", n_processes, "."),
      x = paste0("Got series = ", series, "."),
      i = paste0(
        "The fitted VAR has ", n_processes, " latent processes."
      )
    )))
  }
  resp_ids <- validate_var_plot_ids(responses, n_processes, "responses")
  h <- dim(x[[1]][[1]])[1]
  ndraws <- length(x)

  # Pack the (ndraws x h) draws matrix for each response process
  # side-by-side, tagged with a faceting key. `mvgam_band_layer`
  # and `mvgam_median_layer` then handle quantile-band + median
  # construction per panel via their `group` arg.
  resp_keys <- paste0(
    "Process_~", series, " %->% Process_~", resp_ids
  )
  draws_mat <- do.call(cbind, lapply(resp_ids, function(resp) {
    t(vapply(x, function(draw) draw[[series]][, resp], numeric(h)))
  }))
  times <- rep(seq_len(h), times = length(resp_ids))
  group <- rep(resp_keys, each = h)

  band_probs <- c(0.2, 0.4, 0.6, 0.8)
  ggplot2::ggplot() +
    mvgam_band_layer(
      draws_mat, times = times, probs = band_probs, group = group
    ) +
    mvgam_median_layer(draws_mat, times = times, group = group) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "dashed", colour = "black"
    ) +
    ggplot2::facet_wrap(
      ~series, scales = "free_y", labeller = ggplot2::label_parsed
    ) +
    ggplot2::labs(
      x = "Horizon",
      y = paste0(attr(x, "irf_type"), " impulse response")
    ) +
    mvgam_theme()
}
