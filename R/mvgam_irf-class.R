#' `mvgam_irf` object description
#'
#' A \code{mvgam_irf} object returned by function \code{\link{irf}}.
#' Run `methods(class = "mvgam_irf")` to see an overview of available methods.
#'
#' @details Generalized or Orthogonalized Impulse Response Functions can be
#'   computed using the posterior estimates of Vector Autoregressive parameters.
#'   This function generates a positive "shock" for a target process at time
#'   `t = 0` and then calculates how each of the remaining processes in the
#'   latent VAR are expected to respond over the forecast horizon `h`. The
#'   function computes IRFs for all processes in the object and returns them in
#'   an array that can be plotted using the S3 `plot` function. To inspect
#'   community-level metrics of stability using latent VAR processes, you can
#'   use the related [stability()] function.
#'
#'   A `mvgam_irf` object contains a `list` of posterior impulse response
#'   functions, each stored as its own `list`
#'
#' @seealso [mvgam], [VAR]
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
plot.mvgam_irf = function(x, series = 1, ...) {
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
  h <- dim(x[[1]][[1]])[1]
  ndraws <- length(x)

  # Pack the (ndraws x h) draws matrix for each response process
  # side-by-side, tagged with a faceting key. `mvgam_band_layer`
  # and `mvgam_median_layer` then handle quantile-band + median
  # construction per panel via their `group` arg.
  resp_keys <- paste0(
    "Process_~", series, " %->% Process_~", seq_len(n_processes)
  )
  draws_mat <- do.call(cbind, lapply(seq_len(n_processes), function(resp) {
    t(vapply(x, function(draw) draw[[series]][, resp], numeric(h)))
  }))
  times <- rep(seq_len(h), times = n_processes)
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
