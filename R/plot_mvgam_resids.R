#' Residual diagnostics for a fitted \pkg{mvgam} object
#'
#' This function takes a fitted \code{mvgam} object and returns various
#' residual diagnostic plots
#'
#' @importFrom graphics layout title
#'
#' @importFrom stats complete.cases qqnorm qqline acf pacf na.pass
#'
#' @importFrom mgcv bam
#'
#' @param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'
#' @param series \code{integer} specifying which series in the set is to be
#'   plotted
#'
#' @param n_draws \code{integer} specifying the number of posterior residual
#'   draws to use for calculating uncertainty in the `"ACF"` and `"pACF"` frames.
#'   Default is `100`
#'
#' @param n_points \code{integer} specifying the maximum number of points to
#'   show in the "Resids vs Fitted" and "Normal Q-Q Plot" frames. Default is
#'   `1000`
#'
#' @author Nicholas J Clark
#'
#' @details A total of four ggplot plots are generated to examine posterior
#'   Dunn-Smyth residuals for the specified series. Plots include a residuals
#'   vs fitted values plot, a Q-Q plot, and two plots to check for any
#'   remaining temporal autocorrelation in the residuals. Note, all plots only
#'   report statistics from a sample of up to `100` posterior draws (to save
#'   computational time), so uncertainty in these relationships may not be
#'   adequately represented.
#'
#' @return A facetted `ggplot` object
#'
#' @author Nicholas J Clark and Matthijs Hollanders
#'
#' @examples
#' \dontrun{
#' simdat <- sim_mvgam(
#'   n_series = 3,
#'   trend_model = AR()
#' )
#'
#' mod <- mvgam(
#'   y ~ s(season, bs = 'cc', k = 6),
#'   trend_model = AR(),
#'   noncentred = TRUE,
#'   data = simdat$data_train,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Plot Dunn Smyth residuals for some series
#' plot_mvgam_resids(mod)
#' plot_mvgam_resids(mod, series = 2)
#' }
#'
#' @export
plot_mvgam_resids = function(
  object,
  series = 1,
  n_draws = 100L,
  n_points = 1000L
) {
  # Check arguments
  if (!(inherits(object, "mvgam"))) {
    stop('argument "object" must be of class "mvgam"')
  }

  validate_pos_integer(series)
  validate_pos_integer(n_draws)
  validate_pos_integer(n_points)

  if (series > NCOL(object$ytimes)) {
    stop(
      paste0(
        'object only contains data / predictions for ',
        NCOL(object$ytimes),
        ' series'
      ),
      call. = FALSE
    )
  }

  # Take a sample of posterior draws to compute autocorrelation statistics
  # This is because acf(posterior_median_residual) can induce spurious patterns
  # due to the randomness of DS residuals;
  # rather, we want median(acf(residual_i)), where i indexes all possible draws
  # But this is computationally expensive for some models so we compromise
  # by only taking a few draws
  n_total_draws <- NROW(object$resids[[series]])
  n_samps <- min(n_draws, n_total_draws)
  hcs <- hindcast(object, type = 'expected')$hindcasts[[series]]
  resids <- object$resids[[series]]

  resid_df <- do.call(
    rbind,
    lapply(seq_len(n_samps), function(x) {
      data.frame(preds = hcs[x, ], resids = resids[x, ], .draw = x) %>%
        dplyr::filter(!is.na(resids))
    })
  )

  # Plot predictions and residuals (but limit number of points to n_points to
  # speed up plotting)
  if (NROW(resid_df) > n_points) {
    resid_df <- resid_df[sample(1:NROW(resid_df), n_points, replace = FALSE), ]
  }

  fvr_plot <- ggplot2::ggplot(resid_df, ggplot2::aes(preds, resids)) +
    ggplot2::geom_point(shape = 16, col = 'white', size = 1.25, alpha = 0.4) +
    ggplot2::geom_point(shape = 16, col = 'black', size = 1, alpha = 0.4) +
    ggplot2::geom_smooth(
      method = "gam",
      formula = y ~ s(x, bs = "cs"),
      colour = "#7C000060",
      fill = "#7C000040"
    ) +
    ggplot2::labs(
      title = "Resids vs Fitted",
      x = "Fitted values",
      y = "DS residuals"
    ) +
    ggplot2::theme_bw()

  # Q-Q plot
  qq_plot <- ggplot2::ggplot(resid_df, ggplot2::aes(sample = resids)) +
    ggplot2::stat_qq_line(colour = "#8F2727", linewidth = 1) +
    ggplot2::stat_qq(shape = 16, col = 'white', size = 1.25, alpha = 0.4) +
    ggplot2::stat_qq(shape = 16, col = 'black', size = 1, alpha = 0.4) +
    ggplot2::labs(
      title = "Normal Q-Q Plot",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggplot2::theme_bw()

  # ACF plot
  acf_stats <- do.call(
    rbind,
    lapply(seq_len(n_samps), function(x) {
      acf_calc <- acf(resids[x, ], plot = FALSE, na.action = na.pass)
      data.frame(
        acf = acf_calc$acf[,, 1],
        lag = acf_calc$lag[, 1, 1],
        denom = sqrt(acf_calc$n.used)
      ) %>%
        dplyr::filter(lag > 0)
    })
  ) %>%
    dplyr::group_by(lag) %>%
    dplyr::mutate(
      ylow = quantile(acf, probs = 0.05),
      yqlow = quantile(acf, probs = 0.2),
      ymidlow = quantile(acf, probs = 0.25),
      ymidhigh = quantile(acf, probs = 0.75),
      yqhigh = quantile(acf, probs = 0.8),
      yhigh = quantile(acf, probs = 0.95)
    ) %>%
    dplyr::select(-acf) %>%
    dplyr::distinct()

  acf_plot <- ggplot2::ggplot(acf_stats, ggplot2::aes(x = lag)) +
    ggplot2::geom_hline(
      yintercept = c(-1, 1) *
        qnorm((1 + 0.95) / 2) /
        acf_stats$denom[1],
      linetype = "dashed"
    ) +
    ggplot2::geom_hline(yintercept = 0, colour = "#7C0000", linewidth = 0.25) +
    ggplot2::geom_segment(
      colour = "#DCBCBC",
      linewidth = 1.5,
      ggplot2::aes(y = ylow, yend = yhigh)
    ) +
    ggplot2::geom_segment(
      colour = "#B97C7C",
      linewidth = 1.5,
      ggplot2::aes(y = yqlow, yend = yqhigh)
    ) +
    ggplot2::geom_segment(
      colour = "#7C0000",
      linewidth = 1.5,
      ggplot2::aes(y = ymidlow, yend = ymidhigh)
    ) +
    ggplot2::labs(title = "ACF", x = "Lag", y = "Autocorrelation") +
    ggplot2::theme_bw()

  # PACF plot
  pacf_stats <- do.call(
    rbind,
    lapply(seq_len(n_samps), function(x) {
      acf_calc <- pacf(resids[x, ], plot = FALSE, na.action = na.pass)
      data.frame(
        pacf = acf_calc$acf[,, 1],
        lag = acf_calc$lag[, 1, 1],
        denom = sqrt(acf_calc$n.used)
      ) %>%
        dplyr::filter(lag > 0)
    })
  ) %>%
    dplyr::group_by(lag) %>%
    dplyr::mutate(
      ylow = quantile(pacf, probs = 0.05),
      yqlow = quantile(pacf, probs = 0.2),
      ymidlow = quantile(pacf, probs = 0.25),
      ymidhigh = quantile(pacf, probs = 0.75),
      yqhigh = quantile(pacf, probs = 0.8),
      yhigh = quantile(pacf, probs = 0.95)
    ) %>%
    dplyr::select(-pacf) %>%
    dplyr::distinct()

  pacf_plot <- ggplot2::ggplot(pacf_stats, ggplot2::aes(x = lag)) +
    ggplot2::geom_hline(
      yintercept = c(-1, 1) *
        qnorm((1 + 0.95) / 2) /
        pacf_stats$denom[1],
      linetype = "dashed"
    ) +
    ggplot2::geom_hline(yintercept = 0, colour = "#7C0000", linewidth = 0.25) +
    ggplot2::geom_segment(
      colour = "#DCBCBC",
      linewidth = 1.5,
      ggplot2::aes(y = ylow, yend = yhigh)
    ) +
    ggplot2::geom_segment(
      colour = "#B97C7C",
      linewidth = 1.5,
      ggplot2::aes(y = yqlow, yend = yqhigh)
    ) +
    ggplot2::geom_segment(
      colour = "#7C0000",
      linewidth = 1.5,
      ggplot2::aes(y = ymidlow, yend = ymidhigh)
    ) +
    ggplot2::labs(title = "pACF", x = "Lag", y = "Partial autocorrelation") +
    ggplot2::theme_bw()

  # return
  patchwork::wrap_plots(
    fvr_plot,
    qq_plot,
    acf_plot,
    pacf_plot,
    ncol = 2,
    nrow = 2,
    byrow = TRUE
  )
}
