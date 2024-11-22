#'Residual diagnostics for a fitted mvgam object
#'
#'This function takes a fitted \code{mvgam} object and returns various residual diagnostic plots
#'
#'@importFrom graphics layout title
#'@importFrom stats complete.cases qqnorm qqline acf pacf na.pass
#'@importFrom mgcv bam
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@author Nicholas J Clark
#'@details A total of four ggplot plots are generated to examine posterior
#'Dunn-Smyth residuals for the specified series. Plots include a residuals vs fitted values plot,
#'a Q-Q plot, and two plots to check for any remaining temporal autocorrelation in the residuals.
#'Note, all plots use only report statistics from a sample of up to 20 posterior
#'draws (to save computational time), so uncertainty in these relationships may not be adequately represented.
#'@return A series of facetted ggplot object
#'@author Nicholas J Clark and Matthijs Hollanders
#' @examples
#' \dontrun{
#' simdat <- sim_mvgam(n_series = 3, trend_model = AR())
#' mod <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'             trend_model = AR(),
#'             noncentred = TRUE,
#'             data = simdat$data_train,
#'             chains = 2,
#'             silent = 2)
#'
#' # Plot Dunn Smyth residuals for some series
#' plot_mvgam_resids(mod)
#' plot_mvgam_resids(mod, series = 2)
#' }
#'@export
plot_mvgam_resids = function(object,
                             series = 1){

  # Check arguments
  if (!(inherits(object, "mvgam"))) {
    stop('argument "object" must be of class "mvgam"')
  }

  validate_pos_integer(series)

  if(series > NCOL(object$ytimes)){
    stop(paste0('object only contains data / predictions for ',
                NCOL(object$ytimes), ' series'),
         call. = FALSE)
  }

  # Plotting colours
  c_dark <- c("#8F2727")

  # Take a sample of posterior draws to compute autocorrelation statistics
  # This is because acf(posterior_median_residual) can induce spurious patterns
  # due to the randomness of DS residuals;
  # rather, we want median(acf(residual_i)), where i indexes all possible draws
  # But this is computationally expensive for some models so we compromise
  # by only taking a few draws
  n_draws <- NROW(object$resids[[series]])
  n_samps <- min(20, n_draws)
  hcs <- hindcast(object, type = 'expected')$hindcasts[[series]]
  resids <- object$resids[[series]]

  resid_df <- do.call(rbind, lapply(seq_len(n_samps), function(x){
    data.frame(preds = hcs[x, ],
               resids = resids[x, ]) %>%
                 dplyr::filter(!is.na(resids))
  }))

  # Plot predictions and residuals
  fvr_plot <- ggplot2::ggplot(resid_df, ggplot2::aes(preds, resids)) +
    ggplot2::geom_point(shape = 16, col = 'white', size = 1.25) +
    ggplot2::geom_point(shape = 16, col = 'black', size = 1) +
    ggplot2::geom_smooth(method = "gam",
                         formula = y ~ s(x, bs = "cs"),
                         colour = "#7C000060",
                         fill = "#7C000040") +
    ggplot2::labs(title = "Resids vs Fitted",
                  x = "Fitted values",
                  y = "DS residuals") +
    ggplot2::theme_bw()

  # Q-Q plot
  qq_plot <- ggplot2::ggplot(resid_df, ggplot2::aes(sample = resids)) +
    ggplot2::stat_qq_line(colour = c_dark,
                          linewidth = 1) +
    ggplot2::stat_qq(shape = 16, col = 'white', size = 1.25) +
    ggplot2::stat_qq(shape = 16, col = 'black',
                     size = 1) +
    ggplot2::labs(title = "Normal Q-Q Plot",
                  x = "Theoretical Quantiles",
                  y = "Sample Quantiles") +
    ggplot2::theme_bw()

  # ACF plot
  acf_stats <- do.call(rbind, lapply(seq_len(n_samps), function(x){
    acf_calc <- acf(resids[x, ], plot = FALSE, na.action = na.pass)
    data.frame(acf = acf_calc$acf[,,1],
               lag = acf_calc$lag[,1,1],
               denom = sqrt(acf_calc$n.used)) %>%
      dplyr::filter(lag > 0)
  })) %>%
    dplyr::group_by(lag) %>%
    dplyr::summarise_all(median)
  acf_plot <- ggplot2::ggplot(acf_stats, ggplot2::aes(x = lag, y = 0, yend = acf)) +
    ggplot2::geom_hline(yintercept = c(-1, 1) *
                          qnorm((1 + 0.95) / 2) / acf_stats$denom[1],
                        linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0,
                        colour = c_dark,
                        linewidth = 0.25) +
    ggplot2::geom_segment(colour = c_dark,
                          linewidth = 1) +
    ggplot2::labs(title = "ACF",
                  x = "Lag",
                  y = "Autocorrelation") +
    ggplot2::theme_bw()

  # PACF plot
  pacf_stats <- do.call(rbind, lapply(seq_len(n_samps), function(x){
    acf_calc <- pacf(resids[x, ], plot = FALSE, na.action = na.pass)
    data.frame(pacf = acf_calc$acf[,,1],
               lag = acf_calc$lag[,1,1],
               denom = sqrt(acf_calc$n.used)) %>%
      dplyr::filter(lag > 0)
  })) %>%
    dplyr::group_by(lag) %>%
    dplyr::summarise_all(median)

  pacf_plot <- ggplot2::ggplot(pacf_stats, ggplot2::aes(x = lag, y = 0, yend = pacf)) +
    ggplot2::geom_hline(yintercept = c(-1, 1) *
                          qnorm((1 + 0.95) / 2) / pacf_stats$denom[1],
                        linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0,
                        colour = c_dark,
                        linewidth = 0.25) +
    ggplot2::geom_segment(colour = c_dark,
                          linewidth = 1) +
    ggplot2::labs(title = "pACF",
                  x = "Lag",
                  y = "Partial autocorrelation") +
    ggplot2::theme_bw()

  # return
  patchwork::wrap_plots(fvr_plot,
                        qq_plot,
                        acf_plot,
                        pacf_plot,
                        ncol = 2,
                        nrow = 2,
                        byrow = T)
}
