#' `mvgam_irf` object description
#'
#' A \code{mvgam_irf} object returned by function \code{\link{irf}}.
#' Run `methods(class = "mvgam_irf")` to see an overview of available methods.
#' @details A `mvgam_irf` object contains a list of posterior IRFs, each stored as
#' its own list
#' @seealso [mvgam], [VAR]
#' @author Nicholas J Clark
#' @name mvgam_irf-class
NULL

#'Plot impulse responses from an `mvgam_irf` object
#'
#'This function takes an \code{mvgam_irf} object and produces plots of Impulse Response Functions
#'
#'@param x \code{list} object of class \code{mvgam_irf}. See [irf()]
#'@param series \code{integer} specifying which process series should be given the shock
#'@param ... ignored
#'@return A ggplot object showing the expected response of each latent time series to
#'a shock of the focal `series`
#'@author Nicholas J Clark
#'@export
plot.mvgam_irf = function(x, series = 1, ...) {
  all_irfs <- x
  validate_pos_integer(series)
  n_processes <- dim(all_irfs[[1]][[1]])[2]
  h <- dim(all_irfs[[1]][[1]])[1]

  # Extract IRFs for the specific series
  impulse_responses <- lapply(seq_along(all_irfs), function(j) {
    all_irfs[[j]][series]
  })

  # Extract impulse responses to a shock in the focal series
  # in tidy format for ggploting
  responses <- do.call(
    rbind,
    lapply(seq_along(impulse_responses), function(j) {
      data.frame(
        horizon = 1:h,
        imp_resp = as.vector(impulse_responses[[j]][[1]]),
        resp_var = sort(rep(
          paste0('Process~', 1:n_processes),
          NROW(impulse_responses[[j]][[1]])
        ))
      )
    })
  ) %>%
    dplyr::mutate(resp_var = paste0('Process~', series, ' %->% ', resp_var)) %>%

    # Calculate posterior empirical quantiles of impulse responses
    dplyr::group_by(resp_var, horizon) %>%
    dplyr::summarise(
      med = median(imp_resp),
      lower1 = quantile(imp_resp, 0.1),
      lower2 = quantile(imp_resp, 0.2),
      lower3 = quantile(imp_resp, 0.3),
      lower4 = quantile(imp_resp, 0.4),
      upper1 = quantile(imp_resp, 0.9),
      upper2 = quantile(imp_resp, 0.8),
      upper3 = quantile(imp_resp, 0.7),
      upper4 = quantile(imp_resp, 0.6),
      .groups = 'keep'
    ) %>%
    dplyr::ungroup()

  # Plot the IRFs
  ggplot2::ggplot(data = responses, ggplot2::aes(x = horizon, y = med)) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(ymin = lower1, ymax = upper1),
      fill = "#DCBCBC"
    ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(ymin = lower2, ymax = upper2),
      fill = "#C79999"
    ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(ymin = lower3, ymax = upper3),
      fill = "#B97C7C"
    ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(ymin = lower4, ymax = upper4),
      fill = "#A25050"
    ) +
    ggplot2::geom_line(col = "#8F2727", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
    ggplot2::facet_wrap(
      ~resp_var,
      scales = 'free_y',
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::labs(
      x = "Horizon",
      y = paste0(attr(x, 'irf_type'), " impulse response")
    ) +
    ggplot2::theme_bw()
}
