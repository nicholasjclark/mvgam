#' Latent factor summaries for a fitted \pkg{mvgam} object
#'
#' This function takes a fitted \code{mvgam} object and returns plots and
#' summary statistics for the latent dynamic factors
#'
#' @param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'
#' @param plot \code{logical} specifying whether factors should be plotted
#'
#' @author Nicholas J Clark
#'
#' @details If the model in \code{object} was estimated using dynamic factors,
#'   it is possible that not all factors contributed to the estimated trends.
#'   This is due to the regularisation penalty that acts independently on each
#'   factor's Gaussian precision, which will squeeze un-needed factors to a
#'   white noise process (effectively dropping that factor from the model). In
#'   this function, each factor is tested against a null hypothesis of white
#'   noise by calculating the sum of the factor's 2nd derivatives. A factor
#'   that has a larger contribution will have a larger sum due to the weaker
#'   penalty on the factor's precision. If \code{plot == TRUE}, the factors
#'   are also plotted.
#'
#' @return A \code{data.frame} of factor contributions
#'
#' @examples
#' \donttest{
#' simdat <- sim_mvgam()
#'
#' mod <- mvgam(
#'   y ~ s(season, bs = 'cc', k = 6),
#'   trend_model = AR(),
#'   use_lv = TRUE,
#'   n_lv = 2,
#'   data = simdat$data_train,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' plot_mvgam_factors(mod)
#' }
#'
#' @export
plot_mvgam_factors = function(object, plot = TRUE) {
  # Check arguments
  if (!(inherits(object, "mvgam"))) {
    stop('argument "object" must be of class "mvgam"')
  }

  # Check object has latent dynamic factors
  if (!object$use_lv) {
    stop('No latent factors used in object')
  }

  # Get indices of LV estimates
  ends <- seq(
    0,
    dim(mcmc_chains(object$model_output, 'LV'))[2],
    length.out = object$n_lv + 1
  )
  starts <- ends + 1
  starts <- c(1, starts[-c(1, object$n_lv + 1)])
  ends <- ends[-1]
  probs <- c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)

  # Loop across each lv and calculate probability that the lv was dropped
  lv_estimates <- do.call(
    rbind,
    lapply(1:object$n_lv, function(x) {
      if (object$fit_engine == 'stan') {
        inds_lv <- seq(
          x,
          dim(mcmc_chains(object$model_output, 'LV'))[2],
          by = object$n_lv
        )
        preds <- mcmc_chains(object$model_output, 'LV')[, inds_lv]
      } else {
        preds <- mcmc_chains(object$model_output, 'LV')[, starts[x]:ends[x]]
      }

      # Keep only the in-sample observations for testing against the null of white noise
      preds <- preds[, 1:(length(object$obs_data$y) / NCOL(object$ytimes))]

      cred <- as.data.frame(t(sapply(
        1:NCOL(preds),
        function(n) quantile(preds[, n], probs = probs)
      ))) %>%
        dplyr::mutate(lv = paste0('Factor ', x), time = 1:NCOL(preds))
      colnames(cred) <- c(
        paste0('lower', 1:4),
        'med',
        paste0('upper', 4:1),
        'lv',
        'time'
      )
      cred
    })
  )

  # If plot = TRUE, plot the LVs
  if (plot) {
    p <- ggplot2::ggplot(
      data = lv_estimates,
      mapping = ggplot2::aes(x = time, y = med)
    ) +
      ggplot2::facet_wrap(~lv) +
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
      ggplot2::geom_line(
        mapping = ggplot2::aes(x = time, y = med),
        col = "#8F2727",
        linewidth = 1
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = 'Time', y = 'Posterior prediction')
  }

  # Calculate second derivatives of empirical medians and upper / lower intervals;
  # factors with small second derivatives are moving in roughly a straight line and not
  # likely contributing much (or at all) to the latent trend estimates
  lv_contributions <- lv_estimates %>%
    dplyr::group_by(lv) %>%
    dplyr::reframe(
      med_deriv = abs(diff(diff(med))),
      upper_deriv = abs(diff(diff(upper2))),
      lower_deriv = abs(diff(diff(lower2)))
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(contribution = sum(med_deriv, upper_deriv, lower_deriv)) %>%
    dplyr::group_by(lv) %>%
    dplyr::summarise(sum_contribution = sum(contribution)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      total = sum(sum_contribution),
      Contribution = sum_contribution / total,
      Factor = lv
    ) %>%
    dplyr::select(Factor, Contribution)

  if (plot) {
    print(p)
  }

  lv_contributions
}
