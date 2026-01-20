#' Plot latent trend predictions from \pkg{mvgam} models
#'
#' @importFrom graphics par lines polygon box abline
#'
#' @importFrom stats sd quantile
#'
#' @param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'
#' @param series \code{integer} specifying which series in the set is to be
#'   plotted
#'
#' @param newdata Optional \code{dataframe} or \code{list} of test data
#'   containing at least 'series' and 'time' in addition to any other
#'   variables included in the linear predictor of the original \code{formula}.
#'
#' @param data_test Deprecated. Still works in place of \code{newdata} but
#'   users are recommended to use \code{newdata} instead for more seamless
#'   integration into `R` workflows
#'
#' @param derivatives \code{logical}. If \code{TRUE}, an additional plot will
#'   be returned to show the estimated 1st derivative for the estimated trend
#'
#' @param realisations \code{logical}. If \code{TRUE}, posterior trend
#'   realisations are shown as a spaghetti plot, making it easier to visualise
#'   the diversity of possible trend paths. If \code{FALSE}, the default,
#'   empirical quantiles of the posterior distribution are shown
#'
#' @param n_realisations \code{integer} specifying the number of posterior
#'   realisations to plot, if \code{realisations = TRUE}. Ignored otherwise
#'
#' @param n_cores Deprecated. Parallel processing is no longer supported
#'
#' @param xlab Label for x axis
#'
#' @param ylab Label for y axis
#'
#' @return A `ggplot` object
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
#'   chains = 2
#' )
#'
#' # Plot estimated trends for some series
#' plot_mvgam_trend(mod)
#' plot_mvgam_trend(mod, series = 2)
#'
#' # Extrapolate trends forward in time and plot on response scale
#' plot_mvgam_trend(
#'   mod,
#'   newdata = simdat$data_test
#' )
#'
#' plot_mvgam_trend(
#'   mod,
#'   newdata = simdat$data_test,
#'   series = 2
#' )
#'
#' # But it is recommended to compute extrapolations for all series
#' # first and then plot
#' trend_fc <- forecast(
#'   mod,
#'   newdata = simdat$data_test
#' )
#'
#' plot(trend_fc, series = 1)
#' plot(trend_fc, series = 2)
#' }
#'
#' @author Nicholas J Clark
#'
#' @export
plot_mvgam_trend = function(
  object,
  series = 1,
  newdata,
  data_test,
  realisations = FALSE,
  n_realisations = 15,
  n_cores = 1,
  derivatives = FALSE,
  xlab,
  ylab
) {
  # Check arguments
  if (!(inherits(object, "mvgam"))) {
    stop('argument "object" must be of class "mvgam"')
  }

  validate_pos_integer(series)

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

  if (
    attr(object$model_data, 'trend_model') == 'None' &
      !object$use_lv
  ) {
    stop('no trend was estimated in object', call. = FALSE)
  }

  if (!missing("newdata")) {
    data_test <- newdata
  }

  # Prediction indices for the particular series
  data_train <- object$obs_data
  ends <- seq(
    0,
    dim(mcmc_chains(object$model_output, 'trend'))[2],
    length.out = NCOL(object$ytimes) + 1
  )
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes) + 1))])
  ends <- ends[-1]

  if (object$fit_engine == 'stan') {
    preds <- mcmc_chains(object$model_output, 'trend')[, seq(
      series,
      dim(mcmc_chains(object$model_output, 'trend'))[2],
      by = NCOL(object$ytimes)
    )]
  } else {
    preds <- mcmc_chains(object$model_output, 'trend')[,
      starts[series]:ends[series]
    ]
  }

  # If the posterior predictions do not already cover the data_test period, the forecast needs to be
  # generated using the latent trend dynamics; note, this assumes that there is no gap between the training and
  # testing datasets

  # Add variables to data_test if missing
  s_name <- levels(data_train$series)[series]
  if (!missing(data_test)) {
    if (!'y' %in% names(data_test)) {
      data_test$y <- rep(NA, NROW(data_test))
    }

    if (!'series' %in% names(data_test)) {
      data_test$series <- factor('series1')
    }

    if (!'time' %in% names(data_test)) {
      stop('data_test does not contain a "time" column')
    }

    if (inherits(data_test, 'list')) {
      all_obs <- c(
        data.frame(
          y = data_train$y,
          series = data_train$series,
          time = data_train$time
        ) %>%
          dplyr::filter(series == s_name) %>%
          dplyr::select(time, y) %>%
          dplyr::distinct() %>%
          dplyr::arrange(time) %>%
          dplyr::pull(y),
        data.frame(
          y = data_test$y,
          series = data_test$series,
          time = data_test$time
        ) %>%
          dplyr::filter(series == s_name) %>%
          dplyr::select(time, y) %>%
          dplyr::distinct() %>%
          dplyr::arrange(time) %>%
          dplyr::pull(y)
      )
    } else {
      all_obs <- c(
        data_train %>%
          dplyr::filter(series == s_name) %>%
          dplyr::select(time, y) %>%
          dplyr::distinct() %>%
          dplyr::arrange(time) %>%
          dplyr::pull(y),
        data_test %>%
          dplyr::filter(series == s_name) %>%
          dplyr::select(time, y) %>%
          dplyr::distinct() %>%
          dplyr::arrange(time) %>%
          dplyr::pull(y)
      )
    }

    if (dim(preds)[2] != length(all_obs)) {
      fc_preds <- forecast(
        object,
        data_test = data_test,
        type = 'trend',
        n_cores = n_cores
      )$forecasts[[series]]
      preds <- cbind(preds, fc_preds)
    }
  }

  preds_last <- preds[1, ]
  pred_vals <- seq(1:length(preds_last))

  # Plot quantiles of the smooth function, along with observed values
  # if specified
  probs <- c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
  cred <- sapply(1:NCOL(preds), function(n) quantile(preds[, n], probs = probs))

  if (missing(xlab)) {
    xlab <- 'Time'
  }

  if (missing(ylab)) {
    ylab <- paste0('Estimated trend for ', levels(data_train$series)[series])
  }

  # Create a base plot using posterior credible intervals and observations
  # for the specified series
  plot_dat <- data.frame(
    time = 1:NCOL(cred),
    med = cred[5, ],
    lower1 = cred[1, ],
    lower2 = cred[2, ],
    lower3 = cred[3, ],
    lower4 = cred[4, ],
    upper1 = cred[9, ],
    upper2 = cred[8, ],
    upper3 = cred[7, ],
    upper4 = cred[6, ]
  )

  base_plot <- ggplot2::ggplot(
    data = plot_dat,
    mapping = ggplot2::aes(x = time, y = med)
  ) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = xlab, y = ylab)

  # Add to the base plot accordingly
  if (realisations) {
    for (i in 1:n_realisations) {
      base_plot <- base_plot +
        ggplot2::geom_line(
          data = data.frame(
            y = preds[i, ],
            time = 1:NCOL(cred)
          ),
          mapping = ggplot2::aes(x = time, y = y),
          col = "white",
          linewidth = 1
        ) +
        ggplot2::geom_line(
          data = data.frame(
            y = preds[i, ],
            time = 1:NCOL(cred)
          ),
          mapping = ggplot2::aes(x = time, y = y),
          col = sample(
            c("#DCBCBC", "#C79999", "#B97C7C", "#A25050", "#7C0000"),
            1
          ),
          linewidth = 0.75
        )
    }
  } else {
    base_plot <- base_plot +
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
      )
  }

  if (!missing(data_test)) {
    if (class(data_train)[1] == 'list') {
      base_plot <- base_plot +
        ggplot2::geom_vline(
          xintercept = length(data_train$y) / NCOL(object$ytimes),
          linetype = 'dashed'
        )
    } else {
      base_plot <- base_plot +
        ggplot2::geom_vline(
          xintercept = NROW(data_train) / NCOL(object$ytimes),
          linetype = 'dashed'
        )
    }
  }

  # Add the 1st derivative plot if necessary
  if (derivatives) {
    first_derivs <- cbind(rep(0, NROW(preds)), t(apply(preds, 1, diff)))
    cred <- sapply(
      1:NCOL(first_derivs),
      function(n) quantile(first_derivs[, n], probs = probs, na.rm = TRUE)
    )
    plot_dat <- data.frame(
      time = 1:NCOL(cred),
      med = cred[5, ],
      lower1 = cred[1, ],
      lower2 = cred[2, ],
      lower3 = cred[3, ],
      lower4 = cred[4, ],
      upper1 = cred[9, ],
      upper2 = cred[8, ],
      upper3 = cred[7, ],
      upper4 = cred[6, ]
    )

    deriv_plot <- ggplot2::ggplot(
      data = plot_dat,
      mapping = ggplot2::aes(x = time, y = med)
    ) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = xlab, y = '1st derivative')

    # Add to the base plot accordingly
    if (realisations) {
      for (i in 1:n_realisations) {
        deriv_plot <- deriv_plot +
          ggplot2::geom_line(
            data = data.frame(
              y = first_derivs[i, ],
              time = 1:NCOL(cred)
            ),
            mapping = ggplot2::aes(x = time, y = y),
            col = "white",
            linewidth = 1
          ) +
          ggplot2::geom_line(
            data = data.frame(
              y = first_derivs[i, ],
              time = 1:NCOL(cred)
            ),
            mapping = ggplot2::aes(x = time, y = y),
            col = sample(
              c("#DCBCBC", "#C79999", "#B97C7C", "#A25050", "#7C0000"),
              1
            ),
            linewidth = 0.75
          )
      }
    } else {
      deriv_plot <- deriv_plot +
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
        )
    }

    if (!missing(data_test)) {
      if (class(data_train)[1] == 'list') {
        deriv_plot <- deriv_plot +
          ggplot2::geom_vline(
            xintercept = length(data_train$y) / NCOL(object$ytimes),
            linetype = 'dashed'
          )
      } else {
        deriv_plot <- deriv_plot +
          ggplot2::geom_vline(
            xintercept = NROW(data_train) / NCOL(object$ytimes),
            linetype = 'dashed'
          )
      }
    }

    out <- patchwork::wrap_plots(base_plot, deriv_plot, ncol = 1)
  } else {
    out <- base_plot
  }

  return(out)
}
