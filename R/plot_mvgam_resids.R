#'Residual diagnostics for a fitted mvgam object
#'
#'This function takes a fitted \code{mvgam} object and returns various residual diagnostic plots
#'
#'@importFrom graphics layout title
#'@importFrom stats complete.cases qqnorm qqline acf pacf na.pass
#'@importFrom mgcv bam
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing at least 'series', 'y', and 'time'
#'in addition to any other variables included in the linear predictor of \code{formula}. If included, the
#'covariate information in \code{newdata} will be used to generate forecasts from the fitted model equations. If
#'this same \code{newdata} was originally included in the call to \code{mvgam}, then forecasts have already been
#'produced by the generative model and these will simply be extracted and used to calculate residuals.
#'However if no \code{newdata} was supplied to the original model call, an assumption is made that
#'the \code{newdata} supplied here comes sequentially after the data supplied as \code{data} in
#'the original model (i.e. we assume there is no time gap between the last
#'observation of series 1 in \code{data_train} and the first observation for series 1 in \code{newdata}).
#'@author Nicholas J Clark
#'@details A total of four ggplot plots are generated to examine posterior median
#'Dunn-Smyth residuals for the specified series. Plots include a residuals vs fitted values plot,
#'a Q-Q plot, and two plots to check for any remaining temporal autocorrelation in the residuals.
#'Note, all plots use only posterior medians of fitted values / residuals, so uncertainty is not represented.
#'@return A series of facetted ggplot object
#'@author Nicholas J Clark and Matthijs Hollanders
#' @examples
#' \donttest{
#' simdat <- sim_mvgam(n_series = 3, trend_model = AR())
#' mod <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'             trend_model = AR(),
#'             noncentred = TRUE,
#'             data = simdat$data_train,
#'             chains = 2)
#'
#' # Plot Dunn Smyth residuals for some series
#' plot_mvgam_resids(mod)
#' plot_mvgam_resids(mod, series = 2)
#' }
#'@export
plot_mvgam_resids = function(object,
                             series = 1,
                             newdata){

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

  if(!missing("newdata")){
    data_test <- validate_series_time(newdata,
                                      trend_model = attr(object$model_data,
                                                         'trend_model'))
  } else {
    data_test <- rlang::missing_arg()
  }

  # Plotting colours
  c_dark <- c("#8F2727")

  # Prediction indices for the particular series
  data_train <- validate_series_time(object$obs_data,
                                     trend_model = attr(object$model_data,
                                                        'trend_model'))
  ends <- seq(0, dim(mcmc_chains(object$model_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]

  # Pull out series' residuals
  series_residuals <- object$resids[[series]]

  # Get indices of training horizon
  if(inherits(data_train, 'list')){
    data_train_df <- data.frame(time = data_train$index..time..index,
                                y = data_train$y,
                                series = data_train$series)
    obs_length <- length(data_train_df %>%
                           dplyr::filter(series == !!(levels(data_train_df$series)[series])) %>%
                           dplyr::select(time, y) %>%
                           dplyr::distinct() %>%
                           dplyr::arrange(time) %>%
                           dplyr::pull(y))
  } else {
    obs_length <- length(data_train %>%
                           dplyr::filter(series == !!(levels(data_train$series)[series])) %>%
                           dplyr::select(index..time..index, y) %>%
                           dplyr::distinct() %>%
                           dplyr::arrange(index..time..index) %>%
                           dplyr::pull(y))
  }

  if(missing(data_test)){

  } else {

    if(object$fit_engine == 'stan'){
      linkfun <- family_invlinks(object$family)
      preds <- linkfun(mcmc_chains(object$model_output, 'mus')[,seq(series,
                                                                    dim(mcmc_chains(object$model_output, 'mus'))[2],
                                                                    by = NCOL(object$ytimes))])
    } else {
      preds <- mcmc_chains(object$model_output, 'mus')[,starts[series]:ends[series]]
    }

    # Add variables to data_test if missing
    s_name <- levels(data_train$series)[series]
    if(!missing(data_test)){

      if(!'y' %in% names(data_test)){
        data_test$y <- rep(NA, NROW(data_test))
      }

      if(class(data_test)[1] == 'list'){
        if(!'time' %in% names(data_test)){
          stop('data_train does not contain a "time" column')
        }

        if(!'series' %in% names(data_test)){
          data_test$series <- factor('series1')
        }

      } else {
        if(!'time' %in% colnames(data_test)){
          stop('data_train does not contain a "time" column')
        }

        if(!'series' %in% colnames(data_test)){
          data_test$series <- factor('series1')
        }
      }
      # If the posterior predictions do not already cover the data_test period,
      # the forecast needs to be generated using the latent trend dynamics;
      # note, this assumes that there is no gap between the training and testing datasets
      if(class(data_train)[1] == 'list'){
        all_obs <- c(data.frame(y = data_train$y,
                                series = data_train$series,
                                time = data_train$index..time..index) %>%
                       dplyr::filter(series == s_name) %>%
                       dplyr::select(time, y) %>%
                       dplyr::distinct() %>%
                       dplyr::arrange(time) %>%
                       dplyr::pull(y),
                     data.frame(y = data_test$y,
                                series = data_test$series,
                                time = data_test$time) %>%
                       dplyr::filter(series == s_name) %>%
                       dplyr::select(time, y) %>%
                       dplyr::distinct() %>%
                       dplyr::arrange(time) %>%
                       dplyr::pull(y))
      } else {
        all_obs <- c(data_train %>%
                       dplyr::filter(series == s_name) %>%
                       dplyr::select(index..time..index, y) %>%
                       dplyr::distinct() %>%
                       dplyr::arrange(index..time..index) %>%
                       dplyr::pull(y),
                     data_test %>%
                       dplyr::filter(series == s_name) %>%
                       dplyr::select(time, y) %>%
                       dplyr::distinct() %>%
                       dplyr::arrange(time) %>%
                       dplyr::pull(y))
      }

      if(dim(preds)[2] != length(all_obs)){
        linkfun <- family_invlinks(object$family)
        fc_preds <- linkfun(forecast.mvgam(object, series = series,
                                           data_test = data_test,
                                           type = 'link'))
        preds <- cbind(preds, fc_preds)
      }

      # Calculate out of sample residuals
      preds <- preds[,tail(1:dim(preds)[2], length(data_test$time))]
      truth <- data_test$y
      n_obs <- length(truth)

      if(NROW(preds) > 2000){
        sample_seq <- sample(1:NROW(preds), 2000, F)
      } else {
        sample_seq <- 1:NROW(preds)
      }

      series_residuals <- get_forecast_resids(object = object,
                                              series = series,
                                              truth = truth,
                                              preds = preds,
                                              family = object$family,
                                              sample_seq = sample_seq)

    }
  }

  # plot predictions and residuals
  median_resids <- apply(object$resids[[series]], 2, median)
  fvr_plot <- data.frame(preds =  apply(hindcast(object, type = 'expected')$hindcasts[[series]],
                                        2, median),
                         resids = median_resids) %>%
    ggplot2::ggplot(ggplot2::aes(preds, resids)) +
    ggplot2::geom_smooth(method = "gam",
                         formula = y ~ s(x, bs = "cs"),
                         colour = "#7C000060",
                         fill = "#7C000040") +
    ggplot2::geom_point(shape = 16, col = 'white', size = 1.5) +
    ggplot2::geom_point(shape = 16, col = 'black', size = 1.25) +
    ggplot2::labs(title = "Resids vs Fitted",
                  x = "Fitted values",
                  y = "DS residuals") +
    ggplot2::theme_bw()

  # Q-Q plot
  qq_plot <- data.frame(resids = median_resids) %>%
    ggplot2::ggplot(ggplot2::aes(sample = resids)) +
    ggplot2::stat_qq_line(colour = c_dark,
                          linewidth = 0.75) +
    ggplot2::stat_qq(shape = 16, col = 'white', size = 1.5) +
    ggplot2::stat_qq(shape = 16, col = 'black',
                     size = 1.25) +
    ggplot2::labs(title = "Normal Q-Q Plot",
                  x = "Theoretical Quantiles",
                  y = "Sample Quantiles") +
    ggplot2::theme_bw()

  # ACF plot
  acf_resids <- acf(median_resids, plot = FALSE, na.action = na.pass)
  acf_plot <- data.frame(acf = acf_resids$acf[,,1],
                         lag = acf_resids$lag[,1,1]) %>%
    dplyr::filter(lag > 0) %>%
    ggplot2::ggplot(ggplot2::aes(x = lag, y = 0, yend = acf)) +
    ggplot2::geom_hline(yintercept = c(-1, 1) *
                          qnorm((1 + 0.95) / 2) / sqrt(acf_resids$n.used),
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
  pacf_resids <- pacf(median_resids, plot = FALSE, na.action = na.pass)
  pacf_plot <- data.frame(pacf = pacf_resids$acf[,,1],
                          lag = pacf_resids$lag[,1,1]) %>%
    dplyr::filter(lag > 0) %>%
    ggplot2::ggplot(ggplot2::aes(x = lag, y = 0, yend = pacf)) +
    ggplot2::geom_hline(yintercept = c(-1, 1) *
                          qnorm((1 + 0.95) / 2) / sqrt(pacf_resids$n.used),
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
  suppressWarnings(print(patchwork::wrap_plots(fvr_plot,
                        qq_plot,
                        acf_plot,
                        pacf_plot,
                        ncol = 2,
                        nrow = 2,
                        byrow = T)))
}
