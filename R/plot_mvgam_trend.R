#'Plot mvgam latent trend for a specified series
#'@importFrom graphics par lines polygon box abline
#'@importFrom stats sd quantile
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing at least 'series' and 'time'
#'in addition to any other variables included in the linear predictor of the original \code{formula}.
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param derivatives \code{logical}. If \code{TRUE}, an additional plot will be returned to show the
#'estimated 1st derivative for the estimated trend
#'@param realisations \code{logical}. If \code{TRUE}, posterior trend realisations are shown as a spaghetti plot,
#'making it easier to visualise the diversity of possible trend paths. If \code{FALSE}, the default,
#'empirical quantiles of the posterior distribution are shown
#'@param n_realisations \code{integer} specifying the number of posterior realisations to plot, if
#'\code{realisations = TRUE}. Ignored otherwise
#'@param n_cores \code{integer} specifying number of cores for generating trend forecasts in parallel
#'@param hide_xlabels \code{logical}. If \code{TRUE}, no xlabels are printed to allow the user to add custom labels using
#'\code{axis} from base \code{R}. Ignored if \code{derivatives = TRUE}
#'@param xlab label for x axis.
#'@param ylab label for y axis.
#'@param ... further \code{\link[graphics]{par}} graphical parameters.
#'@return A base \code{R} graphics plot
#' @examples
#' \donttest{
#' simdat <- sim_mvgam(n_series = 3, trend_model = 'AR1')
#' mod <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'             trend_model = AR(),
#'             noncentred = TRUE,
#'             data = simdat$data_train,
#'             chains = 2)
#'
#' # Plot estimated trends for some series
#' plot_mvgam_trend(mod)
#' plot_mvgam_trend(mod, series = 2)
#'
#' # Extrapolate trends forward in time and plot on response scale
#' plot_mvgam_trend(mod, newdata = simdat$data_test)
#' plot_mvgam_trend(mod, newdata = simdat$data_test, series = 2)
#'
#' # But it is recommended to compute extrapolations for all series
#' # first and then plot
#' trend_fc <- forecast(mod, newdata = simdat$data_test)
#' plot(trend_fc, series = 1)
#' plot(trend_fc, series = 2)
#' }
#'@export
plot_mvgam_trend = function(object, series = 1, newdata, data_test,
                            realisations = FALSE, n_realisations = 15,
                            n_cores = 1,
                            derivatives = FALSE, hide_xlabels = FALSE,
                            xlab, ylab,
                            ...){

  # Check arguments
  if (!(inherits(object, "mvgam"))) {
    stop('argument "object" must be of class "mvgam"')
  }

  if(sign(series) != 1){
    stop('argument "series" must be a positive integer',
         call. = FALSE)
  } else {
    if(series%%1 != 0){
      stop('argument "series" must be a positive integer',
           call. = FALSE)
    }
  }

  if(series > NCOL(object$ytimes)){
    stop(paste0('object only contains data / predictions for ',
                NCOL(object$ytimes), ' series'),
         call. = FALSE)
  }

  if(attr(object$model_data, 'trend_model') == 'None' &
     !object$use_lv){
    stop('no trend was estimated in object',
         call. = FALSE)
  }

  if(!missing("newdata")){
    data_test <- newdata
  }

  # Prediction indices for the particular series
  data_train <- object$obs_data
  ends <- seq(0, dim(mcmc_chains(object$model_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]

  if(object$fit_engine == 'stan'){
    preds <- mcmc_chains(object$model_output, 'trend')[,seq(series,
                                                           dim(mcmc_chains(object$model_output,
                                                                                   'trend'))[2],
                                                           by = NCOL(object$ytimes))]
  } else {
    preds <- mcmc_chains(object$model_output, 'trend')[,starts[series]:ends[series]]
  }

  # If the posterior predictions do not already cover the data_test period, the forecast needs to be
  # generated using the latent trend dynamics; note, this assumes that there is no gap between the training and
  # testing datasets

  # Add variables to data_test if missing
  s_name <- levels(data_train$series)[series]
  if(!missing(data_test)){
    if(!'y' %in% names(data_test)){
      data_test$y <- rep(NA, NROW(data_test))
    }

    if(!'series' %in% names(data_test)){
      data_test$series <- factor('series1')
    }

    if(!'time' %in% names(data_test)){
      stop('data_test does not contain a "time" column')
    }

    if(inherits(data_test, 'list')){
      all_obs <- c(data.frame(y = data_train$y,
                              series = data_train$series,
                              time = data_train$time) %>%
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
                     dplyr::select(time, y) %>%
                     dplyr::distinct() %>%
                     dplyr::arrange(time) %>%
                     dplyr::pull(y),
                   data_test %>%
                     dplyr::filter(series == s_name) %>%
                     dplyr::select(time, y) %>%
                     dplyr::distinct() %>%
                     dplyr::arrange(time) %>%
                     dplyr::pull(y))
    }

    if(dim(preds)[2] != length(all_obs)){
      fc_preds <- forecast(object, data_test = data_test,
                           type = 'trend',
                           n_cores = n_cores)$forecasts[[series]]
      preds <- cbind(preds, fc_preds)
    }
  }

  preds_last <- preds[1,]
  pred_vals <- seq(1:length(preds_last))

  # Plot quantiles of the smooth function, along with observed values
  # if specified
  probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
  cred <- sapply(1:NCOL(preds),
                 function(n) quantile(preds[,n],
                                      probs = probs))
  c_light <- c("#DCBCBC")
  c_light_highlight <- c("#C79999")
  c_mid <- c("#B97C7C")
  c_mid_highlight <- c("#A25050")
  c_dark <- c("#8F2727")
  c_dark_highlight <- c("#7C0000")

  if(missing(xlab)){
    xlab <- 'Time'
  }

  if(missing(ylab)){
    ylab <- paste0('Estimated trend for ', levels(data_train$series)[series])
  }

  if(derivatives){
    .pardefault <- par(no.readonly=T)
    on.exit(par(.pardefault))
    par(mfrow = c(2, 1))

    plot(1, type = "n", bty = 'L',
           xlab = xlab,
           ylab = ylab,
           xlim = c(0, length(preds_last)),
           ylim = range(cred), ...)

    if(realisations){
      for(i in 1:n_realisations){
        lines(x = pred_vals,
              y = preds[i,],
              col = 'white',
              lwd = 2.5)
        lines(x = pred_vals,
              y = preds[i,],
              col = sample(c("#DCBCBC",
                             "#C79999",
                             "#B97C7C",
                             "#A25050",
                             "#7C0000"), 1),
              lwd = 2.25)
      }
    } else {
      polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[9,])),
              col = c_light, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[2,], rev(cred[8,])),
              col = c_light_highlight, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[3,], rev(cred[7,])),
              col = c_mid, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[4,], rev(cred[6,])),
              col = c_mid_highlight, border = NA)
      lines(pred_vals, cred[5,], col = c_dark, lwd = 2.5)
    }
    box(bty = 'L', lwd = 2)

    if(!missing(data_test)){
      if(class(data_train)[1] == 'list'){
        abline(v = length(data_train$y) / NCOL(object$ytimes), col = '#FFFFFF60', lwd = 2.85)
        abline(v = length(data_train$y) / NCOL(object$ytimes), col = 'black', lwd = 2.5, lty = 'dashed')
      } else {
        abline(v = NROW(data_train) / NCOL(object$ytimes), col = '#FFFFFF60', lwd = 2.85)
        abline(v = NROW(data_train) / NCOL(object$ytimes), col = 'black', lwd = 2.5, lty = 'dashed')
      }

    }

    # Compute 1st derivative from posterior draws
    first_derivs <- cbind(rep(NA, NROW(preds)), t(apply(preds, 1, diff)))
    cred <- sapply(1:NCOL(first_derivs),
                   function(n) quantile(first_derivs[,n],
                                        probs = probs, na.rm = T))

    plot(1, type = "n", bty = "L",
         xlab = xlab,
         ylab = '1st derivative',
         xlim = c(min(pred_vals), max(pred_vals)),
         ylim = c(min(cred, na.rm = T) - sd(first_derivs, na.rm = T),
                  max(cred, na.rm = T) + sd(first_derivs, na.rm = T)), ...)


    if(realisations){
      for(i in 1:n_realisations){
        lines(x = pred_vals,
              y = first_derivs[i,],
              col = 'white',
              lwd = 2.5)
        lines(x = pred_vals,
              y = first_derivs[i,],
              col = sample(c("#DCBCBC",
                             "#C79999",
                             "#B97C7C",
                             "#A25050",
                             "#7C0000"), 1),
              lwd = 2.25)
      }
    } else {
      polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[9,])),
              col = c_light, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[2,], rev(cred[8,])),
              col = c_light_highlight, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[3,], rev(cred[7,])),
              col = c_mid, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[4,], rev(cred[6,])),
              col = c_mid_highlight, border = NA)
      lines(pred_vals, cred[5,], col = c_dark, lwd = 2.5)
    }
    box(bty = 'L', lwd = 2)

    abline(h = 0, lty = 'dashed', lwd = 2)

    invisible()

  } else {
    if(hide_xlabels){
      plot(1, type = "n", bty = 'L',
           xlab = '',
           xaxt = 'n',
           ylab = ylab,
           xlim = c(0, length(preds_last)),
           ylim = range(cred))

    } else {
      plot(1, type = "n", bty = 'L',
           xlab = xlab,
           ylab = ylab,
           xlim = c(0, length(preds_last)),
           ylim = range(cred), ...)
    }

    if(realisations){
      for(i in 1:n_realisations){
        lines(x = pred_vals,
              y = preds[i,],
              col = 'white',
              lwd = 2.5)
        lines(x = pred_vals,
              y = preds[i,],
              col = sample(c("#DCBCBC",
                             "#C79999",
                             "#B97C7C",
                             "#A25050",
                             "#7C0000"), 1),
              lwd = 2.25)
      }
    } else {
      polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[9,])),
              col = c_light, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[2,], rev(cred[8,])),
              col = c_light_highlight, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[3,], rev(cred[7,])),
              col = c_mid, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[4,], rev(cred[6,])),
              col = c_mid_highlight, border = NA)
      lines(pred_vals, cred[5,], col = c_dark, lwd = 2.5)
    }
    box(bty = 'L', lwd = 2)

    if(!missing(data_test)){
      if(class(data_train)[1] == 'list'){
        abline(v = length(data_train$y) / NCOL(object$ytimes), col = '#FFFFFF60', lwd = 2.85)
        abline(v = length(data_train$y) / NCOL(object$ytimes), col = 'black', lwd = 2.5, lty = 'dashed')
      } else {
        abline(v = NROW(data_train) / NCOL(object$ytimes), col = '#FFFFFF60', lwd = 2.85)
        abline(v = NROW(data_train) / NCOL(object$ytimes), col = 'black', lwd = 2.5, lty = 'dashed')
      }

    }
  }


}
