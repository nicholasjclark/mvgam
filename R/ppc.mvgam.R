#'@title Plot mvgam posterior predictive checks for a specified series
#'@importFrom stats quantile density ecdf formula terms
#'@importFrom graphics hist abline box rect lines polygon par
#'@importFrom grDevices rgb
#'@name ppc.mvgam
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing at least 'series' and 'time'
#'for the forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}. If
#'included, the observed values in the test data are compared to the model's forecast distribution for exploring
#'biases in model predictions.
#'Note this is only useful if the same \code{newdata} was also included when fitting the original model.
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param type \code{character} specifying the type of posterior predictive check to calculate and plot.
#'Valid options are: 'rootogram', 'mean', 'hist', 'density', 'prop_zero', 'pit' and 'cdf'
#'@param n_bins \code{integer} specifying the number of bins to use for binning observed values when plotting
#'a rootogram or histogram. Default is `50` bins for a rootogram, which means that if
#'there are `>50` unique observed values, bins will
#'be used to prevent overplotting and facilitate interpretation. Default for a histogram is to use the
#'number of bins returned by a call to `hist` in base `R`
#'@param legend_position The location may also be specified by setting x to a single keyword from the
#'list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#'This places the legend on the inside of the plot frame at the given location. Or alternatively,
#'use "none" to hide the legend.
#'@param xlab label for x axis.
#'@param ylab label for y axis.
#'@param ... further \code{\link[graphics]{par}} graphical parameters.
#'@details Posterior predictions are drawn from the fitted \code{mvgam} and compared against
#'the empirical distribution of the observed data for a specified series to help evaluate the model's
#'ability to generate unbiased predictions. For all plots apart from `type = 'rootogram'`, posterior predictions
#'can also be compared to out of sample observations as long as these observations were included as
#''data_test' in the original model fit and supplied here. Rootograms are currently only plotted using the
#''hanging' style.
#'\cr
#'Note that the predictions used for these plots are those that have been generated directly within
#'the `mvgam()` model, so they can be misleading if the model included flexible dynamic trend components. For
#'a broader range of posterior checks that are created using "new data" predictions, see
#'\code{\link{pp_check.mvgam}}
#'@return A base \code{R} graphics plot showing either a posterior rootogram (for \code{type == 'rootogram'}),
#'the predicted vs observed mean for the
#'series (for \code{type == 'mean'}), predicted vs observed proportion of zeroes for the
#'series (for \code{type == 'prop_zero'}),predicted vs observed histogram for the
#'series (for \code{type == 'hist'}), kernel density or empirical CDF estimates for
#'posterior predictions (for \code{type == 'density'} or \code{type == 'cdf'}) or a Probability
#'Integral Transform histogram (for \code{type == 'pit'}).
#'@author Nicholas J Clark
#'@seealso \code{\link{pp_check.mvgam}}, \code{\link{predict.mvgam}}
#' @examples
#' \dontrun{
#' # Simulate some smooth effects and fit a model
#' set.seed(0)
#' dat <- mgcv::gamSim(1, n = 200, scale = 2)
#' mod <- mvgam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
#'             data = dat,
#'             family = gaussian(),
#'             burnin = 300,
#'             samples = 300,
#'             chains = 2)
#'
#' # Posterior checks
#' ppc(mod, type = 'hist')
#' ppc(mod, type = 'density')
#' ppc(mod, type = 'cdf')
#'
#' # Many more options are available with pp_check()
#' pp_check(mod)
#' pp_check(mod, type = "ecdf_overlay")
#' pp_check(mod, type = 'freqpoly')
#' }
#'@export
ppc <- function(object, ...){
  UseMethod("ppc", object)
}

#'@rdname ppc.mvgam
#'@method ppc mvgam
#'@export
ppc.mvgam = function(object, newdata, data_test, series = 1, type = 'hist',
                     n_bins, legend_position, xlab, ylab, ...){

  # Check arguments
  type <- match.arg(arg = type, choices = c("rootogram", "mean", "histogram",
                                            "density", "pit", "cdf",
                                            "prop_zero"))
  if(type == 'histogram') type = 'hist'

  if(type == 'rootogram'){
    if(!object$family %in% c('poisson', 'negative binomial', 'tweedie', 'nmix',
                             'binomial', 'beta_binomial')){
      stop('Rootograms not supported for checking non-count data',
           call. = FALSE)
    }
  }

  optional_args <- list(...)

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

  if(type == 'rootogram' & missing(n_bins)){
    n_bins <- 50

    if(sign(n_bins) != 1){
      stop('argument "n_bins" must be a positive integer',
           call. = FALSE)
    } else {
      if(n_bins%%1 != 0){
        stop('argument "n_bins" must be a positive integer',
             call. = FALSE)
      }
    }
  }

  if(!missing("newdata")){
    data_test <- newdata

    # Ensure outcome is labelled 'y' when feeding data to the model for simplicity
    if(terms(formula(object$call))[[2]] != 'y'){
      data_test$y <- data_test[[terms(formula(object$call))[[2]]]]
    }
  }

  # Pull out observations and posterior predictions for the specified series
  data_train <- object$obs_data
  ends <- seq(0, dim(mcmc_chains(object$model_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]

  # Colours needed for plotting quantiles
  c_light <- c("#DCBCBC")
  c_light_highlight <- c("#C79999")
  c_mid <- c("#B97C7C")
  c_mid_highlight <- c("#A25050")
  c_dark <- c("#8F2727")
  c_dark_highlight <- c("#7C0000")
  s_name <- levels(data_train$series)[series]

  if(!missing(data_test)){

    if(class(data_test)[1] == 'list'){
      if(!'time' %in% names(data_test)){
        stop('data_test does not contain a "time" column')
      }

      if(!'series' %in% names(data_test)){
        data_test$series <- factor('series1')
      }

    } else {
      if(!'time' %in% colnames(data_test)){
        stop('data_test does not contain a "time" column')
      }

      if(!'series' %in% colnames(data_test)){
        data_test$series <- factor('series1')
      }
    }

    if(class(object$obs_data)[1] == 'list'){
      truths <- data.frame(y = data_test$y,
                           time = data_test$time,
                           series = data_test$series) %>%
        dplyr::arrange(time, series) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::pull(y)

      if(object$fit_engine == 'stan'){

        # For stan objects, ypred is stored as a vector in column-major order
        preds <- mcmc_chains(object$model_output, 'ypred')[,seq(series,
                                                                        dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                        by = NCOL(object$ytimes))]
      } else {
        preds <- mcmc_chains(object$model_output, 'ypred')[,starts[series]:ends[series]]
      }

      preds <- preds[,((length(data_train$y) / NCOL(object$ytimes))+1):
                       ((length(data_train$y) / NCOL(object$ytimes))+length(truths))]

    } else {
      truths <- data_test %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(time, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)

      if(object$fit_engine == 'stan'){

        # For stan objects, ypred is stored as a vector in column-major order
        preds <- mcmc_chains(object$model_output, 'ypred')[,seq(series,
                                                                        dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                        by = NCOL(object$ytimes))]
      } else {
        preds <- mcmc_chains(object$model_output, 'ypred')[,starts[series]:ends[series]]
      }

      preds <- preds[,((NROW(data_train) / NCOL(object$ytimes))+1):
                       ((NROW(data_train) / NCOL(object$ytimes))+length(truths))]
    }

    if(NROW(preds) > 4000){
      preds <- preds[sample(1:NROW(preds), 4000, F), ]
    }

  } else {
    if(class(object$obs_data)[1] == 'list'){
      truths <- data.frame(y = data_train$y,
                           time = data_train$time,
                           series = data_train$series) %>%
        dplyr::arrange(series, time) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::pull(y)

    } else {
      truths <- data_train %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(time, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)
    }

    if(object$fit_engine == 'stan'){

      # For stan objects, ypred is stored as a vector in column-major order
      preds <- mcmc_chains(object$model_output, 'ypred')[,seq(series,
                                                                      dim(mcmc_chains(object$model_output, 'ypred'))[2],
                                                                      by = NCOL(object$ytimes))]
    } else {
      preds <- mcmc_chains(object$model_output, 'ypred')[,starts[series]:ends[series]]
    }

    preds <- preds[,1:length(truths), drop = FALSE]

    if(NROW(preds) > 4000){
      preds <- preds[sample(1:NROW(preds), 4000, F), ]
    }
  }

  # Can't deal with missing values in these diagnostic plots
  preds[is.nan(preds)] <- NA
  preds <- preds[, !is.na(truths)]
  truths <- truths[!is.na(truths)]
  probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)

  if(type == 'prop_zero'){
    pred_props <- apply(preds, 1, function(x) length(which(x == 0)) / length(x))
    lower <- quantile(pred_props, probs = 0.01, na.rm = TRUE)
    upper <- quantile(pred_props, probs = 0.99, na.rm = TRUE)

    if(lower == 0 & upper == 0){
      stop('No predictions covered zero')
    }

    pred_props <- pred_props[-which(pred_props > upper)]
    if(lower!=0){
      pred_props <- pred_props[-which(pred_props < lower)]
    }
    obs_prop <- length(which(truths == 0)) / length(truths)

    if(missing(ylab)){
      ylab <- 'Density'
    }

    if(missing(xlab)){
      xlab <- paste0('Predicted proportion of zeroes for ', levels(data_train$series)[series])
    }

    hist(pred_props, lwd = 2,
         xlim = c(min(min(pred_props), min(obs_prop)),
                  max(max(pred_props), max(obs_prop))),
         main = '',
         breaks = seq(min(pred_props), max(pred_props),
                      length.out = 15),
         border = "#B97C7C",
         col = "#C79999",
         ylab = ylab,
         xlab = xlab,
         ...)
    abline(v = obs_prop, lwd = 3, col = 'white')
    abline(v = obs_prop, lwd = 2.5, col = 'black')
    box(bty = 'L', lwd = 2)

    if(missing(legend_position)){
      legend_position = 'topright'
    }

    if(legend_position != 'none'){
      legend(legend_position,
             legend = c(expression(hat(y)[propzero]),
                        expression(y[propzero])),
             bg = 'white',
             col = c(c_mid,
                     'black'),
             lty = 1, lwd = 2,
             bty = 'n')
    }

  }

  if(type == 'rootogram'){
    ymax <- floor(max(max(truths), quantile(preds, prob = 0.99, na.rm = TRUE)))
    ymin <- 0L
    xpos <- ymin:ymax

    # Bin if necessary to prevent overplotting
    if(length(xpos) > n_bins){
      cutpoints <- seq(ymin, ymax, length.out = n_bins)
      xpos <- floor(cutpoints)

      # Find the cutpoint interval that each prediction falls in
      tpreds <- as.list(rep(NA, NROW(preds)))
      for (i in seq_along(tpreds)) {
        tpreds[[i]] <- table(xpos[findInterval(preds[i, ], cutpoints)])
        matches <- match(xpos, rownames(tpreds[[i]]))
        tpreds[[i]] <- as.numeric(tpreds[[i]][matches])
      }
      tpreds <- do.call(rbind, tpreds)
      tpreds[is.na(tpreds)] <- 0
      tyquantile <- sqrt(t(apply(tpreds, 2, quantile, probs = probs, na.rm = TRUE)))
      tyexp <- tyquantile[,5]

      # Repeat for truths
      ty <- table(xpos[findInterval(truths, cutpoints)])
      ty <- sqrt(as.numeric(ty[match(xpos, rownames(ty))]))

    } else {

      tpreds <- as.list(rep(NA, NROW(preds)))
      for (i in seq_along(tpreds)) {
        tpreds[[i]] <- table(as.vector(preds[i,]))
        matches <- match(xpos, rownames(tpreds[[i]]))
        tpreds[[i]] <- as.numeric(tpreds[[i]][matches])
      }
      tpreds <- do.call(rbind, tpreds)
      tpreds[is.na(tpreds)] <- 0
      tyquantile <- sqrt(t(apply(tpreds, 2, quantile, probs = probs, na.rm = TRUE)))
      tyexp <- tyquantile[,5]
      ty <- table(truths)
      ty <- sqrt(as.numeric(ty[match(xpos, rownames(ty))]))
    }

    ty[is.na(ty)] <- 0
    ypos <- ty / 2
    ypos <- tyexp - ypos
    data <- data.frame(xpos, ypos, ty, tyexp, tyquantile)
    N <- length(xpos)
    idx <- rep(1:N, each = 2)
    repped_x <- rep(xpos, each = 2)
    x <- sapply(1:length(idx),
                function(k) if(k %% 2 == 0)
                  repped_x[k] + min(diff(xpos))/2 else
                    repped_x[k] - min(diff(xpos))/2)

    if(missing(xlab)){
      xlab <- expression(y)
    }

    if(missing(ylab)){
      ylab <- expression(sqrt(frequency))
    }

    # Plot the rootogram
    plot(1, type = "n", bty = 'L',
         xlab = xlab,
         ylab = ylab,
         xlim = range(xpos),
         ylim = range(c(data$tyexp, data[,13],
                        data[,5],
                        data$tyexp - data$ty)),
         ...)
    rect(xleft = x[seq(1, N*2, by = 2)],
         xright = x[seq(2, N*2, by = 2)],
         ytop =  data$tyexp,
         ybottom =  data$tyexp - data$ty,
         col = 'grey80',
         border = 'grey10',
         lwd = 2)
    rect(xleft = x[seq(1, N*2, by = 2)],
         xright = x[seq(2, N*2, by = 2)],
         ytop =  data[,13],
         ybottom =  data[,5],
         col = "#DCBCBC85",
         border = 'transparent')
    rect(xleft = x[seq(1, N*2, by = 2)],
         xright = x[seq(2, N*2, by = 2)],
         ytop =  data[,12],
         ybottom =  data[,6],
         col = "#C7999985",
         border = 'transparent')
    rect(xleft = x[seq(1, N*2, by = 2)],
         xright = x[seq(2, N*2, by = 2)],
         ytop =  data[,11],
         ybottom =  data[,7],
         col = "#B97C7C85",
         border = 'transparent')
    rect(xleft = x[seq(1, N*2, by = 2)],
         xright = x[seq(2, N*2, by = 2)],
         ytop =  data[,10],
         ybottom =  data[,8],
         col = "#A2505085",
         border = 'transparent')
    abline(h = 0, col = 'white', lwd = 3)
    abline(h = 0, col = 'black', lwd = 2.5)
    for (k in 1:N) {
      lines(x = c(x[seq(1, N*2, by = 2)][k],x[seq(2, N*2, by = 2)][k]),
            y = c(data[k,9], data[k,9]),
            col = "#8F2727", lwd = 3)
    }
    box(bty = 'L', lwd = 2)
  }

  if(type == 'mean'){
    # Plot observed and predicted means
    pred_means <- apply(preds, 1, mean, na.rm = TRUE)
    lower <- quantile(pred_means, probs = 0.01, na.rm = TRUE)
    upper <- quantile(pred_means, probs = 0.99, na.rm = TRUE)
    pred_means <- pred_means[-which(pred_means > upper)]
    if(lower!=0){
      pred_means <- pred_means[-which(pred_means < lower)]
    }
    obs_mean <- mean(truths)

    if(missing(ylab)){
      ylab <- 'Density'
    }

    if(missing(xlab)){
      xlab <- paste0('Predicted mean for ', levels(data_train$series)[series])
    }

    hist(pred_means,
         xlim = c(min(min(pred_means, na.rm = TRUE), min(obs_mean, na.rm = TRUE)),
                  max(max(pred_means, na.rm = TRUE), max(obs_mean, na.rm = TRUE))),
         lwd = 2,
         main = '',
         breaks = seq(min(pred_means, na.rm = TRUE),
                      max(pred_means, na.rm = TRUE), length.out = 20),
         border = "#B97C7C",
         col = "#C79999",
         ylab = ylab,
         xlab = xlab,
         ...)
    abline(v = obs_mean, lwd = 3, col = 'white')
    abline(v = obs_mean, lwd = 2.5, col = 'black')
    box(bty = 'L', lwd = 2)

    if(missing(legend_position)){
      legend_position = 'topright'
    }

    if(legend_position != 'none'){
    legend(legend_position,
           legend = c(expression(hat(mu)),
                      expression(mu)),
           bg = 'white',
           col = c(c_mid,
                   'black'),
           lty = 1, lwd = 2,
           bty = 'n')
    }
  }


  # Generate a sample sequence and plot
  if(type == 'density'){
    max_x <- max(max(density(preds[1,], na.rm = TRUE)$x),
                 max(density(truths, na.rm = TRUE)$x))
    min_x <- min(min(density(preds[1,], na.rm = TRUE)$x),
                 min(density(truths, na.rm = TRUE)$x))
    pred_densities <- do.call(rbind, (lapply(1:NROW(preds), function(x){
      if(length(which(is.na(preds[x,]))) > (length(preds[x,]) - 3)){
        rep(0, length(density(truths, from = min_x,
                              to = max_x, na.rm = TRUE)$y))
      } else {
        dens <- density(preds[x,], from = min_x,
                        to = max_x, na.rm = TRUE)
        dens$y
      }

    })))

    cred <- sapply(1:NCOL(pred_densities),
                   function(n) quantile(pred_densities[,n],
                                        probs = probs, na.rm = TRUE))
    true_dens <- density(truths, from = min_x,
                         to = max_x, na.rm = TRUE)
    ymax <- max(c(max(cred, na.rm = TRUE),
                  max(true_dens$y, na.rm = TRUE)))

    if(missing(ylab)){
      ylab <- paste0('Predictive density for ', levels(data_train$series)[series])
    }

    if(missing(xlab)){
      xlab <- ''
    }

    if(object$family == 'beta'){
      xlimits <- c(0, 1)
    } else if(object$family %in% c('poisson', 'negative binomial', 'lognormal', 'Gamma')){
      xlimits <- c(0, max_x)
    } else {
      xlimits <- c(min_x, max_x)
    }

    plot(1, type = "n", bty = 'L',
         xlab = xlab,
         ylab = ylab,
         xlim = xlimits,
         ylim = c(0, ymax),
         ...)

    polygon(c(true_dens$x, rev(true_dens$x)), c(cred[1,], rev(cred[9,])),
            col = c_light, border = NA)
    polygon(c(true_dens$x, rev(true_dens$x)), c(cred[2,], rev(cred[8,])),
            col = c_light_highlight, border = NA)
    polygon(c(true_dens$x, rev(true_dens$x)), c(cred[3,], rev(cred[7,])),
            col = c_mid, border = NA)
    polygon(c(true_dens$x, rev(true_dens$x)), c(cred[4,], rev(cred[6,])),
            col = c_mid_highlight, border = NA)
    lines(true_dens$x, cred[5,], col = c_dark, lwd = 2.5)


    lines(x = true_dens$x, y = true_dens$y, lwd = 3, col = 'white')
    lines(x = true_dens$x, y = true_dens$y, lwd = 2.5, col = 'black')

    if(missing(legend_position)){
      legend_position = 'topright'
    }

    if(legend_position != 'none'){
    legend(legend_position,
           legend = c(expression(hat(y)),
                      'y'),
           bg = 'white',
           col = c(c_mid,
                   "black"),
           lty = 1,
           lwd = 2,
           bty = 'n')
    }
    box(bty = 'L', lwd = 2)
  }

  if(type == 'hist'){
    if(missing(n_bins)){
      n_bins <- max(c(length(hist(c(truths, as.vector(preds)), plot = F)$breaks),
                      20))
    }

    if(sign(n_bins) != 1){
      stop('argument "n_bins" must be a positive integer',
           call. = FALSE)
    } else {
      if(n_bins%%1 != 0){
        stop('argument "n_bins" must be a positive integer',
             call. = FALSE)
      }
    }

    xlim <- c(min(min(density(preds[1,], na.rm = TRUE)$x),
                  min(density(truths, na.rm = TRUE)$x)),
              max(max(density(preds[1,], na.rm = TRUE)$x),
                  max(density(truths, na.rm = TRUE)$x)))

    if(object$family == 'beta'){
      xlim <- c(0, 1)
    } else if(object$family %in% c('poisson', 'negative binomial', 'lognormal', 'Gamma')){
      xlim <- c(0, xlim[2])
    } else {
      xlim <- xlim
    }

    breaks <- seq(xlim[1], xlim[2], length.out = n_bins)
    truths <- truths[truths<=xlim[2]]
    truths <- truths[truths>=xlim[1]]
    preds <- preds[preds<=xlim[2]]
    preds <- preds[preds>=xlim[1]]

    ylim <- c(0, max(c(max(hist(truths, breaks = breaks, plot = F)$density, na.rm = TRUE),
                       max(hist(preds, breaks = breaks, plot = F)$density, na.rm = TRUE)),
                     na.rm = TRUE))

    if(missing(xlab)){
      xlab <- paste0('Count')
    }

    if(missing(ylab)){
      ylab = ''
    }

    hist(preds, breaks=breaks, lwd = 2,
         main='',
         xlab = xlab,
         ylab = ylab,
         ylim=ylim,
         xlim=xlim,
         border = "#B97C7C",
         col = "#C79999",
         freq = F,
         ...)

    par(lwd=2)
    hist(truths, breaks=breaks,
         main='', xlab='',
         ylim=ylim,
         xlim=xlim,
         ylab='', yaxt='n',
         col=rgb(red = 0, green = 0, blue = 0, alpha = 0),
         border="black",
         add=T, freq = F)
    par(lwd=1)
    box(bty = 'L', lwd = 2)

    if(missing(legend_position)){
      legend_position = 'topright'
    }

    if(legend_position != 'none'){
    legend(legend_position,
           legend = c(expression(hat(y)),
                      'y'),
           bg = 'white',
           col = c(c_mid,
                   "black"),
           lty = 1,
           lwd = 2,
           bty = 'n')
    }
  }


  if(type == 'cdf'){
    ecdf_plotdat = function(vals, x){
      if(length(which(is.na(vals))) > (length(vals) - 3)){
      } else {
        func <- ecdf(vals)
        func(x)
      }
    }

    plot_x <- seq(from = min(truths, na.rm = T),
                  to = max(truths, na.rm = T),
                  length.out = 100)

    pred_cdfs <- do.call(rbind, (lapply(1:NROW(preds), function(x){
      ecdf_plotdat(preds[x,], x = plot_x)
    })))

    probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
    cred <- sapply(1:NCOL(pred_cdfs),
                   function(n) quantile(pred_cdfs[,n],
                                        probs = probs, na.rm = TRUE))

    if(missing(ylab)){
      ylab = paste0('Predictive CDF for ', levels(data_train$series)[series])
    }

    if(missing(xlab)){
      xlab = ''
    }

    plot(1, type = "n", bty = 'L',
         xlab = xlab,
         ylab = ylab,
         xlim = c(min(plot_x, na.rm = TRUE), max(plot_x, na.rm = TRUE)),
         ylim = c(0, 1),
         ...)

    polygon(c(plot_x, rev(plot_x)), c(cred[1,], rev(cred[9,])),
            col = c_light, border = NA)
    polygon(c(plot_x, rev(plot_x)), c(cred[2,], rev(cred[8,])),
            col = c_light_highlight, border = NA)
    polygon(c(plot_x, rev(plot_x)), c(cred[3,], rev(cred[7,])),
            col = c_mid, border = NA)
    polygon(c(plot_x, rev(plot_x)), c(cred[4,], rev(cred[6,])),
            col = c_mid_highlight, border = NA)
    lines(plot_x, cred[5,], col = c_dark, lwd = 2.5)

    lines(x = plot_x,
          y = ecdf_plotdat(truths,
                           plot_x),
          col = 'white',
          lwd = 3)
    lines(x = plot_x,
          y = ecdf_plotdat(truths,
                           plot_x),
          col = 'black',
          lwd = 2.5)

    if(missing(legend_position)){
      legend_position = 'bottomright'
    }

    if(legend_position != 'none'){
    legend(legend_position,
           legend = c(expression(hat(y)),
                      'y'),
           bg = 'white',
           col = c(c_mid,
                   'black'),
           lty = 1, lwd = 2,
           bty = 'n')
    }
    box(bty = 'L', lwd = 2)
  }

  if(type == 'pit'){

    # Calculate emipirical cumulative distribution function as the
    # portion of (y_predicted <= y_true)
    n_pred <- ncol(preds)
    P_x <- vapply(seq_along(truths),
                  function(i) {
                    sum(preds[i, ] <= truths[i]) / n_pred
                  },
                  .0)

    P_xm1 <- vapply(seq_along(truths),
                    function(i) {
                      sum(preds[i,] <= truths[i] - 1.e-6) / n_pred
                    },
                    .0)
    # 1000 replicates for randomised PIT
    u <- replicate(1000, P_xm1 + stats::runif(length(truths)) * (P_x - P_xm1))
    pit_hist <- hist(u, breaks = seq(0, 1, by = 0.1), plot = F)$density
    pit_hist <- (pit_hist / sum(pit_hist)) * 10

    barplot(pit_hist, lwd = 2,
            col = "#B97C7C",
            xlab = paste0('Predictive PIT for ', levels(data_train$series)[series]),
            border = NA,
            ...)
    abline(h = 1, col = '#FFFFFF60', lwd = 2.85)
    abline(h = 1, col = 'black', lwd = 2.5, lty = 'dashed')
    box(bty = 'L', lwd = 2)
  }
}

#' Posterior Predictive Checks for \code{mvgam} Objects
#'
#' Perform posterior predictive checks with the help
#' of the \pkg{bayesplot} package.
#'
#' @aliases pp_check
#' @inheritParams brms::pp_check
#' @importFrom insight get_predictors
#' @importFrom brms do_call
#' @param object An object of class \code{mvgam}.
#' @param newdata Optional \code{dataframe} or \code{list} of test data containing the
#' variables included in the linear predictor of \code{formula}. If not supplied,
#' predictions are generated for the original observations used for the model fit.
#' @param ... Further arguments passed to \code{\link{predict.mvgam}}
#'   as well as to the PPC function specified in \code{type}.
#' @inheritParams prepare_predictions.brmsfit
#'
#' @return A ggplot object that can be further
#'  customized using the \pkg{ggplot2} package.
#'
#' @details For a detailed explanation of each of the ppc functions,
#' see the \code{\link[bayesplot:PPC-overview]{PPC}}
#' documentation of the \pkg{\link[bayesplot:bayesplot-package]{bayesplot}}
#' package.
#' @seealso \code{\link{ppc}} \code{\link{predict.mvgam}}
#' @examples
#' \dontrun{
#'simdat <- sim_mvgam(seasonality = 'hierarchical')
#'mod <- mvgam(y ~ series +
#'               s(season, bs = 'cc', k = 6) +
#'               s(season, series, bs = 'fs', k = 4),
#'             data = simdat$data_train,
#'             burnin = 300,
#'             samples = 300)
#'
#'# Use pp_check(mod, type = "xyz") for a list of available plot types
#'
#'# Default is a density overlay for all observations
#'pp_check(mod)
#'
#'# Rootograms particularly useful for count data
#'pp_check(mod, type = "rootogram")
#'
#'# Grouping plots by series is useful
#'pp_check(mod, type = "bars_grouped",
#'         group = "series", ndraws = 50)
#'pp_check(mod, type = "ecdf_overlay_grouped",
#'         group = "series", ndraws = 50)
#'pp_check(mod, type = "stat_freqpoly_grouped",
#'         group = "series", ndraws = 50)
#'
#'# Custom functions accepted
#'prop_zero <- function(x) mean(x == 0)
#'pp_check(mod, type = "stat", stat = "prop_zero")
#'pp_check(mod, type = "stat_grouped",
#'         stat = "prop_zero",
#'         group = "series")
#'
#'# Some functions accept covariates to set the x-axes
#'pp_check(mod, x = "season",
#'         type = "ribbon_grouped",
#'         prob = 0.5,
#'         prob_outer = 0.8,
#'         group = "series")
#'
#'# Many plots can be made without the observed data
#'pp_check(mod, prefix = "ppd")
#' }
#'
#' @importFrom bayesplot pp_check
#' @export pp_check
#' @author Nicholas J Clark
#' @export
pp_check.mvgam <- function(object, type, ndraws = NULL, prefix = c("ppc", "ppd"),
                             group = NULL, x = NULL, newdata = NULL, ...) {

  # Set red colour scheme
  col_scheme <- attr(color_scheme_get(),
                     'scheme_name')
  color_scheme_set('red')

  dots <- list(...)
  if (missing(type)) {
    type <- "dens_overlay"
  }

  prefix <- match.arg(prefix)
  ndraws_given <- "ndraws" %in% names(match.call())

  if(is.null(newdata)){
    newdata <- object$obs_data
  }

  if (prefix == "ppc") {
    # No type checking for prefix 'ppd' yet
    valid_types <- as.character(bayesplot::available_ppc(""))
    valid_types <- sub("^ppc_", "", valid_types)
    if (!type %in% valid_types) {
      stop("Type '", type, "' is not a valid ppc type. ",
            "Valid types are:\n",
           paste0("'", valid_types, "'", collapse = ", "),
           call. = FALSE)
    }
  }
  ppc_fun <- get(paste0(prefix, "_", type), asNamespace("bayesplot"))

  family <- object$family
  if (family == 'nmix') {
    stop("'pp_check' is not implemented for this family.",
         call. = FALSE)
  }
  valid_vars <- names(get_predictors(object))
  if ("group" %in% names(formals(ppc_fun))) {
    if (is.null(group)) {
      stop("Argument 'group' is required for ppc type '", type, "'.",
           call. = FALSE)
    }
    if (!group %in% valid_vars) {
      stop("Variable '", group, "' could not be found in the data.",
           call. = FALSE)
    }
  }
  if ("x" %in% names(formals(ppc_fun))) {
    if (!is.null(x) && !x %in% valid_vars) {
      stop("Variable '", x, "' could not be found in the data.",
           call. = FALSE)
    }
  }
  if (type == "error_binned") {
    method <- "posterior_epred"
  } else {
    method <- "posterior_predict"
  }
  if (!ndraws_given) {
    aps_types <- c(
      "error_scatter_avg", "error_scatter_avg_vs_x",
      "intervals", "intervals_grouped",
      "loo_intervals", "loo_pit", "loo_pit_overlay",
      "loo_pit_qq", "loo_ribbon",
      'pit_ecdf', 'pit_ecdf_grouped',
      "ribbon", "ribbon_grouped",
      "rootogram", "scatter_avg", "scatter_avg_grouped",
      "stat", "stat_2d", "stat_freqpoly_grouped", "stat_grouped",
      "violin_grouped"
    )
    if (type %in% aps_types) {
      ndraws <- NULL
      message("Using all posterior draws for ppc type '",
              type, "' by default.")
    } else {
      ndraws <- 10
      message("Using 10 posterior draws for ppc type '",
              type, "' by default.")
    }
  }

  y <- NULL
  if (prefix == "ppc") {
    # y is ignored in prefix 'ppd' plots; get the response variable,
    # but take care that binomial models use the cbind() lhs
    resp_terms <- as.character(terms(formula(object$call))[[2]])
    if(length(resp_terms) == 1){
      out_name <- as.character(terms(object$call)[[2]])
    } else {
      if(any(grepl('cbind', resp_terms))){
        resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
        out_name <- resp_terms[1]
      }
    }
    y <- newdata[[out_name]]
  }

  pred_args <- list(
    object, newdata = newdata, ndraws = ndraws, ...)
  yrep <- do_call(method, pred_args)

  if (anyNA(y)) {
    warning("NA responses are not shown in 'pp_check'.")
    take <- !is.na(y)
    y <- y[take]
    yrep <- yrep[, take, drop = FALSE]
  } else {
    take <- NULL
  }

  # Prepare plotting arguments
  ppc_args <- list()
  if (prefix == "ppc") {
    ppc_args$y <- y
    ppc_args$yrep <- yrep
  } else if (prefix == "ppd") {
    ppc_args$ypred <- yrep
  }
  if (!is.null(group)) {
    if(!exists(group, newdata)) stop(paste0('Variable ',
                                            group,
                                            ' not in newdata'),
                                     call. = FALSE)
    ppc_args$group <- newdata[[group]]

    if(!is.null(take)){
      ppc_args$group <- ppc_args$group[take]
    }
  }

  is_like_factor = function(x){
    is.factor(x) || is.character(x) || is.logical(x)
  }

  if (!is.null(x)) {
    ppc_args$x <- newdata[[x]]
    if (!is_like_factor(ppc_args$x)) {
      ppc_args$x <- as.numeric(ppc_args$x)
    }

    if(!is.null(take)){
      ppc_args$x <- ppc_args$x[take]
    }
  }

  if ("psis_object" %in% setdiff(names(formals(ppc_fun)), names(ppc_args))) {
    # ppc_args$psis_object <- do_call(
    #   compute_loo, c(pred_args, criterion = "psis")
    # )
    # compute_loo() not available yet for mvgam
    ppc_args$psis_object <- NULL
  }
  if ("lw" %in% setdiff(names(formals(ppc_fun)), names(ppc_args))) {
    # ppc_args$lw <- weights(
    #   do_call(compute_loo, c(pred_args, criterion = "psis"))
    # )
    # compute_loo() not available yet for mvgam
    ppc_args$lw <- NULL
  }

  # Most ... arguments are meant for the prediction function
  for_pred <- names(dots) %in% names(formals(posterior_predict.mvgam))
  ppc_args <- c(ppc_args, dots[!for_pred])

  # Generate plot and reset colour scheme
  out_plot <- do_call(ppc_fun, ppc_args)
  color_scheme_set(col_scheme)

  # Return the plot
  return(out_plot)
}
