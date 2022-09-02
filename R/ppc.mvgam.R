#'@title Plot mvgam posterior predictive checks for a specified series
#'@name ppc.mvgam
#'@param object \code{list} object returned from \code{mvgam}
#'@param data_test Optional \code{dataframe} or \code{list} of test data containing at least 'series' and 'time'
#'for the forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}. If
#'included, the observed values in the test data are compared to the model's forecast distribution for exploring
#'biases in model predictions.
#'Note this is only useful if the same \code{data_test} was also included when fitting the original model.
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
#'ability to generate unbiased predictions. For all plots apart from the 'rootogram', posterior predictions
#'can also be compared to out of sample observations as long as these observations were included as
#''data_test' in the original model fit and supplied here. Rootograms are currently only plotted using the
#''hanging' style
#'@return A base \code{R} graphics plot showing either a posterior rootogram (for \code{type == 'rootogram'}),
#'the predicted vs observed mean for the
#'series (for \code{type == 'mean'}), predicted vs observed proportion of zeroes for the
#'series (for \code{type == 'prop_zero'}),predicted vs observed histogram for the
#'series (for \code{type == 'hist'}), kernel density or empirical CDF estimates for
#'posterior predictions (for \code{type == 'density'} or \code{type == 'cdf'}) or a Probability
#'Integral Transform histogram (for \code{type == 'pit'}).
#'
NULL
#'@export
ppc <- function(x, what, ...){
  UseMethod("ppc")
}

#'@rdname ppc.mvgam
#'@method ppc mvgam
#'@export
ppc.mvgam = function(object, data_test, series = 1, type = 'density',
                     n_bins, legend_position, xlab, ylab, ...){

  # Check arguments
  type <- match.arg(arg = type, choices = c("rootogram", "mean", "hist",
                                            "density", "pit", "cdf",
                                            "prop_zero"))

  optional_args <- list(...)

  if(class(object) != 'mvgam'){
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

  # Pull out observations and posterior predictions for the specified series
  data_train <- object$obs_data
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$model_output, 'ypred'))[2],
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
        preds <- MCMCvis::MCMCchains(object$model_output, 'ypred')[,seq(series,
                                                                        dim(MCMCvis::MCMCchains(object$model_output, 'ypred'))[2],
                                                                        by = NCOL(object$ytimes))]
      } else {
        preds <- MCMCvis::MCMCchains(object$model_output, 'ypred')[,starts[series]:ends[series]]
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
        preds <- MCMCvis::MCMCchains(object$model_output, 'ypred')[,seq(series,
                                                                        dim(MCMCvis::MCMCchains(object$model_output, 'ypred'))[2],
                                                                        by = NCOL(object$ytimes))]
      } else {
        preds <- MCMCvis::MCMCchains(object$model_output, 'ypred')[,starts[series]:ends[series]]
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
        dplyr::arrange(time, series) %>%
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
      preds <- MCMCvis::MCMCchains(object$model_output, 'ypred')[,seq(series,
                                                                      dim(MCMCvis::MCMCchains(object$model_output, 'ypred'))[2],
                                                                      by = NCOL(object$ytimes))]
    } else {
      preds <- MCMCvis::MCMCchains(object$model_output, 'ypred')[,starts[series]:ends[series]]
    }

    preds <- preds[,1:length(truths)]

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
    #min_x <- min(density(preds[1,])$x)
    min_x <- 0
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

    plot(1, type = "n", bty = 'L',
         xlab = xlab,
         ylab = ylab,
         xlim = c(min_x, max_x),
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

    bin_lims <- range(c(truths, as.vector(preds)), na.rm = TRUE)
    #delta <- diff(range(preds)) / n_bins
    breaks <- seq(bin_lims[1], bin_lims[2], length.out = n_bins)
    xlim <- c(0,
              max(max(density(preds[1,], na.rm = TRUE)$x),
                  max(density(truths, na.rm = TRUE)$x)))
    ylim <- c(0, max(c(max(hist(truths, breaks = breaks, plot = F)$density, na.rm = TRUE),
                       max(hist(preds, breaks = breaks, plot = F)$density, na.rm = TRUE))))

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

    plot_x <- seq(min(truths, na.rm = T),
                  max(truths, na.rm = T))

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
                      sum(preds[i,] <= truths[i] - 1) / n_pred
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
