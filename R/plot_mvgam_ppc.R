#'Plot mvjagam posterior predictive checks for a specified series
#'@param object \code{list} object returned from \code{mvjagam}
#'@param data_test Optional \code{dataframe} or \code{list} of test data containing at least 'series', 'season' and 'year'
#'for the forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}. If
#'included, the observed values in the test data are compared to the model's forecast distribution for exploring
#'biases in model predictions.
#'Note this is only useful if the same \code{data_test} was also included when fitting the original model.
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param type \code{character} specifying the type of posterior predictive check to calculate and plot.
#'Valid options are: 'mean', 'density', 'pit' and 'cdf'
#'@param legend_position The location may also be specified by setting x to a single keyword from the
#'list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#'This places the legend on the inside of the plot frame at the given location.
#'@details Posterior predictions are drawn from the fitted \code{mvjagam} and compared against
#'the empirical distribution of the observed data for a specified series to help evaluate the model's
#'ability to generate unbiased predictions.
#'@return A base \code{R} graphics plot showing either the predicted vs observed mean for the
#'series (for \code{type == 'mean'}), kernel density or empirical CDF estimates for
#'posterior predictions (for \code{type == 'density'} or \code{type == 'cdf'}) or a Probability
#'Integral Transform histogram (for \code{type == 'pit'})
#'@export
plot_mvgam_ppc = function(object, data_test, series, type = 'density', legend_position){

  # Check arguments
  type <- match.arg(arg = type, choices = c("mean","density", "pit", "cdf"))

  # Pull out observations and posterior predictions for the specified series
  data_train <- object$obs_data
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]

  s_name <- levels(data_train$series)[series]

  if(!missing(data_test)){
    if(class(object$obs_data) == 'list'){
      truths <- data.frame(y = data_test$y,
                           year = data_test$year,
                           season = data_test$season,
                           series = data_test$series) %>%
        dplyr::arrange(year, season, series) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::pull(y)

    } else {
    truths <- data_test %>%
      dplyr::filter(series == s_name) %>%
      dplyr::select(year, season, y) %>%
      dplyr::distinct() %>%
      dplyr::arrange(year, season) %>%
      dplyr::pull(y)
    }

    preds <- MCMCvis::MCMCchains(object$jags_output, 'ypred')[,starts[series]:ends[series]]
    preds <- preds[,((NROW(data_train) / NCOL(object$ytimes))+1):
                     ((NROW(data_train) / NCOL(object$ytimes))+length(truths))]

  } else {
    if(class(object$obs_data) == 'list'){
      truths <- data.frame(y = data_train$y,
                           year = data_train$year,
                           season = data_train$season,
                           series = data_train$series) %>%
        dplyr::arrange(year, season, series) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::pull(y)

    } else {
      truths <- data_train %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(year, season, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(year, season) %>%
        dplyr::pull(y)
    }

    preds <- MCMCvis::MCMCchains(object$jags_output, 'ypred')[,starts[series]:ends[series]]
    preds <- preds[,1:length(truths)]
  }

  # Can't deal with missing values in these diagnostic plots
  preds <- preds[, !is.na(truths)]
  truths <- truths[!is.na(truths)]

  # Colours needed for plotting quantiles
  c_light <- c("#DCBCBC")
  c_light_highlight <- c("#C79999")
  c_mid <- c("#B97C7C")
  c_mid_highlight <- c("#A25050")
  c_dark <- c("#8F2727")
  c_dark_highlight <- c("#7C0000")

  if(type == 'mean'){
    # Plot observed and predicted means
    pred_means <- apply(preds, 1, mean)
    lower <- quantile(pred_means, probs = 0.01)
    upper <- quantile(pred_means, probs = 0.99)
    pred_means <- pred_means[-which(pred_means > upper)]
    pred_means <- pred_means[-which(pred_means < lower)]
    obs_mean <- mean(truths)
    hist(pred_means,
         xlim = c(min(min(pred_means), min(obs_mean)),
                  max(max(pred_means), max(obs_mean))),
         main = '',
         breaks = seq(min(pred_means), max(pred_means), length.out = 20),
         border = "#B97C7C",
         col = "#C79999",
         ylab = 'Density',
         xlab = paste0('Predicted mean for ', levels(data_train$series)[series]))
    abline(v = obs_mean, lwd = 3, col = 'white')
    abline(v = obs_mean, lwd = 2.5, col = 'black')
    box()

    if(missing(legend_position)){
      legend_position = 'topright'
    }

    legend(legend_position,
           legend = c(expression(hat(mu)),
                      expression(mu)),
           bg = 'white',
           col = c(c_mid,
                   'black'),
           lty = 1, lwd = 2,
           bty = 'n')
  }

  # Generate a sample sequence and plot
  if(type == 'density'){

    probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)

    max_x <- max(density(preds[1,])$x)
    min_x <- min(density(preds[1,])$x)
    pred_densities <- do.call(rbind, (lapply(1:NROW(preds), function(x){
      dens <- density(preds[x,], from = min_x,
                      to = max_x)
      dens$y
    })))

    cred <- sapply(1:NCOL(pred_densities),
                   function(n) quantile(pred_densities[,n],
                                        probs = probs))
    true_dens <- density(truths, from = min_x,
                         to = max_x)
    ymax <- max(c(max(cred),
                max(true_dens$y)))

    plot(1, type = "n",
         xlab = '',
         ylab = paste0('Predictive density for ', levels(data_train$series)[series]),
         xlim = c(min_x, max_x),
         ylim = c(0, ymax))

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

  if(type == 'cdf'){
    ecdf_plotdat = function(vals, x){
      func <- ecdf(vals)
      func(x)
    }

    plot_x <- seq(min(truths, na.rm = T),
                  max(truths, na.rm = T))

    pred_cdfs <- do.call(rbind, (lapply(1:NROW(preds), function(x){
      ecdf_plotdat(preds[x,], x = plot_x)
    })))

    probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
    cred <- sapply(1:NCOL(pred_cdfs),
                   function(n) quantile(pred_cdfs[,n],
                                        probs = probs))

    plot(1, type = "n",
         xlab = '',
         ylab = paste0('Predictive CDF for ', levels(data_train$series)[series]),
         xlim = c(min(plot_x), max(plot_x)),
         ylim = c(0, 1))

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

    legend(legend_position,
           legend = c(expression(hat(y)),
                      'y'),
           bg = 'white',
           col = c(c_mid,
                   'black'),
           lty = 1, lwd = 2,
           bty = 'n')
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

    barplot(pit_hist,
            col = "#B97C7C",
            xlab = paste0('Predictive PIT for ', levels(data_train$series)[series]),
            border = NA)
    abline(h = 1, lty = 'dashed', lwd = 2)
    box()
  }
}
