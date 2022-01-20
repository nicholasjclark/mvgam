#'Plot mvjagam posterior predictive checks for a specified series
#'@param object \code{list} object returned from \code{mvjagam}
#'@param data_test Optional \code{dataframe} of test data containing at least 'series', 'season' and 'year'
#'for the forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}. If
#'included, the observed values in the test data are compared to the model's forecast distribution for exploring
#'biases in model predictions.
#'Note this is only useful if the same \code{data_test} was also included when fitting the original model.
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param type \code{character} specifying the type of posterior predictive check to calculate and plot.
#'Valid options are: 'mean', 'density', 'pit' and 'cdf'
#'@param n_samples \code{integer} specifying the number of posterior simulations to draw when plotting
#'either the kernel density (\code{type == 'density'}) or empirical CDF (\code{type == 'cdf'})
#'@details Posterior predictions are drawn from the fitted \code{mvjagam} and compared against
#'the empirical distribution of the observed data for a specified series to help evaluate the model's
#'ability to generate unbiased predictions.
#'@return A base \code{R} graphics plot showing either the predicted vs observed mean for the
#'series (for \code{type == 'mean'}), kernel density or empirical CDF estimates for
#'posterior predictions (for \code{type == 'density'} or \code{type == 'cdf'}) or a Probability
#'Integral Transform histogram (for \code{type == 'pit'})
#'@export
plot_mvgam_ppc = function(object, data_test, series, type = 'density', n_samples = 500){

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
    truths <- data_test %>%
      dplyr::filter(series == s_name) %>%
      dplyr::select(year, season, y) %>%
      dplyr::distinct() %>%
      dplyr::arrange(year, season) %>%
      dplyr::pull(y)

    preds <- MCMCvis::MCMCchains(object$jags_output, 'ypred')[,starts[series]:ends[series]]
    preds <- preds[,((NROW(data_train) / NCOL(object$ytimes))+1):
                     ((NROW(data_train) / NCOL(object$ytimes))+length(truths))]

  } else {
    truths <- data_train %>%
      dplyr::filter(series == s_name) %>%
      dplyr::select(year, season, y) %>%
      dplyr::distinct() %>%
      dplyr::arrange(year, season) %>%
      dplyr::pull(y)

    preds <- MCMCvis::MCMCchains(object$jags_output, 'ypred')[,starts[series]:ends[series]]
    preds <- preds[,1:length(truths)]
  }

  # Can't deal with missing values in these diagnostic plots
  preds <- preds[, !is.na(truths)]
  truths <- truths[!is.na(truths)]

  if(type == 'mean'){
    # Plot observed and predicted location (means)
    pred_means <- apply(preds, 1, mean)
    obs_mean <- mean(truths)
    hist(pred_means,
         xlim = c(min(min(pred_means), min(obs_mean)),
                  max(max(pred_means), max(obs_mean))),
         main = '',
         breaks = seq(min(pred_means), max(pred_means), length.out = 20),
         border = "#B97C7C",
         col = "#C79999",
         ylab = '',
         xlab = paste0('Predicted mean for ', levels(data_train$series)[series]))
    abline(v = obs_mean, lwd = 3, col = 'white')
    abline(v = obs_mean, lwd = 2.5, col = 'black')
    box()

    legend('topright',
           legend = c(expression(hat(mu)),
                      expression(mu)),
           bg = 'white',
           col = c("#B97C7C",
                   'black'),
           lty = 1, lwd = 2,
           bty = 'n')
  }

  # Generate a sample sequence and plot
  if(type == 'density'){
    sample_seq <- sample(1:NROW(preds), n_samples, T)
    ymax <- max(c(apply(preds[sample_seq,], 1, function(x) max(density(x)$y))),
                max(density(truths)$y))

    plot(density(preds[1,]),
         main = '', xlab = '',
         ylim = c(0, ymax),
         ylab = paste0('Predictive density for ', levels(data_train$series)[series]),
         col = rgb(150, 0, 0, max = 255, alpha = 10),
         lwd = 0.8)
    for(i in 1:n_samples){
      lines(density(preds[sample_seq[i],]),
            col = rgb(150, 0, 0, max = 255, alpha = 10),
            lwd = 0.8)
    }

    lines(density(truths), lwd = 3, col = 'white')
    lines(density(truths), lwd = 2.5, col = 'black')
    legend('topright',
           legend = c(expression(hat(y)),
                      'y'),
           bg = 'white',
           col = c(rgb(150, 0, 0, max = 255, alpha = 100),
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

    sample_seq <- sample(1:NROW(preds), n_samples, T)
    plot_x <- seq(min(truths, na.rm = T),
                  max(truths, na.rm = T))
    plot(x = plot_x,
         y = ecdf_plotdat(preds[1,],
                          plot_x),
         main = '', xlab = '',
         ylab = paste0('Predictive CDF for ', levels(data_train$series)[series]),
         ylim = c(0, 1),
         col = rgb(150, 0, 0, max = 255, alpha = 10),
         type = 'l', lwd = 0.8)

    for(i in 1:n_samples){
      lines(x = plot_x,
            y = ecdf_plotdat(preds[i,],
                             plot_x),
            col = rgb(150, 0, 0, max = 255, alpha = 10),
            lwd = 0.8)
    }

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
    legend('bottomright',
           legend = c(expression(hat(y)),
                      'y'),
           bg = 'white',
           col = c(rgb(150, 0, 0, max = 255, alpha = 180),
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
