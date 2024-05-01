#'Latent factor summaries for a fitted mvgam object
#'
#'This function takes a fitted \code{mvgam} object and returns plots and summary statistics for
#'the latent dynamic factors
#'
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'@param plot \code{logical} specifying whether factors should be plotted
#'@author Nicholas J Clark
#'@details If the model in \code{object} was estimated using dynamic factors, it is possible that not all factors
#'contributed to the estimated trends. This is due to the regularisation penalty that acts independently on each
#'factor's Gaussian precision, which will squeeze un-needed factors to a white noise process (effectively dropping
#'that factor from the model). In this function, each factor is tested against a null hypothesis of white noise by
#'calculating the sum of the factor's 2nd derivatives. A factor that has a larger contribution will have a larger
#'sum due to the weaker penalty on the factor's precision. If
#'\code{plot == TRUE}, the factors are also plotted.
#'@return A \code{dataframe} of factor contributions and,
#'optionally, a series of base \code{R} plots
#'@examples
#'\dontrun{
#'simdat <- sim_mvgam()
#'mod <- mvgam(y ~ s(season, bs = 'cc',
#'                   k = 6),
#'             trend_model = AR(),
#'             use_lv = TRUE,
#'             n_lv = 2,
#'             data = simdat$data_train,
#'             chains = 2)
#'plot_mvgam_factors(mod)
#'}
#'@export
plot_mvgam_factors = function(object, plot = TRUE){

  # Check arguments
  if (!(inherits(object, "mvgam"))) {
    stop('argument "object" must be of class "mvgam"')
  }

  # Check object has latent dynamic factors
  if(!object$use_lv){
    stop('No latent factors used in object')
  }

  # Get indices of LV estimates
  ends <- seq(0, dim(mcmc_chains(object$model_output, 'LV'))[2],
              length.out = object$n_lv + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, object$n_lv + 1)])
  ends <- ends[-1]
  probs <- c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)

  # Set up plot environment
  if(plot){
    c_light <- c("#DCBCBC")
    c_light_highlight <- c("#C79999")
    c_mid <- c("#B97C7C")
    c_mid_highlight <- c("#A25050")
    c_dark <- c("#8F2727")
    c_dark_highlight <- c("#7C0000")
    .pardefault <- par(no.readonly=T)
    on.exit(par(.pardefault))

    if(object$n_lv <= 2){
      par(mfrow = c(1, 2))
    } else if(object$n_lv <= 4){
      par(mfrow = c(2, 2))
    } else {
      par(mfrow = c(3, 2))
    }
  }

  # Loop across each lv and calculate probability that the lv was dropped
  lv_estimates <- do.call(rbind, lapply(1:object$n_lv, function(x){

    if(object$fit_engine == 'stan'){
      inds_lv <- seq(x, dim(mcmc_chains(object$model_output, 'LV'))[2], by = object$n_lv)
      preds <- mcmc_chains(object$model_output, 'LV')[,inds_lv]
    } else {
      preds <- mcmc_chains(object$model_output, 'LV')[,starts[x]:ends[x]]
    }

    # Keep only the in-sample observations for testing against the null of white noise
    preds <- preds[,1:(length(object$obs_data$y) / NCOL(object$ytimes))]

    cred <- sapply(1:NCOL(preds),
                   function(n) quantile(preds[,n],
                                        probs = probs))
    # If plot = TRUE, plot the LVs
    if(plot){
      preds_last <- preds[1,]
      ylim <- range(cred)
      ylab <- paste0('Factor ', x)
      pred_vals <- seq(1:length(preds_last))
      plot(1, type = "n", bty = 'L',
           xlab = 'Time',
           ylab = ylab,
           xlim = c(0, length(preds_last)),
           ylim = ylim)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[9,])),
              col = c_light, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[2,], rev(cred[8,])),
              col = c_light_highlight, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[3,], rev(cred[7,])),
              col = c_mid, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[4,], rev(cred[6,])),
              col = c_mid_highlight, border = NA)
      lines(pred_vals, cred[5,], col = c_dark, lwd = 2.5)
      box(bty = 'L', lwd = 2)

    }

    # Calculate second derivatives of empirical medians and upper / lower intervals;
    # factors with small second derivatives are moving in roughly a straight line and not
    # likely contributing much (or at all) to the latent trend estimates
    meds <- cred[5,]
    uppers <- cred[8,]
    lowers <- cred[2,]
    data.frame('Contribution' = sum(abs(diff(diff(meds)) +
                                          diff(diff(uppers)) +
                                          diff(diff(lowers)))))
  }))

  rownames(lv_estimates) <- paste0('Factor', 1:object$n_lv)

  if(plot){
    layout(1)
  }

  lv_estimates / sum(lv_estimates)
}
