#'Plot mvjagam parametric term partial effects
#'
#'This function plots posterior empirical quantiles for partial effects of parametric terms
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@details Posterior empirical quantiles of each parametric term's partial effect estimates
#'(on the link scale) are calculated and visualised as ribbon plots. These effects can
#'be interpreted as the partial effect that a parametric term contributes when all other
#'terms in the model have been set to \code{0}
#'@return A base \code{R} graphics plot
#'@export
plot_mvgam_pterms = function(object){
# General plotting colours and empirical quantile probabilities
c_light <- c("#DCBCBC")
c_light_trans <- c("#DCBCBC70")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_mid_highlight_trans <- c("#A2505095")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")
probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)

# Look for parametric terms in the model
pterms <- attr(object$mgcv_model$pterms, 'term.labels')

# Convert stanfit objects to coda samples
if(class(object$model_output) == 'stanfit'){
  object$model_output <- coda::mcmc.list(lapply(1:NCOL(object$model_output),
                                                function(x) coda::mcmc(as.array(object$model_output)[,x,])))
}

if(length(pterms) > 0){
  # Graphical parameters
  .pardefault <- par(no.readonly=T)
  par(.pardefault)

  if(length(pterms) == 1){
    par(mfrow = c(1,1),
        mar = c(4, 4.5, 3, 4))
  }

  if(length(pterms) == 2){
    par(mfrow = c(2, 1))
  }

  if(length(pterms) %in% c(3, 4)){
    par(mfrow = c(2, 2))
  }

  for(i in 1:length(pterms)){
    # Find out which beta corresponds to the associated parametric term
    betas_keep <- grepl(paste0(pterms[i]),
                        colnames(predict(object$mgcv_model, type = 'lpmatrix')),
                        fixed = T)
    betas <- MCMCvis::MCMCchains(object$model_output, 'b')[ ,betas_keep]
    beta_creds <- quantile(betas, probs = probs)

    # Generate linear predictor matrix from fitted mgcv model
    Xp <- predict(object$mgcv_model, newdata = object$obs_data, type = 'lpmatrix')

    # Zero out all other columns in Xp
    Xp[,!betas_keep] <- 0

    # X-axis values
    if(class(pred_dat)[1] == 'list'){
      pred_vals_orig <- sort(object$obs_data[[pterms[i]]])
    } else {
      pred_vals_orig <- sort(object$obs_data %>%
                               dplyr::pull(pterms[i]))
    }

    pred_vals <- seq(min(pred_vals_orig), max(pred_vals_orig), length.out = 500)
    cred <- as.matrix(beta_creds) %*% pred_vals

    # Plot
    plot(1, type = "n",
         xlab = pterms[i],
         ylab = paste0('Partial effect of ', pterms[i]),
         xlim = c(min(pred_vals), max(pred_vals)),
         ylim = c(min(cred), max(cred)))
    polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[9,])),
            col = c_light, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[2,], rev(cred[8,])),
            col = c_light_highlight, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[3,], rev(cred[7,])),
            col = c_mid, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[4,], rev(cred[6,])),
            col = c_mid_highlight, border = NA)
    lines(pred_vals, cred[5,], col = c_dark, lwd = 2.5)
    rug(pred_vals_orig,
        lwd = 1.75, ticksize = 0.025, col = c_mid_highlight)

  }

  invisible()
  par(.pardefault)
} else {
  stop('No parametric terms (apart from intercept) found in model')
}
}
