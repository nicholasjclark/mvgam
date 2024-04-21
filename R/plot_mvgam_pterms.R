#'Plot mvgam parametric term partial effects
#'
#'This function plots posterior empirical quantiles for partial effects of parametric terms
#'
#'@importFrom graphics layout title rug bxp
#'@importFrom stats coef predict
#'@inheritParams plot.mvgam
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'@details Posterior empirical quantiles of each parametric term's partial effect estimates
#'(on the link scale) are calculated and visualised as ribbon plots. These effects can
#'be interpreted as the partial effect that a parametric term contributes when all other
#'terms in the model have been set to \code{0}
#'@return A base \code{R} graphics plot
#'@export
plot_mvgam_pterms = function(object, trend_effects = FALSE){

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

# Check arguments
if (!(inherits(object, "mvgam"))) {
  stop('argument "object" must be of class "mvgam"')
}

object2 <- object

if(trend_effects){
  if(is.null(object$trend_call)){
    stop('no trend_formula exists so there no trend-level smooths to plot')
  }

  object2$mgcv_model <- object2$trend_mgcv_model
}

# Look for parametric terms in the model
pterms <- attr(object2$mgcv_model$pterms, 'term.labels')

if(length(pterms) > 0){
  # Graphical parameters
  .pardefault <- par(no.readonly=T)
  on.exit(par(.pardefault))

  if(length(pterms) == 1){
    par(mfrow = c(1,1),
        mar = c(4, 4.5, 3, 4))
  }

  if(length(pterms) == 2){
    par(mfrow = c(2, 1),
        mar=c(2.5, 2.3, 2, 2),
        oma = c(1, 1, 0, 0),
        mgp = c(1.5, 0.5, 0))
  }

  if(length(pterms) %in% c(3, 4)){
    par(mfrow = c(2, 2),
        mar=c(2.5, 2.3, 2, 2),
        oma = c(1, 1, 0, 0),
        mgp = c(1.5, 0.5, 0))
  }

  for(i in 1:length(pterms)){
    # Find out which beta corresponds to the associated parametric term
    betas_keep <- grepl(paste0('^(?=.*',pterms[i], ')(?!.*s\\()'),
                        colnames(predict(object2$mgcv_model, type = 'lpmatrix')),
                        perl = TRUE)
    if(trend_effects){
      betas <- mcmc_chains(object2$model_output, 'b_trend')[ ,betas_keep]
    } else {
      betas <- mcmc_chains(object2$model_output, 'b')[ ,betas_keep]
    }

    # Generate linear predictor matrix from fitted mgcv model
    Xp <- obs_Xp_matrix(newdata = object2$obs_data,
                        mgcv_model = object2$mgcv_model)

    # Zero out all other columns in Xp
    Xp[,!betas_keep] <- 0

    # X-axis values
    if(inherits(object2$obs_data, 'list')){
      pred_vals_orig <- sort(object2$obs_data[[pterms[i]]])
    } else {
      pred_vals_orig <- sort(object2$obs_data %>%
                               dplyr::pull(pterms[i]))
    }

    if(inherits(object2$obs_data[[pterms[i]]], 'factor')){

      # Use a simple Boxplot for factor terms for now
      if(is.matrix(betas)){
        beta_creds <- apply(betas, 2, function(x)
          quantile(x, probs = c(0, 0.05, 0.5, 0.95, 1)))
      } else {
        beta_creds <- matrix(quantile(betas, probs = c(0, 0.05, 0.5, 0.95, 1)))
      }

      colnames(beta_creds) <-
        substr(names(coef(object2$mgcv_model))[grepl(paste0('^(?=.*',
                                                           pterms[i], ')(?!.*s\\()'),
                                                    colnames(predict(object2$mgcv_model, type = 'lpmatrix')),
                                                    perl = TRUE)], nchar(pterms[i]) + 1, 1000000L)

      bp <- boxplot(beta_creds, range=0, plot=FALSE)
      bxp(bp, whisklty=0, staplelty=0,
          boxfill = c_light, boxcol = c_light, medcol = c_dark,
          frame.plot = FALSE, ylab = paste0('Partial effect'))

      if(is.matrix(betas)){
        bp$stats <- apply(betas, 2, function(x)
          quantile(x, probs = c(0, 0.3, 0.5, 0.7, 1)))
      } else {
        bp$stats <- matrix(quantile(betas, probs = c(0, 0.3, 0.5, 0.7, 1)))
      }

      bxp(bp, whisklty=0, staplelty=0, add=TRUE, frame.plot = FALSE,
          boxcol = c_light_highlight, medcol = c_dark, boxfill = c_light_highlight)

      if(is.matrix(betas)){
        bp$stats <- apply(betas, 2, function(x)
          quantile(x, probs = c(0, 0.2, 0.5, 0.8, 1)))
      } else {
        bp$stats <- matrix(quantile(betas, probs = c(0, 0.2, 0.5, 0.8, 1)))
      }

      bxp(bp, whisklty=0, staplelty=0, add=TRUE, frame.plot = FALSE,
          boxcol = c_mid, medcol = c_dark, boxfill = c_mid)

      if(is.matrix(betas)){
        bp$stats <- apply(betas, 2, function(x)
          quantile(x, probs = c(0, 0.4, 0.5, 0.6, 1)))
      } else {
        bp$stats <- matrix(quantile(betas, probs = c(0, 0.4, 0.5, 0.6, 1)))
      }

      bxp(bp, whisklty=0, staplelty=0, add=TRUE, frame.plot = FALSE,
          boxcol = c_mid_highlight, medcol = c_dark, boxfill = c_mid_highlight)

      box(bty = 'L', lwd = 2)
      title(pterms[i], adj = 0)

    } else {
      beta_creds <- quantile(betas, probs = probs)
      pred_vals <- seq(min(pred_vals_orig), max(pred_vals_orig), length.out = 500)
      cred <- as.matrix(beta_creds) %*% pred_vals

    # Plot
    plot(1, type = "n", bty = 'L',
         xlab = pterms[i],
         ylab = paste0('Partial effect'),
         xlim = c(min(pred_vals), max(pred_vals)),
         ylim = c(min(cred), max(cred)))
    title(pterms[i], adj = 0)
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
    box(bty = 'L', lwd = 2)

  }
}
  layout(1)
} else {
  message('No parametric terms in model formula')
}
}
