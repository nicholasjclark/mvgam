#'Plot mvgam random effect terms
#'
#'This function plots posterior empirical quantiles for random effect smooths (bs = re)
#'
#'@importFrom graphics layout title
#'@inheritParams plot.mvgam
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'@details Posterior empirical quantiles of random effect coefficient estimates
#'(on the link scale) are calculated and visualised as ribbon plots.
#'Labels for coefficients are taken from the levels of the original factor variable
#'that was used to specify the smooth in the model's formula
#'@return A base \code{R} graphics plot
#'@export
plot_mvgam_randomeffects = function(object, trend_effects = FALSE){

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

object2 <- object

if(trend_effects){
  if(is.null(object$trend_call)){
    stop('no trend_formula exists so there no trend-level smooths to plot')
  }

  object2$mgcv_model <- object2$trend_mgcv_model
}

# Labels of smooths in formula
smooth_labs <- do.call(rbind, lapply(seq_along(object2$mgcv_model$smooth), function(x){
  data.frame(label = object2$mgcv_model$smooth[[x]]$label,
             class = class(object2$mgcv_model$smooth[[x]])[1])
}))

# Check if any smooths were bs = "re"; if not, return a message
if(any(smooth_labs$class == 'random.effect')){
  re_smooths <- smooth_labs %>%
    dplyr::mutate(smooth_num = dplyr::row_number()) %>%
    dplyr::filter(class == 'random.effect') %>%
    dplyr::pull(label)

  if(trend_effects){
    re_smooths <- gsub('series', 'trend', re_smooths, fixed = TRUE)
  }

  .pardefault <- par(no.readonly=T)
  on.exit(par(.pardefault))

  if(length(re_smooths) == 1){
    par(mfrow = c(1,1))
  }

  if(length(re_smooths) == 2){
    par(mfrow = c(2, 1))
  }

  if(length(re_smooths) %in% c(3, 4)){
    par(mfrow = c(2, 2))
  }

  for(i in 1:length(re_smooths)){
    # Find out which betas correspond to the associated random effect estimates
    (smooth_labs %>%
      dplyr::mutate(smooth_num = dplyr::row_number()) %>%
      dplyr::filter(class == 'random.effect') %>%
      dplyr::pull(smooth_num))[i] -> smooth_number

    betas_keep <- object2$mgcv_model$smooth[[smooth_number]]$first.para:
      object2$mgcv_model$smooth[[smooth_number]]$last.para

    if(trend_effects){
      betas <- mcmc_chains(object2$model_output, 'b_trend')[ ,betas_keep]
    } else {
      betas <- mcmc_chains(object2$model_output, 'b')[ ,betas_keep]
    }

    # Plot the random effect estimates
    beta_creds <- sapply(1:NCOL(betas),
                          function(n) quantile(betas[,n],
                                               probs = probs))
    N <- NCOL(betas)
    x <- 1:N
    idx <- rep(1:N, each = 2)
    repped_x <- rep(x, each = 2)
    x <- sapply(1:length(idx),
                function(k) if(k %% 2 == 0)
                  repped_x[k] + min(diff(x))/2 else
                    repped_x[k] - min(diff(x))/2)

    plot(1, type = "n", bty = 'L',
         ylab = 'Partial effect',
         xlab = '',
         xlim = range(x),
         xaxt = 'n',
         ylim = range(c(as.vector(beta_creds))))
    title(re_smooths[i], adj = 0)
    rect(xleft = x[seq(1, N*2, by = 2)],
         xright = x[seq(2, N*2, by = 2)],
         ytop =  as.numeric(c(beta_creds[9,])),
         ybottom =  as.numeric(c(beta_creds[1,])),
         col = c_light,
         border = 'transparent')
    rect(xleft = x[seq(1, N*2, by = 2)],
         xright = x[seq(2, N*2, by = 2)],
         ytop =  as.numeric(c(beta_creds[8,])),
         ybottom =  as.numeric(c(beta_creds[2,])),
         col = c_light_highlight,
         border = 'transparent')
    rect(xleft = x[seq(1, N*2, by = 2)],
         xright = x[seq(2, N*2, by = 2)],
         ytop =  as.numeric(c(beta_creds[7,])),
         ybottom =  as.numeric(c(beta_creds[3,])),
         col = c_mid,
         border = 'transparent')
    rect(xleft = x[seq(1, N*2, by = 2)],
         xright = x[seq(2, N*2, by = 2)],
         ytop =  as.numeric(c(beta_creds[6,])),
         ybottom =  as.numeric(c(beta_creds[4,])),
         col = c_mid_highlight,
         border = 'transparent')

    for (k in 1:(N)) {
      lines(x = c(x[seq(1, N*2, by = 2)][k],x[seq(2, N*2, by = 2)][k]),
            y = c(beta_creds[5,k], beta_creds[5,k]),
            col = c_dark, lwd = 2)
    }
    box(bty = 'L', lwd = 2)

    # Label x-axis with the factor variable levels
    factor_var_name <- tail(strsplit(gsub('\\)', '',
                                          gsub('s\\(', '',
                                               re_smooths[i])), ',')[[1]], 1)
    if(trend_effects & factor_var_name == 'trend'){
      # Just use trend labels
      axis(side = 1, at = 1:N,
           labels = paste0('trend_', 1:N))
    } else {
      if(inherits(object2$obs_data, 'list')){
        axis(side = 1, at = 1:N,
             labels = levels(object2$obs_data[[factor_var_name]]))
      } else {
        axis(side = 1, at = 1:N,
             labels = levels(object2$obs_data %>%
                               dplyr::pull(factor_var_name)))
      }
    }

  }
  layout(1)

} else {
  message('No random effect smooths (bs = "re") in model formula')
}
}
