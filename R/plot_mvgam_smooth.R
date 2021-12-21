#'Plot mvjagam smooth terms
#'
#'This function plots credible intervals for a series-specific smooth term on the scale of the linear predictor
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param newdata Optional \code{dataframe} for predicting the smooth, containing at least 'series', 'season' and 'year',
#'in addition to any other variables included in the linear predictor of the original model's \code{formula}
#'@details Smooth functions are shown as the expectation from the GAM component of the linear predictor across
#'a sequence of 100 values between the variable's \code{min} and \code{max}, while holding all other variables either at
#'their means (for numeric varibles) or at the first level (factor variables). At present, only univariate smooth plots
#'are allowed (no smooth interactions can be plotted). For more nuanced visualisation, supply
#'\code{newdata} just as you would when predicting from a \code{\link[mgcv]{gam}} model
#'@return A base \code{R} graphics plot
#'@export
plot_mvgam_smooth = function(object, series = 1, smooth, newdata){

  data_train <- object$obs_data
  smooth_terms <- unique(gsub("[\\(\\)]", "", regmatches(paste(unlist(purrr::map(object$mgcv_model$smooth, 'label')),
                                        collapse = ','),
                                  gregexpr("\\(.*?\\)",
                                           paste(unlist(purrr::map(object$mgcv_model$smooth, 'label')),
                                                 collapse = ',')))[[1]]))

  # Be sure that parametric and by variables are included in newdata
  smooth_terms <- unique(c(smooth_terms, attr(object$mgcv_model$pterms, 'term.labels'),
                    trimws(strsplit(gsub('\\+', ',', as.character(object$mgcv_model$pred.formula)[2]), ',')[[1]])))

  # Can't yet plot bivariate smooth effects
  smooth_terms[!grepl(',', smooth_terms)] -> smooth_terms

  if(!smooth %in% smooth_terms){
    stop(smooth, ' not found in smooth terms of object')
  }

  # Filter training data to take means of all other smooth terms while providing a prediction
  # sequence for the smooth of interest
  mean_not_fac = function(x){
    if(is.factor(x)){
      x <- levels(x)[1]
    } else {
      x <- mean(x, na.rm = T)
    }
  }
  if(missing(newdata)){
    data_train %>%
      dplyr::select(c(series, year, smooth_terms)) %>%
      dplyr::filter(series == !!(levels(data_train$series)[series])) %>%
      dplyr::mutate_at(c('year', smooth_terms)[c('year', smooth_terms) != smooth], mean_not_fac) %>%
      dplyr::mutate(series = !!(levels(data_train$series)[series])) -> pred_dat

   pred_dat%>%dplyr::select(-smooth) %>% dplyr::distinct() %>%
      dplyr::bind_cols(smooth.var = seq(min(pred_dat[,smooth]), max(pred_dat[,smooth]), length.out = 100)) -> pred_dat
     colnames(pred_dat) <- gsub('smooth.var', smooth, colnames(pred_dat))

    #pred_dat[,smooth] <- seq(min(pred_dat[,smooth]), max(pred_dat[,smooth]), length.out = NROW(pred_dat))
  } else {
    pred_dat <- newdata
  }

  # Generate linear predictor matrix from fitted mgcv model
  Xp <- predict(object$mgcv_model, newdata = pred_dat, type = 'lpmatrix')

  # Extract GAM coefficients
  betas <- MCMCvis::MCMCchains(object$jags_output, 'b')
  gam_comps <- MCMCvis::MCMCchains(object$jags_output, 'gam_comp')[,series]

  # Predictions
  pred_vals <- as.vector(as.matrix(pred_dat[,smooth]))
  preds <- matrix(NA, nrow = length(gam_comps), ncol = length(pred_vals))
  for(i in 1:length(gam_comps)){
    preds[i,] <- gam_comps[i] * (Xp %*% betas[i,])
  }

  # Plot 95% and 68% credible intervals
  int <- apply(preds,
               2, hpd, 0.95)
  preds_last <- preds[1,]
  plot(preds_last ~ pred_vals,
       type = 'l', ylim = c(min(int) - sd(preds), max(int) + sd(preds)),
       col = rgb(1,0,0, alpha = 0),
       ylab = paste0('s(', smooth, ') for ', unique(pred_dat$series)),
       xlab = smooth)
  polygon(c(pred_vals, rev(pred_vals)),
          c(int[1,], rev(int[3,])),
          col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)

  int <- apply(preds,
               2, hpd, 0.8)
  polygon(c(pred_vals, rev(pred_vals)),
          c(int[1,], rev(int[3,])),
          col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
  lines(int[2,] ~ pred_vals, col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')

  # Show observed values as a rug
  rug((as.vector(as.matrix(data_train[,smooth])))[which(data_train$series ==
                                                         levels(data_train$series)[series])],
      lwd = 1.75, ticksize = 0.025)
}
