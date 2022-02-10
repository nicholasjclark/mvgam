#'Plot mvjagam smooth terms
#'
#'This function plots credible intervals for a series-specific smooth term on the scale of the linear predictor
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param smooth either a \code{character} or \code{integer} specifying which smooth term to be plotted
#'@param newdata Optional \code{dataframe} for predicting the smooth, containing at least 'series', 'season' and 'year',
#'in addition to any other variables included in the linear predictor of the original model's \code{formula}.
#'Note that this currently is only supported for plotting univariate smooths
#'@details Smooth functions are shown as empirical quantiles of posterior expectations from the GAM component of the linear
#'predictor across a sequence of 500 values between the variable's \code{min} and \code{max},
#'while holding all other variables either at their means (for numeric varibles) or at the first
#'level (factor variables). At present, only univariate and bivariate smooth plots
#'are allowed, though note that bivariate smooths rely on default behaviour from
#'\code{\link[mgcv]{plot.gam}}. For more nuanced visualisation, supply
#'\code{newdata} just as you would when predicting from a \code{\link[mgcv]{gam}} model
#'@return A base \code{R} graphics plot
#'@seealso \code{\link[mgcv]{plot.gam}}
#'@export
plot_mvgam_smooth = function(object, series = 1, smooth,
                             newdata){

  data_train <- object$obs_data
  smooth_terms <- unique(gsub("[\\(\\)]", "", regmatches(paste(unlist(purrr::map(object$mgcv_model$smooth, 'label')),
                                        collapse = ','),
                                  gregexpr("\\(.*?\\)",
                                           paste(unlist(purrr::map(object$mgcv_model$smooth, 'label')),
                                                 collapse = ',')))[[1]]))

  if(is.character(smooth)){
    if(!smooth %in% smooth_terms){
      stop(smooth, ' not found in smooth terms of object')
    }
    smooth_int <- which(smooth_terms == smooth)
  }

  if(is.numeric(smooth)){
    if(!smooth %in% seq_along(smooth_terms)){
      stop(smooth, ' not found in smooth terms of object')
    }
    smooth_int <- smooth
    smooth <- smooth_terms[smooth]
  }

  if(length(unlist(strsplit(smooth, ','))) > 2){
    stop('Cannot plot smooths of more than 2 dimensions')
  }

  # Be sure that parametric and by variables are included in newdata
  smooth_terms <- unique(c(smooth_terms, attr(object$mgcv_model$pterms, 'term.labels'),
                    trimws(strsplit(gsub('\\+', ',', as.character(object$mgcv_model$pred.formula)[2]), ',')[[1]])))

  # Can't yet plot bivariate smooth effects
  smooth_terms[!grepl(',', smooth_terms)] -> smooth_terms


  # Filter training data to take means of all other smooth terms while providing a prediction
  # sequence for the smooth of interest
  mean_not_fac = function(x){
    if(is.factor(x)){
      x <- levels(x)[1]
    } else {
      x <- mean(x, na.rm = T)
    }
  }
  if(missing(newdata) && class(object$obs_data)[1] != 'list'){
    data_train %>%
      dplyr::select(c(series, year, smooth_terms)) %>%
      dplyr::filter(series == !!(levels(data_train$series)[series])) %>%
      dplyr::mutate_at(c('year', smooth_terms)[c('year', smooth_terms) != smooth], mean_not_fac) %>%
      dplyr::mutate(series = !!(levels(data_train$series)[series])) -> pred_dat

   pred_dat %>% dplyr::select(-smooth) %>% dplyr::distinct() %>%
      dplyr::bind_cols(smooth.var = seq(min(pred_dat[,smooth]),
                                        max(pred_dat[,smooth]),
                                        length.out = 500)) -> pred_dat
     colnames(pred_dat) <- gsub('smooth.var', smooth, colnames(pred_dat))

  } else if(missing(newdata) && class(object$obs_data)[1] == 'list'){
    pred_dat <- object$obs_data
  } else {
    pred_dat <- newdata
  }

  # Generate linear predictor matrix from fitted mgcv model
  Xp <- predict(object$mgcv_model, newdata = pred_dat, type = 'lpmatrix')

  # Extract GAM coefficients
  betas <- MCMCvis::MCMCchains(object$jags_output, 'b')

  # Predictions and plots
  if(length(unlist(strsplit(smooth, ','))) == 2){

    # Use default mgcv plotting for bivariate smooths as it is quicker
    # Extract median beta params for smooths and their covariances
    # so that uncertainty from mgcv plots is reasonably accurate
    V <- cov(MCMCvis::MCMCchains(object$jags_output, 'b'))
    object$mgcv_model$Ve <- V
    object$mgcv_model$Vp <- V
    object$mgcv_model$Vc <- V
    p <- MCMCvis::MCMCsummary(object$jags_output, 'b')[,c(4)]
    coef_names <- names(object$mgcv_model$coefficients)
    names(p) <- coef_names
    object$mgcv_model$coefficients <- p

    suppressWarnings(plot(object$mgcv_model, select = smooth_int,
         scheme = 2,
         main = '', too.far = 0,
         contour.col = c("black"),
         nlevels = 4,
         lwd = 1, labcex = 1,
         seWithMean = T ))


  } else {

  if(class(pred_dat) == 'list'){
    pred_vals <- as.vector(as.matrix(pred_dat[[smooth]]))
  } else{
    pred_vals <- as.vector(as.matrix(pred_dat[,smooth]))
  }

  preds <- matrix(NA, nrow = NROW(betas), ncol = length(pred_vals))
  for(i in 1:NROW(betas)){
    if(class(pred_dat) == 'list'){
      preds[i,] <- (Xp[order(pred_vals),grepl(smooth, names(object$mgcv_model$coefficients))] %*%
                      betas[i,  grepl(smooth, names(object$mgcv_model$coefficients))])
    } else {
      preds[i,] <- (Xp %*% betas[i, ])
    }

  }

  if(class(pred_dat) == 'list'){
    pred_vals <- pred_vals[order(pred_vals)]
  }

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

  plot(1, type = "n",
       xlab = smooth,
       ylab = paste0('s(', smooth, ') for ', unique(pred_dat$series)),
       xlim = c(min(pred_vals), max(pred_vals)),
       ylim = c(min(cred) - sd(preds), max(cred) + sd(preds)))
  polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[9,])),
          col = c_light, border = NA)
  polygon(c(pred_vals, rev(pred_vals)), c(cred[2,], rev(cred[8,])),
          col = c_light_highlight, border = NA)
  polygon(c(pred_vals, rev(pred_vals)), c(cred[3,], rev(cred[7,])),
          col = c_mid, border = NA)
  polygon(c(pred_vals, rev(pred_vals)), c(cred[4,], rev(cred[6,])),
          col = c_mid_highlight, border = NA)
  lines(pred_vals, cred[5,], col = c_dark, lwd = 2.5)

  # Show observed values of the smooth as a rug
  if(class(object$obs_data) == 'list'){
    rug((as.vector(as.matrix(pred_dat[[smooth]])))[which(pred_dat[['series']] ==
                                                            levels(pred_dat[['series']])[series])],
        lwd = 1.75, ticksize = 0.025, col = c_mid_highlight)
  } else {
    rug((as.vector(as.matrix(data_train[,smooth])))[which(data_train$series ==
                                                            levels(data_train$series)[series])],
        lwd = 1.75, ticksize = 0.025, col = c_mid_highlight)
  }
  }

}
