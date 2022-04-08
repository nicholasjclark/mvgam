#'Plot mvjagam smooth terms
#'
#'This function plots credible intervals for a series-specific smooth term on the scale of the linear predictor
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param smooth either a \code{character} or \code{integer} specifying which smooth term to be plotted
#'@param residuals \code{logical}. If \code{TRUE} then posterior quantiles of partial residuals are added
#'to plots of 1-D smooths as a series of ribbon rectangles.
#'Partial residuals for a smooth term are the median Dunn-Smyth residuals that would be obtained by dropping the term
#'concerned from the model, while leaving all other estimates fixed (i.e. the
#'estimates for the term plus the original median Dunn-Smyth residuals). Note that because \code{mvgam} works with
#'Dunn-Smyth residuals and not working residuals, which are used by \code{mgcv}, the magnitudes of
#'partial residuals will be different to what you would expect from \code{\link[mgcv]{plot.gam}}. Interpretation
#'is similar though, as these partial residuals should be evenly scattered
#'around the smooth function if the function is well estimated
#'@param n_resid_bins \code{integer} specifying the number of bins group the covariate into when plotting partial residuals.
#'Setting this argument too high can make for messy plots that are difficult to interpret, while setting it too
#'low will likely mask some potentially useful patterns in the partial residuals. Default is \code{25}
#'@param derivatives \code{logical}. If \code{TRUE}, an additional plot will be returned to show the
#'estimated 1st derivative for the specified smooth (Note, this only works for univariate smooths)
#'@param newdata Optional \code{dataframe} for predicting the smooth, containing at least 'series', 'season' and 'year',
#'in addition to any other variables included in the linear predictor of the original model's \code{formula}.
#'Note that this currently is only supported for plotting univariate smooths
#'@details Smooth functions are shown as empirical quantiles of posterior partial expectations
#' across a sequence of 500 values between the variable's \code{min} and \code{max},
#'while zeroing out effects of all other variables. At present, only univariate and bivariate smooth plots
#'are allowed, though note that bivariate smooths rely on default behaviour from
#'\code{\link[mgcv]{plot.gam}}. For more nuanced visualisation, supply
#'\code{newdata} just as you would when predicting from a \code{\link[mgcv]{gam}} model
#'@return A base \code{R} graphics plot
#'@seealso \code{\link[mgcv]{plot.gam}}
#'@export
plot_mvgam_smooth = function(object, series = 1, smooth,
                             residuals = FALSE,
                             n_resid_bins = 25,
                             derivatives = FALSE,
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

  # Remove comma separated names as these won't match the column names in data
  smooth_terms[!grepl(',', smooth_terms)] -> smooth_terms


  # Predictions and plots for 2-dimensional smooths
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
                          residuals = residuals,
                          scheme = 2,
                          main = '', too.far = 0,
                          contour.col = c("black"),
                          lwd = 1,
                          seWithMean = T ))

  } else {

    # Use posterior predictions to generate univariate smooth plots
    if(missing(newdata) && class(object$obs_data)[1] != 'list'){
      data_train %>%
        dplyr::select(c(series, smooth_terms)) %>%
        dplyr::filter(series == !!(levels(data_train$series)[series])) %>%
        dplyr::mutate(series = !!(levels(data_train$series)[series])) -> pred_dat

      # Use a larger sample size when estimating derivatives so they can be better approximated
      if(derivatives){
        pred_dat %>% dplyr::select(-smooth) %>% dplyr::distinct() %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::bind_cols(smooth.var = seq(min(pred_dat[,smooth]),
                                            max(pred_dat[,smooth]),
                                            length.out = 1000)) -> pred_dat
      } else {
        pred_dat %>% dplyr::select(-smooth) %>% dplyr::distinct() %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::bind_cols(smooth.var = seq(min(pred_dat[,smooth]),
                                            max(pred_dat[,smooth]),
                                            length.out = 500)) -> pred_dat
      }
      colnames(pred_dat) <- gsub('smooth.var', smooth, colnames(pred_dat))

    } else if(missing(newdata) && class(object$obs_data)[1] == 'list'){
      pred_dat <- object$obs_data
    } else {
      pred_dat <- newdata
    }

    # Generate linear predictor matrix from fitted mgcv model
    Xp <- predict(object$mgcv_model, newdata = pred_dat, type = 'lpmatrix')

    # Zero out all other columns in Xp
    Xp[,!grepl(paste0('(', smooth, ')'), colnames(Xp), fixed = T)] <- 0

    # Extract GAM coefficients
    #betas <- t(matrix(coef(object$mgcv_model)))
    betas <- MCMCvis::MCMCchains(object$jags_output, 'b')

  if(class(pred_dat)[1] == 'list'){
    pred_vals <- as.vector(as.matrix(pred_dat[[smooth]]))
  } else{
    pred_vals <- as.vector(as.matrix(pred_dat[,smooth]))
  }

  preds <- matrix(NA, nrow = NROW(betas), ncol = length(pred_vals))
  for(i in 1:NROW(betas)){
    if(class(pred_dat)[1] == 'list'){
      preds[i,] <- (Xp[order(pred_vals),grepl(smooth, names(object$mgcv_model$coefficients))] %*%
                      betas[i,  grepl(smooth, names(object$mgcv_model$coefficients))])
    } else {
      preds[i,] <- (Xp %*% betas[i, ])
    }

  }

  if(class(pred_dat)[1] == 'list'){
    pred_vals <- pred_vals[order(pred_vals)]
  }

  if(residuals){
    # Need to predict from a reduced set that zeroes out all terms apart from the
    # smooth of interest
    Xp2 <- predict(object$mgcv_model, newdata = object$obs_data, type = 'lpmatrix')

    # Zero out all other columns in Xp2
    Xp2[,!grepl(paste0('(', smooth, ')'), colnames(Xp2), fixed = T)] <- 0

    # Find index for the end of training for this series
    end_train <- object$obs_data %>%
      dplyr::filter(series == !!(levels(data_train$series)[series])) %>%
      NROW()

    # Calculate residuals from full prediction set
    all_resids <- object$resids[[series]][1:end_train]

    partial_resids <- matrix(NA, nrow = nrow(betas), ncol = length(all_resids))
    for(i in 1:NROW(betas)){
      partial_resids[i,] <- (Xp2 %*% betas[i, ]) + all_resids
    }
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

  if(derivatives){
    .pardefault <- par(no.readonly=T)
    par(.pardefault)
    par(mfrow = c(2, 1),
        mgp = c(2.5, 1, 0),
        mai = c(0.8, 0.8, 0.4, 0.4))

    if(residuals){
      plot(1, type = "n",
           xlab = smooth,
           ylab = paste0('s(', smooth, ') for ', unique(pred_dat$series)),
           xlim = c(min(pred_vals), max(pred_vals)),
           ylim = c(min(min(partial_resids, min(cred) - sd(preds), na.rm = T)),
                    max(max(partial_resids, max(cred) + sd(preds), na.rm = T))))
    } else {
      plot(1, type = "n",
           xlab = smooth,
           ylab = paste0('s(', smooth, ') for ', unique(pred_dat$series)),
           xlim = c(min(pred_vals), max(pred_vals)),
           ylim = c(min(cred) - sd(preds), max(cred) + sd(preds)))
    }

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
    if(class(object$obs_data)[1] == 'list'){
      rug((as.vector(as.matrix(pred_dat[[smooth]])))[which(pred_dat[['series']] ==
                                                             levels(pred_dat[['series']])[series])],
          lwd = 1.75, ticksize = 0.025, col = c_mid_highlight)
    } else {
      rug((as.vector(as.matrix(data_train[,smooth])))[which(data_train$series ==
                                                              levels(data_train$series)[series])],
          lwd = 1.75, ticksize = 0.025, col = c_mid_highlight)
    }

    # Compute 1st derivatives
    first_derivs <- cbind(rep(NA, NROW(preds)), t(apply(preds, 1, diff)))
    cred <- sapply(1:NCOL(first_derivs),
                   function(n) quantile(first_derivs[,n],
                                        probs = probs, na.rm = T))
    plot(1, type = "n",
         xlab = smooth,
         ylab = '1st derivative',
         xlim = c(min(pred_vals), max(pred_vals)),
         ylim = c(min(cred, na.rm = T) - sd(first_derivs, na.rm = T),
                  max(cred, na.rm = T) + sd(first_derivs, na.rm = T)))
    polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[9,])),
            col = c_light, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[2,], rev(cred[8,])),
            col = c_light_highlight, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[3,], rev(cred[7,])),
            col = c_mid, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[4,], rev(cred[6,])),
            col = c_mid_highlight, border = NA)
    lines(pred_vals, cred[5,], col = c_dark, lwd = 2.5)
    abline(h = 0, lty = 'dashed', col = 'grey70', lwd = 2)

    invisible()
    par(.pardefault)

  } else {
    if(residuals){
      plot(1, type = "n",
           xlab = smooth,
           ylab = paste0('s(', smooth, ') for ', unique(pred_dat$series)),
           xlim = c(min(pred_vals), max(pred_vals)),
           ylim = c(min(min(partial_resids, min(cred) - sd(preds), na.rm = T)),
                    max(max(partial_resids, max(cred) + sd(preds), na.rm = T))))

      # Get x-axis values and bin if necessary to prevent overplotting
      sorted_x <- sort(unique(round(object$obs_data[,smooth], 6)))

      if(length(sorted_x) > n_resid_bins){
        sorted_x <- seq(min(sorted_x), max(sorted_x), length.out = n_resid_bins)
        resid_probs <- do.call(rbind, lapply(2:n_resid_bins, function(i){
          quantile(as.vector(partial_resids[,which(round(object$obs_data[,smooth], 6) <= sorted_x[i] &
                                                     round(object$obs_data[,smooth], 6) > sorted_x[i-1])]),
                   probs = probs)
        }))
        resid_probs <- rbind(quantile(as.vector(partial_resids[,which(round(object$obs_data[,smooth], 6) == sorted_x[1])]),
                                                probs = probs),
                             resid_probs)

      } else {
        resid_probs <- do.call(rbind, lapply(sorted_x, function(i){
          quantile(as.vector(partial_resids[,which(round(object$obs_data[,smooth], 6) == i)]),
                   probs = probs)
        }))
      }


      # Get polygon coordinates and plot
      N <- length(sorted_x)
      idx <- rep(1:N, each = 2)
      repped_x <- rep(sorted_x, each = 2)

      x <- sapply(1:length(idx),
                  function(k) if(k %% 2 == 0)
                    repped_x[k] + min(diff(sorted_x))/2 else
                      repped_x[k] - min(diff(sorted_x))/2)

      rect(xleft = x[seq(1, N*2, by = 2)],
           xright = x[seq(2, N*2, by = 2)],
           ytop =  resid_probs[,9],
           ybottom =  resid_probs[,1],
           col = c_light,
           border = 'transparent')
      rect(xleft = x[seq(1, N*2, by = 2)],
           xright = x[seq(2, N*2, by = 2)],
           ytop =  resid_probs[,8],
           ybottom =  resid_probs[,2],
           col = c_light_highlight,
           border = 'transparent')
      rect(xleft = x[seq(1, N*2, by = 2)],
           xright = x[seq(2, N*2, by = 2)],
           ytop =  resid_probs[,7],
           ybottom =  resid_probs[,3],
           col = c_mid,
           border = 'transparent')
      rect(xleft = x[seq(1, N*2, by = 2)],
           xright = x[seq(2, N*2, by = 2)],
           ytop =  resid_probs[,6],
           ybottom =  resid_probs[,4],
           col = c_mid_highlight,
           border = 'transparent')

      for (k in 1:N) {
        lines(x = c(x[seq(1, N*2, by = 2)][k],x[seq(2, N*2, by = 2)][k]),
              y = c(resid_probs[k,5], resid_probs[k,5]),
              col = c_dark, lwd = 2)
      }

      # Overlay a minimalist version of the estimated smooth function
      polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[9,])),
              col = rgb(red = 0, green = 0, blue = 0, alpha = 30, maxColorValue = 200),
              border = NA)
      lines(pred_vals, cred[5,],
            col = rgb(red = 0, green = 0, blue = 0, alpha = 45, maxColorValue = 200),
            lwd = 3)

    } else {
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
    }

    # Show observed values of the smooth as a rug
    if(class(object$obs_data)[1] == 'list'){
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

}
