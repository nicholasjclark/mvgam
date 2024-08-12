#'Plot mvgam smooth terms
#'
#'This function plots posterior empirical quantiles for a series-specific smooth term
#'
#'@importFrom grDevices hcl.colors
#'@importFrom stats quantile predict
#'@inheritParams plot.mvgam
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
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
#'@param realisations \code{logical}. If \code{TRUE}, posterior realisations are shown as a spaghetti plot,
#'making it easier to visualise the diversity of possible functions. If \code{FALSE}, the default,
#'empirical quantiles of the posterior distribution are shown
#'@param n_realisations \code{integer} specifying the number of posterior realisations to plot, if
#'\code{realisations = TRUE}. Ignored otherwise
#'@param newdata Optional \code{dataframe} for predicting the smooth, containing at least 'series'
#'in addition to any other variables included in the linear predictor of the original model's \code{formula}.
#'Note that this currently is only supported for plotting univariate smooths
#'@details Smooth functions are shown as empirical quantiles (or spaghetti plots) of posterior partial expectations
#' across a sequence of values between the variable's \code{min} and \code{max},
#'while zeroing out effects of all other variables. At present, only univariate and bivariate smooth plots
#'are allowed, though note that bivariate smooths rely on default behaviour from
#'\code{\link[mgcv]{plot.gam}}. `plot_mvgam_smooth` generates posterior predictions from an
#'object of class \code{mvgam}, calculates posterior empirical quantiles and plots them.
#'If `realisations = FALSE`, the returned plot shows 90, 60, 40 and 20 percent posterior
#'quantiles (as ribbons of increasingly darker shades or red) as well as the posterior
#' median (as a dark red line). If `realisations = FALSE`, a set of `n_realisations` posterior
#' draws are shown. For more nuanced visualisation, supply
#'\code{newdata} just as you would when predicting from a \code{\link[mgcv]{gam}} model or use the more flexible \code{\link{conditional_effects.mvgam}}. Alternatively, if you prefer to use partial
#'effect plots in the style of `gratia`, and if you have the `gratia` package installed, you can
#'use `draw.mvgam`. See \code{\link{gratia_mvgam_enhancements}} for details.
#'@return A base \code{R} graphics plot
#'@seealso \code{\link[mgcv]{plot.gam}}, \code{\link{conditional_effects.mvgam}},
#'\code{\link{gratia_mvgam_enhancements}}
#'@author Nicholas J Clark
#'@export
plot_mvgam_smooth = function(object,
                             trend_effects = FALSE,
                             series = 1,
                             smooth,
                             residuals = FALSE,
                             n_resid_bins = 25,
                             realisations = FALSE,
                             n_realisations = 15,
                             derivatives = FALSE,
                             newdata){

  # Check arguments
  if (!(inherits(object, "mvgam"))) {
    stop('argument "object" must be of class "mvgam"')
  }

  object2 <- object

  if(trend_effects){
    if(is.null(object$trend_call)){
      stop('no trend_formula exists so there are no trend-level smooths to plot')
    }

    residuals <- FALSE
    object2$mgcv_model <- object2$trend_mgcv_model
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

  if(series > NCOL(object2$ytimes)){
    stop(paste0('object only contains data / predictions for ',
                NCOL(object2$ytimes), ' series'),
         call. = FALSE)
  }

  if(sign(n_resid_bins) != 1){
    stop('argument "n_resid_bins" must be a positive integer',
         call. = FALSE)
  } else {
    if(n_resid_bins%%1 != 0){
      stop('argument "n_resid_bins" must be a positive integer',
           call. = FALSE)
    }
  }

  if(missing(smooth)){
    smooth <- 1
  }

  # Get smooth term names
  s_name <- levels(object2$obs_data$series)[series]
  data_train <- object2$obs_data
  smooth_terms <- unlist(purrr::map(object2$mgcv_model$smooth, 'label'))

  if(is.character(smooth)){
    if(!grepl('\\(', smooth)){
      smooth <- paste0('s(', smooth, ')')
    }
    if(!smooth %in% smooth_terms){
      stop(smooth, ' not found in smooth terms of object2\nAppropriate names are: ',
           paste(smooth_terms, collapse = ', '))
    }
    smooth_int <- which(smooth_terms == smooth)
  } else {
    smooth_int <- smooth
  }

  # Check whether this type of smooth is even plottable
  if(!object2$mgcv_model$smooth[[smooth_int]]$plot.me){
    stop(paste0('unable to plot ', object2$mgcv_model$smooth[[smooth_int]]$label,
                ' (class = ', attr(object2$mgcv_model$smooth[[smooth_int]], 'class')[1]),
         ')')
  }

  if(is.numeric(smooth)){
    if(!smooth %in% seq_along(smooth_terms)){
      stop(smooth, ' not found in smooth terms of object')
    }
    smooth_int <- smooth
    smooth <- smooth_terms[smooth]
  }

  if(length(unlist(strsplit(smooth, ','))) > 3){
    stop('mvgam cannot yet plot smooths of more than 3 dimensions')
  }

  # Check that this is not a random effect smooth
  smooth_labs <- do.call(rbind, lapply(seq_along(object2$mgcv_model$smooth), function(x){
    data.frame(label = object2$mgcv_model$smooth[[x]]$label,
               class = class(object2$mgcv_model$smooth[[x]])[1])
  }))

  if(smooth_labs$class[smooth_int] == 'random.effect'){
    message('use function "plot_mvgam_randomeffects" to plot "re" bases')
    return(invisible)
  }

  # Be sure that parametric and by variables are included in newdata
  smooth_terms <- unique(trimws(strsplit(gsub('\\+', ',',
                                              as.character(object2$mgcv_model$pred.formula)[2]),
                                         ',')[[1L]]))

  # Remove comma separated names as these won't match the column names in data
  smooth_terms[!grepl(',', smooth_terms)] -> smooth_terms

  # Change smooth name to the covariate that needs a sequence of prediction values
  smooth <- all.vars(parse(text = object2$mgcv_model$smooth[[smooth_int]]$term))

  # Predictions and plots for multi-dimensional smooths
  if(length(unlist(strsplit(smooth, ','))) >= 2L){

    # Use default mgcv plotting for bivariate smooths as it is quicker
    if(inherits(object2$mgcv_model$smooth[[smooth_int]], 'tprs.smooth') |
       inherits(object2$mgcv_model$smooth[[smooth_int]], 't2smooth') |
       inherits(object2$mgcv_model$smooth[[smooth_int]], 'tensor.smooth')){
      suppressWarnings(plot(object2$mgcv_model, select = smooth_int,
                            residuals = residuals,
                            scheme = 2,
                            main = '', too.far = 0,
                            contour.col = 'black',
                            hcolors = hcl.colors(25,
                                                 palette = 'Reds 2'),
                            lwd = 1,
                            seWithMean = TRUE))
      box(col = 'white')
      box(bty = 'l', lwd = 2)
    } else {
      suppressWarnings(plot(object2$mgcv_model, select = smooth_int,
                            residuals = residuals,
                            scheme = 2,
                            main = '', too.far = 0,
                            contour.col = 'black',
                            hcolors = hcl.colors(25,
                                                 palette = 'Reds 2'),
                            lwd = 1,
                            seWithMean = TRUE,
                            ylab = 'Partial effect'))
      box(col = 'white')
      box(bty = 'l', lwd = 2)
    }

    if(trend_effects){
      title(sub('series', 'trend', object2$mgcv_model$smooth[[smooth_int]]$label,
                fixed = TRUE),
            adj = 0)
    } else {
      title(object2$mgcv_model$smooth[[smooth_int]]$label,
            adj = 0)
    }


  } else {

    # Use posterior predictions to generate univariate smooth plots
    if(missing(newdata) && !inherits(data_train, 'list')){
      data_train %>%
        dplyr::select(c(series, smooth_terms)) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::mutate(series = s_name) -> pred_dat

      # Use a larger sample size when estimating derivatives so they can be better approximated
      if(derivatives){
        pred_dat %>% dplyr::select(-smooth) %>% dplyr::distinct() %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::slice(rep(1:dplyr::n(), each = 1000)) %>%
          dplyr::mutate(smooth.var = seq(min(pred_dat[,smooth]),
                                         max(pred_dat[,smooth]),
                                         length.out = 1000)) -> pred_dat
      } else {
        pred_dat %>% dplyr::select(-smooth) %>% dplyr::distinct() %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::slice(rep(1:dplyr::n(), each = 500)) %>%
          dplyr::mutate(smooth.var = seq(min(pred_dat[,smooth]),
                                            max(pred_dat[,smooth]),
                                            length.out = 500)) -> pred_dat
      }
      colnames(pred_dat) <- gsub('smooth.var', smooth, colnames(pred_dat))

    } else if(missing(newdata) && inherits(object2$obs_data, 'list')){
      # Make fake data by zeroing all other terms apart from the selected smooth
      # and the series indicator
      pred_dat <- vector(mode = 'list')
      for(x in 1:length(data_train)){
        if(is.matrix(data_train[[x]])){
          pred_dat[[x]] <- matrix(0, nrow = 500, ncol = NCOL(data_train[[x]]))
        } else {
          pred_dat[[x]] <- rep(0, 500)
        }
      }
      names(pred_dat) <- names(object2$obs_data)
      pred_dat$series <- rep((levels(data_train$series)[series]), 500)

      if(!is.matrix(pred_dat[[smooth]])){
        pred_dat[[smooth]] <- seq(min(data_train[[smooth]]), max(data_train[[smooth]]),
                                  length.out = 500)
      } else {
        pred_dat[[smooth]] <- matrix(seq(min(data_train[[smooth]]), max(data_train[[smooth]]),
                                         length.out = length(pred_dat[[smooth]])),
                                     nrow = nrow(pred_dat[[smooth]]),
                                     ncol = ncol(pred_dat[[smooth]]))
      }

      if('lag' %in% names(pred_dat)){
        pred_dat[['lag']] <- matrix(0:(NCOL(data_train$lag)-1),
                                    nrow(pred_dat$lag), NCOL(data_train$lag),
                                    byrow = TRUE)
      }
    } else {
      pred_dat <- newdata

      # Add series factor variable if missing
      if(class(pred_dat)[1] != 'list'){
        if(!'series' %in% colnames(pred_dat)){
          pred_dat$series <- factor('series1')
        }
      }

      if(class(pred_dat)[1] == 'list'){
        if(!'series' %in% names(pred_dat)){
          pred_dat$series <- factor('series1')
        }
      }
    }

    # Generate linear predictor matrix from fitted mgcv model
    if(trend_effects){
      Xp <- trend_Xp_matrix(newdata = pred_dat,
                            trend_map = object2$trend_map,
                          mgcv_model = object2$trend_mgcv_model)
    } else {
      Xp <- obs_Xp_matrix(newdata = pred_dat,
                          mgcv_model = object2$mgcv_model)
    }

    # Zero out all other columns in Xp
    keeps <- object2$mgcv_model$smooth[[smooth_int]]$first.para:
      object2$mgcv_model$smooth[[smooth_int]]$last.para
    Xp[, ! seq_len(length.out = NCOL(Xp)) %in% keeps] <- 0

    # Prediction x-axis values
    if(class(pred_dat)[1] == 'list'){
      if(is.matrix(pred_dat[[smooth]])){
        pred_vals <- as.vector(as.matrix(pred_dat[[smooth]][,1]))
      } else {
        pred_vals <- as.vector(as.matrix(pred_dat[[smooth]]))
      }
    } else{
      pred_vals <- as.vector(as.matrix(pred_dat[,smooth]))
    }

    # If this term has a by variable, need to use mgcv's plotting utilities
    if(object2$mgcv_model$smooth[[smooth_int]]$by != "NA"){

      # Check if this is a gp() term
      gp_term <- FALSE
      if(!is.null(attr(object2$mgcv_model, 'gp_att_table'))){
        gp_term <- object2$mgcv_model$smooth[[smooth_int]]$gp_term
      }

      if(gp_term){
        object2$mgcv_model$smooth[[smooth_int]]$label <-
          gsub('s(', 'gp(',
               object2$mgcv_model$smooth[[smooth_int]]$label,
               fixed = TRUE)
        # Check if this is a factor by variable
        is_fac <- is.factor(object2$obs_data[[object2$mgcv_model$smooth[[smooth_int]]$by]])

        if(is_fac){
          fac_levels <- levels(object2$obs_data[[object2$mgcv_model$smooth[[smooth_int]]$by]])
          whichlevel <- vector()
          for(i in seq_along(fac_levels)){
            whichlevel[i] <- grepl(fac_levels[i], object2$mgcv_model$smooth[[smooth_int]]$label,
                  fixed = TRUE)
          }

          pred_dat[[object2$mgcv_model$smooth[[smooth_int]]$by]] <-
            rep(fac_levels[whichlevel], length(pred_dat$series))
        }

        if(!is_fac){
          pred_dat[[object2$mgcv_model$smooth[[smooth_int]]$by]] <-
            rep(1, length(pred_dat$series))
        }

        if(trend_effects){
          Xp_term <- trend_Xp_matrix(newdata = pred_dat,
                                trend_map = object2$trend_map,
                                mgcv_model = object2$trend_mgcv_model)
        } else {
          Xp_term <- obs_Xp_matrix(newdata = pred_dat,
                              mgcv_model = object2$mgcv_model)
        }
        Xp[,object2$mgcv_model$smooth[[smooth_int]]$first.para:
             object2$mgcv_model$smooth[[smooth_int]]$last.para] <-
          Xp_term[,object2$mgcv_model$smooth[[smooth_int]]$first.para:
                    object2$mgcv_model$smooth[[smooth_int]]$last.para]

      } else {
        # Deal with by variables in non-gp() smooths
        by <- rep(1,length(pred_vals)); dat <- data.frame(x = pred_vals, by = by)
        names(dat) <- c(object2$mgcv_model$smooth[[smooth_int]]$term,
                        object2$mgcv_model$smooth[[smooth_int]]$by)

        Xp_term <- mgcv::PredictMat(object2$mgcv_model$smooth[[smooth_int]], dat)
        Xp[,object2$mgcv_model$smooth[[smooth_int]]$first.para:
             object2$mgcv_model$smooth[[smooth_int]]$last.para] <- Xp_term
      }

    }

    # Extract GAM coefficients
    if(trend_effects){
      betas <- mcmc_chains(object2$model_output, 'b_trend')
    } else {
      betas <- mcmc_chains(object2$model_output, 'b')
    }

    # Calculate posterior marginal predictions
    preds <- matrix(NA, nrow = NROW(betas), ncol = NROW(Xp))
    for(i in 1:NROW(betas)){
      preds[i,] <- (Xp %*% betas[i, ])
    }

  if(residuals){
    # Need to predict from a reduced set that zeroes out all terms apart from the
    # smooth of interest
    if(trend_effects){
      Xp2 <- trend_Xp_matrix(newdata = object2$obs_data,
                           trend_map = object2$trend_map,
                          mgcv_model = object2$trend_mgcv_model)
    } else {
      Xp2 <- obs_Xp_matrix(newdata = object2$obs_data,
                           mgcv_model = object2$mgcv_model)
    }

    if(!missing(newdata)){
      stop('Partial residual plots not available when using newdata')
    }

    if(object2$mgcv_model$smooth[[smooth_int]]$by != "NA"){
      by <- rep(1,length(object2$obs_data$series))
      dat <- data.frame(x = object2$obs_data[[object2$mgcv_model$smooth[[smooth_int]]$term]],
                        by = by)
      names(dat) <- c(object2$mgcv_model$smooth[[smooth_int]]$term,
                      object2$mgcv_model$smooth[[smooth_int]]$by)

      Xp_term <- mgcv::PredictMat(object2$mgcv_model$smooth[[smooth_int]], dat)
      Xp2[,object2$mgcv_model$smooth[[smooth_int]]$first.para:
            object2$mgcv_model$smooth[[smooth_int]]$last.para] <- Xp_term
    }

    # Find index for the end of training for this series and keep only those training
    # observations for the particular series
    if(class(pred_dat)[1] == 'list'){
      end_train <- length(which(object2$obs_data[['series']] == (levels(data_train$series)[series])))
    } else {
      end_train <- object2$obs_data %>%
        dplyr::filter(series == s_name) %>%
        NROW()
    }

    Xp2 <- Xp2[object2$ytimes[,series][1:end_train], ]

    # # Zero out all other columns in Xp2
    Xp2[,!grepl(paste0('(', smooth, ')'), colnames(Xp), fixed = T)] <- 0

    # Calculate residuals from full prediction set
    all_resids <- object2$resids[[series]][,1:end_train]

    partial_resids <- matrix(NA, nrow = nrow(betas), ncol = NCOL(all_resids))
    for(i in 1:NROW(betas)){
      partial_resids[i,] <- (Xp2 %*% betas[i, ]) + all_resids[i,]
    }
  }

  # Plot quantiles of the smooth function, along with observed values
  # if specified
  probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
  cred <- sapply(1:NCOL(preds),
                 function(n) quantile(preds[,n],
                                      probs = probs,
                                      na.rm = TRUE))

  c_light <- c("#DCBCBC")
  c_light_highlight <- c("#C79999")
  c_mid <- c("#B97C7C")
  c_mid_highlight <- c("#A25050")
  c_dark <- c("#8F2727")
  c_dark_highlight <- c("#7C0000")

  if(derivatives){
    .pardefault <- par(no.readonly=T)
    on.exit(par(.pardefault))
    par(mfrow = c(2, 1))

    if(residuals){
      plot(1, type = "n", bty = 'L',
           xlab = smooth,
           ylab = 'Partial effect',
           xlim = c(min(pred_vals), max(pred_vals)),
           ylim = c(min(min(partial_resids, min(cred) - 0.4 * sd(preds), na.rm = T)),
                    max(max(partial_resids, max(cred) + 0.4 * sd(preds), na.rm = T))))

      if(object2$mgcv_model$smooth[[smooth_int]]$by != "NA"){
        if(trend_effects){
          title(sub('series', 'trend',
                    object2$mgcv_model$smooth[[smooth_int]]$label,
                    fixed = TRUE),
                adj = 0)
        } else {
          title(object2$mgcv_model$smooth[[smooth_int]]$label,
                adj = 0)
        }
      } else {
        if(trend_effects){
          title(paste0('s(', smooth, ')'),
                adj = 0)
        } else {
          title(paste0('s(', smooth, ')'),
                adj = 0)
        }
      }

    } else {
      plot(1, type = "n", bty = 'L',
           xlab = smooth,
           ylab = 'Partial effect',
           xlim = c(min(pred_vals), max(pred_vals)),
           ylim = c(min(cred) - 0.9 * sd(preds),
                    max(cred) + 0.9 * sd(preds)))
      if(object2$mgcv_model$smooth[[smooth_int]]$by != "NA"){
        if(trend_effects){
          title(sub('series', 'trend',
                    object2$mgcv_model$smooth[[smooth_int]]$label,
                    fixed = TRUE),
                adj = 0)
        } else {
          title(object2$mgcv_model$smooth[[smooth_int]]$label,
                adj = 0)
        }
      } else {
        if(trend_effects){
          title(paste0('s(', smooth, ')'),
                adj = 0)
        } else {
          title(paste0('s(', smooth, ')'),
                adj = 0)
        }
      }
    }

    if(realisations){
      for(i in 1:n_realisations){
        index <- sample(1:NROW(preds), 1, replace = TRUE)
        lines(x = pred_vals,
              y = preds[index,],
              col = 'white',
              lwd = 2.5)
        lines(x = pred_vals,
              y = preds[index,],
              col = sample(c("#DCBCBC",
                             "#C79999",
                             "#B97C7C",
                             "#A25050",
                             "#7C0000"), 1),
              lwd = 2.25)
      }
    } else {

      if(residuals){

        # Get x-axis values and bin if necessary to prevent overplotting
        sorted_x <- sort(unique(round(object2$obs_data[[smooth]], 6)))

        s_name <- levels(object2$obs_data$series)[series]
        obs_x <- round(data.frame(series = object2$obs_data$series,
                                  smooth_vals = object2$obs_data[[smooth]]) %>%
                         dplyr::filter(series == s_name) %>%
                         dplyr::pull(smooth_vals), 6)

        if(length(sorted_x) > n_resid_bins){
          sorted_x <- seq(min(sorted_x), max(sorted_x), length.out = n_resid_bins)
          resid_probs <- do.call(rbind, lapply(2:n_resid_bins, function(i){
            quantile(as.vector(partial_resids[,which(obs_x <= sorted_x[i] &
                                                       obs_x > sorted_x[i-1])]),
                     probs = probs,
                     na.rm = TRUE)
          }))
          resid_probs <- rbind(quantile(as.vector(partial_resids[,which(obs_x == sorted_x[1])]),
                                        probs = probs,
                                        na.rm = TRUE),
                               resid_probs)

        } else {
          resid_probs <- do.call(rbind, lapply(sorted_x, function(i){
            quantile(as.vector(partial_resids[,which(obs_x == i)]),
                     probs = probs,
                     na.rm = TRUE)
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
        box(bty = 'L', lwd = 2)

      } else {
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

    }

    box(bty = 'L', lwd = 2)

    # Show observed values of the smooth as a rug
    if(class(object2$obs_data)[1] == 'list'){
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
    plot(1, type = "n", bty = 'L',
         xlab = smooth,
         ylab = '1st derivative',
         xlim = c(min(pred_vals), max(pred_vals)),
         ylim = c(min(cred, na.rm = T) - sd(first_derivs, na.rm = T),
                  max(cred, na.rm = T) + sd(first_derivs, na.rm = T)))

    if(realisations){
      for(i in 1:n_realisations){
        index <- sample(1:NROW(first_derivs), 1, replace = TRUE)
        lines(x = pred_vals,
              y = first_derivs[index,],
              col = 'white',
              lwd = 2.5)
        lines(x = pred_vals,
              y = first_derivs[index,],
              col = sample(c("#DCBCBC",
                             "#C79999",
                             "#B97C7C",
                             "#A25050",
                             "#7C0000"), 1),
              lwd = 2.25)
      }

    } else {
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
    box(bty = 'L', lwd = 2)

    abline(h = 0, lty = 'dashed', lwd = 2)

    invisible()

  } else {
    if(residuals){
      plot(1, type = "n", bty = 'L',
           xlab = smooth,
           ylab = 'Partial effect',
           xlim = c(min(pred_vals), max(pred_vals)),
           ylim = c(min(min(partial_resids, min(cred) - 0.4 * sd(preds), na.rm = T)),
                    max(max(partial_resids, max(cred) + 0.4 * sd(preds), na.rm = T))))
      if(object2$mgcv_model$smooth[[smooth_int]]$by != "NA"){
        if(trend_effects){
          title(sub('series', 'trend',
                    object2$mgcv_model$smooth[[smooth_int]]$label,
                    fixed = TRUE),
                adj = 0)
        } else {
          title(object2$mgcv_model$smooth[[smooth_int]]$label,
                adj = 0)
        }
      } else {
        if(trend_effects){
          title(paste0('s(', smooth, ')'),
                adj = 0)
        } else {
          title(paste0('s(', smooth, ')'),
                adj = 0)
        }
      }

      # Get x-axis values and bin if necessary to prevent overplotting
      sorted_x <- sort(unique(round(object2$obs_data[[smooth]], 6)))

      s_name <- levels(object2$obs_data$series)[series]
      obs_x <- round(data.frame(series = object2$obs_data$series,
                                smooth_vals = object2$obs_data[[smooth]]) %>%
                       dplyr::filter(series == s_name) %>%
                       dplyr::pull(smooth_vals), 6)

      if(length(sorted_x) > n_resid_bins){
        sorted_x <- seq(min(sorted_x), max(sorted_x), length.out = n_resid_bins)
        resid_probs <- do.call(rbind, lapply(2:n_resid_bins, function(i){
          quantile(as.vector(partial_resids[,which(obs_x <= sorted_x[i] &
                                                     obs_x > sorted_x[i-1])]),
                   probs = probs,
                   na.rm = TRUE)
        }))
        resid_probs <- rbind(quantile(as.vector(partial_resids[,which(obs_x == sorted_x[1])]),
                                                probs = probs, na.rm = TRUE),
                             resid_probs)

      } else {
        resid_probs <- do.call(rbind, lapply(sorted_x, function(i){
          quantile(as.vector(partial_resids[,which(obs_x == i)]),
                   probs = probs,
                   na.rm = TRUE)
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
      box(bty = 'L', lwd = 2)

    } else {
      plot(1, type = "n", bty = 'L',
           xlab = smooth,
           ylab = 'Partial effect',
           xlim = c(min(pred_vals), max(pred_vals)),
           ylim = c(min(cred) - 0.9 * sd(preds),
                    max(cred) + 0.9 * sd(preds)))
      if(object2$mgcv_model$smooth[[smooth_int]]$by != "NA"){
        if(trend_effects){
          title(sub('series', 'trend',
                    object2$mgcv_model$smooth[[smooth_int]]$label,
                    fixed = TRUE),
                adj = 0)
        } else {
          title(object2$mgcv_model$smooth[[smooth_int]]$label,
                adj = 0)
        }
      } else {
        if(trend_effects){
          title(paste0('s(', smooth, ')'),
                adj = 0)
        } else {
          title(paste0('s(', smooth, ')'),
                adj = 0)
        }
      }

      if(realisations){
        for(i in 1:n_realisations){
          index <- sample(1:NROW(preds), 1, replace = TRUE)
          lines(x = pred_vals,
                y = preds[index,],
                col = 'white',
                lwd = 2.5)
          lines(x = pred_vals,
                y = preds[index,],
                col = sample(c("#DCBCBC",
                               "#C79999",
                               "#B97C7C",
                               "#A25050",
                               "#7C0000"), 1),
                lwd = 2.25)
        }
      } else {
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
      box(bty = 'L', lwd = 2)

    }

    # Show observed values of the smooth as a rug
    if(class(object2$obs_data)[1] == 'list'){
       rug((as.vector(as.matrix(data_train[[smooth]])))[which(data_train$series ==
                                                              levels(data_train$series)[series])],
          lwd = 1.75, ticksize = 0.025, col = c_mid_highlight)

    } else {
      rug((as.vector(as.matrix(data_train[,smooth])))[which(data_train$series ==
                                                              levels(data_train$series)[series])],
          lwd = 1.75, ticksize = 0.025, col = c_mid_highlight)
    }

  }

  }

}
