#' Latent variable ordination plots from jsdgam objects
#'
#' Plot an ordination of latent variables and their factor loadings from
#' \code{jsdgam} models
#'
#' @name ordinate.jsdgam
#' @param object \code{list} object of class \code{jsdgam} resulting from a call to [jsdgam()]
#' @param which_lvs A `vector` of indices indicating the two latent variables to be plotted
#' (if number of the latent variables specified in the model was more than 2).
#' Defaults to \code{c(1, 2)}
#' @param biplot  `Logical`. If `TRUE`, both the site and the species
#' scores will be plotted, with names for the taxa interpreted based on the `species`
#' argument in the original call to [jsdgam()]. If `FALSE`, only the site scores
#' will be plotted
#' @param alpha A proportional numeric scalar between `0` and `1` that controls the relative
#' scaling of the latent variables and their loading coefficients
#' @param label_sites \code{Logical} flag. If `TRUE`, site scores will be
#' plotted as labels using names based on the `unit` argument in the original call to [jsdgam()].
#' If `FALSE`, site scores will be shown as points only
#' @param ... ignored
#' @details
#' This function constructs a two-dimensional scatterplot in ordination space. The
#' chosen latent variables are first re-rotated using singular value decomposition,
#' so that the first plotted latent variable does not have to be the first latent variable
#' that was estimated in the original model. Posterior median estimates of the variables
#' and the species' loadings on these variables are then used to construct the resulting plot.
#' Some attempt at de-cluttering the resulting plot is made by using `geom_label_repel()` and
#' `geom_text_repel` from the \pkg{ggrepel} package, but if there are many sites and/or species
#' then some labels may be removed automatically
#' @return An `ggplot` object
#' @author Nicholas J Clark
#' @seealso [jsdgam()], [residual_cor()]
#' @examples
#'\donttest{
#' # Fit a JSDGAM to the portal_data captures
#' mod <- jsdgam(
#'   formula = captures ~ s(ndvi_ma12, by = series, k = 4),
#'   factor_formula = ~ -1,
#'   data = portal_data,
#'   unit = time,
#'   species = series,
#'   family = nb(),
#'   n_lv = 2,
#'   silent = 2,
#'   chains = 2
#' )
#'
#' # Plot an ordination biplot
#' ordinate(
#'   mod,
#'   alpha = 0.7
#' )
#'
#' # Compare to a correlation plot
#' plot(
#'   residual_cor(mod)
#' )
#' }
#'
#' @export
ordinate <- function(object, ...) {
  UseMethod("ordinate", object)
}

#' @rdname ordinate.jsdgam
#' @method ordinate jsdgam
#' @importFrom grid arrow
#' @export
ordinate.jsdgam <- function(
    object,
    which_lvs = c(1, 2),
    biplot = TRUE,
    alpha = 0.5,
    label_sites = TRUE,
    ...
) {

  insight::check_if_installed('ggrepel')

  # Check arguments
  if (length(which_lvs) != 2L) {
    stop("argument 'which_lvs' must be a vector of length 2",
         call. = FALSE)
  }
  if (object$n_lv > 2 & any(which_lvs > object$n_lv)){
    stop("Fewer latent variables available than those chosen by which_lvs",
         call. = FALSE)
  }
  validate_proportional(alpha)

  # Get indices of LV estimates
  ends <- seq(
    0,
    dim(mcmc_chains(object$model_output, 'LV'))[2],
    length.out = object$n_lv + 1
  )
  starts <- ends + 1
  starts <- c(1, starts[-c(1, object$n_lv + 1)])
  ends <- ends[-1]

  # Loop across each lv and calculate median estimates
  lv_estimates <- do.call(
    cbind,
    lapply(1:object$n_lv, function(x) {
      inds_lv <- seq(
        x,
        dim(mcmc_chains(object$model_output, 'LV'))[2],
        by = object$n_lv
      )
      preds <- mcmc_chains(object$model_output, 'LV')[, inds_lv]

      # Keep only the in-sample observations of the factors
      preds <- preds[, 1:(length(object$obs_data$y) / NCOL(object$ytimes))]

      # Calculate posterior medians
      apply(preds, 2, median)

    })
  )

  # Extract loadings, compute the SVD to re-rotate the variables and loadings
  # Credit for much of this code goes to Francis Hui, original author of the BORAL
  # R package (https://github.com/emitanaka/boral)
  lv_estimates <- as.matrix(lv_estimates)
  lv_coefs <- apply(mcmc_chains(object$model_output, 'lv_coefs'),
                    2, median)
  lv_coefs <- t(matrix(lv_coefs, nrow = object$n_lv))
  testcov <- tcrossprod(lv_estimates, lv_coefs)
  do_svd <- svd(testcov, object$n_lv, object$n_lv)
  choose_lvs <- scale(
    do_svd$u *
      matrix(do_svd$d[1:object$n_lv] ^ alpha,
             nrow = NROW(lv_estimates),
             ncol = object$n_lv,
             byrow = TRUE),
    center = TRUE,
    scale = FALSE
  )
  choose_lv_coefs <- scale(
    do_svd$v *
      matrix(do_svd$d[1:object$n_lv] ^ (1 - alpha),
             nrow = NROW(lv_coefs),
             ncol = object$n_lv,
             byrow = TRUE),
    center = TRUE,
    scale = FALSE
  )

  largest_lnorms <- order(
    rowSums(choose_lv_coefs ^ 2),
    decreasing = TRUE
  )[1:NROW(lv_coefs)]

  # Extract site and species loadings into dataframes for plotting
  sp_dat <- data.frame(choose_lv_coefs)[, which_lvs]
  colnames(sp_dat) <- c('x', 'y')

  site_dat <- data.frame(choose_lvs)[, which_lvs]
  colnames(site_dat) <- c('x', 'y')

  plot_dat <- rbind(
    sp_dat,
    site_dat
  )

  # Get taxa names
  sp_names <- object$trend_map$series

  # Get site names
  unit_name <- attr(object$model_data,
                    'prepped_trend_model')$unit
  site_names <- unique(
    object$obs_data[[unit_name]]
  )

  # Create the base ggplot
  base_plot <- ggplot2::ggplot(plot_dat,
                               ggplot2::aes(x, y)) +
    ggplot2::labs(x = paste("Latent variable", which_lvs[1]),
                  y = paste("Latent variable", which_lvs[2]))

  # Add layers accordingly
  if(label_sites) {
    p <- base_plot +
        ggrepel::geom_text_repel(
          data = site_dat,
          aes(label = site_names),
          alpha = 0.75,
          size = 3,
          max.overlaps = 20
        )
  } else {
    p <- base_plot +
      ggplot2::geom_point(
        data = site_dat,
        pch = 21,
        fill = 'black',
        colour = 'white'
      )
  }

  if(biplot) {
    p <- p +
      ggplot2::geom_segment(
        data = sp_dat,
        ggplot2::aes(
          x = 0, y = 0,
          xend = x,
          yend = y
        ),
        arrow = arrow(
          length = unit(0.1, "cm"),
          type = 'closed'
        ),
        alpha = 0.5,
        color = 'darkred') +
      ggrepel::geom_label_repel(
        data = sp_dat,
        ggplot2::aes(label = sp_names),
        color = 'darkred',
        box.padding = 0.1,
        label.size = 0.1,
        alpha = 0.75,
        max.overlaps = 20
      )
  }

  # Return the plot
  p <- p + ggplot2::theme_classic()
  return(p)
}
