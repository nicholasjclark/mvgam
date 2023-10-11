#' Effect plot as implemented in \pkg{marginaleffects}
#'
#' Convenient way to call marginal or conditional effect plotting functions
#' implemented in the \pkg{marginaleffects} package
#' @importFrom marginaleffects plot_predictions
#' @name plot_effects.mvgam
#' @inheritParams marginaleffects::plot_predictions
#' @return A \code{\link[ggplot2:ggplot]{ggplot}} object
#' that can be further customized using the \pkg{ggplot2} package
#'@export
plot_effects <- function(object, ...){
  UseMethod("plot_effects", object)
}

#' @name plot_effects.mvgam
#' @method plot_effects mvgam
#' @export
plot_effects.mvgam = function(object,
                              condition = NULL,
                              by = NULL,
                              newdata = NULL,
                              type = NULL,
                              conf_level = 0.95,
                              wts = NULL,
                              transform = NULL,
                              points = 0,
                              rug = FALSE,
                              ...){
  # Set colour scheme
  col_scheme <- attr(bayesplot::color_scheme_get(),
                     'scheme_name')
  bayesplot::color_scheme_set('viridis')

  # Generate plot and reset colour scheme
  out_plot <- plot_predictions(model = object,
                               condition = condition,
                               by = by,
                               newdata = newdata,
                               type = type,
                               vcov = NULL,
                               conf_level = conf_level,
                               wts = wts,
                               transform = transform,
                               points = points,
                               rug = rug,
                               gray = FALSE,
                               draw = TRUE,
                               ...) + bayesplot::bayesplot_theme_get()
  color_scheme_set(col_scheme)

  # Return the plot
  return(out_plot)
}
