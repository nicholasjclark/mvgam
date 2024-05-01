#' Display Conditional Effects of Predictors
#'
#' Display conditional effects of one or more numeric and/or categorical
#' predictors in `mvgam` models, including two-way interaction effects.
#' @importFrom brms conditional_effects
#' @importFrom ggplot2 scale_colour_discrete scale_fill_discrete theme_classic
#' @importFrom marginaleffects plot_predictions
#' @importFrom graphics plot
#' @importFrom grDevices devAskNewPage
#' @inheritParams brms::conditional_effects.brmsfit
#' @inheritParams brms::plot.brms_conditional_effects
#' @param x Object of class `mvgam` or `mvgam_conditional_effects`
#' @param points `Logical`. Indicates if the original data points should be added,
#' but only if `type == 'response'`. Default is `TRUE`.
#' @param rug `Logical`. Indicates if displays tick marks should be plotted on the
#' axes to mark the distribution of raw data, but only if `type == 'response'`.
#'  Default is `TRUE`.
#' @param ask `Logical`. Indicates if the user is prompted before a new page is plotted.
#' Only used if plot is `TRUE`. Default is `FALSE`.
#' @param type `character` specifying the scale of predictions.
#' When this has the value \code{link} (default) the linear predictor is calculated on the link scale.
#' If \code{expected} is used, predictions reflect the expectation of the response (the mean)
#' but ignore uncertainty in the observation process. When \code{response} is used,
#' the predictions take uncertainty in the observation process into account to return
#' predictions on the outcome scale. Two special cases are also allowed:
#' type `latent_N` will return the estimated latent abundances from an N-mixture distribution,
#' while type `detection` will return the estimated detection probability from an N-mixture distribution
#' @param ... other arguments to pass to \code{\link[marginaleffects]{plot_predictions}}
#' @return `conditional_effects` returns an object of class
#' \code{mvgam_conditional_effects} which is a
#'  named list with one slot per effect containing a \code{\link[ggplot2]{ggplot}} object,
#'  which can be further customized using the \pkg{ggplot2} package.
#'  The corresponding `plot` method will draw these plots in the active graphic device
#'
#' @details This function acts as a wrapper to the more
#'   flexible \code{\link[marginaleffects]{plot_predictions}}.
#'   When creating \code{conditional_effects} for a particular predictor
#'   (or interaction of two predictors), one has to choose the values of all
#'   other predictors to condition on. By default, the mean is used for
#'   continuous variables and the reference category is used for factors. Use
#'   \code{\link[marginaleffects]{plot_predictions}} to change these
#'   and create more bespoke conditional effects plots.
#' @name conditional_effects.mvgam
#' @author Nicholas J Clark
#' @seealso \code{\link[marginaleffects]{plot_predictions}}, \code{\link[marginaleffects]{plot_slopes}}
#' @examples
#' \dontrun{
#' # Simulate some data
#' simdat <- sim_mvgam(family = poisson(),
#'                     seasonality = 'hierarchical')
#'
#' # Fit a model
#' mod <- mvgam(y ~ s(season, by = series, k = 5) + year:series,
#'              family = poisson(),
#'              data = simdat$data_train,
#'              chains = 2,
#'              burnin = 300,
#'              samples = 300)
#'
#' # Plot all main effects on the response scale
#' conditional_effects(mod)
#'
#' # Change the prediction interval to 70% using plot_predictions() argument
#' # 'conf_level'
#' conditional_effects(mod, conf_level = 0.7)
#'
#' # Plot all main effects on the link scale
#' conditional_effects(mod, type = 'link')
#'
#' # Works the same for smooth terms, including smooth interactions
#' set.seed(0)
#' dat <- mgcv::gamSim(1, n = 200, scale = 2)
#' mod <- mvgam(y ~ te(x0, x1, k = 5) + s(x2, k = 6) + s(x3, k = 6),
#'             data = dat,
#'             family = gaussian(),
#'             chains = 2,
#'             burnin = 300,
#'             samples = 300)
#' conditional_effects(mod)
#' conditional_effects(mod, conf_level = 0.5, type = 'link')
#' }
#' @export
conditional_effects.mvgam = function(x,
                                     effects = NULL,
                                     type = 'response',
                                     points = TRUE,
                                     rug = TRUE,
                                     ...){

  use_def_effects <- is.null(effects)
  type <- match.arg(type, c('response', 'link',
                            'detection', 'latent_N',
                            'expected'))

  if(type == 'response'){
    if(points){
      points <- 0.5
    } else {
      points <- 0
    }
  } else {
    points <- 0
    rug <- FALSE
  }

  # Can't plot points or rugs with binomial models due to the
  # cbind syntax
  if(rug){
    if(x$family %in% c('binomial', 'beta_binomial')){
      rug <- FALSE
      message('Cannot show observation rug for binomial models')
    }
  }

  if(points){
    if(x$family %in% c('binomial', 'beta_binomial')){
      points <- 0
      message('Cannot show observation points for binomial models')
    }
  }

  if(use_def_effects){
    # Get all term labels in the model
    termlabs <- attr(terms(formula(x),
                           keep.order = TRUE), 'term.labels')
    #termlabs <- unlist(find_predictors(x), use.names = FALSE)
    if(!is.null(x$trend_call)){
      termlabs <- c(termlabs,
                    gsub('trend', 'series',
                         attr(terms(formula(x, trend_effects = TRUE),
                                    keep.order = TRUE), 'term.labels')))
    }

    # Find all possible (up to 2-way) plot conditions
    cond_labs <- purrr::flatten(lapply(termlabs, function(i){
      split_termlabs(i)
    }))
  } else {
    cond_labs <- strsplit(as.character(effects), split = ":")
  }

  if(any(lengths(cond_labs) > 3L)) {
    stop("To display interactions of order higher than 3 ",
         "please use plot_predictions()",
         call. = FALSE)
  }

  if(length(cond_labs) > 0){
    # Make the plot data with plot_predictions
    out <- list()
    for(i in seq_along(cond_labs)){
      if(length(cond_labs[[i]]) == 1){
        out[[i]] <- plot_predictions(x,
                                     condition = cond_labs[[i]],
                                     draw = TRUE,
                                     type = type,
                                     points = points,
                                     rug = rug,
                                     ...) +
          scale_fill_discrete(label = roundlabs) +
          scale_colour_discrete(label = roundlabs) +
          theme_classic()

      }

      if(length(cond_labs[[i]]) == 2){
        out[[i]] <- plot_predictions(x,
                                     condition = c(cond_labs[[i]][1],
                                                   cond_labs[[i]][2]),
                                     draw = TRUE,
                                     type = type,
                                     points = points,
                                     rug = rug,
                                     ...) +
          scale_fill_discrete(label = roundlabs) +
          scale_colour_discrete(label = roundlabs) +
          theme_classic()
      }

      if(length(cond_labs[[i]]) == 3){
        out[[i]] <- plot_predictions(x,
                                     condition = c(cond_labs[[i]][1],
                                                   cond_labs[[i]][2],
                                                   cond_labs[[i]][3]),
                                     draw = TRUE,
                                     type = type,
                                     points = points,
                                     rug = rug,
                                     ...) +
          scale_fill_discrete(label = roundlabs) +
          scale_colour_discrete(label = roundlabs) +
          theme_classic()
      }


    }
  } else {
    out <- NULL
  }

  class(out) <- 'mvgam_conditional_effects'
  return(out)

}

#' @rdname conditional_effects.mvgam
#' @export
plot.mvgam_conditional_effects = function(x,
                                          plot = TRUE,
                                          ask = FALSE,
                                          ...){
  out <- x
  for(i in seq_along(out)){
    if(plot){
      plot(out[[i]])
      if(i == 1){
        devAskNewPage(ask = ask)
      }
    }
  }
  invisible(out)
}

#' A helper function so ggplot2 labels in the legend don't have
#' ridiculous numbers of digits for numeric bins
#' @noRd
decimalplaces <- function(x) {
  x <- as.numeric(x)
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".",
                   fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

#' A helper function so ggplot2 labels in the legend don't have
#' ridiculous numbers of digits for numeric bins
#' @noRd
roundlabs = function(x){
  if(all(suppressWarnings(is.na(as.numeric(x))))){
    out <- x
  } else if(all(sapply(x, decimalplaces) == 0)) {
    out <- x
  }else if(all(sapply(x, decimalplaces) <= 1)) {
    out <- sprintf("%.1f", as.numeric(x))
    } else {
    out <- sprintf("%.4f", as.numeric(x))
  }
  out
}

#' @rdname conditional_effects.mvgam
#' @export
print.mvgam_conditional_effects <- function(x, ...) {
  plot(x, ...)
}

#' @noRd
split_termlabs = function(lab){
  out <- list()
  if(grepl(':', lab, fixed = TRUE)){
    out[[1]] <- strsplit(lab, ':')[[1]]
  } else if(grepl('*', lab, fixed = TRUE)){
    out[[1]] <- strsplit(lab, '\\*')[[1]]
  } else if(grepl('s(', lab, fixed = TRUE) |
            grepl('gp(', lab, fixed = TRUE) |
            grepl('te(', lab, fixed = TRUE) |
            grepl('t2(', lab, fixed = TRUE) |
            grepl('ti(', lab, fixed = TRUE)){
    term_struc <- eval(rlang::parse_expr(lab))
    term_struc$by <- if(term_struc$by == 'NA'){
      NULL
    } else {
      term_struc$by
    }
    if(length(term_struc$term) <= 2){
      out[[1]] <- c(all.vars(parse(text = term_struc$term)),
                    term_struc$by)
    }
    if(length(term_struc$term) == 3){
      out[[1]] <- c(all.vars(parse(text = term_struc$term[1:2])),
                    term_struc$by)
      out[[2]] <- c(all.vars(parse(text = term_struc$term[1, 3])),
                    term_struc$by)
      out[[3]] <- c(all.vars(parse(text = term_struc$term[2, 3])),
                    term_struc$by)
    }

    if(length(term_struc$term) == 4){
      out[[1]] <- c(all.vars(parse(text = term_struc$term[1:2])),
                    term_struc$by)
      out[[2]] <- c(all.vars(parse(text = term_struc$term[1, 3])),
                    term_struc$by)
      out[[3]] <- c(all.vars(parse(text = term_struc$term[1, 4])),
                    term_struc$by)
      out[[4]] <- c(all.vars(parse(text = term_struc$term[2, 3])),
                    term_struc$by)
      out[[5]] <- c(all.vars(parse(text = term_struc$term[2, 4])),
                    term_struc$by)
    }

  } else if(grepl('dynamic(', lab, fixed = TRUE)){
    term_struc <- eval(rlang::parse_expr(lab))
    out[[1]] <- c('time', all.vars(parse(text = term_struc$term[2, 4])))
  } else {
    out[[1]] <- lab
  }

  return(out)
}
