# `posterior_smooths.mvgam()` and `conditional_smooths.mvgam()`
# evaluate a smooth as the linear predictor composer evaluates every
# term. The basis comes from the Stan data brms writes for the
# prediction frame. The coefficients come from the draws. Both carry
# the suffix of the smooth's predictor. brms numbers the smooth objects
# of a predictor in order, one per level of a `by` factor. It records
# each term's levels on `attr(Xs, "bylevels")`.


#' Posterior draws of one smooth term
#'
#' Evaluates one smooth at the rows of `newdata` for each posterior
#' draw, in the shape [brms::posterior_smooths()] returns. The smooth
#' can be any `s()` or `t2()` term of the observation formula, a
#' distributional or non-linear parameter's formula, or the trend
#' formula, `by` factors included.
#'
#' @param object A fitted `mvgam` object.
#' @param smooth The smooth term as the formula writes it, such as
#'   `"s(x)"`, `"s(z, by = group)"` or `"t2(x, y)"`. Spacing is ignored.
#'   [smooths.mvgam()] lists the terms.
#' @param newdata A `data.frame` holding the smooth's own variables, or
#'   `NULL` for the training data.
#' @param resp The response the smooth belongs to, required on a
#'   model with several.
#' @param dpar,nlpar The distributional or non-linear parameter the
#'   smooth belongs to. Name at most one. With both `NULL` the smooth
#'   belongs to the mean.
#' @param ndraws,draw_ids The number of posterior draws, or their
#'   indices. Give at most one. `NULL` for both uses every draw.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A numeric matrix with one row per draw and one column per
#'   row of `newdata`: the smooth's contribution to the linear
#'   predictor.
#'
#' @details
#' The smooth's contribution is its unpenalised basis times its
#' fixed coefficients plus each penalised basis times its random
#' coefficients, per draw. The bases come from [brms::standata()]
#' evaluated on `newdata`. The coefficients come from the draws.
#' [brms::posterior_smooths()] adds a predictor's offsets to each of
#' its smooths. This method leaves them out.
#'
#' A term written in both the observation and the trend formula is
#' taken from the observation formula.
#'
#' @seealso [conditional_smooths.mvgam()] summarises every smooth.
#'   [smooths.mvgam()] lists the terms.
#'
#' @author Nicholas J Clark
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Twenty draws of s(x) over a grid of x
#' grid <- data.frame(x = seq(-2, 2, length.out = 50))
#' sm <- posterior_smooths(mod, smooth = "s(x, k = 5)", newdata = grid)
#' draws <- data.frame(x = rep(grid$x, each = 20),
#'                     value = as.vector(sm[1:20, ]),
#'                     draw = rep(1:20, times = 50))
#' library(ggplot2)
#' ggplot(draws, aes(x, value, group = draw)) +
#'   geom_line(colour = "#8F2727", alpha = 0.4) +
#'   labs(y = "s(x)") +
#'   mvgam_theme()
#'
#' # Next, conditional_smooths(mod) summarises every smooth in one call
#' }
#'
#' @family predictions
#' @method posterior_smooths mvgam
#' @export
posterior_smooths.mvgam <- function(object, smooth, newdata = NULL,
                                     resp = NULL, dpar = NULL, nlpar = NULL,
                                     ndraws = NULL, draw_ids = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_string(smooth)
  checkmate::assert_data_frame(newdata, min.rows = 1L, null.ok = TRUE)
  validate_draw_selectors(ndraws, draw_ids)
  checkmate::assert_string(dpar, null.ok = TRUE)
  checkmate::assert_string(nlpar, null.ok = TRUE)
  rlang::check_dots_empty()
  if (!is.null(dpar) && !is.null(nlpar)) {
    stop(insight::format_error(
      "Supply only one of 'dpar' and 'nlpar'."
    ), call. = FALSE)
  }
  resolve_resp(object, resp, required = TRUE,
               caller = "posterior_smooths()")
  hit <- resolve_mvgam_smooth(object, smooth, resp = resp, dpar = dpar,
                              nlpar = nlpar)
  # A count becomes indices here, at the boundary, and nothing below
  # chooses its own draws.
  mvgam_smooth_eta(
    object, hit, newdata,
    draw_ids = resolve_draw_ids(object, ndraws, draw_ids)
  )
}


#' @importFrom brms posterior_smooths
#' @export
brms::posterior_smooths


#' Plots of each smooth term
#'
#' Summarises the posterior of every smooth over a grid of its own
#' variables, in the shape [brms::conditional_smooths()] returns. Its
#' `plot()` method draws a smooth of one variable as a line and ribbon,
#' and a smooth of two as a heatmap.
#'
#' @param x A fitted `mvgam` object.
#' @param smooths The terms to summarise, as [smooths.mvgam()] lists
#'   them, or `NULL` for every smooth of every predictor, the trend's
#'   included.
#' @param int_conditions A named list setting the grid of a variable:
#'   a vector of values, or a function of the variable's training
#'   values, such as [stats::quantile()].
#' @param prob A number in `[0, 1]`: the mass of the interval in
#'   `lower__` and `upper__`.
#' @param spaghetti Logical. `TRUE` keeps the draws of a smooth of one
#'   variable, which the plot draws as one line each.
#' @param surface Logical. `TRUE` evaluates a smooth of two variables
#'   on a `resolution` by `resolution` grid, drawn as a heatmap.
#'   `FALSE` draws lines over the first variable at `facets` values of
#'   the second.
#' @param facets An integer of at least 2: the number of values taken
#'   by the second variable and by a numeric `by` variable. They run
#'   evenly from one standard deviation below the mean to one above.
#' @param resolution An integer of at least 2: the number of grid
#'   points along each variable.
#' @param too_far A number in `[0, 1]`. On a surface, grid points
#'   further than this from the training data, in the unit square, are
#'   dropped by [mgcv::exclude.too.far()]. `0` keeps every point.
#' @param ndraws,draw_ids The number of posterior draws, or their
#'   indices. Give at most one. `NULL` for both uses every draw.
#' @param resp The response whose smooths to summarise, or `NULL` for
#'   every response.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A list of class `mvgam_conditional_smooths`, one data frame
#'   per smooth, named by predictor and term. Each holds its grid with
#'   the columns `estimate__`, `se__`, `lower__`, `upper__`,
#'   `effect1__` and `cond__`. A smooth of two variables adds
#'   `effect2__`.
#'
#' @seealso [posterior_smooths.mvgam()] gives the draws of one smooth.
#'   [conditional_effects.mvgam()] plots covariate effects on the
#'   response.
#'
#' @author Nicholas J Clark
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # The smooth with 30 of its draws
#' cs <- conditional_smooths(mod, spaghetti = TRUE, ndraws = 30)
#' plot(cs)
#' head(cs[[1]])
#'
#' # Next, plot(mod, type = "smooths") draws the same panels in one
#' # figure
#' }
#'
#' @family predictions
#' @method conditional_smooths mvgam
#' @export
conditional_smooths.mvgam <- function(x, smooths = NULL,
                                       int_conditions = NULL,
                                       prob = 0.95, spaghetti = FALSE,
                                       surface = TRUE, facets = 3L,
                                       resolution = 100L, too_far = 0,
                                       ndraws = NULL, draw_ids = NULL,
                                       resp = NULL,
                                       ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(smooths, null.ok = TRUE)
  checkmate::assert_list(int_conditions, null.ok = TRUE)
  checkmate::assert_number(prob, lower = 0, upper = 1)
  checkmate::assert_flag(spaghetti)
  checkmate::assert_flag(surface)
  checkmate::assert_int(facets, lower = 2L)
  checkmate::assert_int(resolution, lower = 2L)
  checkmate::assert_number(too_far, lower = 0, upper = 1)
  validate_draw_selectors(ndraws, draw_ids)
  resolve_resp(x, resp, caller = "conditional_smooths()")
  rlang::check_dots_empty()
  terms_list <- require_smooth_terms(x)
  if (!is.null(resp)) {
    # A univariate fit carries no response on its terms.
    # `resolve_resp()` has already refused a name the model lacks. A
    # term without a response then belongs to the response named.
    keep <- vapply(
      terms_list, function(t) identical(t$resp %||% resp, resp),
      logical(1L)
    )
    terms_list <- terms_list[keep]
    if (length(terms_list) == 0L) {
      stop(insight::format_error(c(
        paste0("Response '", resp, "' has no smooth terms."),
        i = "Use 'smooths(x)' to list the terms this fit has."
      )), call. = FALSE)
    }
  }
  if (!is.null(smooths)) {
    keep <- vapply(terms_list, function(t) t$term %in% smooths,
                   logical(1L))
    terms_list <- terms_list[keep]
    if (length(terms_list) == 0L) {
      stop(insight::format_error(c(
        "'smooths' must name smooth terms of the model.",
        i = "Use 'smooths(x)' to list the available terms."
      )), call. = FALSE)
    }
  }
  probs <- c((1 - prob) / 2, 1 - (1 - prob) / 2)
  out <- vector("list", length(terms_list))
  names(out) <- vapply(terms_list, smooth_panel_name, character(1L))
  # One set of draws for the whole figure. Left as a count, each term
  # in the loop below would subsample on its own and the panels would
  # be drawn from different iterations of the same posterior.
  draw_ids <- resolve_draw_ids(x, ndraws, draw_ids)
  for (i in seq_along(terms_list)) {
    hit <- terms_list[[i]]
    grid_spec <- build_smooth_grid(
      x, hit, surface = surface, facets = facets,
      resolution = resolution, int_conditions = int_conditions,
      too_far = too_far
    )
    eta <- mvgam_smooth_eta(
      x, hit, grid_spec$newdata, draw_ids = draw_ids
    )
    summary_arr <- brms::posterior_summary(
      eta, probs = probs, robust = TRUE
    )
    colnames(summary_arr) <- c("estimate__", "se__",
                                "lower__", "upper__")
    res <- cbind(grid_spec$cond_data, as.data.frame(summary_arr))
    attr(res, "response") <- names(out)[i]
    attr(res, "effects") <- grid_spec$effects
    attr(res, "surface") <- isTRUE(grid_spec$surface)
    attr(res, "spaghetti") <- if (
      isTRUE(spaghetti) && !isTRUE(grid_spec$surface) &&
        length(grid_spec$effects) == 1L
    ) {
      build_spaghetti_data(eta, grid_spec$cond_data,
                            grid_spec$effects)
    } else {
      NULL
    }
    attr(res, "points") <- mvgam_smooth_points(x, hit, grid_spec)
    out[[i]] <- res
  }
  # The plot method draws a smooth of one variable itself. It hands
  # a smooth of two to brms's plot method, under brms's class.
  structure(
    out,
    class = c("mvgam_conditional_smooths", "list"),
    smooths_only = TRUE
  )
}


#' Plot or print a mvgam_conditional_smooths object
#'
#' @param x An object of class `mvgam_conditional_smooths`
#'   returned by [conditional_smooths.mvgam()].
#' @param plot Logical. `TRUE` draws each panel. `FALSE` only returns
#'   them.
#' @param ask Logical. `TRUE` prompts before each panel after the
#'   first.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return The list of `ggplot` panels, one per smooth, invisibly.
#'
#' @rdname conditional_smooths.mvgam
#' @method plot mvgam_conditional_smooths
#' @export
plot.mvgam_conditional_smooths <- function(x, plot = TRUE,
                                            ask = FALSE, ...) {
  rlang::check_dots_empty()
  if (length(x) == 0L) return(invisible(x))
  set_color_scheme_local()
  labels <- names(x) %||% rep_len(NA_character_, length(x))
  ggs <- Map(build_mvgam_smooth_plot, x, labels)
  if (isTRUE(plot)) {
    default_ask <- grDevices::devAskNewPage()
    on.exit(grDevices::devAskNewPage(default_ask))
    grDevices::devAskNewPage(ask = isTRUE(ask))
    for (g in ggs) graphics::plot(g)
  }
  invisible(ggs)
}


#' @rdname conditional_smooths.mvgam
#' @method print mvgam_conditional_smooths
#' @export
print.mvgam_conditional_smooths <- function(x, ...) plot(x, ...)


#' A facet layer splitting a smooth's panel by condition
#'
#' A `by`-factor smooth gives one curve per level in a single frame,
#' told apart by `cond__`. Drawn as one group, the curves join into a
#' sawtooth under one ribbon. brms facets its own conditional-effects
#' panels on `cond__`. The same split here gives both renderers one
#' definition of a condition.
#'
#' @param df Data frame of one smooth's plotting data
#' @return A ggplot2 facet layer, or `NULL` for a smooth with one
#'   condition. Adding `NULL` leaves the plot unchanged.
#' @noRd
smooth_condition_facet <- function(df) {
  checkmate::assert_data_frame(df)
  if (!"cond__" %in% names(df) || length(unique(df$cond__)) < 2L) {
    return(NULL)
  }
  ggplot2::facet_wrap(ggplot2::vars(.data[["cond__"]]))
}


# The panel of one smooth. A smooth of one variable is drawn here as
# a ribbon and a median line, with the optional spaghetti. A surface
# or a smooth drawn as faceted lines goes to brms's plot method.
#'@noRd
build_mvgam_smooth_plot <- function(df, label) {
  effs <- attr(df, "effects")
  is_surface <- isTRUE(attr(df, "surface"))
  label <- label %||% attr(df, "response") %||% ""
  # brms draws a smooth of two variables from a one-element
  # `brms_conditional_effects`. The mvgam theme then matches the panel
  # to the others.
  if (length(effs) != 1L || is_surface) {
    single <- structure(
      list(df),
      class = c("brms_conditional_effects", "list"),
      smooths_only = TRUE
    )
    names(single) <- label
    return(plot(single, plot = FALSE)[[1L]] + mvgam_theme())
  }
  # A 1D smooth takes the active mvgam palette.
  x_var <- effs[[1L]]
  pal <- mvgam_palette()
  # A smooth over a factor, `s(series, bs = "re")` say, has one grid
  # point per level. A ribbon or a line needs two points in a group.
  # Built from them, the panel comes out empty. An interval per level
  # shows each estimate, as brms draws a categorical effect.
  if (!is.numeric(df[[x_var]])) {
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_var]])) +
        ggplot2::geom_pointrange(
          ggplot2::aes(y = estimate__, ymin = lower__, ymax = upper__),
          colour = pal[5L], linewidth = 0.8, size = 0.4
        ) +
        smooth_condition_facet(df) +
        ggplot2::labs(x = x_var, y = label) +
        mvgam_theme()
    )
  }
  gg <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_var]])) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower__, ymax = upper__),
      fill = pal[3L], alpha = 0.5
    )
  # One line per draw, in the columns `build_spaghetti_data()` writes.
  spaghetti_df <- attr(df, "spaghetti")
  if (!is.null(spaghetti_df) && nrow(spaghetti_df) > 0L) {
    gg <- gg + ggplot2::geom_line(
      data = spaghetti_df,
      ggplot2::aes(
        x = effect1__, y = estimate__, group = sample__
      ),
      colour = pal[4L], alpha = 0.15, linewidth = 0.35,
      inherit.aes = FALSE
    )
  }
  # Palette slot 5 is the colour `mvgam_median_layer()` gives the
  # median line of the forecast and trend plots.
  gg +
    ggplot2::geom_line(
      ggplot2::aes(y = estimate__),
      colour = pal[5L], linewidth = 1
    ) +
    smooth_condition_facet(df) +
    ggplot2::labs(x = x_var, y = label) +
    mvgam_theme()
}


#' @importFrom brms conditional_smooths
#' @export
brms::conditional_smooths


#' The smooth terms of a fitted model
#'
#' Lists every `s()` and `t2()` term of every predictor. The
#' predictors are each response's mean, each distributional or
#' non-linear parameter with a formula and the trend.
#' [posterior_smooths.mvgam()] and [conditional_smooths.mvgam()] take
#' these labels.
#'
#' @param x A fitted `mvgam` object.
#'
#' @return A character vector of term labels, one per term of each
#'   predictor, in the order of the panels of
#'   [conditional_smooths.mvgam()]. Empty for a fit without smooths.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                     n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x, k = 5) + s(time, k = 6),
#'              trend_formula = ~ AR(p = 1),
#'              data = simdat$data_train, family = poisson(),
#'              chains = 2, silent = 2)
#'
#' # The labels posterior_smooths() takes
#' smooths(mod)
#'
#' # Next, posterior_smooths(mod, smooth = "s(x, k = 5)") draws one of
#' # them
#' }
#'
#' @author Nicholas J Clark
#'
#' @family posterior summaries
#' @export
smooths.mvgam <- function(x) {
  checkmate::assert_class(x, "mvgam")
  vapply(mvgam_smooth_terms(x), function(t) t$term, character(1L))
}


#' @rdname smooths.mvgam
#' @export
smooths <- function(x) {
  UseMethod("smooths")
}


# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------


#' Every smooth term of a fit, as brms numbers it
#'
#' The Stan data brms writes for the fitted frame records each
#' predictor's terms and their `by` levels on `attr(Xs, "bylevels")`.
#' A term with a `by` factor holds one smooth object per level. Any
#' other term holds one. The objects are numbered in order across the
#' predictor's terms.
#'
#' @param x A fitted `mvgam` object or a prefit
#' @return A list with one entry per term of each predictor, observation
#'   side first: `term`, the label as the formula writes it; `side`;
#'   `resp`, `dpar` and `nlpar`, naming the predictor as
#'   `predictor_suffix()` takes them; `objects`, the numbers of its
#'   smooth objects; `covars`, the variables it is a smooth of;
#'   `by_var`, its `by` variable or `NA`; and `bylevels`, the `by`
#'   level of each object in turn, `NULL` without a `by` factor
#' @noRd
mvgam_smooth_terms <- function(x) {
  checkmate::assert_class(x, "mvgam")
  out <- list()
  for (side in c("obs", "trend")) {
    model <- side_model(x, side)
    if (is.null(model)) {
      next
    }
    sdata <- brms::standata(model, internal = TRUE)
    for (predictor in model_predictors(model$formula)) {
      Xs <- sdata[[paste0("Xs", predictor$suffix)]]
      if (is.null(Xs)) {
        next
      }
      bylevels <- attr(Xs, "bylevels")
      labels <- names(bylevels)
      counts <- vapply(bylevels, function(lv) max(1L, length(lv)),
                       integer(1L))
      if (length(labels) == 0L ||
          sum(counts) != length(attr(Xs, "smcols"))) {
        stop_mvgam_fault(
          "A predictor's smooth terms do not match its smooth objects.",
          paste0("Got terms ", paste(labels, collapse = ", "), " for ",
                 length(attr(Xs, "smcols")), " smooth objects.")
        )
      }
      specs <- mgcv::interpret.gam(
        stats::reformulate(labels, response = ".y")
      )$smooth.spec
      checkmate::assert_list(specs, len = length(labels))
      last <- cumsum(counts)
      for (i in seq_along(labels)) {
        by_var <- specs[[i]]$by
        out[[length(out) + 1L]] <- c(
          list(term = labels[i], side = side),
          predictor[c("resp", "dpar", "nlpar")],
          list(
            objects = seq.int(last[i] - counts[i] + 1L, last[i]),
            covars = specs[[i]]$term,
            by_var = if (identical(by_var, "NA")) NA_character_ else by_var,
            bylevels = if (length(bylevels[[i]])) bylevels[[i]]
          )
        )
      }
    }
  }
  out
}


#' Every smooth term of a fit, or an error for a fit without one
#'
#' @param x A fitted `mvgam` object
#' @return The list `mvgam_smooth_terms()` returns
#' @noRd
require_smooth_terms <- function(x) {
  terms_list <- mvgam_smooth_terms(x)
  if (length(terms_list) == 0L) {
    stop(insight::format_error(c(
      "Smooth summaries require a smooth term in the model.",
      i = "'s()' or 't2()' in either formula adds one."
    )), call. = FALSE)
  }
  terms_list
}


#' The name a smooth's panel and listing carry
#'
#' The predictor, then the term, then whether it belongs to the trend:
#' `mu: s(x)`, `y2_sigma: s(x)` or `mu: s(time) (trend)`.
#'
#' @param hit One entry of `mvgam_smooth_terms()`
#' @return A single string
#' @noRd
smooth_panel_name <- function(hit) {
  predictor <- paste(c(hit$resp, hit$dpar %||% hit$nlpar %||% "mu"),
                     collapse = "_")
  paste0(predictor, ": ", hit$term,
         if (identical(hit$side, "trend")) " (trend)")
}


#' The smooth term a caller named
#'
#' A label matches whatever its spacing, as brms matches it. The term
#' has to belong to the predictor named by `dpar` or `nlpar`, or the
#' mean when neither is given. On a model with several responses it
#' has to belong to `resp`. A trend shared by every response belongs to
#' each of them. The observation side is searched first.
#'
#' @param x A fitted `mvgam` object
#' @param smooth The label the caller gave
#' @param resp,dpar,nlpar The predictor, as `posterior_smooths()` takes
#'   it
#' @return One entry of `mvgam_smooth_terms()`
#' @noRd
resolve_mvgam_smooth <- function(x, smooth, resp = NULL, dpar = NULL,
                                 nlpar = NULL) {
  terms_list <- require_smooth_terms(x)
  norm <- function(s) gsub("[[:space:]]+", "", s)
  dpar <- if (!identical(dpar, "mu")) dpar
  for (hit in terms_list) {
    same_predictor <- identical(hit$dpar, dpar) &&
      identical(hit$nlpar, nlpar) &&
      (is.null(hit$resp) || identical(hit$resp, resp))
    if (same_predictor && identical(norm(hit$term), norm(smooth))) {
      return(hit)
    }
  }
  available <- vapply(terms_list, smooth_panel_name, character(1L))
  stop(insight::format_error(c(
    paste0("Smooth term '", smooth, "' not found in this 'mvgam' fit",
           if (!is.null(dpar %||% nlpar)) {
             paste0(" for '", dpar %||% nlpar, "'")
           }, "."),
    i = paste0("Available smooth terms: ",
               paste0("'", available, "'", collapse = ", "), ".")
  )), call. = FALSE)
}


#' The frame one side of the model was fitted on
#'
#' A smooth is evaluated on the frame of its own side. The observation
#' side's is the frame the user supplied. The trend side's is the trend
#' grid. Under `by = lv_axis()` that grid holds one row per time and
#' latent factor, with the `.trend` factor a smooth is split by. The
#' observation frame lacks that column. The grid builder, the evaluator
#' and the rug of observed points all take the frame from here.
#'
#' @param object A fitted `mvgam` object
#' @param side Either `"trend"` or `"obs"`
#' @return The model frame that side was fitted on
#' @noRd
mvgam_side_data <- function(object, side) {
  if (!identical(side, "trend")) {
    return(object$data)
  }
  # Only an object assembled by hand reaches here with a trend model
  # but no stored trend frame. It takes the user's frame instead.
  object$trend_model$data %||% object$data
}


#' Posterior draws of one smooth term's contribution
#'
#' `smooth_pred()`, which the linear predictor composer sums every
#' smooth with, evaluates the term. It takes the Stan data brms writes
#' for `newdata` and the draws of the term's side. brms fills every
#' variable outside the term.
#'
#' @param object A fitted `mvgam` object
#' @param hit One entry of `mvgam_smooth_terms()`
#' @param newdata Data frame to evaluate the term at, or `NULL` for the
#'   frame its side was fitted on
#' @param draw_ids Draw indices to use, or `NULL` for all
#' @return A `[ndraws x nrow(newdata)]` matrix
#' @noRd
mvgam_smooth_eta <- function(object, hit, newdata, draw_ids = NULL) {
  pars <- side_parameters(object, hit$side)
  full_draws <- subset_draws_rows(
    fit_draws(object$fit, pars),
    draw_ids = draw_ids
  )
  prep <- prepare_linpred_data(
    draws = side_draws(full_draws, pars, hit$side),
    brmsfit = side_model(object, hit$side),
    newdata = newdata %||% mvgam_side_data(object, hit$side),
    re_formula = NA,
    req_vars = setdiff(c(hit$covars, hit$by_var), NA_character_)
  )
  smooth_pred(
    as_plain_matrix(prep$draws), prep$sdata,
    predictor_suffix(hit$resp, hit$dpar, hit$nlpar),
    predictor_nobs(prep, hit$resp),
    objects = hit$objects
  )
}


# The prediction grid of one smooth term, as brms builds it. A smooth
# of one variable sweeps it. A smooth of two takes a surface, or lines
# at `facets` values of the second variable with `surface = FALSE`.
# Variables past the second join the `by` variables as facets.
#'@noRd
build_smooth_grid <- function(x, hit, surface, facets, resolution,
                                int_conditions, too_far) {
  mf <- mvgam_side_data(x, hit$side)
  covars <- hit$covars
  byvars <- if (is.na(hit$by_var)) character(0L) else hit$by_var
  if (length(covars) > 2L) {
    byvars <- c(covars[-(1:2)], byvars)
    covars <- covars[1:2]
  }
  # A variable's grid holds the user's values, an even sweep, the
  # levels of a factor, or `facets` values from one standard deviation
  # below the mean to one above.
  grid_values <- function(cv, sweep) {
    v <- mf[[cv]]
    if (cv %in% names(int_conditions)) {
      ic <- int_conditions[[cv]]
      return(if (is.function(ic)) ic(v) else ic)
    }
    if (!is.numeric(v)) return(levels(factor(v)))
    if (sweep) {
      return(seq(min(v, na.rm = TRUE), max(v, na.rm = TRUE),
                 length.out = resolution))
    }
    mean(v, na.rm = TRUE) +
      seq(-1, 1, length.out = facets) * stats::sd(v, na.rm = TRUE)
  }
  values <- c(
    lapply(stats::setNames(seq_along(covars), covars), function(i) {
      grid_values(covars[i], sweep = i == 1L || surface)
    }),
    lapply(stats::setNames(byvars, byvars), grid_values, sweep = FALSE)
  )
  grid <- expand.grid(values, stringsAsFactors = FALSE)
  # A surface drops the grid points far from the training data
  if (length(covars) == 2L && isTRUE(surface) && too_far > 0) {
    ex <- mgcv::exclude.too.far(
      g1 = grid[[covars[1L]]], g2 = grid[[covars[2L]]],
      d1 = mf[[covars[1L]]], d2 = mf[[covars[2L]]],
      dist = too_far
    )
    grid <- grid[!ex, , drop = FALSE]
  }
  cond_data <- grid
  cond_data$effect1__ <- cond_data[[covars[1L]]]
  if (length(covars) == 2L) {
    if (isTRUE(surface)) {
      cond_data$effect2__ <- cond_data[[covars[2L]]]
    } else {
      lvls <- sort(unique(cond_data[[covars[2L]]]), decreasing = TRUE)
      cond_data$effect2__ <- factor(
        round(cond_data[[covars[2L]]], 3L), levels = round(lvls, 3L)
      )
    }
  }
  if (length(byvars) > 0L) {
    cond_data$cond__ <- factor(do.call(
      paste, c(cond_data[byvars], list(sep = " : "))
    ))
  } else {
    cond_data$cond__ <- factor(1L)
  }
  # The grid holds the term's own variables alone. brms fills the
  # model's other variables when the term is evaluated.
  list(
    newdata = grid, cond_data = cond_data,
    effects = covars, surface = isTRUE(surface) &&
      length(covars) == 2L
  )
}


# The draws `build_mvgam_smooth_plot()` draws as spaghetti: one row
# per draw and grid point, in the columns `effect1__`, `estimate__` and
# `sample__`.
#'@noRd
build_spaghetti_data <- function(eta, cond_data, effects) {
  data.frame(
    effect1__ = rep(cond_data[[effects[1L]]], times = nrow(eta)),
    estimate__ = as.numeric(t(eta)),
    sample__ = factor(rep(seq_len(nrow(eta)), each = ncol(eta)))
  )
}


# The training values of a smooth's own variables, from every row of
# the frame its side was fitted on.
#'@noRd
mvgam_smooth_points <- function(x, hit, grid_spec) {
  mf <- mvgam_side_data(x, hit$side)
  effects <- grid_spec$effects
  if (any(!effects %in% names(mf))) {
    return(data.frame())
  }
  out <- data.frame(mf[, effects, drop = FALSE])
  colnames(out) <- paste0("effect", seq_along(effects), "__")
  out
}
