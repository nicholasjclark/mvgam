# Implementation of `posterior_smooths.mvgam` and
# `conditional_smooths.mvgam`. A smooth is read the way the linear
# predictor composer reads every term: its basis from the Stan data
# brms writes for the prediction's frame, its coefficients from the
# fit's draws, both under the suffix of the predictor it belongs to.
# brms numbers the smooth objects of a predictor in order, one per
# level of a `by` factor, and records each term and its levels on
# `attr(Xs, "bylevels")`, which is where a term's objects are read
# from.


#' Compute posterior draws of a smooth term in a fitted
#' \pkg{mvgam} model
#'
#' Returns the posterior draws of a single smooth term evaluated
#' at the supplied prediction grid, matching the shape of
#' [brms::posterior_smooths()]. Supports any smooth declared by
#' `s()` or `t2()` on the observation formula, a distributional
#' parameter's formula, a non-linear parameter's formula or the
#' trend formula, including `by = factor` expansion.
#'
#' @param object A fitted `mvgam` object.
#' @param smooth Character string naming the smooth term as it
#'   appears in the formula (e.g. `"s(x)"`,
#'   `"s(z, by = group)"`, `"t2(x, y)"`). Use
#'   [smooths.mvgam()] to enumerate the available terms.
#' @param newdata Optional `data.frame` to evaluate the smooth
#'   at. `NULL` (the default) uses the training data. It needs to
#'   carry only the smooth's own variables.
#' @param resp The response the smooth belongs to, required on a
#'   model with several.
#' @param dpar,nlpar The distributional or non-linear parameter the
#'   smooth belongs to. Name at most one; the mean is read when both
#'   are `NULL`.
#' @param ndraws,draw_ids Optional posterior-draw subsetting,
#'   matching the [brms::posterior_smooths()] semantics.
#' @param ... Unused; present for S3 / brms-parity.
#'
#' @return A numeric matrix of dimension
#'   \[n_draws x n_grid_points\] containing the posterior draws
#'   of the smooth term's contribution to the linear predictor.
#'
#' @details
#' The smooth's contribution is its unpenalised basis times its
#' fixed coefficients plus each penalised basis times its random
#' coefficients, per draw. The bases come from [brms::standata()]
#' evaluated on `newdata`, and the coefficients from the fit's
#' draws. An offset is not part of any smooth and is left out,
#' where [brms::posterior_smooths()] adds a predictor's offsets to
#' each of its smooths.
#'
#' A term written on both the observation formula and the trend
#' formula is read from the observation formula.
#'
#' @seealso [brms::posterior_smooths()],
#'   [conditional_smooths.mvgam()], [smooths.mvgam()].
#'
#' @author Nicholas J Clark
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Posterior draws of the s(x) smooth at the training x values.
#' sm <- posterior_smooths(mod, smooth = "s(x)")
#' dim(sm)
#' }
#'
#' @method posterior_smooths mvgam
#' @export
posterior_smooths.mvgam <- function(object, smooth, newdata = NULL,
                                     resp = NULL, dpar = NULL, nlpar = NULL,
                                     ndraws = NULL, draw_ids = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_string(smooth)
  checkmate::assert_data_frame(newdata, min.rows = 1L, null.ok = TRUE)
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1L, min.len = 1L,
                               null.ok = TRUE)
  checkmate::assert_string(dpar, null.ok = TRUE)
  checkmate::assert_string(nlpar, null.ok = TRUE)
  if (!is.null(dpar) && !is.null(nlpar)) {
    stop(insight::format_error(
      "Name a distributional parameter or a non-linear parameter, not both."
    ), call. = FALSE)
  }
  resolve_resp(object, resp, required = TRUE, caller = "posterior_smooths()")
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


#' Marginal smooth-effect plots for a fitted \pkg{mvgam} model
#'
#' Compute and display posterior conditional effects for every
#' smooth term in a fitted `mvgam` object, matching the shape of
#' \code{\link[brms]{conditional_smooths.brmsfit}}. Returns a named list of
#' `data.frame`s with the brms-style `estimate__` / `lower__` /
#' `upper__` / `effect1__` / `effect2__` / `cond__` columns. The
#' returned object carries the `mvgam_conditional_smooths` class;
#' one-dimensional smooths render with the package's own theme via
#' [plot.mvgam_conditional_smooths()], while two-dimensional cases
#' are drawn as heatmap panels through the same method.
#'
#' @param x A fitted `mvgam` object.
#' @param smooths Optional character vector restricting which
#'   smooth terms to compute. `NULL` (default) returns every
#'   smooth of every predictor, the trend's included.
#' @param int_conditions Optional named list. Each name is a
#'   covariate; each value either a numeric vector of conditioning
#'   values or a function applied to the original covariate. Used
#'   for non-focal covariates that appear in by-factor faceting.
#' @param prob Numeric in `(0, 1)`. The credible-interval mass
#'   reported via `lower__` / `upper__`. Defaults to `0.95`.
#' @param spaghetti Logical. When `TRUE` (and the focal smooth is
#'   one-dimensional, not a surface), individual posterior draws
#'   are returned as an attribute for spaghetti-style overlays.
#' @param surface Logical. For two-dimensional smooths, `TRUE`
#'   (the default) returns a `resolution`-by-`resolution` grid for
#'   surface / heatmap rendering; `FALSE` returns a faceted line
#'   plot at three values of the second covariate (mean +/- 1 SD).
#'   See `facets` for finer control.
#' @param facets Optional positive integer. When `surface = FALSE`
#'   and the smooth has two or more dimensions, controls how many
#'   facet levels of the non-focal covariate(s) to use (default:
#'   `3`, matching brms). Setting `facets = 5` produces 5 evenly
#'   spaced facets across the non-focal range, an mvgam-only knob
#'   for marginaleffects-style multidimensional displays.
#' @param resolution Positive integer. Grid resolution per
#'   covariate dimension. Defaults to `100`.
#' @param too_far Numeric in \[0, 1\]. For surface smooths, grid
#'   points further than this from the training data (measured in
#'   the unit square) are dropped via
#'   [mgcv::exclude.too.far()]. `0` (default) keeps all points.
#' @param ndraws,draw_ids Optional posterior-draw subsetting.
#' @param ... Unused; present for S3 / brms-parity.
#'
#' @return A named list of class `mvgam_conditional_smooths` with
#'   one element per smooth term. Each element is a `data.frame`
#'   with the brms-style summary columns (`estimate__`, `se__`,
#'   `lower__`, `upper__`, `effect1__` (optionally `effect2__`),
#'   `cond__`) plus the focal covariate(s). Per-element attributes
#'   `response`, `effects`, `surface`, `spaghetti`, and `points`
#'   drive the plot method's layout choices (1D ribbon vs 2D
#'   heatmap panel).
#'
#' @seealso \code{\link[brms]{conditional_smooths.brmsfit}},
#'   [posterior_smooths.mvgam()],
#'   [conditional_effects.mvgam()].
#'
#' @author Nicholas J Clark
#'
#' @method conditional_smooths mvgam
#' @export
conditional_smooths.mvgam <- function(x, smooths = NULL,
                                       int_conditions = NULL,
                                       prob = 0.95, spaghetti = FALSE,
                                       surface = TRUE, facets = 3L,
                                       resolution = 100L, too_far = 0,
                                       ndraws = NULL, draw_ids = NULL,
                                       ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(smooths, null.ok = TRUE)
  checkmate::assert_list(int_conditions, null.ok = TRUE)
  checkmate::assert_number(prob, lower = 0, upper = 1)
  checkmate::assert_logical(spaghetti, len = 1L)
  checkmate::assert_logical(surface, len = 1L)
  checkmate::assert_int(facets, lower = 2L)
  checkmate::assert_int(resolution, lower = 2L)
  checkmate::assert_number(too_far, lower = 0, upper = 1)
  terms_list <- mvgam_smooth_terms(x)
  if (length(terms_list) == 0L) {
    stop(insight::format_error(c(
      "This 'mvgam' fit has no smooth terms.",
      i = paste0(
        "Smooth terms are introduced via 's()' or 't2()' in the ",
        "observation or trend formula."
      )
    )), call. = FALSE)
  }
  if (!is.null(smooths)) {
    keep <- vapply(terms_list, function(t) t$term %in% smooths,
                   logical(1L))
    terms_list <- terms_list[keep]
    if (length(terms_list) == 0L) {
      stop(insight::format_error(
        paste0(
          "None of the requested smooth terms were found. ",
          "Use 'smooths(x)' to list the available terms."
        )
      ), call. = FALSE)
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
  # `mvgam_conditional_smooths` owns its own plot method that
  # renders 1D smooths with the house palette + theme. For 2D /
  # surface cases the plot method reuses brms's plot code by
  # re-classing each per-smooth data.frame internally; no brms
  # class is exposed on the returned object.
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
#' @param plot Logical. If `TRUE` (default), draws each smooth's
#'   ggplot; otherwise returns the list invisibly for post-processing.
#' @param ask Logical. If `TRUE`, prompts before each new plot when
#'   multiple smooths are drawn to the same device.
#' @param ... Ignored.
#'
#' @return Invisibly returns the list of ggplot objects, one per
#'   smooth term in `x`.
#'
#' @rdname conditional_smooths.mvgam
#' @method plot mvgam_conditional_smooths
#' @export
plot.mvgam_conditional_smooths <- function(x, plot = TRUE,
                                            ask = FALSE, ...) {
  if (length(x) == 0L) return(invisible(x))
  # Lock the palette to the mvgam red scheme for the duration of
  # this call, matching plot.mvgam_forecast / plot.mvgam_stability
  # / plot.mvgam_irf. Any user-side bayesplot::color_scheme_set is
  # restored on exit.
  set_color_scheme_local("red")
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


# Internal: split a smooth's panel by condition when it has more
# than one.
#
# A `by`-factor smooth returns one curve per level, stacked in a
# single frame and told apart by `cond__`. Drawn as one group the
# line runs the width of the covariate once per level and returns,
# which comes out as a sawtooth under a ribbon spanning every
# curve's uncertainty at once. brms facets its own
# conditional-effects panels on `cond__`, and the same split here
# keeps the two renderers agreeing about what a condition is.
#
# Returns NULL for a smooth with one condition, which adds nothing
# to the plot.
#' @noRd
smooth_condition_facet <- function(df) {
  if (!"cond__" %in% names(df) || length(unique(df$cond__)) < 2L) {
    return(NULL)
  }
  ggplot2::facet_wrap(ggplot2::vars(.data[["cond__"]]))
}


# Build one mvgam-themed ggplot from a single smooth's summary
# data.frame. A 1D smooth is drawn here as a ribbon and a median
# line, with the optional spaghetti; a 2D surface or faceted lines
# go to brms's own plotter.
#'@noRd
build_mvgam_smooth_plot <- function(df, label) {
  effs <- attr(df, "effects")
  is_surface <- isTRUE(attr(df, "surface"))
  label <- label %||% attr(df, "response") %||% ""
  # 2D or surface smooths: reuse brms's existing plot code by
  # wrapping the single data.frame back into a length-1
  # brms_conditional_effects and delegating. The mvgam theme
  # is applied on top so the panel matches the rest of our
  # dispatcher's output.
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
  # point per level. A ribbon and a line each need two points before
  # they draw anything, and a panel built from them comes out empty
  # however many levels there are. An interval per level has something
  # to show, and it is what brms draws for a categorical conditional
  # effect.
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
  # Median line uses palette slot 5 to match `mvgam_median_layer`
  # (the shared plot_helpers primitive) so smooth panels sit in
  # the same visual family as the forecast / trend hindcast plots.
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


#' Enumerate smooth terms in a fitted \pkg{mvgam} model
#'
#' Returns the canonical term labels for every `s()` or `t2()`
#' smooth of every predictor of a fitted `mvgam` object: each
#' response's mean, each distributional or non-linear parameter
#' given a formula, and the trend. Useful for picking a term to
#' pass to [posterior_smooths.mvgam()] or
#' [conditional_smooths.mvgam()].
#'
#' @param x A fitted `mvgam` object.
#'
#' @return A character vector of canonical smooth labels, one per
#'   term of each predictor, in the order the panels of
#'   [conditional_smooths.mvgam()] take. Empty if the fit has no
#'   smooth terms.
#'
#' @author Nicholas J Clark
#'
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
# Internal helpers
# ------------------------------------------------------------------


#' Every smooth term of a fit, as brms numbers it
#'
#' Each predictor's terms and the `by` levels of each are read from
#' the Stan data brms writes for the fitted frame, which records them
#' on `attr(Xs, "bylevels")`. A term with a `by` factor is one smooth
#' object per level and any other term is one, numbered in order
#' across the predictor's terms.
#'
#' @param x A fitted `mvgam` object or a prefit
#' @return A list with one entry per term of each predictor, observation
#'   side first: `term`, the label as the formula writes it; `side`;
#'   `resp`, `dpar` and `nlpar`, naming the predictor as
#'   `predictor_suffix()` takes them; `objects`, the numbers of its
#'   smooth objects; `covars`, the variables it is a smooth of; and
#'   `by_var`, its `by` variable or `NA`
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
      Xs <- sdata[[paste0("Xs", do.call(predictor_suffix, predictor))]]
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
          paste0("Terms: ", paste(labels, collapse = ", "), "; objects: ",
                 length(attr(Xs, "smcols")), ".")
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
          predictor,
          list(
            objects = seq.int(last[i] - counts[i] + 1L, last[i]),
            covars = specs[[i]]$term,
            by_var = if (identical(by_var, "NA")) NA_character_ else by_var
          )
        )
      }
    }
  }
  out
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
#' has to belong to the predictor named by `dpar` or `nlpar`, the mean
#' when neither is given, and to `resp` on a model with several. A trend
#' shared by every response belongs to each of them. The observation
#' side is searched first.
#'
#' @param x A fitted `mvgam` object
#' @param smooth The label the caller gave
#' @param resp,dpar,nlpar The predictor, as `posterior_smooths()` takes
#'   it
#' @return One entry of `mvgam_smooth_terms()`
#' @noRd
resolve_mvgam_smooth <- function(x, smooth, resp = NULL, dpar = NULL,
                                 nlpar = NULL) {
  terms_list <- mvgam_smooth_terms(x)
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
    i = paste0(
      "Available smooth terms: ",
      if (length(available) > 0L) {
        paste0("'", available, "'", collapse = ", ")
      } else {
        "<none>"
      },
      "."
    )
  )), call. = FALSE)
}


#' The frame one side of the model was fitted on
#'
#' A smooth is drawn over, and evaluated on, the frame its own side
#' carries. The observation side's is the frame the user supplied. The
#' trend side's is the trend grid, which under `by = lv_axis()` holds
#' one row per (time, latent factor) and the `.trend` factor a smooth
#' is split by, a column the observation frame does not have. The grid
#' builder, the evaluator and the rug of observed points all read the
#' frame from here.
#'
#' @param object A fitted `mvgam` object
#' @param side Either `"trend"` or `"obs"`
#' @return The model frame that side was fitted on
#' @noRd
mvgam_side_data <- function(object, side) {
  if (!identical(side, "trend")) {
    return(object$data)
  }
  # A trend model reaches here without a stored frame only on an
  # object assembled by hand, and the user's frame is the closest
  # thing to a trend grid such an object has.
  object$trend_model$data %||% object$data
}


#' Posterior draws of one smooth term's contribution
#'
#' The term is read by `smooth_pred()`, the reader the linear
#' predictor composer sums every smooth with, from the Stan data brms
#' writes for `newdata` and the draws of the term's own side. brms fills
#' every variable the term does not read.
#'
#' @param object A fitted `mvgam` object
#' @param hit One entry of `mvgam_smooth_terms()`
#' @param newdata Data frame to evaluate the term at, or `NULL` for the
#'   frame its side was fitted on
#' @param draw_ids Draw indices to use, or `NULL` for all
#' @return A `[ndraws x nrow(newdata)]` matrix
#' @noRd
mvgam_smooth_eta <- function(object, hit, newdata, draw_ids = NULL) {
  full_draws <- subset_draws_rows(posterior::as_draws_matrix(object$fit),
                                  draw_ids = draw_ids)
  prep <- prepare_linpred_data(
    draws = side_draws(object, full_draws, hit$side),
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


# Build the prediction grid for one smooth term. Mirrors brms's
# conditional_smooths grid logic: 1D smooths sweep the focal
# covariate; 2D smooths get a surface grid by default, or a
# faceted-line layout when `surface = FALSE` (with `facets`
# controlling the facet count); >2D smooths recursively move
# non-focal covariates into the by-variable set so the facet
# strip degrades to a discrete grouping.
#'@noRd
build_smooth_grid <- function(x, hit, surface, facets, resolution,
                                int_conditions, too_far) {
  mf <- mvgam_side_data(x, hit$side)
  covars <- hit$covars
  byvars <- if (is.na(hit$by_var)) character(0L) else hit$by_var
  # Promote excess covariates beyond the first two into facet
  # variables to keep the plot dispatch sensible.
  if (length(covars) > 2L) {
    byvars <- c(covars[-(1:2)], byvars)
    covars <- covars[1:2]
  }
  is_numeric <- vapply(covars, function(cv) {
    is.numeric(mf[[cv]])
  }, logical(1L))
  values <- stats::setNames(vector("list", length(covars)), covars)
  for (i in seq_along(covars)) {
    cv <- covars[i]
    if (!is.null(int_conditions) && cv %in% names(int_conditions)) {
      ic <- int_conditions[[cv]]
      if (is.function(ic)) ic <- ic(mf[[cv]])
      values[[cv]] <- ic
    } else if (is_numeric[i]) {
      if (i == 2L && !isTRUE(surface)) {
        m <- mean(mf[[cv]], na.rm = TRUE)
        s <- stats::sd(mf[[cv]], na.rm = TRUE)
        if (facets <= 1L) {
          values[[cv]] <- m
        } else {
          q <- seq(-1, 1, length.out = facets)
          values[[cv]] <- m + q * s
        }
      } else {
        values[[cv]] <- seq(
          min(mf[[cv]], na.rm = TRUE),
          max(mf[[cv]], na.rm = TRUE),
          length.out = resolution
        )
      }
    } else {
      values[[cv]] <- levels(factor(mf[[cv]]))
    }
  }
  for (cv in byvars) {
    if (!is.null(int_conditions) && cv %in% names(int_conditions)) {
      ic <- int_conditions[[cv]]
      if (is.function(ic)) ic <- ic(mf[[cv]])
      values[[cv]] <- ic
    } else if (is.numeric(mf[[cv]])) {
      m <- mean(mf[[cv]], na.rm = TRUE)
      s <- stats::sd(mf[[cv]], na.rm = TRUE)
      values[[cv]] <- m + seq(-1, 1, length.out = facets) * s
    } else {
      values[[cv]] <- levels(factor(mf[[cv]]))
    }
  }
  grid <- expand.grid(values, stringsAsFactors = FALSE)
  # Drop grid points too far from the training data on a 2D
  # surface (brms-parity).
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
  # The grid holds the term's own variables alone. brms fills every
  # other variable the model reads when the term is evaluated.
  list(
    newdata = grid, cond_data = cond_data,
    effects = covars, surface = isTRUE(surface) &&
      length(covars) == 2L
  )
}


# Build the spaghetti-overlay data.frame brms's plot dispatch
# consumes: one row per (draw, grid_point) with columns
# (effect1__, estimate__, sample__).
#'@noRd
build_spaghetti_data <- function(eta, cond_data, effects) {
  n_draws <- nrow(eta)
  n_grid <- ncol(eta)
  out <- data.frame(
    effect1__ = rep(cond_data[[effects[1L]]], times = n_draws),
    estimate__ = as.numeric(t(eta)),
    sample__ = factor(rep(seq_len(n_draws), each = n_grid))
  )
  out
}


# The training values of a smooth's own variables, which the plot
# method draws as points under the curve. They come from the frame
# the term's side was fitted on, every row of it.
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
