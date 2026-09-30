#' Display conditional effects of predictors for mvgam models
#'
#' Plot the conditional effects of one or more numeric / categorical
#' predictors in fitted `mvgam` models, including up-to-three-way
#' interactions. A thin wrapper around
#' [marginaleffects::plot_predictions()] that auto-enumerates the
#' model's term labels (across both observation and trend submodels)
#' and produces one ggplot per term.
#'
#' @param x A fitted `mvgam` object.
#' @param effects Optional character vector of effects to plot. If
#'   `NULL` (the default), all main effects and interactions in the
#'   formula are detected automatically.
#' @param type Scale of predictions. One of `"response"` (outcome
#'   scale with observation-process noise), `"link"` (link-scale
#'   linear predictor), `"expected"` (E\[Y\]; default), or one of
#'   the family-specific scales handled by [predict.mvgam()]:
#'   `"variance"`, `"latent_state"`, `"detection"`. For closure-unit
#'   families (`occ()`, `nmix()` and variants), `"latent_state"`
#'   returns the family-aware latent state on the response scale
#'   (latent occupancy `psi` for `occ()`, latent abundance `N` for
#'   `nmix()`), which is the natural marginal-effects display for
#'   the ecological quantity of interest separate from detection
#'   probability.
#' @param points Logical or numeric. If `TRUE` (or a non-zero alpha
#'   between 0 and 1), raw observations are overlaid on the plot.
#'   The overlay applies where the drawn quantity shares the
#'   observations' scale, which covers `type = "response"` and
#'   `type = "expected"`.
#' @param rug Logical. If `TRUE`, rug tick marks are drawn along the
#'   axes, on the same scales the points overlay applies to.
#' @param process_error Logical. Passed to `plot_predictions()` /
#'   `get_predict.mvgam`. Defaults to `FALSE`, so the trend
#'   contributes its deterministic submodel and the panel shows the
#'   covariate effect with the latent process held out. Set `TRUE` to
#'   integrate over the trend's dynamics, which widens the band by the
#'   latent state's own spread.
#' @param series Optional control over the `series` factor in
#'   multi-series fits. `NULL` (the default) marginalises over series,
#'   matching brms's behaviour for a grouping factor. `"all"` adds
#'   `series` to each condition tuple so the plot facets by series.
#'   A single integer or character value picks one series and filters
#'   the prediction grid to that series's observations.
#' @inheritParams forecast.mvgam
#' @param ... Additional arguments forwarded to
#'   [marginaleffects::plot_predictions()].
#'
#' @return An object of class `mvgam_conditional_effects`: a named
#'   list with one `ggplot` per effect, drawn by the `plot` and
#'   `print` methods.
#'
#' @seealso
#'   [marginaleffects::plot_predictions()] for the underlying
#'     conditional-effect engine,
#'   [marginaleffects::predictions()] for the same content as a
#'     `data.frame`,
#'   [marginaleffects::avg_slopes()],
#'   [marginaleffects::avg_comparisons()] for marginal-effect tables,
#'   [posterior_epred.mvgam()] for the prediction primitive that
#'     `plot_predictions` calls,
#'   [pp_check.mvgam()] for posterior predictive checks,
#'   \[mvgam_diagnostics\] for parameter-level diagnostics
#'   (`fixef`, `rhat`, `bayes_R2`, ...),
#'   \[mvgam_draws\] for raw draws extraction
#'
#' @references
#' Arel-Bundock, V., Greifer, N. and Heiss, A. (2024). How to
#' interpret statistical models using marginaleffects for R and
#' Python. \emph{Journal of Statistical Software}, 111(9):1-32.
#' \doi{10.18637/jss.v111.i09}
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
#' # Marginal effect of x on the response scale.
#' conditional_effects(mod)
#' }
#'
#' @author Nicholas J Clark
#' @method conditional_effects mvgam
#' @export
#' @importFrom brms conditional_effects
conditional_effects.mvgam <- function(x,
                                      effects = NULL,
                                      type = "expected",
                                      points = FALSE,
                                      rug = FALSE,
                                      process_error = FALSE,
                                      series = NULL,
                                      resp = NULL,
                                      ...) {
  checkmate::assert_class(x, "mvgam")
  require_fitted_model(x, "conditional_effects")
  checkmate::assert_character(effects, null.ok = TRUE)
  checkmate::assert_logical(process_error, len = 1L)
  # `resp` is the per-response selector for multivariate fits
  # (mvbind / mvbrmsformula). NULL on a mv fit fans out across
  # responses and returns a named list of `brms_conditional_effects`
  # objects; a single response name scopes the result. Univariate
  # fits ignore the arg.
  checkmate::assert_string(resp, null.ok = TRUE)

  # Multivariate fan-out: build one effects list per response and
  # return a named list, mirroring residuals.mvgam / pp_check.mvgam.
  # We tag the outer wrapper as `mvgam_conditional_effects` so
  # `plot()` / `print()` dispatch is uniform whether the fit is
  # uni- or multivariate; extraction via
  # `as.data.frame.mvgam_conditional_effects()` also relies on
  # the class marker to recognise the fan-out shape.
  fan <- mv_resp_fan_out(x, resp, class = "mvgam_conditional_effects")
  if (!is.null(fan)) {
    return(fan)
  }
  type <- match.arg(
    type,
    c("response", "link", "expected",
      "variance", "latent_state", "detection")
  )
  # Family-specific types are valid only for the families that
  # register them on `attr(family, "mvgam_predict_types")`. Defer
  # the deeper check to predict.mvgam (which surfaces a typed error
  # listing the family's exposed types), but reject the
  # non-closure-unit case here so the user gets the error early
  # rather than via a generic marginaleffects failure.
  if (type %in% c("latent_state", "detection")) {
    require_closure_unit_predict_type(x$family, type)
  }
  # `series` is polymorphic (NULL / "all" / character / integer) so a
  # single checkmate::assert_* call cannot validate it; the resolver
  # owns the per-branch validation and surfaces typed errors.
  series_mode <- resolve_series_arg(series, x)

  # Observation rugs and overlaid points apply where the drawn
  # quantity shares the observations' scale. The fan-out above leaves
  # one response in scope on a multivariate fit, which supplies the
  # column the points come from.
  obs_scale <- on_observation_scale(type)
  points_alpha <- 0
  if (obs_scale) {
    if (isTRUE(points)) {
      points_alpha <- 0.5
    } else if (is.numeric(points) && length(points) == 1L &&
               points > 0 && points <= 1) {
      points_alpha <- as.numeric(points)
    }
  }
  if (!obs_scale) rug <- FALSE

  cond_labs <- if (is.null(effects)) {
    detect_conditional_effects(x)
  } else {
    lapply(as.character(effects), function(e) strsplit(e, ":")[[1L]])
  }

  if (any(lengths(cond_labs) > 3L)) {
    stop(insight::format_error(c(
      "Effects of order higher than 3 are not supported by conditional_effects.",
      i = cli::format_inline(paste0(
        "Use {.fn marginaleffects::plot_predictions} directly to ",
        "build the higher-order plot."
      ))
    )))
  }

  if (length(cond_labs) == 0L) {
    out <- list()
    class(out) <- "mvgam_conditional_effects"
    return(out)
  }

  # Pre-check the four-way facet case before the dispatch loop:
  # marginaleffects caps `condition` at three variables, and `series`
  # facetting would push a 3-var effect to 4.
  if (identical(series_mode$kind, "all") &&
        any(lengths(cond_labs) >= 3L)) {
    stop(insight::format_error(c(
      "Cannot facet by series when an effect already has three variables.",
      i = "Drop one effect or omit 'series'."
    )))
  }

  # Reject collisions between named arguments mvgam controls and any
  # the user passes through `...`. Without this, R's positional-merge
  # in do.call() silently drops the user's override. Only args that
  # mvgam injects into pp_args but does NOT take as formal parameters
  # are vulnerable here: `type`, `points`, `rug` and `process_error` are
  # already consumed by the signature and cannot reach `...`.
  reserved <- c("condition", "draw", "newdata")
  dot_names <- names(list(...))
  clash <- intersect(dot_names, reserved)
  if (length(clash) > 0L) {
    stop(insight::format_error(c(
      paste0(
        "Cannot pass ",
        paste(shQuote(clash), collapse = ", "),
        " through `...`."
      ),
      i = "conditional_effects() sets these itself."
    )))
  }

  # The collision check above owns the names mvgam sets itself. What
  # remains of `...` reaches `marginaleffects::plot_predictions`,
  # which drops a name it does not read, so a misspelling drew the
  # panel on the default the caller meant to override.
  refuse_unread_dots(
    list(...),
    names(formals(marginaleffects::plot_predictions)),
    "conditional_effects"
  )

  # marginaleffects validates `type` against its shipped mvgam
  # `type_dictionary`, which names the latent state `latent_N`.
  # mvgam's user-facing token is `latent_state`; translate it to the
  # wire token so the upstream check passes. get_predict.mvgam maps
  # `latent_N` back to predict(type = "latent_state").
  wire_type <- if (identical(type, "latent_state")) "latent_N" else type
  # marginaleffects warns on each argument its per-class list omits,
  # and the list has no entry for mvgam. The response and the trend
  # setting go to get_predict.mvgam on this copy of the model. The
  # response makes the multivariate draws a single matrix.
  model <- x
  attr(model, "mvgam_predict_args") <- list(
    resp = resp, process_error = process_error
  )
  out <- lapply(cond_labs, function(cond) {
    pp_args <- list(
      condition = cond,
      draw = TRUE,
      type = wire_type,
      points = points_alpha,
      rug = rug
    )
    if (identical(series_mode$kind, "all")) {
      # marginaleffects validates `condition` against the model's own
      # variables. A column added to the frame here never reaches it.
      # A fit deriving its series from `gr` and `subgr` has no such
      # variable, and the refusal names that.
      series_var <- axis_vars(x)$series_var
      if (!series_var %in% names(x$data)) {
        stop(insight::format_error(c(
          "Faceting by series needs a series column in the data.",
          x = "The fit derives its series from the trend grouping.",
          i = paste0(
            "Choose one series with 'series = <name>', or add a '",
            series_var, "' column to the data."
          )
        )), call. = FALSE)
      }
      pp_args$condition <- c(cond, series_var)
    } else if (identical(series_mode$kind, "one")) {
      # Restrict the prediction grid to one series's observations.
      rows <- axis_row_series(x, x$data) %||% x$data$series
      pp_args$newdata <- x$data[
        !is.na(rows) & rows == series_mode$level, , drop = FALSE
      ]
    }
    p <- style_effect_panel(
      do.call(marginaleffects::plot_predictions,
              c(list(model), pp_args, list(...))),
      pp_args$condition, axis_vars(x)$series_var
    )
    # marginaleffects defaults the y-axis label to the model's
    # first response name. For multi-response fits we know which
    # arm we are plotting (`resp`); overwrite so the user sees
    # the correct response on the y-axis.
    if (!is.null(resp)) {
      p <- p + ggplot2::labs(y = resp)
      # The overlay inside `plot_predictions()` takes the
      # observation column from `insight::find_response()`, which
      # names every response on a wide fit. One column is what it
      # needs. `resp` names that one, and the layer goes on here
      # against the frame the panel was given.
      if (points_alpha > 0) {
        pts <- pp_args$newdata %||% x$data
        p <- p + ggplot2::geom_point(
          data = pts,
          mapping = ggplot2::aes(
            x = .data[[cond[1L]]],
            y = .data[[response_column(x, resp)]]
          ),
          alpha = points_alpha,
          inherit.aes = FALSE
        )
      }
    }
    p
  })
  # User-visible list names hide the internal `series` / `.trend`
  # rewrite tokens that `detect_and_rewrite_by_lv()` swaps in for the
  # original `by = lv_axis()` argument. The tokens stay in the
  # `condition` passed to marginaleffects above so per-series facets
  # still render. `mvgam_had_by_lv()` is the single accessor for the
  # display-only marker persisted on `trend_metadata`;
  # `strip_by_lv_rewrite_tokens()` does the strip with the
  # empty-grouping guard.
  display_labs <- strip_by_lv_rewrite_tokens(
    cond_labs, mvgam_had_by_lv(x)
  )
  names(out) <- vapply(display_labs, paste, FUN.VALUE = character(1L),
                       collapse = ":")
  class(out) <- "mvgam_conditional_effects"
  out
}


#' Plot or print a mvgam_conditional_effects object
#' @param x An object of class `mvgam_conditional_effects`.
#' @param plot Logical. If `TRUE` (default), draws each plot;
#'   otherwise returns the list invisibly so callers can post-process.
#' @param ask Logical. If `TRUE`, prompts before each new plot.
#' @param ... Ignored.
#' @return Invisibly, a flat named list of ggplot objects, as
#'   `brms::conditional_effects()` plots return one. A multivariate
#'   fit's panels are named `<response>.<effect>`, response by
#'   response.
#' @rdname conditional_effects.mvgam
#' @export
plot.mvgam_conditional_effects <- function(x, plot = TRUE, ask = FALSE,
                                           ...) {
  plots <- conditional_effects_panels(x)
  if (isTRUE(plot) && length(plots) > 0L) {
    default_ask <- grDevices::devAskNewPage()
    on.exit(grDevices::devAskNewPage(default_ask))
    grDevices::devAskNewPage(ask = isTRUE(ask))
    for (p in plots) graphics::plot(p)
  }
  invisible(plots)
}


# Internal: the panels of a conditional-effects object as one list.
# A multivariate fit holds one set of panels per response, and each
# panel's name takes its response as a prefix.
#'@noRd
conditional_effects_panels <- function(x) {
  if (!isTRUE(attr(x, "mv_wrapper"))) {
    return(unclass(x))
  }
  panels <- lapply(names(x), function(r) {
    p <- conditional_effects_panels(x[[r]])
    stats::setNames(p, paste0(r, ".", names(p)))
  })
  do.call(c, panels)
}


#' @rdname conditional_effects.mvgam
#' @export
print.mvgam_conditional_effects <- function(x, ...) plot(x, ...)


#' @rdname conditional_effects.mvgam
#' @param row.names Ignored, present for S3 signature compatibility.
#' @param optional Ignored, present for S3 signature compatibility.
#' @description
#' `as.data.frame.mvgam_conditional_effects()` returns the underlying
#' prediction grid as a long-format `data.frame` (one row per grid
#' point, per effect, per response). Columns are `resp` (present only
#' when the fit is multivariate), `effect` (the primary conditioning
#' variable name), `estimate__`, `lower__`, `upper__`, followed by
#' whichever additional grid columns marginaleffects populated. Those
#' three are brms's names for them, and [conditional_smooths.mvgam()]
#' answers with the same three, so one frame can be read the way the
#' other is.
#'
#' Use this to reach the raw numbers when you want to build a custom
#' plot, overlay several fits, or compare against a known truth. It
#' removes the need to know whether the fit was univariate or
#' multivariate before extracting.
#' @export
#' @method as.data.frame mvgam_conditional_effects
as.data.frame.mvgam_conditional_effects <- function(x,
                                                    row.names = NULL,
                                                    optional = FALSE,
                                                    ...) {
  rlang::check_dots_empty()
  if (length(x) == 0L) return(data.frame())
  # For each entry, build a per-piece DF tagged with a label
  # column, then rbind under a shared column union. mv wrapper and
  # univariate shape only differ in what label they contribute
  # (`resp` vs `effect`) and how they resolve the entry's raw DF.
  is_mv <- isTRUE(attr(x, "mv_wrapper"))
  label_col <- if (is_mv) "resp" else "effect"
  labels <- names(x) %||% as.character(seq_along(x))
  entry_df <- function(entry) {
    if (is_mv) {
      as.data.frame.mvgam_conditional_effects(entry)
    } else {
      d <- entry$data
      if (is.null(d) || nrow(d) == 0L) return(data.frame())
      d$rowid <- NULL
      # brms names the estimate and its interval with a trailing
      # underscore in both drawn views, and
      # `conditional_smooths.mvgam()` already answers that way. The
      # ggplot itself keeps marginaleffects' own names, which its
      # layers map to aesthetics.
      spellings <- c(estimate = "estimate__", conf.low = "lower__",
                     conf.high = "upper__")
      hit <- intersect(names(spellings), names(d))
      names(d)[match(hit, names(d))] <- spellings[hit]
      d
    }
  }
  dfs <- Map(function(entry, lab) {
    d <- entry_df(entry)
    if (nrow(d) == 0L) return(d)
    cbind(setNames(list(lab), label_col), d)
  }, x, labels)
  dfs <- Filter(function(d) nrow(d) > 0L, dfs)
  if (length(dfs) == 0L) return(data.frame())
  # Union column sets so effects / responses with heterogeneous
  # grid columns still rbind cleanly.
  all_cols <- unique(unlist(lapply(dfs, colnames)))
  dfs <- lapply(dfs, function(d) {
    for (m in setdiff(all_cols, colnames(d))) d[[m]] <- NA
    d[, all_cols, drop = FALSE]
  })
  out <- do.call(rbind, dfs)
  rownames(out) <- NULL
  out
}


# Enumerate the conditional-effects term labels for an mvgam fit:
# walks the observation formula plus the trend formula (when
# present), splits smooth / interaction terms, and returns a list of
# unique up-to-3-way variable groupings.
#
# An offset needs no exclusion here: `stats::terms()` records it
# under the "offset" attribute rather than in "term.labels", so it
# never reaches this list, which is also how brms arrives at the
# same set. Its value still enters the prediction, held at the
# reference the conditioning grid gives it.
detect_conditional_effects <- function(x) {
  mvgam_term_list(x)$groupings
}


# Split a single formula term label into a list of variable
# groupings suitable for plot_predictions(condition = ...). Handles
# `:` / `*` interactions, smooth wrappers (`s`, `te`, `t2`, `ti`,
# `gp`, `mo`) and bare terms. For three-variable smooths returns the
# three pairwise marginal groupings.
split_term_labels <- function(lab) {
  if (grepl(":", lab, fixed = TRUE)) {
    return(list(strsplit(lab, ":", fixed = TRUE)[[1L]]))
  }
  # A label `terms()` wrote always parses.
  expr <- rlang::parse_expr(lab)
  # A `*` reaching here is part of an expression, not a separator
  # between two effects. `terms()` expands a genuine `a * b` into its
  # main effects and `a:b` before this is called. The only labels
  # still carrying one are calls such as `exp(b2 * x)`, which name a
  # single covariate. Splitting those on the text gave `exp(b2 ` and
  # ` x)`, neither of which is a column: the term was dropped and the
  # model offered no effects at all. Anything else falls through to
  # `all.vars()` below, which reads `x` out of the call.
  if (is.call(expr) && identical(expr[[1L]], as.name("*"))) {
    return(list(vapply(as.list(expr)[-1L], deparse, character(1L))))
  }
  if (!is.call(expr) || !is.name(expr[[1L]]) ||
        !as.character(expr[[1L]]) %in% c(MVGAM_SMOOTH_CALLS, "mo")) {
    return(list(all.vars(expr)))
  }
  # A smooth names its covariates as unnamed arguments and its
  # grouping as `by`; every other argument is a setting. All three are
  # read off the call. Evaluating the call needed every setting to
  # resolve: `k = kk` failed and fell back to reporting `kk` as a
  # covariate. It also returned `by` as deparsed text, and
  # `by = interaction(a, b)` offered a column named after the call.
  args <- as.list(expr)[-1L]
  arg_names <- names(args) %||% character(length(args))
  by_vars <- if ("by" %in% arg_names) all.vars(args[["by"]]) else NULL
  term_vars <- lapply(args[!nzchar(arg_names)], all.vars)
  if (length(term_vars) <= 2L) {
    return(list(c(unlist(term_vars), by_vars)))
  }
  lapply(list(1:2, c(1L, 3L), 2:3), function(pair) {
    c(unlist(term_vars[pair]), by_vars)
  })
}


# Resolve the user-facing `series` argument into a structured mode
# the dispatch loop can branch on. Returns a list with
#   kind   = "none" | "all" | "one"
#   level  = NA_character_ | <resolved series factor level>
# Validates against the model's series factor levels and surfaces a
# targeted error when a non-existent level is requested.
resolve_series_arg <- function(series, x) {
  if (is.null(series)) {
    return(list(kind = "none", level = NA_character_))
  }
  # The axis record names the series. A fit whose series comes from
  # `gr` and `subgr` carries no series column, and this function
  # refused four series the record names.
  series_levels <- mvgam_axes(x)$series$levels
  if (!length(series_levels)) {
    stop(insight::format_error(c(
      "'series' selects among the series a model records.",
      x = "The fit records an empty series axis.",
      i = "Drop the 'series' argument for a fit with one time series."
    )))
  }
  if (length(series) != 1L) {
    stop(insight::format_error(
      "'series' must be NULL, 'all', a series name or a 1-based index."
    ))
  }
  if (identical(series, "all")) {
    return(list(kind = "all", level = NA_character_))
  }
  if (is.numeric(series)) {
    n_levels <- length(series_levels)
    checkmate::assert_integerish(series, len = 1L)
    if (series < 1L || series > n_levels) {
      stop(insight::format_error(c(
        "'series' index is out of range.",
        x = paste0("Got ", series, ", valid indices run from 1 to ",
                   n_levels, "."),
        i = paste0(
          "Available levels: ",
          paste(shQuote(series_levels), collapse = ", "), "."
        )
      )))
    }
    return(list(kind = "one", level = series_levels[as.integer(series)]))
  }
  if (is.character(series)) {
    if (!series %in% series_levels) {
      stop(insight::format_error(c(
        "'series' is not one of the model's series levels.",
        x = paste0("Got: '", series, "'."),
        i = paste0(
          "Available: ",
          paste(shQuote(series_levels), collapse = ", "), "."
        )
      )))
    }
    return(list(kind = "one", level = series))
  }
  stop(insight::format_error(
    "'series' must be NULL, 'all', a series name or a 1-based index."
  ))
}


# Draw a `plot_predictions()` panel as the other mvgam effect plots
# are drawn. marginaleffects colours a second conditioning variable's
# levels on one panel. Where that variable is the series, each series
# is a panel of its own with its own y scale, since series counts can
# differ by an order of magnitude. Any other second variable keeps its
# colours, taken from the active scheme.
#'@noRd
style_effect_panel <- function(p, condition, series_var) {
  set_color_scheme_local("red")
  group_var <- if (length(condition) >= 2L) condition[[2L]] else NULL
  by_series <- identical(group_var, series_var)
  for (layer in p$layers) {
    if (by_series) {
      layer$mapping[c("colour", "fill")] <- NULL
    }
    # An occasion with a missing response has no point to draw.
    if (inherits(layer$geom, "GeomPoint")) {
      layer$geom_params$na.rm <- TRUE
    }
    if (any(c("colour", "fill") %in% names(layer$mapping))) {
      next
    }
    if (inherits(layer$geom, "GeomRibbon")) {
      layer$aes_params$fill <- mvgam_colour("mid")
      layer$aes_params$alpha <- 0.5
    } else if (inherits(layer$geom, "GeomPoint") &&
                 !inherits(layer$geom, "GeomPointrange")) {
      layer$aes_params$colour <- "black"
    } else {
      layer$aes_params$colour <- mvgam_colour("dark")
    }
  }
  if (by_series) {
    # marginaleffects labels the colour and fill it mapped. The facet
    # strips name the series now, and a label with no mapping left to
    # carry it draws a notice.
    p$labels[c("colour", "fill")] <- NULL
    return(p + ggplot2::facet_wrap(ggplot2::vars(.data[[series_var]]),
                                   scales = "free_y") +
             mvgam_theme())
  }
  if (!is.null(group_var)) {
    p <- p + mvgam_model_colour_scale(
      length(unique(p$data[[group_var]])), name = group_var,
      aesthetics = c("colour", "fill"), labels = round_legend_labels
    )
  }
  p + mvgam_theme()
}


# Tidy ggplot legend labels: keep integers untouched, round numeric
# values to the minimum precision needed across the label set so
# legends don't carry 8+ digit floating-point noise.
round_legend_labels <- function(x) {
  numeric_x <- suppressWarnings(as.numeric(x))
  if (all(is.na(numeric_x))) {
    return(x)
  }
  decimals <- vapply(numeric_x, function(v) {
    if (is.na(v) ||
        abs(v - round(v)) <= .Machine$double.eps^0.5) {
      return(0L)
    }
    parts <- strsplit(sub("0+$", "", format(v, scientific = FALSE)),
                      ".", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) 0L else nchar(parts[2L])
  }, integer(1L))
  n_dec <- min(max(decimals, na.rm = TRUE), 4L)
  if (n_dec == 0L) {
    return(format(numeric_x, scientific = FALSE))
  }
  sprintf(paste0("%.", n_dec, "f"), numeric_x)
}
