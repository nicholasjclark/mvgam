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
#'   between 0 and 1) and `type = "response"`, raw observations are
#'   overlaid on the plot.
#' @param rug Logical. If `TRUE` and `type = "response"`, rug tick
#'   marks are drawn along the axes.
#' @param process_error Logical. Passed to `plot_predictions()` /
#'   `get_predict.mvgam`. Defaults to `FALSE` so the latent trend
#'   collapses to its posterior mean; set `TRUE` to include per-draw
#'   latent-state uncertainty.
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
#'     `plot_predictions` calls under the hood,
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
#' \donttest{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 60L, trend_model = AR())
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
  fan <- mv_resp_fan_out(x, resp)
  if (!is.null(fan)) {
    class(fan) <- "mvgam_conditional_effects"
    attr(fan, "mv_wrapper") <- TRUE
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
  if (type %in% c("latent_state", "detection") &&
        !is_closure_unit_family(x$family)) {
    family_types <- attr(x$family, "mvgam_predict_types",
                          exact = TRUE) %||% character(0)
    stop(insight::format_error(c(
      paste0(
        "type = '", type, "' is not available for this family."
      ),
      x = paste0(
        "Family '", resolve_family_name(x$family),
        "' exposes types: ",
        if (length(family_types) > 0L) {
          paste(paste0("'", family_types, "'"), collapse = ", ")
        } else {
          "none (not a closure-unit family)"
        },
        "."
      ),
      i = "Refit with family = nmix() or family = occ() to enable closure-unit predict types."
    )))
  }
  # `series` is polymorphic (NULL / "all" / character / integer) so a
  # single checkmate::assert_* call cannot validate it; the resolver
  # owns the per-branch validation and surfaces typed errors.
  series_mode <- resolve_series_arg(series, x)

  # Observation rugs and overlaid points only make sense on response
  # scale, and only for univariate fits (multivariate `mvbind`
  # responses don't have a single column to anchor points on).
  on_response <- identical(type, "response")
  is_mv <- brms::is.mvbrmsformula(x$formula)
  points_alpha <- 0
  if (on_response && !is_mv) {
    if (isTRUE(points)) {
      points_alpha <- 0.5
    } else if (is.numeric(points) && length(points) == 1L &&
               points > 0 && points <= 1) {
      points_alpha <- as.numeric(points)
    }
  }
  if (!on_response || is_mv) rug <- FALSE

  cond_labs <- if (is.null(effects)) {
    detect_conditional_effects(x)
  } else {
    lapply(as.character(effects), function(e) strsplit(e, ":")[[1L]])
  }

  if (any(lengths(cond_labs) > 3L)) {
    stop(insight::format_error(c(
      "Effects of order higher than 3 are not supported by conditional_effects.",
      i = "Use {.fn marginaleffects::plot_predictions} directly to build the higher-order plot."
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
  # are vulnerable here — `type`, `points`, `rug`, `process_error` are
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
      i = paste0(
        "These are set by conditional_effects.mvgam; pass via the ",
        "named arguments instead."
      )
    )))
  }

  # marginaleffects validates `type` against its shipped mvgam
  # `type_dictionary`, which names the latent state `latent_N`.
  # mvgam's user-facing token is `latent_state`; translate it to the
  # wire token so the upstream check passes. get_predict.mvgam maps
  # `latent_N` back to predict(type = "latent_state").
  wire_type <- if (identical(type, "latent_state")) "latent_N" else type
  out <- lapply(cond_labs, function(cond) {
    pp_args <- list(
      condition = cond,
      draw = TRUE,
      type = wire_type,
      points = points_alpha,
      rug = rug
    )
    # Forward process_error only when TRUE. get_predict.mvgam defaults
    # it to FALSE, so omitting the default keeps behaviour identical
    # while avoiding marginaleffects' "argument not known to be
    # supported" note on the common conditional_effects path.
    if (isTRUE(process_error)) {
      pp_args$process_error <- TRUE
    }
    if (!is.null(resp)) {
      # Multivariate: thread the per-response selector through to
      # get_predict.mvgam so its posterior_predict / posterior_epred
      # calls return a single matrix and the marginaleffects pipeline
      # does not error on the list-shaped multi-response draws.
      pp_args$resp <- resp
    }
    if (identical(series_mode$kind, "all")) {
      pp_args$condition <- c(cond, "series")
    } else if (identical(series_mode$kind, "one")) {
      # Restrict the prediction grid to one series's observations.
      pp_args$newdata <- x$data[
        x$data$series == series_mode$level, , drop = FALSE
      ]
    }
    p <- do.call(marginaleffects::plot_predictions,
                  c(list(x), pp_args, list(...))) +
      ggplot2::scale_fill_discrete(label = round_legend_labels) +
      ggplot2::scale_colour_discrete(label = round_legend_labels) +
      ggplot2::theme_classic()
    # marginaleffects defaults the y-axis label to the model's
    # first response name. For multi-response fits we know which
    # arm we are plotting (`resp`); overwrite so the user sees
    # the correct response on the y-axis.
    if (!is.null(resp)) {
      p <- p + ggplot2::labs(y = resp)
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
#' @return Invisibly returns the list of ggplot objects.
#' @rdname conditional_effects.mvgam
#' @export
plot.mvgam_conditional_effects <- function(x, plot = TRUE, ask = FALSE,
                                           ...) {
  if (length(x) == 0L) return(invisible(x))
  if (isTRUE(plot)) {
    default_ask <- grDevices::devAskNewPage()
    on.exit(grDevices::devAskNewPage(default_ask))
    grDevices::devAskNewPage(ask = isTRUE(ask))
    if (isTRUE(attr(x, "mv_wrapper"))) {
      # Multivariate wrapper: `x` is a named list of per-response
      # `mvgam_conditional_effects` objects. Recurse in per-arm
      # order so the display sequence matches user expectation
      # (arm-by-arm, effect-by-effect within each arm).
      for (r in names(x)) plot(x[[r]], plot = TRUE, ask = FALSE, ...)
    } else {
      for (p in x) graphics::plot(p)
    }
  }
  invisible(x)
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
#' variable name), `estimate`, `conf.low`, `conf.high`, followed by
#' whichever additional grid columns marginaleffects populated.
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
detect_conditional_effects <- function(x) {
  # Multivariate brmsformula has no single `$formula` slot.
  # `$forms` is a list of per-response brmsformula objects with
  # potentially distinct RHSs (`bf(yA ~ a) + bf(yB ~ b)`); for
  # mvbind both forms share the RHS but the shape is the same.
  # Union the term labels across responses so every covariate
  # the user might want to plot ends up in the term list.
  if (inherits(x$formula, "mvbrmsformula")) {
    per_resp <- lapply(x$formula$forms, function(bf) {
      attr(stats::terms(bf$formula, keep.order = TRUE),
           "term.labels")
    })
    termlabs <- unique(unlist(per_resp, use.names = FALSE))
    if (!is.null(x$trend_formula)) {
      termlabs <- c(
        termlabs,
        attr(stats::terms(x$trend_formula, keep.order = TRUE),
             "term.labels")
      )
    }
    termlabs <- termlabs[!grepl("^offset\\(", termlabs)]
    cond <- unlist(lapply(termlabs, split_term_labels),
                   recursive = FALSE)
    return(unique(cond))
  }
  obs_f <- if (inherits(x$formula, "brmsformula")) {
    x$formula$formula
  } else {
    x$formula
  }
  # Reason: for non-linear formulas (bf(..., nl = TRUE)) the
  # top-level RHS only enumerates nlpar names (e.g. `a + b * env`),
  # not the user-relevant covariates. The actual fixed-effect terms
  # live in the per-nlpar sub-formulas under `$pforms`. Collect
  # term labels from each sub-formula's RHS so callers see env,
  # trait1 etc. instead of just `a` and `b`.
  is_nl <- isTRUE(attr(obs_f, "nl"))
  nlpar_names <- if (is_nl && inherits(x$formula, "brmsformula")) {
    names(x$formula$pforms %||% list())
  } else {
    character(0L)
  }
  termlabs <- if (length(nlpar_names) > 0L) {
    nlpar_terms <- unlist(lapply(x$formula$pforms, function(pf) {
      attr(stats::terms(pf, keep.order = TRUE), "term.labels")
    }), use.names = FALSE)
    top <- attr(stats::terms(obs_f, keep.order = TRUE), "term.labels")
    c(nlpar_terms, top)
  } else {
    attr(stats::terms(obs_f, keep.order = TRUE), "term.labels")
  }
  if (!is.null(x$trend_formula)) {
    termlabs <- c(
      termlabs,
      attr(stats::terms(x$trend_formula, keep.order = TRUE),
           "term.labels")
    )
  }
  termlabs <- termlabs[!grepl("^offset\\(", termlabs)]
  cond <- unlist(lapply(termlabs, split_term_labels),
                 recursive = FALSE)
  # Filter out nlpar tokens that survived from the top-level
  # nl formula. `b * env` splits into c("b", "env"); the nlpar
  # `b` must be dropped from the grouping so we plot env on its
  # own. Empty groupings (a bare nlpar like `a`) are pruned.
  if (length(nlpar_names) > 0L) {
    cond <- lapply(cond, function(g) setdiff(g, nlpar_names))
    cond <- cond[lengths(cond) > 0L]
  }
  # Drop brms `|id|` correlation-tag tokens and other non-data
  # names so marginaleffects only sees addressable columns. Shares
  # the filter with find_predictors.mvgam via mvgam_keep_data_columns().
  cond <- lapply(cond, function(g) mvgam_keep_data_columns(g, x))
  cond <- cond[lengths(cond) > 0L]
  # Drop the empty-obs-formula placeholder column from
  # user-visible effect groupings. The pinned `constant(0)`
  # coefficient contributes zero to the linear predictor; plotting
  # it as a covariate leaks the workaround into the figure.
  cond <- lapply(cond, function(g) {
    setdiff(g, MVGAM_EMPTY_OBS_PLACEHOLDER)
  })
  cond <- cond[lengths(cond) > 0L]
  # Drop duplicates while preserving order
  keys <- vapply(cond, paste, FUN.VALUE = character(1L), collapse = ":")
  cond[!duplicated(keys)]
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
  if (grepl("*", lab, fixed = TRUE)) {
    return(list(strsplit(lab, "*", fixed = TRUE)[[1L]]))
  }
  smooth_starts <- c("s(", "te(", "t2(", "ti(", "gp(", "mo(")
  if (any(vapply(smooth_starts, grepl, FUN.VALUE = logical(1L),
                 x = lab, fixed = TRUE))) {
    parsed <- tryCatch(eval(rlang::parse_expr(lab)),
                       error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$term)) {
      return(list(all.vars(rlang::parse_expr(lab))))
    }
    by_var <- if (!is.null(parsed$by) && !identical(parsed$by, "NA")) {
      parsed$by
    } else {
      NULL
    }
    if (length(parsed$term) <= 2L) {
      return(list(c(all.vars(parse(text = parsed$term)), by_var)))
    }
    list(
      c(all.vars(parse(text = parsed$term[1:2])), by_var),
      c(all.vars(parse(text = parsed$term[c(1, 3)])), by_var),
      c(all.vars(parse(text = parsed$term[c(2, 3)])), by_var)
    )
  } else {
    list(all.vars(rlang::parse_expr(lab)))
  }
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
  if (!"series" %in% names(x$data)) {
    stop(insight::format_error(c(
      paste0(
        "'series' was supplied but the model's data has no ",
        "'series' column."
      ),
      i = paste0(
        "Drop the 'series' argument for fits without multiple ",
        "time series."
      )
    )))
  }
  series_levels <- levels(x$data$series)
  if (length(series) != 1L) {
    stop(insight::format_error(
      "'series' must be NULL, 'all', a series name, or a 1-based index."
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
        x = paste0(
          "Got: ", series, "; valid range is 1 to upper bound ",
          n_levels, "."
        ),
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
    "'series' must be NULL, 'all', a series name, or a 1-based index."
  ))
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
