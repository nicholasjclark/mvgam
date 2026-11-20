#' How each species responds to covariates
#'
#' @description
#' Gives each species' response to each named covariate, draw by draw,
#' with the posterior probability of its sign. A [jsdgam()] fit with
#' `trait_slopes` also gives each trait's effect on those responses,
#' the fourth-corner coefficients. The heatmaps show which species
#' respond positively, which negatively and which the data leave
#' uncertain.
#'
#' @param object A fitted `mvgam` object of several series.
#' @param variables Character vector of the covariates to report, each
#'   a column of the data that a formula of the model uses.
#' @param ... Unused. Anything passed here is refused.
#' @param prob_threshold The posterior probability of its sign an effect
#'   needs to count as supported, at least 0.5 and below 1. Defaults to
#'   0.95. It sets the `sig` column of `summary()` and the effects
#'   `plot()` outlines.
#' @param probs Two probabilities for the lower and upper bounds of the
#'   credible interval. Defaults to `c(0.025, 0.975)`.
#' @param robust Logical. `TRUE` summarises each effect by its median
#'   and median absolute deviation. Defaults to `FALSE`, the mean and
#'   the standard deviation.
#' @param ndraws,draw_ids The posterior draws to use, as
#'   [hindcast.mvgam()] takes them. `NULL` for both uses every draw.
#' @param resp The response to report, needed on a model with several.
#'
#' @details
#' *Species effects*. A species' response to a numeric covariate is the
#' average slope of its linear predictor, over the rows the species was
#' fitted to, as [marginaleffects::avg_slopes()] gives it with
#' `type = "link"`. A linear term gives the species' own coefficient,
#' such as `b_env` plus `b_env:seriessp_2` in `y ~ env * series`. A
#' smooth or an interaction gives the slope averaged over the species'
#' rows. A smooth that rises and falls can average to a slope near zero.
#' [conditional_effects.mvgam()] shows its shape. A covariate in
#' `trend_formula`, including one written with [lv_axis()], reaches each
#' species through its loadings. A factor covariate gives one effect per
#' level, its difference from the reference level.
#'
#' A [diri()], [multi()] or [categ()] fit gives each species' share
#' through a softmax, which ignores a slope every species shares. Each
#' draw is then centred across the species. An effect is the slope of
#' the species' log share relative to the geometric mean of the shares,
#' the centred log-ratio scale.
#'
#' An effect is on the link scale, per unit of the covariate. Effects of
#' covariates measured in different units compare better when the
#' covariates are standardised. A covariate that only a distributional
#' formula uses, such as the detection formula of [occ()], moves no
#' species' linear predictor and is refused.
#'
#' *Trait effects*. `trait_slopes = ~ trait` in [jsdgam()] makes each
#' species' response depend on its traits. A trait effect is how far the
#' response moves per unit of the trait, with each species' own deviation
#' held. A linear term gives its coefficient, such as `b_env:size`. A
#' smooth of the covariate or of the trait gives the effect averaged over
#' the fitted rows. A factor trait gives one effect per level, its
#' difference from the reference level. A covariate of `factor_formula`
#' reaches no trait. Its trait effects are zero.
#'
#' *Support*. `prob_nonzero` is the larger of the posterior
#' probabilities that an effect is positive and that it is negative. An
#' effect is supported when `prob_nonzero` exceeds `prob_threshold`.
#'
#' @return An object of class `mvgam_species_effects`. `print()` counts
#'   the supported positive and negative responses to each covariate.
#'   `summary()` returns a tibble of each effect's estimate, interval and
#'   sign probabilities, with `sig` marking the supported ones.
#'   `plot()` draws a heatmap of species by covariate, or of trait by
#'   covariate with `type = "traits"`. The `species` element holds the
#'   summary matrices, `[species, effect]`. The `draws` element holds
#'   the `[draws, species, effect]` array. A `trait_slopes` fit adds a
#'   `traits` element of the same form.
#'
#' @references Ovaskainen, O. and Abrego, N. (2020). *Joint Species
#'   Distribution Modelling: With Applications in R*. Cambridge
#'   University Press. \doi{10.1017/9781108591720}
#'
#' @seealso [residual_cor()] for the associations the covariates leave,
#'   [conditional_effects.mvgam()] for the shape of an effect,
#'   [variance_partition()] for how much each source moves a species
#'
#' @inherit jsdgam examples
#'
#' @author Nicholas J Clark
#' @export
species_effects <- function(object, ...) {
  UseMethod("species_effects")
}


#' @rdname species_effects
#' @method species_effects mvgam
#' @export
species_effects.mvgam <- function(object, variables, ...,
                                  prob_threshold = 0.95,
                                  probs = c(0.025, 0.975), robust = FALSE,
                                  ndraws = NULL, draw_ids = NULL,
                                  resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  rlang::check_dots_empty()
  if (missing(variables)) {
    variables <- character(0L)
  }
  checkmate::assert_character(variables, any.missing = FALSE, unique = TRUE)
  assert_prob_threshold(prob_threshold)
  checkmate::assert_numeric(probs, len = 2L, lower = 0, upper = 1,
                            any.missing = FALSE, unique = TRUE)
  checkmate::assert_flag(robust)
  validate_draw_selectors(ndraws, draw_ids)
  require_fitted_model(object, "species_effects")
  require_series_axis(object, "species_effects")
  keyed <- is_response_keyed(object)
  resolve_resp(object, resp, required = !keyed, caller = "species_effects")
  assert_effect_variables(object, variables, resp)
  probs <- sort(probs)
  draw_ids <- resolve_draw_indices(posterior::ndraws(object), ndraws,
                                   draw_ids)
  draws <- species_slope_draws(object, variables, draw_ids, resp)
  raw <- draws
  # A softmax ignores a shift shared by every species, which the data
  # leave to the prior. Each draw is centred across the species.
  simplex <- is_simplex_response_family(model_families(object, resp))
  if (simplex) {
    draws <- sweep(draws, c(1L, 3L), apply(draws, c(1L, 3L), mean))
  }
  inert <- vapply(dimnames(draws)[[3L]], function(effect) {
    all(draws[, , effect] == 0)
  }, logical(1L))
  if (any(inert)) {
    stop(insight::format_error(c(
      "Every 'variables' entry must move the linear predictor.",
      x = paste0("'", names(inert)[inert][1L], "' leaves every species' ",
                 "predictor unchanged."),
      i = if (simplex) {
        "A slope every species shares leaves each species' share unchanged."
      } else {
        "A covariate of a distributional formula alone leaves it unchanged."
      }
    )), call. = FALSE)
  }
  traits <- trait_effect_draws(object, variables, draw_ids, resp, raw)
  summarise <- function(arr) {
    summarise_signed_array(arr, robust, probs, prob_threshold,
                           dimnames(arr)[2:3])
  }
  structure(
    list(
      species = summarise(draws),
      traits = if (!is.null(traits)) summarise(traits),
      draws = list(species = draws, traits = traits),
      variables = variables,
      species_column = jsdgam_axis_names(object)$species %||%
        axis_vars(object)$series_var,
      prob_threshold = prob_threshold,
      probs = probs,
      robust = robust,
      draw_ids = draw_ids
    ),
    class = "mvgam_species_effects"
  )
}


#' Refuse a name that is no covariate of the model
#'
#' The covariates are the columns a formula of the model uses, less the
#' axis columns that place each row and the traits of `trait_slopes`. An empty `variables` is refused
#' with the list of covariates.
#'
#' @param object A fitted `mvgam` object
#' @param variables The names the caller gave
#' @param resp A response name, or `NULL`
#' @return `variables`, invisibly
#' @noRd
assert_effect_variables <- function(object, variables, resp) {
  axes <- c(unlist(axis_vars(object)), unlist(jsdgam_axis_names(object)))
  terms <- mvgam_term_list(object, resp)
  # `terms$conditional` leaves out the traits of `trait_slopes`, which
  # are constant within each species
  traits <- terms$traits
  covariates <- setdiff(terms$conditional, axes)
  listed <- if (length(covariates)) {
    paste0("Covariates: ", paste0("'", covariates, "'", collapse = ", "), ".")
  } else {
    "The model's formulas hold no covariate."
  }
  if (!length(variables)) {
    stop(insight::format_error(c(
      "'variables' must name the covariates to report.",
      i = listed
    )), call. = FALSE)
  }
  unknown <- setdiff(variables, covariates)
  if (length(unknown)) {
    grouping <- intersect(unknown, terms$random)
    trait <- intersect(unknown, traits)
    stop(insight::format_error(c(
      "Each 'variables' entry must be a covariate of the model.",
      x = paste0("Not a covariate: ",
                 paste0("'", unknown, "'", collapse = ", "), "."),
      x = if (length(grouping)) {
        paste0("'", grouping[1L], "' groups a random effect.")
      },
      x = if (length(trait)) {
        paste0("'", trait[1L], "' is a trait of 'trait_slopes'.")
      },
      i = if (length(trait)) {
        "summary(type = \"traits\") gives each trait's effects."
      },
      i = listed
    )), call. = FALSE)
  }
  invisible(variables)
}


#' Each species' average slope on each covariate, draw by draw
#'
#' [marginaleffects::avg_slopes()] averages the slopes over each
#' species' fitted rows. The draws, the response and the trend setting
#' reach `get_predict.mvgam()` as an attribute of the model, which
#' marginaleffects would otherwise warn on. A fit whose series are its
#' responses takes each response's rows in turn.
#'
#' @param object A fitted `mvgam` object
#' @param variables The covariates
#' @param draw_ids Indices of the draws
#' @param resp A response name, or `NULL`
#' @param frame The rows to average over, each placed on a fitted series
#' @return `[draws, species, effect]` array. A factor covariate gives one
#'   effect per level, labelled `<covariate>: <level> - <reference>`.
#' @noRd
species_slope_draws <- function(object, variables, draw_ids, resp,
                                frame = fitted_data(object)) {
  species <- fitted_series_levels(object)
  slopes <- function(rows, key, by) {
    model <- object
    attr(model, "mvgam_predict_args") <- list(
      resp = key, process_error = FALSE, draw_ids = draw_ids
    )
    tab <- marginaleffects::avg_slopes(
      model, variables = variables, newdata = frame[rows, , drop = FALSE],
      by = by, type = "link"
    )
    list(table = tab, draws = marginaleffects::get_draws(tab, shape = "DxP"))
  }
  parts <- if (is_response_keyed(object)) {
    lapply(species, function(key) {
      part <- slopes(!is.na(frame[[response_column(object, key)]]), key,
                     FALSE)
      part$species <- rep(key, nrow(part$table))
      part
    })
  } else {
    # A fit whose series come from `gr` and `subgr` has no series column.
    # The column takes the series the fit placed each row on.
    series_var <- axis_vars(object)$series_var
    frame[[series_var]] <- as.character(
      axis_row_series(object, frame, required = TRUE)
    )
    part <- slopes(seq_len(nrow(frame)), resp, series_var)
    part$species <- as.character(part$table[[series_var]])
    list(part)
  }
  # A numeric covariate's one slope takes its name. Each level of a
  # factor names its contrast.
  for (k in seq_along(parts)) {
    tab <- parts[[k]]$table
    parts[[k]]$effect <- ifelse(tab$contrast == "dY/dX", tab$term,
                                paste0(tab$term, ": ", tab$contrast))
  }
  # The effects follow `variables`, a factor's levels in their own order
  found <- unique(unlist(lapply(parts, `[[`, "effect")))
  expected <- unlist(lapply(variables, function(v) {
    if (is.numeric(frame[[v]])) {
      return(v)
    }
    lev <- levels(as.factor(frame[[v]]))
    paste0(v, ": ", lev[-1L], " - ", lev[1L])
  }))
  effects <- c(intersect(expected, found), setdiff(found, expected))
  out <- array(NA_real_, c(length(draw_ids), length(species), length(effects)),
               dimnames = list(NULL, species, effects))
  for (part in parts) {
    for (j in seq_along(part$effect)) {
      out[, part$species[j], part$effect[j]] <- part$draws[, j]
    }
  }
  out
}


#' Each trait's effect on the species' responses, draw by draw
#'
#' A trait's effect on the response to a covariate is how far the
#' species' slopes move as the trait moves, with each species' own
#' deviation held. The slopes are taken twice, on the fitted rows and on
#' those rows with the trait shifted. A numeric trait shifts by a small
#' step, which gives the derivative. A factor trait takes each level in
#' turn, which gives its difference from the reference level. A linear
#' trait term recovers its coefficient, such as `b_env:size`.
#'
#' @param object A fitted `mvgam` object
#' @param variables The covariates
#' @param draw_ids Indices of the draws
#' @param resp A response name, or `NULL`
#' @param slopes The `[draws, species, effect]` slopes on the fitted rows
#' @return `[draws, trait effect, effect]` array, or `NULL` for a fit
#'   without `trait_slopes`
#' @noRd
trait_effect_draws <- function(object, variables, draw_ids, resp, slopes) {
  trait_slopes <- object$jsdgam_args$trait_slopes
  if (is.null(trait_slopes)) {
    return(NULL)
  }
  frame <- fitted_data(object)
  shifts <- unlist(lapply(all.vars(trait_slopes), function(trait) {
    values <- frame[[trait]]
    if (is.numeric(values)) {
      spread <- stats::sd(unique(values))
      step <- 1e-3 * if (is.finite(spread) && spread > 0) spread else 1
      shifted <- frame
      shifted[[trait]] <- values + step
      return(stats::setNames(list(list(frame = shifted, step = step,
                                       base = NULL)), trait))
    }
    lev <- levels(as.factor(values))
    at <- function(level) {
      out <- frame
      out[[trait]] <- if (is.factor(values)) {
        factor(level, levels = levels(values))
      } else {
        level
      }
      out
    }
    stats::setNames(lapply(lev[-1L], function(level) {
      list(frame = at(level), step = 1, base = at(lev[1L]))
    }), paste0(trait, ": ", lev[-1L], " - ", lev[1L]))
  }), recursive = FALSE)
  # The trait effect is common to the species. Their mean is reported.
  across <- function(arr) apply(arr, c(1L, 3L), mean)
  effects <- lapply(shifts, function(shift) {
    moved <- species_slope_draws(object, variables, draw_ids, resp,
                                 shift$frame)
    base <- if (is.null(shift$base)) {
      slopes
    } else {
      species_slope_draws(object, variables, draw_ids, resp, shift$base)
    }
    across(moved - base) / shift$step
  })
  out <- array(unlist(effects),
               c(length(draw_ids), dim(slopes)[3L], length(effects)),
               dimnames = list(NULL, dimnames(slopes)[[3L]], names(shifts)))
  aperm(out, c(1L, 3L, 2L))
}


#' Each effect of one panel as a row of a table
#'
#' @param stats The summary matrices of the species or the traits
#' @param rows_name The name of the column holding the matrices' rows
#' @param probs The interval probabilities
#' @return Data frame, one row per cell, rows varying fastest
#' @noRd
effect_rows <- function(stats, rows_name, probs) {
  cells <- expand.grid(rownames(stats$point), colnames(stats$point),
                       stringsAsFactors = FALSE)
  out <- data.frame(cells[[1L]], effect = cells[[2L]],
                    Estimate = as.vector(stats$point),
                    Est.Error = as.vector(stats$se),
                    as.vector(stats$lower), as.vector(stats$upper),
                    prob_positive = as.vector(stats$prob_positive),
                    prob_negative = as.vector(stats$prob_negative),
                    prob_nonzero = as.vector(stats$prob_nonzero),
                    sig = as.vector(stats$sig) != 0)
  names(out)[c(1L, 5L, 6L)] <- c(rows_name, paste0("Q", probs * 100))
  out
}


#' The summary matrices of the species or the trait panel
#'
#' @param x An `mvgam_species_effects` object
#' @param type `"species"` or `"traits"`
#' @return The summary matrices
#' @noRd
effects_panel <- function(x, type) {
  if (type == "traits" && is.null(x$traits)) {
    stop(insight::format_error(c(
      "'type = \"traits\"' needs a jsdgam() fit with 'trait_slopes'.",
      i = "Refit with 'jsdgam(trait_slopes = ~ trait)'."
    )), call. = FALSE)
  }
  x[[type]]
}


#' @param type `"species"` (the default) for the species by covariate
#'   panel, or `"traits"` for the trait by covariate panel of a
#'   `trait_slopes` fit.
#' @rdname species_effects
#' @method summary mvgam_species_effects
#' @export
summary.mvgam_species_effects <- function(object,
                                          type = c("species", "traits"),
                                          ...) {
  type <- match.arg(type)
  rlang::check_dots_empty()
  rows_name <- if (type == "species") object$species_column else "trait"
  tibble::as_tibble(effect_rows(effects_panel(object, type), rows_name,
                                object$probs))
}


#' @param x An `mvgam_species_effects` object.
#' @param digits Integer number of significant digits. Defaults to 2.
#' @rdname species_effects
#' @method print mvgam_species_effects
#' @export
print.mvgam_species_effects <- function(x, digits = 2L, ...) {
  rlang::check_dots_empty()
  stats <- x$species
  supported <- stats$sig != 0
  counts <- data.frame(
    effect = colnames(stats$point),
    positive = colSums(supported & stats$point > 0),
    negative = colSums(supported & stats$point < 0),
    species = nrow(stats$point)
  )
  cat_titled_table(
    paste0("Species with a supported response, prob_nonzero > ",
           x$prob_threshold, ", ", length(x$draw_ids), " draws"),
    counts, digits,
    c("summary() gives each species' estimate and interval.",
      if (!is.null(x$traits)) {
        "summary(type = \"traits\") gives the trait effects."
      }),
    max_rows = 20L
  )
  invisible(x)
}


#' @param supported_only Logical. `TRUE` leaves blank every effect whose
#'   `prob_nonzero` is at most `prob_threshold`. Defaults to `FALSE`.
#' @rdname species_effects
#' @method plot mvgam_species_effects
#' @export
plot.mvgam_species_effects <- function(x, type = c("species", "traits"),
                                       supported_only = FALSE, ...) {
  type <- match.arg(type)
  checkmate::assert_flag(supported_only)
  rlang::check_dots_empty()
  stats <- effects_panel(x, type)
  panel <- support_panel(stats$point, stats$prob_nonzero, x$prob_threshold,
                         supported_only)
  lim <- symmetric_limit(panel$value)
  set_color_scheme_local()
  matrix_heatmap(
    panel,
    mvgam_diverging_scale(name = "Posterior\neffect", limits = c(-lim, lim)),
    layout = "matrix", x_lab = "Covariate",
    y_lab = if (type == "species") x$species_column else "Trait"
  )
}
