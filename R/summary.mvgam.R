# ==============================================================================
# MVGAM SUMMARY METHODS
# ==============================================================================
# Summary methods for mvgam fitted models following brms conventions.
# Uses posterior package for all computation, organized by parameter category.

#' Summary of mvgam model fits
#'
#' @description
#' Provides posterior summary statistics for mvgam model parameters, organized
#' by category (fixed effects, smooth terms, family parameters, trend parameters,
#' and factor loadings).
#'
#' @param object An object of class \code{mvgam}.
#' @param probs Numeric vector of length 2 specifying quantile probabilities
#'   for credible intervals. Default is \code{c(0.025, 0.975)} for 95% intervals.
#' @param robust Logical; if \code{TRUE}, the median and MAD measure
#'   central tendency and spread. The default \code{FALSE} uses the mean
#'   and SD.
#' @param matrices Logical; if \code{TRUE}, also print every cell of
#'   the matrices a correlated, VAR or hierarchical trend derives
#'   from its sampled parameters (\code{Phi_trend}, \code{Sigma_trend},
#'   \code{Omega_trend}, \code{Phi_group_trend},
#'   \code{Sigma_group_trend}). The default \code{FALSE} prints the
#'   sampled parameters alone, among them \code{sigma_trend},
#'   \code{Amu_trend}, \code{Aomega_trend} and
#'   \code{alpha_cor_trend}. [posterior_transition_matrix()] and
#'   [residual_cor()] summarise the matrices a cell at a time.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return An object of class \code{mvgam_summary}. Each block is a
#'   data frame of the columns below, one row per parameter, named as
#'   \pkg{brms} prints it, and is absent when the model has none. The
#'   observation model's blocks are:
#' \describe{
#'   \item{\code{smooth}}{Smoothing standard deviations, one row per
#'     penalty of each smooth. The basis coefficients are available by
#'     name from \code{as_draws(object, variable = "^s_", regex =
#'     TRUE)}.}
#'   \item{\code{gp}}{Gaussian-process marginal standard deviations
#'     and length-scales.}
#'   \item{\code{random}}{Group-level standard deviations and
#'     correlations, as a list of tables named by grouping factor.
#'     \code{ngrps} gives each factor's number of levels, and
#'     \code{ranef()} the per-level effects.}
#'   \item{\code{fixed}}{Regression coefficients, those of every
#'     distributional parameter given a formula among them.}
#'   \item{\code{mo}}{The simplex of each monotonic effect.}
#'   \item{\code{spec}}{Family parameters without a formula of their
#'     own, such as \code{sigma} or \code{shape}.}
#' }
#' The trend model's blocks take the same names with the prefix
#' \code{trend_}, joined by \code{trend_spec} for the trend's
#' dynamics, \code{loadings} for the factor loadings and
#' \code{loadings_prior} for the hyperparameters of their prior. The
#' formula, family, draw counts and data dimensions are stored with
#' them.
#'
#' @details
#' The summary includes the following columns:
#' \describe{
#'   \item{\code{Estimate}}{Posterior mean (or median if robust = TRUE)}
#'   \item{\code{Est.Error}}{Posterior SD (or MAD if robust = TRUE)}
#'   \item{Quantile bounds}{Credible interval limits based on probs argument}
#'   \item{\code{Rhat}}{Potential scale reduction factor (should be < 1.05)}
#'   \item{\code{Bulk_ESS}}{Bulk effective sample size}
#'   \item{\code{Tail_ESS}}{Tail effective sample size}
#' }
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
#' summary(mod)
#' }
#'
#' @export
summary.mvgam <- function(object, probs = c(0.025, 0.975),
                          robust = FALSE,
                          matrices = FALSE, ...) {
  # Input validation
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_numeric(probs, len = 2, lower = 0, upper = 1)
  checkmate::assert_true(probs[1] < probs[2])
  checkmate::assert_logical(robust, len = 1)
  checkmate::assert_flag(matrices)
  reject_removed_args(
    list(...), fn = "summary",
    removed = c(
      include_betas = "use 'matrices = TRUE' to print the trend matrices"
    )
  )
  rlang::check_dots_empty()

  require_fitted_model(object, "summary")

  # Compute summaries once for efficiency (then filter by category)
  all_summaries <- compute_all_summaries(
    object,
    probs = probs,
    robust = robust
  )

  # Get all parameter names from rownames
  pars <- rownames(all_summaries)

  # Drop the trend's own time-indexed states: one row per time point
  # and series, which no summary block claims. Dropping them here
  # spares the classifiers below that work. `hindcast(type = "trend")` returns them as a
  # trajectory, and `as.data.frame(variable = "^trend\\[", regex =
  # TRUE)` as draws.
  pars_to_keep <- !is_trend_state_param(pars)
  all_summaries <- all_summaries[pars_to_keep, , drop = FALSE]
  pars <- rownames(all_summaries)

  # The matrices a trend derives from its sampled parameters print
  # only on request: a 3-series VAR(2) derives 63 cells of them. The
  # sampled parameters stay, among them `Amu_trend`, `Aomega_trend`,
  # `alpha_cor_trend` and the `sigma_group_trend` scalars.
  if (!matrices) {
    pars_to_keep <- !is_trend_matrix_param(pars)
    if (any(!pars_to_keep)) {
      all_summaries <- all_summaries[pars_to_keep, , drop = FALSE]
      pars <- rownames(all_summaries)
    }
  }

  # Hide each raw form whose reported counterpart is in the same
  # posterior: rotation- and sign-indeterminate factor parameters
  # against their QR-identified forms (Heaps & Jermyn 2024), and
  # broadcast shared-coefficient copies against the sampled scalar.
  # The downstream convergence advisor and loadings table then work
  # from the reported names.
  identified <- filter_hidden_pars(pars)
  if (length(identified) < length(pars)) {
    all_summaries <- all_summaries[identified, , drop = FALSE]
    pars <- rownames(all_summaries)
  }

  out <- c(
    list(formula = object$formula, family = object$family),
    draw_counts(object)
  )

  # The blocks brms reports, for each side of the model. A trend-side
  # block takes the `trend_` prefix on its key.
  kind <- mvgam_par_kind(pars)
  for (side in c("observation", "trend")) {
    prefix <- if (identical(side, "trend")) "trend_" else ""
    on_side <- mvgam_par_side(pars) == side
    ngrps <- lengths(mvgam_ranef_metadata(object, side)$group_levels)
    for (block in summary_blocks()) {
      idx <- kind %in% block$kinds & on_side
      if (any(idx)) {
        out[[paste0(prefix, block$key)]] <- summary_block_rows(
          all_summaries[idx, , drop = FALSE], block, names(ngrps)
        )
      }
    }
    if (length(ngrps)) {
      out[[paste0(prefix, "ngrps")]] <- ngrps
    }
  }

  family_idx <- match_family_pars(pars)
  if (any(family_idx)) {
    out$spec <- all_summaries[family_idx, , drop = FALSE]
  }

  trend_spec_idx <- match_trend_specific_pars(pars)
  if (any(trend_spec_idx)) {
    out$trend_spec <- all_summaries[trend_spec_idx, , drop = FALSE]
  }

  z_pars <- pars[match_z_loadings(pars)]
  if (length(z_pars)) {
    z_pars <- varying_pars(as_draws_array(object, variable = z_pars), z_pars)
    out$loadings <- all_summaries[z_pars, , drop = FALSE]
  }

  loadings_prior_idx <- kind == "loadings_prior"
  if (any(loadings_prior_idx)) {
    out$loadings_prior <- all_summaries[
      loadings_prior_idx, , drop = FALSE
    ]
  }

  # Store mvgam-specific metadata for print.summary.mvgam() to display
  # model structure information (formula, trend type, dimensions)
  out$trend_formula <- object$trend_formula
  out$trend_predictors <- trend_predictors(object)

  trend_type <- get_trend_type(object)
  out$trend_model <- if (!identical(trend_type, "None")) trend_type

  # `trend_model` is the bare constructor type, which a correlated
  # `AR()` and an independent one share. Whether this fit estimates
  # cross-series correlations comes from the trend metadata, where
  # the trend spec recorded it.
  out$trend_has_cor <- isTRUE(object$trend_metadata$has_cor)

  # The `ZMVN` test in `build_next_steps()` matches the bare type.
  # The printed line also names the order the trend was fitted at,
  # which `printed_trend_label()` renders from the fit's own
  # metadata. `summary()` stores the label because the helper,
  # handed a summary object, reports a fitted ARMA as `None`.
  out$trend_label <- printed_trend_label(object)

  # The counts `print()` shows, from the same helper. A hierarchical
  # fit derives its series axis from a grouping. `series_info` counts
  # a column, and taking the count from it printed four series for
  # such a fit and summarised none.
  counts <- printed_axis_counts(object)
  out$n_series <- counts$n_series
  out$n_timepoints <- counts$n_timepoints

  # How many rows the model was given. `nobs()` counts these, and a
  # reader takes "Number of observations" to mean them. The product of
  # the two axes above exceeds the frame on a ragged design and
  # counted units on a closure-unit family. The two standard
  # accessors then gave 75 and 300 for one model.
  out$nobs <- nobs(object)

  # Store data name (captured at top-level mvgam() call)
  out$data_name <- object$data.name

  structure(out, class = "mvgam_summary")
}

# ==============================================================================
# CORE COMPUTATION FUNCTION (Internal Helper)
# ==============================================================================

#' Compute posterior summaries for all parameters
#'
#' @description
#' Internal helper that summarises every parameter in one
#' `posterior::summarise_draws()` call, returning the full table for
#' filtering by category.
#'
#' @param object An mvgam fitted object
#' @param probs Quantile probabilities (length 2)
#' @param robust Logical for median/MAD vs mean/SD
#'
#' @return Data frame with columns: variable, Estimate, Est.Error, quantiles,
#'   Rhat, Bulk_ESS, Tail_ESS
#'
#' @noRd
compute_all_summaries <- function(object, probs, robust) {
  # The draws come from the fitted object, whose method gives every
  # parameter the name `mvgam_user_pars()` assigns: `b_x` for the
  # positional `b[1]` of the raw Stan model, and brms's names for the
  # smooth, Gaussian-process and group-level blocks.
  draws <- posterior::as_draws_df(object)

  # Each element is named, which gives the column its name, and holds
  # the function itself, which removes the lookup. An unnamed argument
  # takes its column name from a deparse of what was passed, and for a
  # function object that is the whole body; a bare string is looked up
  # in the calling namespace, where a same-named function defined here
  # later would win.
  measures <- if (robust) {
    list(median = stats::median, mad = stats::mad)
  } else {
    list(mean = base::mean, sd = stats::sd)
  }
  out <- without_ess_cap_notice(do.call(posterior::summarise_draws, c(
    list(draws),
    measures,
    list(
      ~quantile(.x, probs = probs),
      posterior::default_convergence_measures()
    )
  )))

  # Rename columns for display
  names(out) <- rename_summary_cols(names(out), probs, robust)

  # Move parameter names from column to rownames for cleaner printing
  out <- as.data.frame(out)
  rownames(out) <- out$variable
  out$variable <- NULL

  out
}

# ==============================================================================
# COLUMN RENAMING
# ==============================================================================

#' Rename summary columns for display
#'
#' @description
#' Renames columns from posterior::summarise_draws() to match brms conventions.
#'
#' @param col_names Character vector of column names
#' @param probs Quantile probabilities used
#' @param robust Whether robust estimation was used
#'
#' @return Character vector with renamed columns
#'
#' @noRd
rename_summary_cols <- function(col_names, probs, robust) {
  # Rename location and spread measures
  if (robust) {
    col_names[col_names == "median"] <- "Estimate"
    col_names[col_names == "mad"] <- "Est.Error"
  } else {
    col_names[col_names == "mean"] <- "Estimate"
    col_names[col_names == "sd"] <- "Est.Error"
  }

  # Rename convergence diagnostics
  col_names[col_names == "rhat"] <- "Rhat"
  col_names[col_names == "ess_bulk"] <- "Bulk_ESS"
  col_names[col_names == "ess_tail"] <- "Tail_ESS"

  # Rename quantile columns to credible interval format
  # posterior returns "2.5%", "97.5%" etc., convert to "l-95% CI", "u-95% CI"
  # CI coverage = (upper - lower) * 100 = (0.975 - 0.025) * 100 = 95
  ci_coverage <- (probs[2] - probs[1]) * 100
  ci_label <- paste0(ci_coverage, "% CI")

  for (i in seq_along(probs)) {
    q_col <- paste0(probs[i] * 100, "%")
    prefix <- if (i == 1) "l-" else "u-"
    new_name <- paste0(prefix, ci_label)

    if (q_col %in% col_names) {
      col_names[col_names == q_col] <- new_name
    }
  }

  col_names
}

# ==============================================================================
# PARAMETER MATCHING FUNCTIONS (Pattern-Based)
# ==============================================================================

#' The parameter blocks a summary reports for each side, in print order
#'
#' `brms:::summary.brmsfit()` reports these blocks and
#' `brms:::print.brmssummary()` prints them in this order under these
#' headings. Each block names its key in the summary object and the
#' taxonomy kinds it holds. A distributional parameter's coefficients
#' join the mean's, as brms reports `b_sigma_x` as `sigma_x`. The basis
#' coefficients of a smooth or a Gaussian process and the per-level
#' group-level effects have no meaning one at a time and no block
#' holds them. `as_draws()` returns them by name, and
#' `conditional_smooths()` and `ranef()` summarise them.
#'
#' `wrap` names a hyperparameter inside its class, as brms prints
#' `sds(sx_1)`, `sdgp(gpx)` and `lscale(gpx)`. The group-level block
#' splits by grouping factor, as brms prints `sd(Intercept)` under
#' `~g`.
#'
#' @return A list of blocks
#' @noRd
summary_blocks <- function() {
  list(
    list(key = "smooth", heading = "Smoothing Spline Hyperparameters",
         kinds = "smooth_sd", wrap = TRUE),
    list(key = "gp", heading = "Gaussian Process Hyperparameters",
         kinds = "gp", wrap = TRUE),
    list(key = "random", heading = "Multilevel Hyperparameters",
         kinds = "ranef_sd", wrap = TRUE),
    list(key = "fixed", heading = "Regression Coefficients",
         kinds = c("beta", "basis"), wrap = FALSE),
    list(key = "mo", heading = "Monotonic Simplex Parameters",
         kinds = "simplex", wrap = FALSE)
  )
}


#' A block's rows under the names brms prints
#'
#' The block's heading and side name the class and the side, and the
#' class prefix and the trend suffix are dropped: `b_x`, `bs_sigma_sx_1`, `simo_moord1[1]` and
#' `b_elev_trend` print as `x`, `sigma_sx_1`, `moord1[1]` and `elev`.
#' A hyperparameter keeps its class around the rest, as `sds(sx_1)`.
#' A group-level block becomes one table per grouping factor, as
#' `brms:::summary.brmsfit()` builds it: the rows of each factor are
#' those named `sd_<g>__` or `cor_<g>__`, with that prefix dropped and
#' the coefficient pair of a correlation joined by a comma.
#' `sd_g__Intercept` and `cor_g__Intercept__x` print as `sd(Intercept)`
#' and `cor(Intercept,x)` under `g`.
#'
#' @param tab Rows of the summary table, named by parameter
#' @param block One entry of `summary_blocks()`
#' @param groups The side's grouping factors, as
#'   `mvgam_ranef_metadata()` names them
#' @return `tab` with its row names replaced, or for the group-level
#'   block a list of such tables named by grouping factor
#' @noRd
summary_block_rows <- function(tab, block, groups = character()) {
  pars <- sub("_trend(\\[|$)", "\\1", rownames(tab))
  class <- sub("_.*$", "", pars)
  if (identical(block$key, "random")) {
    out <- lapply(stats::setNames(nm = groups), function(g) {
      lead <- paste0(class, "_", g, "__")
      hit <- class %in% c("sd", "cor") & startsWith(pars, lead)
      rows <- tab[hit, , drop = FALSE]
      core <- sub("__", ",", substring(pars[hit], nchar(lead[hit]) + 1L))
      rownames(rows) <- paste0(class[hit], "(", core, ")")
      rows
    })
    return(Filter(nrow, out))
  }
  core <- sub("^[a-z]+_", "", pars)
  rownames(tab) <- if (isTRUE(block$wrap)) {
    paste0(class, "(", core, ")")
  } else {
    core
  }
  tab
}


#' Observation-family parameters
#'
#' A distributional parameter given a formula of its own has no scalar
#' of this kind: brms reports its coefficients with the regression
#' coefficients.
#'
#' @param pars Character vector of all parameter names
#' @return Logical vector
#'
#' @noRd
match_family_pars <- function(pars) {
  mvgam_par_kind(pars) == "family"
}


#' Latent-dynamics parameters of the trend model
#'
#' What is left on the trend side once the formula effects and the
#' states are accounted for: the innovation scales, the
#' autoregressive coefficients and the correlation blocks. Draws
#' whose reported counterpart is in the same posterior are hidden.
#'
#' @param pars Character vector of all parameter names
#' @return Logical vector
#'
#' @noRd
match_trend_specific_pars <- function(pars) {
  mvgam_par_kind(pars) == "dynamics" & !is_hidden_par(pars)
}

#' Match factor loading parameter names
#'
#' @description
#' Identifies factor loading matrix parameters. Prefers
#' `Z_tilde\[i, j\]` (QR-identified loadings emitted by free-Z
#' factor models) when present and falls back to `Z\[i, j\]`
#' for partial-Z fits where the user-supplied pattern is
#' preserved without rotation. Pattern selection delegates to
#' `factor_loading_param_pattern()`.
#'
#' @param pars Character vector of all parameter names
#' @return Logical vector indicating which parameters are factor
#'   loadings
#'
#' @noRd
match_z_loadings <- function(pars) {
  grepl(factor_loading_param_pattern(pars), pars)
}

#' The parameters whose draws are not all equal
#'
#' The identified loadings are lower triangular. Every draw holds the
#' entries above the diagonal at zero, and a summary of them reports
#' a constant with no convergence diagnostics.
#'
#' @param draws A `draws` object holding `pars`.
#' @param pars Character vector of parameter names.
#' @return The subset of `pars` that varies across draws.
#' @noRd
varying_pars <- function(draws, pars) {
  pars[vapply(pars, function(p) {
    v <- as.numeric(posterior::extract_variable(draws, p))
    any(v != v[1L])
  }, logical(1L))]
}

#' The trend's own time-indexed states
#'
#' A fit has one per time point and series, and summary output leaves
#' them out. Elsewhere in the package `latent_state` names the
#' closure-unit quantity, and the trend's own states take
#' `trend_state`.
#'
#' @param pars Character vector of parameter names
#' @return Logical vector
#'
#' @noRd
is_trend_state_param <- function(pars) {
  mvgam_par_kind(pars) == "state"
}


#' The trend's per-cell matrix parameters
#'
#' A correlated or hierarchical trend estimates one entry per cell of
#' a matrix, and a high-dimensional fit prints hundreds of those
#' rows. `summary()` prints them with `matrices = TRUE`. `par_taxonomy.R`
#' declares the names, which keeps one account of what each name
#' means.
#'
#' @param pars Character vector of parameter names
#' @return Logical vector
#'
#' @noRd
is_trend_matrix_param <- function(pars) {
  grepl(MVGAM_PAR_MATRIX_PATTERN, pars)
}

# ==============================================================================
# HELPER FUNCTIONS FOR PRINT METHOD
# ==============================================================================

#' Format a parameter table as brms prints one
#'
#' `brms:::print_format()` prints every column to `digits` places and
#' the effective sample sizes as whole draws. Rounding instead printed
#' an Rhat of 1.00 as `1` and an ESS as `1390.86`.
#'
#' @param x Data frame of numeric columns, named by parameter
#' @param digits Number of decimal places
#' @return Character matrix with the row and column names of `x`
#' @noRd
format_param_table <- function(x, digits = 2) {
  out <- as.matrix(x)
  for (col in colnames(x)) {
    fmt <- if (col %in% c("Bulk_ESS", "Tail_ESS")) "%.0f" else {
      paste0("%.", digits, "f")
    }
    out[, col] <- sprintf(fmt, x[[col]])
  }
  out
}

#' Print a parameter table section if present
#'
#' @description
#' Helper to print a parameter table with header if the table exists.
#'
#' @param table Data frame or NULL
#' @param header Character string for section header or NULL for no header
#' @param digits Number of decimal places
#'
#' @noRd
print_param_section <- function(table, header, digits = 2) {
  if (!is.null(table) && nrow(table) > 0) {
    if (!is.null(header)) {
      cat(header, ":\n", sep = "")
    }
    print(format_param_table(table, digits), quote = FALSE, right = TRUE)
    cat("\n")
  }
}

# ==============================================================================
# PRINT METHOD
# ==============================================================================

#' Print a model's family and links, one line pair per response
#'
#' brms's layout: a model with several responses prefixes each family
#' and link with the response's key, aligned under the first. The
#' family is named by `resolve_family_name()`, which returns "tweedie"
#' where a customfamily stores "custom".
#'
#' @param x A fitted `mvgam`, a prefit or its summary.
#' @return The families `model_families()` gives, invisibly.
#' @noRd
print_family_links <- function(x) {
  fams <- model_families(x)
  if (inherits(fams, "family")) {
    cat(" Family: ", resolve_family_name(fams), " \n", sep = "")
    cat("  Links: ", format_family_links(fams), " \n", sep = "")
    return(invisible(fams))
  }
  keys <- names(fams)
  families <- vapply(fams, resolve_family_name, character(1L))
  links <- vapply(fams, format_family_links, character(1L))
  cat(" Family: ", paste0(keys, ": ", families,
                          collapse = " \n          "), " \n", sep = "")
  cat("  Links: ", paste0(keys, ": ", links,
                          collapse = " \n        "), " \n", sep = "")
  invisible(fams)
}

#' Print method for mvgam_summary objects
#'
#' @param x An object of class \code{mvgam_summary}.
#' @param digits Integer for decimal places. Default is 2.
#' @param ... Additional arguments (currently unused).
#'
#' @return The \code{mvgam_summary} object invisibly.
#'
#' @export
print.mvgam_summary <- function(x, digits = 2, ...) {
  checkmate::assert_class(x, "mvgam_summary")
  checkmate::assert_int(digits, lower = 0)

  # Section 1: Family and Links (aligned with fixed spacing)
  print_family_links(x)

  # Section 2: Formula
  formulas <- format_model_formula(x$formula)
  # Join with newline + 9 spaces to align with "Formula: "
  cat("Formula: ", paste0(formulas, collapse = " \n         "), " \n",
      sep = "")

  # Section 3: Data and dimensions (brms style)
  nobs <- x$nobs
  if (!is.null(x$data_name)) {
    cat("   Data: ", x$data_name, " (Number of observations: ", nobs, ") \n",
        sep = "")
  } else {
    cat("   Data: (Number of observations: ", nobs, ") \n", sep = "")
  }

  # Add series count if multiple series
  if (!is.null(x$n_series) && x$n_series > 1) {
    cat(" Series: ", x$n_series, " \n", sep = "")
  }

  # Section 4: Trend information
  trend_line <- format_trend_line(x)
  if (nzchar(trend_line)) {
    cat(trend_line, "\n", sep = "")
  }

  cat(format_draws_line(x), "\n\n", sep = "")

  # Section 6: Observation Model Parameters
  blocks <- summary_blocks()
  side_blocks <- function(prefix) {
    Filter(Negate(is.null), stats::setNames(
      lapply(blocks, function(b) x[[paste0(prefix, b$key)]]),
      vapply(blocks, `[[`, character(1L), "heading")
    ))
  }
  # The group-level block is one table per grouping factor, each under
  # its factor and level count, as brms prints it
  print_side <- function(tabs, ngrps) {
    for (heading in names(tabs)) {
      tab <- tabs[[heading]]
      if (is.data.frame(tab)) {
        print_param_section(tab, heading, digits)
        next
      }
      cat(heading, ":\n", sep = "")
      for (g in names(tab)) {
        cat("~", g, " (Number of levels: ", ngrps[[g]], ")\n", sep = "")
        print_param_section(tab[[g]], NULL, digits)
      }
    }
  }
  obs_tabs <- side_blocks("")
  if (length(obs_tabs) || !is.null(x$spec)) {
    cat("== Observation Model ==\n")
    print_side(obs_tabs, x$ngrps)
    print_param_section(x$spec, "Further Distributional Parameters", digits)
  }

  # Section 7: Trend Model Parameters
  trend_tabs <- side_blocks("trend_")
  has_trend_params <- length(trend_tabs) > 0L || !is.null(x$trend_spec) ||
                      !is.null(x$loadings) || !is.null(x$loadings_prior)

  if (has_trend_params) {
    cat("== Trend Model ==\n")
    print_side(trend_tabs, x$trend_ngrps)
    print_param_section(x$trend_spec, "Trend Specific Parameters", digits)
    print_param_section(x$loadings, "Factor Loadings", digits)
    print_param_section(x$loadings_prior, "Loadings Prior", digits)
  }
  cat("Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS\n")
  cat("and Tail_ESS are effective sample size measures, and Rhat is the potential\n")
  cat("scale reduction factor on split chains (at convergence, Rhat = 1).\n")

  steps <- build_next_steps(x)
  cat("\nNext steps:\n")
  for (s in steps) cat("  - ", s, "\n", sep = "")
  cat("Use `how_to_cite(fit)` for a citation-ready model description.\n")

  invisible(x)
}


#' How many draws a fit holds and how the sampler was run
#'
#' The sampler's own arguments are the record of what was asked for.
#' `posterior::niterations()` counts the draws kept after warmup, and
#' reporting it as `iter` understated the run: `print()` said 1000
#' where `summary()` said 2000 for one fit. It is used only when the
#' fit recorded no arguments.
#'
#' @param object A fitted `mvgam` object
#' @return List with `nchains`, `niter`, `nwarmup` (`NULL` when
#'   unrecorded), `nthin` and `ndraws`
#' @noRd
draw_counts <- function(object) {
  draws <- posterior::as_draws(object$fit)
  sampler <- mvgam_sampler_inheritance(object)
  list(
    nchains = sampler$chains %||% posterior::nchains(draws),
    niter = sampler$iter %||% posterior::niterations(draws),
    nwarmup = sampler$warmup,
    nthin = sampler$thin %||% 1L,
    ndraws = posterior::ndraws(draws)
  )
}


#' The draws line `print()` and `summary()` show
#'
#' brms's two-line layout. A fit that recorded no warmup says so.
#'
#' @param x The list `draw_counts()` returns, or a summary holding it
#' @return A single string spanning two lines
#' @noRd
format_draws_line <- function(x) {
  paste0(
    "  Draws: ", x$nchains, " chains, each with iter = ", x$niter,
    "; warmup = ", x$nwarmup %||% "unrecorded",
    "; thin = ", x$nthin %||% 1L, "; \n",
    "         total post-warmup draws = ", x$ndraws
  )
}


#' The trend line a summary prints
#'
#' A summary object carries no trend metadata, which
#' `printed_trend_label()` needs. `summary()` stores the label that
#' helper renders from the fit. The bare type
#' gives `AR` for a plain AR(1), for `AR(p = 3)` and for
#' `AR(p = 1, ma = TRUE)` alike.
#'
#' A trend formula carrying only a constructor reduces to `~0`, and
#' one with an intercept alone to `~1`. Neither names a predictor a
#' reader acts on, and both are left out.
#'
#' @param x An `mvgam_summary` object
#' @return Character scalar, empty when the fit carries no trend
#' @noRd
format_trend_line <- function(x) {
  if (is.null(x$trend_model)) {
    return("")
  }
  out <- paste0(" Trends: ", x$trend_label %||% x$trend_model)
  predictors <- x$trend_predictors
  if (!is.null(predictors)) {
    out <- paste0(out, "; formula: ", predictors)
  }
  paste0(out, " ")
}


#' The predictors of a trend formula, as `print()` and `summary()` show
#' them
#'
#' The trend formula as the user wrote it, less its trend constructor,
#' which the trend line names. mvgam stores a rewritten copy for
#' fitting, where `by = lv_axis()` becomes `by = series` and a `- 1` is
#' added, and that copy is the fallback for a fit that records no
#' user formula. A formula reduced to `~0` or `~1` names no predictor
#' a reader acts on.
#'
#' @param object An `mvgam` object
#' @return A one-sided formula as a single string, or `NULL`
#' @noRd
trend_predictors <- function(object) {
  trend_formula <- if (inherits(object$trend_call, "formula")) {
    parse_base_formula_safe(object$trend_call, mvgam_trend_registry())
  } else {
    object$trend_formula
  }
  if (is.null(trend_formula)) {
    return(NULL)
  }
  rhs <- if (length(trend_formula) == 3L) {
    stats::formula(stats::delete.response(stats::terms(trend_formula)))
  } else {
    trend_formula
  }
  out <- paste(trimws(format(rhs)), collapse = " ")
  if (grepl("^~\\s*[01]\\s*$", out)) NULL else out
}


# Adaptive "Next steps" list for `print.summary.mvgam`. Builds a
# candidate set, gated on what the fit can actually offer, then
# caps the list at five entries by priority so the suggestion
# block stays scannable. Universal entries (pp_check, loo) always
# appear; model-specific entries (shared_variation, residual_cor,
# conditional_effects) only appear when relevant.
#'@noRd
build_next_steps <- function(x) {
  has_factors <- !is.null(x$loadings) || !is.null(x$loadings_prior)
  trend_model <- x$trend_model %||% ""
  has_cor_trend <- has_factors || isTRUE(x$trend_has_cor)
  # A covariate may enter either predictor. The observation side
  # always holds an intercept row among its fixed effects, and the
  # trend side holds none.
  covariate_blocks <- c("smooth", "gp", "mo", "trend_fixed",
                        "trend_smooth", "trend_gp", "trend_mo")
  has_covariates <- (!is.null(x$fixed) && nrow(x$fixed) > 1L) ||
    any(covariate_blocks %in% names(x))
  forecastable <- !grepl("^ZMVN", trend_model)
  # A detection process decides the closure-unit suggestions. `mvn()`,
  # `mvt()` and `diri()` share the closure-unit layout and model no
  # detection. Testing the layout sent readers of their summaries to
  # `pp_check(type = "fit_stat")`, which refuses them.
  is_cu <- !is.null(x$family) && is_closure_unit_family(x$family)
  has_latent_state <- "latent_state" %in% family_predict_types(x$family)
  # Candidates in priority order; first five matching entries
  # populate the printed list. Closure-unit (occ / nmix) fits get
  # routed to the family-aware GOF + latent-state surfaces
  # (chi-squared / Freeman-Tukey discrepancy, latent psi or N
  # posterior, per-(series, time) ribbon plot) which are the
  # ecologically meaningful next steps; non-closure-unit fits get
  # the standard mvgam suggestion set.
  pp_text <- if (is_cu) {
    "`pp_check(fit, type = \"fit_stat\")`: closure-unit GOF p-value"
  } else {
    "`pp_check(fit)`: posterior predictive checks"
  }
  candidates <- list(
    list(when = TRUE, text = pp_text),
    list(when = has_latent_state,
         text = paste0(
           "`hindcast(fit, type = \"latent_state\")`: ",
           "psi (occ) or N (nmix)"
         )),
    list(when = grepl("^VAR", trend_model),
         text = paste0(
           "`posterior_transition_matrix(fit)` / `irf(fit)`: ",
           "lagged effects between series"
         )),
    list(when = has_factors,
         text = paste0(
           "`shared_variation(fit)`: ",
           "factor-implied Delta = Z Sigma Z' (rotation-invariant)"
         )),
    list(when = has_cor_trend,
         text = "`residual_cor(fit)`: implied cross-series correlations"),
    list(when = forecastable,
         text = "`forecast(fit, newdata = ...)`: out-of-sample forecasts"),
    # PSIS-LOO leaves out an observation the latent state was fitted
    # to, and a fit with temporal dynamics is compared by refitting on
    # shorter series.
    list(when = TRUE,
         text = if (nzchar(trend_model) && forecastable) {
           "`lfo_cv(fit)`: leave-future-out model comparison"
         } else {
           "`loo(fit)` / `loo_compare(...)`: model fit + comparison"
         }),
    list(when = has_covariates,
         text = "`conditional_effects(fit)`: covariate effects"),
    # The drawn form of the same call, offered on the same terms.
    list(when = has_latent_state,
         text = "`plot(hindcast(fit, type = \"latent_state\"))`: state ribbon")
  )
  texts <- vapply(
    Filter(function(c) isTRUE(c$when), candidates),
    function(c) c$text, character(1L)
  )
  utils::head(texts, 6L)
}


# ==============================================================================
# MULTIPLE IMPUTATION SUMMARY METHOD
# ==============================================================================

#' Summary method for multiple imputation mvgam models
#'
#' @description
#' Provides posterior summary statistics for multiple imputation mvgam
#' models, with additional diagnostics about the imputation process and
#' combined posterior draws.
#'
#' @param object An object of class \code{mvgam_pooled}.
#' @param probs Numeric vector of length 2 specifying quantile
#'   probabilities for credible intervals. Default is \code{c(0.025,
#'   0.975)} for 95% intervals.
#' @param robust Logical; if \code{TRUE}, the median and MAD measure
#'   central tendency and spread. The default \code{FALSE} uses the
#'   mean and SD.
#' @param ... Additional arguments passed to \code{\link{summary.mvgam}}.
#'
#' @return An object of class \code{c("mvgam_pooled_summary",
#'   "mvgam_summary")} containing standard summary components plus
#'   multiple imputation diagnostics.
#'
#' @details
#' This method extends the standard \code{summary.mvgam} output with
#' multiple imputation specific information:
#' \itemize{
#'   \item{Number of imputations}
#'   \item{Total posterior draws (combined across all imputations)}
#'   \item{Per-imputation convergence summaries}
#'   \item{Information about the combination method}
#' }
#'
#' The combined posteriors are created using
#' \code{rstan::sflist2stanfit()}, which concatenates posterior draws
#' from all imputed datasets at the Stan level. Parameter estimates
#' reflect uncertainty from both the model and the imputation process.
#'
#' @seealso \code{\link{summary.mvgam}}, \code{\link{mvgam_multiple}}
#'
#' @export
summary.mvgam_pooled <- function(object, probs = c(0.025, 0.975),
                                  robust = FALSE,
                                  ...) {
  # Input validation
  checkmate::assert_class(object, "mvgam_pooled")
  checkmate::assert_numeric(probs, len = 2, lower = 0, upper = 1)
  checkmate::assert_true(probs[1] < probs[2])
  checkmate::assert_logical(robust, len = 1)

  # Call parent method to get standard summary
  base_summary <- NextMethod("summary")

  # `pool_mvgam_fits()` is the only constructor of this class and
  # writes all three.
  individual_fits <- attr(object, "individual_fits")
  n_imputations <- attr(object, "n_imputations")
  combination_method <- attr(object, "combination_method")

  # Calculate total draws across all imputations
  total_draws <- posterior::ndraws(posterior::as_draws(object$fit))
  draws_per_imp <- posterior::ndraws(
    posterior::as_draws(individual_fits[[1L]]$fit)
  )

  # Extract per-imputation convergence diagnostics
  imp_convergence <- lapply(seq_along(individual_fits), function(i) {
    draws <- posterior::as_draws(individual_fits[[i]]$fit)

    # The worst R-hat and smallest ESS reported here are the numbers a
    # reader attributes to a model parameter. Taking them over every
    # saved variable lets one latent state at one occasion set both,
    # and a trend carries thousands of those. `mvgam_user_pars()` is
    # the projection every user-facing reader shares.
    keep <- intersect(
      unname(mvgam_user_pars(individual_fits[[i]])),
      posterior::variables(draws)
    )
    if (length(keep) > 0L) {
      draws <- posterior::subset_draws(draws, variable = keep)
    }
    summ <- without_ess_cap_notice(posterior::summarise_draws(
      draws,
      posterior::default_convergence_measures()
    ))

    list(
      imputation = i,
      max_rhat = max(summ$rhat, na.rm = TRUE),
      min_bulk_ess = min(summ$ess_bulk, na.rm = TRUE),
      min_tail_ess = min(summ$ess_tail, na.rm = TRUE),
      n_params = nrow(summ)
    )
  })

  # Calculate summary statistics across imputations
  max_rhat_across_imps <- max(
    sapply(imp_convergence, function(x) x$max_rhat, USE.NAMES = FALSE),
    na.rm = TRUE
  )
  min_ess_across_imps <- min(
    sapply(imp_convergence, function(x) x$min_bulk_ess,
           USE.NAMES = FALSE),
    na.rm = TRUE
  )

  # Add MI diagnostics to summary object
  base_summary$mi_diagnostics <- list(
    n_imputations = n_imputations,
    total_draws = total_draws,
    draws_per_imputation = draws_per_imp,
    combination_method = combination_method,
    per_imputation_convergence = imp_convergence,
    max_rhat_across_imputations = max_rhat_across_imps,
    min_ess_across_imputations = min_ess_across_imps
  )

  # Set summary class
  class(base_summary) <- c("mvgam_pooled_summary", "mvgam_summary")

  return(base_summary)
}

# ==============================================================================
# MULTIPLE IMPUTATION PRINT METHOD
# ==============================================================================

#' Print method for multiple imputation summary objects
#'
#' @description
#' Prints a summary of a multiple imputation mvgam model, including
#' standard parameter estimates and multiple imputation diagnostics.
#'
#' @param x An object of class \code{mvgam_pooled_summary}.
#' @param digits Integer indicating number of decimal places. Default 2.
#' @param ... Additional arguments passed to
#'   \code{\link{print.mvgam_summary}}.
#'
#' @return The \code{mvgam_pooled_summary} object is returned
#'   invisibly.
#'
#' @details
#' This method extends \code{print.mvgam_summary} by adding a footer
#' section with multiple imputation diagnostics. The combined
#' posteriors are created using \code{rstan::sflist2stanfit()}, which
#' concatenates draws from all imputations at the Stan level.
#'
#' Diagnostics include:
#' \itemize{
#'   \item{Number of imputations and total draws}
#'   \item{Per-imputation convergence summaries (Rhat, ESS)}
#'   \item{Overall convergence metrics across imputations}
#' }
#'
#' @seealso \code{\link{print.mvgam_summary}},
#'   \code{\link{summary.mvgam_pooled}}
#'
#' @export
print.mvgam_pooled_summary <- function(x, digits = 2, ...) {
  # Input validation
  checkmate::assert_class(x, "mvgam_pooled_summary")
  checkmate::assert_int(digits, lower = 0)

  # Print standard summary using parent method
  NextMethod()

  # Add MI diagnostics section if available
  if (!is.null(x$mi_diagnostics)) {
    cat("\n")
    cat(strrep("=", 70), "\n")
    cat("Multiple Imputation Diagnostics\n")
    cat(strrep("=", 70), "\n\n")

    mi <- x$mi_diagnostics

    # Basic imputation information
    cat("  Number of imputations:", mi$n_imputations, "\n")
    cat("  Total posterior draws:", mi$total_draws, "\n")

    if (!is.na(mi$draws_per_imputation)) {
      cat("  Draws per imputation:", mi$draws_per_imputation, "\n")
    }

    cat("  Combination method:", mi$combination_method, "\n\n")

    # Overall convergence across imputations
    cat("  Convergence Summary Across Imputations:\n")

    if (!is.infinite(mi$max_rhat_across_imputations) &&
        !is.na(mi$max_rhat_across_imputations)) {
      cat(sprintf(
        "    Maximum Rhat: %.3f\n",
        mi$max_rhat_across_imputations
      ))
    }

    if (!is.infinite(mi$min_ess_across_imputations) &&
        !is.na(mi$min_ess_across_imputations)) {
      cat(sprintf(
        "    Minimum Bulk ESS: %.0f\n",
        mi$min_ess_across_imputations
      ))
    }

    # Per-imputation table if available
    if (!is.null(mi$per_imputation_convergence) &&
        length(mi$per_imputation_convergence) > 0) {

      cat("\n  Per-Imputation Convergence:\n")

      # Build table
      imp_df <- do.call(rbind, lapply(
        mi$per_imputation_convergence,
        function(imp) {
          data.frame(
            Imputation = imp$imputation,
            Max_Rhat = sprintf("%.3f", imp$max_rhat),
            Min_Bulk_ESS = sprintf("%.0f", imp$min_bulk_ess),
            Min_Tail_ESS = sprintf("%.0f", imp$min_tail_ess),
            stringsAsFactors = FALSE
          )
        }
      ))

      # Print table with proper alignment
      cat("    ")
      print(imp_df, row.names = FALSE, right = TRUE)
    }

    cat("\n")
    cat(strrep("-", 70), "\n")
    cat("Note: Parameter estimates combine draws from all ",
        "imputations.\n", sep = "")
    cat("      Uncertainty reflects both model and imputation ",
        "process.\n", sep = "")
    cat(strrep("=", 70), "\n")
  }

  invisible(x)
}
