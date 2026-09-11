#' Detect whether a fitted mvgam carries a VAR(1) latent trend
#'
#' Inspects `object$trend_components$types` (the canonical trend-type
#' surface populated at fit time) and returns the matching trend-type
#' string for VAR variants, or `NULL` otherwise. Used by
#' `irf.mvgam()`, `fevd.mvgam()`, and `stability.mvgam()` to gate
#' VAR-only downstream calculations behind a single shared check.
#'
#' @param object A fitted `mvgam` object.
#' @return The trend-type string when the fit carries a VAR, one of
#'   `var_trend_types`; `NULL` otherwise.
#' @noRd
var_trend_types <- c("VAR", "VAR1", "VARcor", "VAR1cor")

detect_var_trend <- function(object) {
  # Read through the shared resolver rather than the first of the
  # three places a trend type is recorded. Reading only
  # `trend_components$types` refused `irf()`, `fevd()`,
  # `stability()` and `posterior_transition_matrix()` on a fit
  # whose type was recoverable from its metadata, while
  # `summary()` named it correctly from the same object.
  trend_type <- get_trend_type(object)
  if (is.null(trend_type) || is.na(trend_type)) {
    return(NULL)
  }
  if (!trend_type %in% var_trend_types) {
    return(NULL)
  }
  trend_type
}

#' Assert that a fitted mvgam is a VAR(1) and return the trend type
#'
#' Thin wrapper around `detect_var_trend()` that errors with a
#' consistent message when the gate fails. The `surface` argument
#' lets the caller name the function the user invoked
#' (`"irf()"`, `"fevd()"`, `"stability()"`) so the error points the
#' right place.
#'
#' @param object A fitted `mvgam` object.
#' @param surface Character; the user-facing function name to cite
#'   in the error message.
#' @return The trend-type string on success; stops on failure.
#' @noRd
assert_var_trend <- function(object, surface) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_string(surface, min.chars = 1L)
  trend_type <- detect_var_trend(object)
  if (is.null(trend_type)) {
    stop(insight::format_error(c(
      paste0("'", surface, "' requires a VAR(1) latent trend."),
      x = paste0(
        "This fit's trend type is '",
        object$trend_components$types[1L] %||% "<none>",
        "'."
      ),
      i = paste0(
        "Refit with 'trend_formula = ~ VAR(p = 1)' to use ",
        "'", surface, "'."
      )
    )))
  }
  trend_type
}

# Internal: walk a VAR posterior one draw at a time.
#
# `irf()` and `fevd()` ask different questions of the same object.
# Each draw is one companion matrix and one innovation covariance,
# assembled the same way, and only the kernel reading them differs.
# Assembling the draw in both places let the two surfaces drift into
# describing different draws by the same index.
#' @noRd
var_draw_surfaces <- function(var_post, kernel, future) {
  mvgam_maybe_future_lapply(
    var_post$ndraws,
    function(draw) {
      kernel(list(
        K = var_post$K,
        A = var_post$A[draw, , , drop = TRUE],
        Sigma = var_post$Sigma[draw, , , drop = TRUE],
        p = 1L
      ))
    },
    future = future
  )
}


#' Extract VAR(1) posterior draws of (A, Sigma) from a fitted mvgam
#'
#' Pulls the per-draw coefficient matrix `A_trend[1, , ]` and
#' innovation covariance `Sigma_trend` from `object$fit` and returns
#' them as 3-D arrays sized `[ndraws, K, K]`. The `[1]` index on
#' `A_trend` selects the (currently only) supported VAR lag (`p = 1`);
#' higher-lag VAR is rejected at the constructor.
#'
#' `K` is the number of latent series in the VAR. For both factor
#' and non-factor VARs the Stan-side dimension is `N_lv_trend` (for
#' a non-factor VAR over the raw series, the compiler sets
#' `N_lv_trend = N_series_trend`). We read this directly from the
#' standata cached on the fit so the value is always what the Stan
#' model actually compiled with.
#'
#' @param object A fitted `mvgam` object whose trend is a VAR(1).
#' @return A list with elements:
#'   * `A`: `[ndraws, K, K]` array of VAR coefficient draws.
#'   * `Sigma`: `[ndraws, K, K]` array of innovation-covariance
#'     draws.
#'   * `K`: integer, the VAR dimension.
#'   * `ndraws`: integer, the number of posterior draws.
#' @noRd
extract_var_posterior <- function(object, ndraws = NULL,
                                  draw_ids = NULL) {
  checkmate::assert_class(object, "mvgam")
  draws_mat <- posterior::as_draws_matrix(object$fit)
  # Impulse responses and variance decompositions are built one
  # transition matrix per draw, so their cost and their size both grow
  # with the number of draws multiplied by the square of the number of
  # processes. A wide VAR is therefore worth summarising from a subset,
  # and the same subset has to serve the coefficients and the
  # innovation covariance or a response would be built from a matrix
  # pair no single draw produced.
  draws_mat <- subset_draws_rows(draws_mat, ndraws, draw_ids)
  # Reads the raw `A_trend`. For factor VAR with Heaps QR
  # identification the generated-quantities block also emits
  # `A_trend_tilde = Q A_trend Q'`; callers that want the
  # QR-identified surface should read `A_trend_tilde` and rotate
  # `Sigma_trend` by the same Q.
  # `N_lv_trend` is the Stan-side VAR dimension for every code
  # path: factor VARs set it to the number of factors, non-factor
  # VARs set it equal to `N_series_trend`. Reading from standata
  # guarantees the value matches what the compiled model uses.
  K <- object$standata$N_lv_trend
  if (is.null(K)) {
    stop(insight::format_error(c(
      "Cannot determine VAR dimension from fit.",
      x = "'standata$N_lv_trend' is missing.",
      i = "Did the fit complete fully? Try refitting."
    )))
  }
  K <- as.integer(K)
  checkmate::assert_int(K, lower = 1L)

  ndraws <- nrow(draws_mat)

  # `A_trend` is declared as `array[size(A_trend)] matrix[K, K]` in
  # Stan, so posterior column names take the form `A_trend[lag,i,j]`.
  # Restrict to the single supported lag (lag = 1) up front.
  A <- extract_indexed_array_2d(
    draws_mat, "A_trend", K, K,
    prefix_ids   = 1L,
    required_for = "A_trend (VAR(1) coefficient extraction)"
  )
  Sigma <- extract_indexed_array_2d(
    draws_mat, "Sigma_trend", K, K,
    required_for = "Sigma_trend (VAR innovation covariance)"
  )

  list(A = A, Sigma = Sigma, K = K, ndraws = ndraws)
}

#' Optionally parallelise a per-draw VAR post-processing lapply
#'
#' Internal helper shared by `irf.mvgam()`, `fevd.mvgam()` and
#' `stability.mvgam()` so their per-draw compute loops can run under
#' a caller-configured `future` plan without duplicating the
#' opt-in check. Mirrors the pattern used inside `.fit_model_rstan`
#' (`R/backends.R:390-406`), so a session already using `future` for
#' chain fitting gets the same behaviour for VAR post-processing.
#'
#' When `future = FALSE` (the default) the loop reduces to base
#' `lapply()`, so there is no `future` dependency needed and no
#' runtime overhead. When `future = TRUE`, `future::future()`
#' schedules each draw on whatever `plan()` the caller set
#' (defaulting to `sequential()`, which behaves like `lapply` with
#' negligible extra overhead); `plan(multisession, workers = N)`
#' before the call spreads draws across `N` R processes. Errors
#' from any draw propagate.
#'
#' `future` is a Suggests-only dependency, so the helper errors
#' with an install hint if the caller asks for parallel execution
#' without the package installed.
#'
#' @param n Integer. Number of draws (loop length).
#' @param fn Function taking a single integer index and returning
#'   the per-draw result.
#' @param future Logical. `FALSE` uses base `lapply()`; `TRUE`
#'   schedules draws with `future::future()` and gathers with
#'   `future::value()`.
#' @return A list of length `n` with the per-draw results.
#' @noRd
mvgam_maybe_future_lapply <- function(n, fn, future = FALSE) {
  checkmate::assert_int(n, lower = 1L)
  checkmate::assert_function(fn)
  checkmate::assert_flag(future)
  if (!future) {
    return(lapply(seq_len(n), fn))
  }
  # Matches the `require_package("future")` pattern the fit
  # backends use for optional runtime dependencies
  # (`R/backends.R:969`) so the error message is consistent
  # across mvgam functions that share a Suggests dependency.
  require_package("future")
  futures <- vector("list", n)
  for (i in seq_len(n)) {
    local_i <- i
    futures[[i]] <- future::future(fn(local_i), seed = TRUE)
  }
  lapply(futures, future::value)
}

#' Validate a subset of process indices for `plot.mvgam_irf()` /
#' `plot.mvgam_fevd()` selectors. Accepts `NULL` (returns the full
#' `1:n_proc` set) or an integer vector; verifies bounds and
#' returns a sorted, de-duplicated integer vector.
#'
#' @param ids `NULL` or an integer vector.
#' @param n_proc Total number of latent processes on the fit.
#' @param arg The user-facing argument name (for error messages).
#' @return Integer vector of indices to keep.
#' @noRd
validate_var_plot_ids <- function(ids, n_proc, arg) {
  if (is.null(ids)) return(seq_len(n_proc))
  checkmate::assert_integerish(
    ids, lower = 1L, upper = n_proc, min.len = 1L, any.missing = FALSE,
    .var.name = arg
  )
  sort(unique(as.integer(ids)))
}

#' Posterior transition matrix for a VAR trend
#'
#' @description
#' Extract the VAR(1) transition matrix $A$ from a fitted
#' `mvgam` object, either as a per-cell posterior summary
#' (default) or as raw per-draw values. Row $i$ of $A$ gives the
#' linear dependence of latent process $i$ at time $t$ on the
#' vector of processes at time $t - 1$; the diagonal is
#' self-persistence, off-diagonals encode cross-outcome dependence
#' at lag one.
#'
#' @details
#' The extractor dispatches on whether the fit is hierarchical
#' (`VAR(gr = <var>, ...)`) or single-panel.
#'
#' * On a **single-panel VAR** the routine reads the fitted
#'   `A_trend` array.
#' * On a **hierarchical VAR** with `group = <int>` or
#'   `group = <name>` it reads the panel-specific matrix
#'   `A_group_trend[c, 1, , ]`, i.e. the transition matrix for
#'   the requested country / patch / hospital.
#' * On a **hierarchical VAR** with `group = NULL` it synthesises
#'   the shrinkage target from the hyperparameters:
#'   diagonal cells take `Amu_trend[1, 1]`, off-diagonal cells
#'   take `Amu_trend[2, 1]`. This is the mean the sampler pulls
#'   every panel's $A_c$ toward, so it summarises the pool
#'   without reading any single country
#'   (Savage [2016](https://rpubs.com/jimsavage/hierarchical_var)).
#'
#' With `summary = TRUE` (default) the per-cell summary uses the
#' same machinery as `residual_cor()`, so the printed and
#' plotted output stays consistent across the two extractors:
#' point estimates default to the posterior mean; intervals use
#' `probs` on the raw draws; effective sample sizes come from
#' `posterior::ess_basic()`; and
#' `prob_positive` / `prob_negative` / `prob_nonzero` count the
#' fraction of draws with each entry above or below zero. See
#' the argument documentation for the median switch.
#'
#' Only VAR(1) fits are supported; the `A_trend` slot has a
#' single lag dimension pinned to one for compatibility with
#' mvgam's `irf()`, `fevd()` and `stability()` methods.
#'
#' @param object A fitted `mvgam` object whose trend is a
#'   VAR(1).
#' @param groups Which panels of a hierarchical VAR fit to
#'   return. `NULL` (the default) gives the global shrinkage
#'   target built from `Amu_trend`, and is the only setting a
#'   non-hierarchical fit accepts. Name one panel, by level name
#'   or by integer index into the fitted grouping factor's levels
#'   as stored on `object$data[[gr]]`, and that panel's matrix is
#'   returned on its own. Name several, or pass `"all"`, and the
#'   result is a named list of `mvgam_var_matrix` objects classed
#'   as `mvgam_var_matrix_list`, so `plot()` produces a faceted
#'   panel (heatmap grid or per-outcome self-persistence
#'   pointrange, see [plot.mvgam_var_matrix_list()]).
#' @param summary Logical. `TRUE` (default) returns an
#'   `mvgam_var_matrix` object with per-cell point estimates,
#'   credible intervals, standard errors, effective sample
#'   sizes and sign probabilities. `FALSE` returns the raw
#'   `[ndraws, K, K]` posterior array with dims 2-3 labelled by
#'   the outcome names (levels of `subgr` on a hierarchical fit,
#'   `series_names` on a single-panel fit) for downstream custom
#'   summaries.
#' @param robust Logical. When `summary = TRUE`, use the median
#'   as the per-cell point estimate; defaults to the posterior
#'   mean when `FALSE`.
#' @param probs Length-2 numeric vector of quantile probabilities
#'   for the per-cell credible interval when `summary = TRUE`.
#'   Defaults to `c(0.025, 0.975)`.
#'
#' @return One matrix, or a list of them when `groups` names more
#'   than one panel. With `summary = TRUE`, an object of class
#'   `mvgam_var_matrix` with slots `A`, `A_se`, `A_lower`,
#'   `A_upper`, `A_ess`, `prob_positive`, `prob_negative`,
#'   `prob_nonzero`, `n_series`, `series_names`, `group_label`,
#'   `probs`. Has `print()` and `plot()` methods. With
#'   `summary = FALSE`, a `[ndraws, K, K]` numeric array with
#'   dims 2-3 labelled by the outcome names.
#'
#' @references Savage J (2016). Hierarchical Vector
#'   Autoregression. RPubs, 27 November 2016.
#'   \url{https://rpubs.com/jimsavage/hierarchical_var}
#'
#' Heaps SE (2023). Enforcing stationarity through the prior in
#'   vector autoregressions.
#'   *Journal of Computational and Graphical Statistics*
#'   32(1), 74-83.
#'   \doi{10.1080/10618600.2022.2079648}
#'
#' @seealso [VAR()], [residual_cor()], [irf()], [fevd()],
#'   [stability()], [print.mvgam_var_matrix()],
#'   [plot.mvgam_var_matrix()].
#' @author Nicholas J Clark
#' @export
posterior_transition_matrix <- function(object, groups = NULL,
                                         summary = TRUE,
                                         robust = FALSE,
                                         probs = c(0.025, 0.975)) {
  checkmate::assert_flag(summary)
  checkmate::assert_flag(robust)
  checkmate::assert_numeric(probs, len = 2L, lower = 0, upper = 1,
                            any.missing = FALSE, unique = TRUE)
  probs <- sort(probs)

  one_matrix <- function(group) {
    arr <- extract_transition_matrix_draws(object, group)
    if (!summary) return(arr)
    finalise_transition_matrix(
      arr,
      group_label = attr(arr, "group_label"),
      robust      = robust,
      probs       = probs
    )
  }

  # No selection asks for the global shrinkage target, which is also
  # the only matrix a non-hierarchical fit has.
  if (is.null(groups)) {
    return(one_matrix(NULL))
  }

  is_hier <- is_hierarchical_var(colnames(
    posterior::as_draws_matrix(object$fit)
  ))
  if (!is_hier) {
    stop(insight::format_error(c(
      "'groups' only applies to hierarchical VAR fits.",
      i = "Leave 'groups' at NULL on a non-hierarchical fit."
    )))
  }

  target_names <- resolve_transition_matrix_groups(object, groups)

  # Naming one panel answers with that panel's matrix. `"all"` is
  # plural whatever the fit holds, so it keeps the list shape that
  # `plot.mvgam_var_matrix_list()` facets.
  if (length(target_names) == 1L && !identical(groups, "all")) {
    return(one_matrix(target_names))
  }

  out <- lapply(target_names, one_matrix)
  names(out) <- target_names
  structure(out, class = "mvgam_var_matrix_list")
}

#' Resolve the `groups` argument of `posterior_transition_matrix()`
#' to a character vector of group names.
#' @noRd
resolve_transition_matrix_groups <- function(object, groups) {
  gr_var <- first_trend_spec(object)$gr
  all_levels <- lookup_factor_levels(
    object$data, gr_var,
    object$standata$N_groups_trend %||% 1L,
    prefix = "group"
  )
  if (identical(groups, "all")) return(all_levels)
  if (is.numeric(groups)) {
    checkmate::assert_integerish(
      groups, lower = 1L, upper = length(all_levels),
      any.missing = FALSE
    )
    return(all_levels[as.integer(groups)])
  }
  checkmate::assert_character(groups, min.chars = 1L,
                              any.missing = FALSE)
  bad <- setdiff(groups, all_levels)
  if (length(bad)) {
    stop(insight::format_error(c(
      paste0("Group '", bad[1L], "' not found."),
      i = paste0("Available: ",
                 paste(all_levels, collapse = ", "), ".")
    )))
  }
  groups
}

#' Extract raw `[ndraws, K, K]` posterior draws of the VAR(1)
#' transition matrix from a fitted mvgam. Returned array carries
#' `attr(., "group_label")` (`"global"`, a country name, or
#' `"single-panel"`) so downstream summarisers can label the
#' object.
#' @noRd
extract_transition_matrix_draws <- function(object, group) {
  assert_var_trend(object, surface = "posterior_transition_matrix()")
  draws_mat <- posterior::as_draws_matrix(object$fit)
  all_cols <- colnames(draws_mat)
  is_hier <- is_hierarchical_var(all_cols)

  if (is_hier && is.null(group)) {
    K <- as.integer(object$standata$N_subgroups_trend %||% 3L)
    labs <- subgroup_labels(object, K)
    diag_col <- "Amu_trend[1,1]"
    off_col <- "Amu_trend[2,1]"
    missing_cols <- setdiff(c(diag_col, off_col), all_cols)
    if (length(missing_cols)) {
      stop(insight::format_error(c(
        paste0("Posterior parameter '", missing_cols[1L],
               "' not found."),
        i = "Required to assemble the global 'Amu_trend' A matrix."
      )))
    }
    ndraws <- nrow(draws_mat)
    diag_draws <- as.numeric(draws_mat[, diag_col])
    off_draws <- as.numeric(draws_mat[, off_col])
    out <- vapply(seq_len(ndraws), function(d) {
      m <- matrix(off_draws[d], K, K)
      diag(m) <- diag_draws[d]
      as.numeric(m)
    }, numeric(K * K))
    out <- aperm(array(out, dim = c(K, K, ndraws)), c(3, 1, 2))
    dimnames(out) <- list(NULL, labs, labs)
    attr(out, "group_label") <- "global"
    return(out)
  }

  if (is_hier) {
    group_int <- resolve_group_index(object, group)
    K <- resolve_var_dim(
      object, "N_subgroups_trend",
      paste0("^A_group_trend\\[", group_int, ",1,"), all_cols
    )
    out <- extract_indexed_array_2d(
      draws_mat, "A_group_trend", K, K,
      prefix_ids   = c(group_int, 1L),
      labels       = subgroup_labels(object, K),
      required_for = "a per-group VAR transition matrix"
    )
    gr_var <- first_trend_spec(object)$gr
    gr_labels <- lookup_factor_levels(object$data, gr_var,
                                       object$standata$N_groups_trend %||%
                                         length(unique(object$data[[gr_var]])),
                                       prefix = "group")
    attr(out, "group_label") <- gr_labels[group_int]
    return(out)
  }

  # Non-hierarchical VAR: single A_trend, lag pinned to one
  K <- resolve_var_dim(object, "N_lv_trend",
                       "^A_trend\\[1,", all_cols)
  # `K` counts latent processes, which are the observed series only
  # when the fit is not a factor model, so the axes take the
  # `process_<k>` labels `irf()`, `fevd()` and `stability()` give
  # the same quantity rather than a series name that would be wrong
  # for a factor VAR.
  labs <- paste0("process_", seq_len(K))
  out <- extract_indexed_array_2d(
    draws_mat, "A_trend", K, K,
    prefix_ids   = 1L,
    labels       = labs[seq_len(K)],
    required_for = "the VAR transition matrix"
  )
  attr(out, "group_label") <- "single-panel"
  out
}

#' Reduce a `[ndraws, K, K]` VAR transition matrix draws array to
#' an `mvgam_var_matrix` per-cell summary. Reuses
#' `summarise_unconstrained_array()` (`R/residual_cor.R`) for
#' point + SE + interval + ESS so the numerics match the
#' `mvgam_residcor` covariance summariser cell-for-cell. Adds
#' `prob_positive` / `prob_negative` / `prob_nonzero` matrices
#' with the same semantic as their `mvgam_residcor` counterparts.
#' @noRd
finalise_transition_matrix <- function(arr, group_label,
                                        robust, probs) {
  series_names <- dimnames(arr)[[2L]] %||%
    paste0("process_", seq_len(dim(arr)[2L]))
  stats <- summarise_unconstrained_array(
    arr, robust = robust, probs = probs, series_names = series_names
  )
  p <- dim(arr)[2L]
  prob_pos <- matrix(0, p, p)
  prob_neg <- matrix(0, p, p)
  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      x <- arr[, i, j]
      prob_pos[i, j] <- mean(x > 0)
      prob_neg[i, j] <- mean(x < 0)
    }
  }
  prob_nz <- pmax(prob_pos, prob_neg)
  rownames(prob_pos) <- colnames(prob_pos) <- series_names
  rownames(prob_neg) <- colnames(prob_neg) <- series_names
  rownames(prob_nz)  <- colnames(prob_nz)  <- series_names
  structure(
    list(
      A             = stats$point,
      A_se          = stats$se,
      A_lower       = stats$lower,
      A_upper       = stats$upper,
      A_ess         = stats$ess,
      prob_positive = prob_pos,
      prob_negative = prob_neg,
      prob_nonzero  = prob_nz,
      n_series      = p,
      series_names  = series_names,
      group_label   = group_label,
      probs         = probs
    ),
    class = "mvgam_var_matrix"
  )
}

#' Print method for `mvgam_var_matrix`
#'
#' Prints a compact header (group label, dimension, mean
#' off-diagonal absolute entry magnitude) and the posterior
#' point-estimate transition matrix rounded to `digits`. Reminds
#' the reader that the per-cell credible interval is available in
#' the `A_lower` / `A_upper` slots. Mirrors the layout of
#' [print.mvgam_residcor()] so a user reading both objects sees a
#' consistent header.
#'
#' @param x An `mvgam_var_matrix` object returned by
#'   [posterior_transition_matrix()].
#' @param digits Integer. Digits used when rounding the printed
#'   matrix.
#' @param ... Currently unused.
#'
#' @return `x`, invisibly, after side-effect printing.
#'
#' @seealso [posterior_transition_matrix()],
#'   [plot.mvgam_var_matrix()],
#'   [print.mvgam_var_matrix_list()].
#' @author Nicholas J Clark
#' @method print mvgam_var_matrix
#' @export
print.mvgam_var_matrix <- function(x, digits = 2L, ...) {
  cat("VAR(1) transition matrix from an mvgam fit\n")
  cat("  Group      : ", x$group_label %||% "(unlabelled)", "\n",
      sep = "")
  cat("  Dimension  : ", x$n_series, " x ", x$n_series, "\n",
      sep = "")
  off_diag_mask <- !diag(TRUE, x$n_series)
  cat(
    sprintf(
      "  Mean |A_ij| (i != j): %.2f\n",
      mean(abs(x$A[off_diag_mask]))
    )
  )
  cat("  Point estimate:\n")
  print(round(x$A, digits))
  cat("  ", sprintf("%.0f%% CI", 100 * (x$probs[2L] - x$probs[1L])),
      " on each cell available in `A_lower` / `A_upper`.\n",
      sep = "")
  invisible(x)
}

#' Plot method for `mvgam_var_matrix`
#'
#' Renders the point-estimate transition matrix as a diverging
#' heatmap. Uses the same `mvgam_diverging_scale()` palette and
#' `mvgam_theme()` as [plot.mvgam_residcor()], so a reader
#' switching between the correlation panel and the transition
#' matrix sees a consistent visual identity. Symmetric limits
#' are chosen from the largest observed absolute cell so a
#' mostly-positive matrix does not wash out the negative entries.
#'
#' @param x An `mvgam_var_matrix` object returned by
#'   [posterior_transition_matrix()].
#' @param cluster Logical. If `TRUE`, reorder rows and columns
#'   using approximate Robinson clustering on `1 - A` (only
#'   meaningful when `A` is roughly symmetric).
#' @param ... Currently unused.
#'
#' @return A [ggplot2::ggplot] object, which can be further
#'   customised with the \pkg{ggplot2} API.
#'
#' @seealso [posterior_transition_matrix()],
#'   [print.mvgam_var_matrix()],
#'   [plot.mvgam_var_matrix_list()], [plot.mvgam_residcor()].
#' @author Nicholas J Clark
#' @method plot mvgam_var_matrix
#' @export
plot.mvgam_var_matrix <- function(x, cluster = FALSE, ...) {
  checkmate::assert_flag(cluster)
  mat <- x$A
  if (cluster) {
    idx <- cluster_cormat(mat)
    mat <- mat[idx, idx]
  }
  long <- gather_matrix(mat, drop_diag = FALSE, drop_upper = FALSE)
  rng <- max(abs(range(long$value, na.rm = TRUE, finite = TRUE)))
  limits <- c(-rng, rng)
  ggplot2::ggplot(
    data = long,
    mapping = ggplot2::aes(x = Var2, y = Var1, fill = value)
  ) +
    ggplot2::geom_tile(colour = "grey50") +
    mvgam_diverging_scale(name = "Posterior\nA", limits = limits) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::scale_x_discrete(
      guide = ggplot2::guide_axis(angle = 45)
    ) +
    ggplot2::scale_y_discrete(limits = rev) +
    mvgam_theme()
}

#' Print method for `mvgam_var_matrix_list`
#'
#' Compact header naming the number of groups and matrix
#' dimension, then the mean off-diagonal absolute A magnitude per
#' group so the reader can see at a glance which panel carries
#' the strongest cross-outcome dependence.
#'
#' @param x An `mvgam_var_matrix_list` returned by
#'   [posterior_transition_matrix()] with `groups` set.
#' @param ... Currently unused.
#'
#' @return `x`, invisibly, after side-effect printing.
#'
#' @seealso [posterior_transition_matrix()],
#'   [plot.mvgam_var_matrix_list()],
#'   [print.mvgam_var_matrix()].
#' @author Nicholas J Clark
#' @method print mvgam_var_matrix_list
#' @export
print.mvgam_var_matrix_list <- function(x, ...) {
  cat("VAR(1) transition matrices from an mvgam fit\n")
  cat("  Groups     : ", length(x), "\n", sep = "")
  cat("  Dimension  : ", x[[1L]]$n_series, " x ",
      x[[1L]]$n_series, "\n", sep = "")
  cat("  Mean |A_ij| (i != j) by group:\n")
  m <- vapply(x, function(g) {
    off_diag_mask <- !diag(TRUE, g$n_series)
    mean(abs(g$A[off_diag_mask]))
  }, numeric(1L))
  print(round(m, 3))
  invisible(x)
}

#' Plot method for `mvgam_var_matrix_list`
#'
#' Faceted view across the panels stored in the list. Two
#' orientations are available:
#'
#' * `type = "heatmap"` (default): one diverging heatmap per
#'   group in a grid, with symmetric fill limits shared across
#'   facets so the eye can compare cell magnitudes directly.
#' * `type = "diagonal"`: self-persistence pointrange with one
#'   point per group per outcome (dodged by outcome), using the
#'   Okabe-Ito categorical palette so outcomes match the fevd
#'   and irf plots.
#'
#' Both use the internal `mvgam_theme()`,
#' `mvgam_diverging_scale()` and `mvgam_categorical_palette()`,
#' so the panel looks consistent with `plot.mvgam_var_matrix()`,
#' `plot.mvgam_residcor()` and the fevd / irf / stability plots.
#'
#' @param x An `mvgam_var_matrix_list` returned by
#'   [posterior_transition_matrix()] with `groups` set.
#' @param type One of `"heatmap"` (default) or `"diagonal"`.
#' @param ncol Optional integer. Number of facet columns for the
#'   heatmap grid; defaults to `ceiling(sqrt(length(x)))`. Only
#'   used when `type = "heatmap"`.
#' @param ... Currently unused.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @seealso [posterior_transition_matrix()],
#'   [plot.mvgam_var_matrix()],
#'   [plot.mvgam_residcor_list()].
#' @author Nicholas J Clark
#' @method plot mvgam_var_matrix_list
#' @export
plot.mvgam_var_matrix_list <- function(x, type = c("heatmap",
                                                     "diagonal"),
                                        ncol = NULL, ...) {
  type <- match.arg(type)
  if (type == "heatmap") return(plot_var_matrix_list_heatmap(x, ncol))
  plot_var_matrix_list_diagonal(x)
}

#' Faceted heatmap panel of per-group transition matrices. Uses
#' `gather_matrix()` on each `A` matrix, binds into one long
#' frame, then facets with symmetric diverging fills that share
#' limits across facets so the eye can compare magnitudes.
#' @noRd
plot_var_matrix_list_heatmap <- function(x, ncol) {
  long <- do.call(rbind, lapply(seq_along(x), function(g) {
    df <- gather_matrix(x[[g]]$A,
                        drop_diag = FALSE, drop_upper = FALSE)
    df$group <- names(x)[g]
    df
  }))
  rng <- max(abs(range(long$value, na.rm = TRUE, finite = TRUE)))
  ncol <- ncol %||% ceiling(sqrt(length(x)))
  ggplot2::ggplot(long,
                  ggplot2::aes(x = Var2, y = Var1, fill = value)) +
    ggplot2::geom_tile(colour = "grey60") +
    mvgam_diverging_scale(name = "Posterior\nA",
                          limits = c(-rng, rng)) +
    ggplot2::facet_wrap(~ group, ncol = ncol) +
    ggplot2::scale_x_discrete(
      guide = ggplot2::guide_axis(angle = 45)
    ) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(x = "", y = "") +
    mvgam_theme()
}

#' Per-outcome self-persistence pointrange: one facet per
#' outcome (diagonal cell), one point per group with the
#' credible interval from `A_lower` / `A_upper`. Uses the
#' Okabe-Ito categorical palette so outcomes match the fevd /
#' irf plots.
#' @noRd
plot_var_matrix_list_diagonal <- function(x) {
  outc <- x[[1L]]$series_names
  df <- do.call(rbind, lapply(seq_along(x), function(g) {
    obj <- x[[g]]
    data.frame(
      group   = names(x)[g],
      outcome = outc,
      lo      = diag(obj$A_lower),
      med     = diag(obj$A),
      hi      = diag(obj$A_upper),
      stringsAsFactors = FALSE
    )
  }))
  n_outcomes <- length(outc)
  fill_values <- mvgam_categorical_palette(n_outcomes)
  names(fill_values) <- outc
  ggplot2::ggplot(df, ggplot2::aes(x = med, y = group,
                                    colour = outcome)) +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = lo, xmax = hi),
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::scale_colour_manual(values = fill_values) +
    ggplot2::labs(x = "self-persistence coefficient",
                  y = NULL, colour = "outcome") +
    mvgam_theme()
}

#' Detect a hierarchical VAR fit by searching for its per-group
#' or global-Cholesky parameter names in the posterior draws
#' column set.
#' @noRd
is_hierarchical_var <- function(all_cols) {
  any(grepl(
    "^(L_Omega_global_trend|A_group_trend|Sigma_group_trend)\\[",
    all_cols
  ))
}

#' Resolve the K dimension for a VAR post-processing extraction:
#' prefer the explicit `standata` slot, fall back to inferring `K`
#' from the number of matching columns in the posterior draws
#' matrix (assumes a `K x K` block).
#' @noRd
resolve_var_dim <- function(object, standata_slot, col_pattern,
                            all_cols) {
  k_slot <- object$standata[[standata_slot]]
  if (!is.null(k_slot)) {
    return(as.integer(k_slot))
  }
  n_match <- length(grep(col_pattern, all_cols))
  if (n_match < 1L) {
    stop(insight::format_error(c(
      "Cannot determine VAR dimension from fit.",
      x = paste0(
        "Standata slot '", standata_slot, "' is missing and no ",
        "posterior columns match '", col_pattern, "'."
      ),
      i = "Refit the model or update the extraction pattern."
    )))
  }
  as.integer(round(sqrt(n_match)))
}

#' Resolve a `group` argument to an integer index against the
#' hierarchical grouping factor stored on the fit. Reuses
#' `first_trend_spec()` + `lookup_factor_levels()` (the label
#' machinery `residual_cor.mvgam` uses for the same hierarchical
#' fits) so name-to-index matching stays consistent across
#' `residual_cor()` and `posterior_transition_matrix()`.
#' @noRd
resolve_group_index <- function(object, group) {
  n_groups <- object$standata$N_groups_trend
  if (is.null(n_groups)) {
    stop(insight::format_error(
      "Fit is hierarchical but 'N_groups_trend' is missing."
    ))
  }
  n_groups <- as.integer(n_groups)
  if (is.character(group)) {
    spec <- first_trend_spec(object)
    gr_labels <- lookup_factor_levels(object$data, spec$gr,
                                       n_groups, prefix = "group")
    idx <- match(group, gr_labels)
    if (is.na(idx)) {
      stop(insight::format_error(c(
        paste0("Group '", group, "' not found."),
        i = paste0("Available: ",
                   paste(gr_labels, collapse = ", "), ".")
      )))
    }
    return(as.integer(idx))
  }
  checkmate::assert_int(group, lower = 1L, upper = n_groups)
  as.integer(group)
}

#' Look up the innovation-outcome (subgroup) labels for a
#' hierarchical VAR fit. Thin wrapper over `lookup_factor_levels`
#' (`R/residual_cor.R`) so the label resolution rule is shared
#' across `residual_cor.mvgam` and the VAR posterior helpers.
#' @noRd
subgroup_labels <- function(object, K) {
  spec <- first_trend_spec(object)
  lookup_factor_levels(object$data, spec$subgr, K,
                       prefix = "outcome")
}
