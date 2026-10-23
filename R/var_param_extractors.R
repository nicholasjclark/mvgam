#' Assert that a fitted mvgam carries a VAR latent trend
#'
#' `irf()`, `fevd()`, `stability()` and `posterior_transition_matrix()`
#' share this gate. `surface` names the function the user called.
#'
#' @param object A fitted `mvgam` object.
#' @param surface Character; the user-facing function name to cite
#'   in the error message.
#' @return `TRUE`, invisibly; stops on failure.
#' @noRd
assert_var_trend <- function(object, surface) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_string(surface, min.chars = 1L)
  trend_type <- get_trend_type(object)
  if (!identical(trend_type, "VAR")) {
    stop(insight::format_error(c(
      paste0("'", surface, "' requires a VAR latent trend."),
      x = paste0("The fitted trend type is '", trend_type, "'."),
      i = paste0(
        "Refit with 'trend_formula = ~ VAR()' to use '", surface, "'."
      )
    )), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate the arguments `irf()`, `fevd()` and `stability()` share
#'
#' @param ndraws,draw_ids The posterior draws to use
#' @param summary Whether to summarise the draws
#' @param probs Lower and upper percentiles of the summary
#' @param future Whether to run the draws under a `future` plan
#' @return `probs`, sorted, or an error
#' @noRd
assert_var_surface_args <- function(ndraws, draw_ids, summary, probs,
                                    future) {
  validate_draw_selectors(ndraws, draw_ids)
  checkmate::assert_flag(summary)
  checkmate::assert_numeric(probs, len = 2L, lower = 0, upper = 1,
                            any.missing = FALSE, unique = TRUE)
  checkmate::assert_flag(future)
  sort(probs)
}

#' A kernel applied to each draw of a VAR posterior
#'
#' `irf()` and `fevd()` apply different kernels to the same draws. Each
#' draw is assembled here once, as one set of transition matrices and
#' one innovation covariance. Both functions then index the same draw by
#' the same number.
#'
#' @param var_post VAR posterior from `extract_var_posterior()`, holding
#'   `ndraws`, `K`, `labels`, `A`, `D` and `Sigma`
#' @param kernel Function taking one assembled draw as a list
#' @param future Logical flag to run the draws through `future`
#' @return List of kernel outputs, one per draw
#' @noRd
var_draw_surfaces <- function(var_post, kernel, future) {
  checkmate::assert_list(var_post)
  checkmate::assert_function(kernel)
  checkmate::assert_flag(future)
  mvgam_maybe_future_lapply(
    var_post$ndraws,
    function(draw) {
      kernel(list(
        K = var_post$K,
        labels = var_post$labels,
        A = draw_slice(var_post$A, draw),
        D = draw_slice(var_post$D, draw),
        Sigma = var_post$Sigma[draw, , , drop = TRUE]
      ))
    },
    future = future
  )
}


#' One draw of a `[ndraws, K, K, lags]` coefficient array
#' @noRd
draw_slice <- function(arr, draw) {
  array(arr[draw, , , , drop = FALSE], dim = dim(arr)[-1L])
}

#' Extract VAR posterior draws of (A, D, Sigma) from a fitted mvgam
#'
#' Pulls the per-draw coefficient matrices `Phi_trend[l, , ]` of every
#' lag, the moving-average matrices `Theta_trend[j, , ]` of a VARMA and
#' the innovation covariance `Sigma_trend` from `object$fit`.
#'
#' `K` is the number of latent series in the VAR. For both factor
#' and non-factor VARs the Stan-side dimension is `N_lv_trend` (for
#' a non-factor VAR over the raw series, the compiler sets
#' `N_lv_trend = N_series_trend`). The function takes it from the Stan
#' data stored on the fit, which hold the dimension the model was
#' compiled with.
#'
#' @param object A fitted `mvgam` object whose trend is a VAR.
#' @return A list with elements:
#'   * `A`: `[ndraws, K, K, p]` array of VAR coefficient draws.
#'   * `D`: `[ndraws, K, K, q]` array of moving-average draws, with
#'     `q = 0` for a VAR.
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
  # transition matrix per draw. Their cost and their size grow with the
  # number of draws multiplied by the square of the number of
  # processes. A subset of draws shortens the work on a wide VAR. The
  # coefficients and the innovation covariance come from the same
  # subset. A response built from two different draws would describe
  # no posterior sample.
  draws_mat <- subset_draws_rows(draws_mat, ndraws, draw_ids)
  # The unrotated `Phi_trend`. For a factor VAR with Heaps QR
  # identification the generated-quantities block also emits
  # `Phi_trend_tilde = Q Phi_trend Q'`. The QR-identified surface takes
  # `Phi_trend_tilde` and rotates `Sigma_trend` by the same Q.
  # `N_lv_trend` is the Stan-side VAR dimension for every code
  # path: factor VARs set it to the number of factors, non-factor
  # VARs set it equal to `N_series_trend`. The Stan data hold the
  # value the compiled model uses.
  K <- var_dim(object, "N_lv_trend")

  ndraws <- nrow(draws_mat)

  # Stan declares `Phi_trend` and `Theta_trend` as arrays of `K x K`
  # matrices, one per lag. Their draws are named `Phi_trend[lag,i,j]`.
  by_lag <- function(name, n_lags) {
    out <- array(0, dim = c(ndraws, K, K, n_lags))
    for (l in seq_len(n_lags)) {
      out[, , , l] <- extract_indexed_array_2d(
        draws_mat, name, K, K, prefix_ids = l,
        required_for = paste0(name, " (VAR coefficient extraction)")
      )
    }
    out
  }
  A <- by_lag("Phi_trend", length(object$trend_metadata$ar_lags %||% 1L))
  D <- by_lag("Theta_trend", length(object$trend_metadata$ma_lags))
  Sigma <- extract_indexed_array_2d(
    draws_mat, "Sigma_trend", K, K,
    required_for = "Sigma_trend (VAR innovation covariance)"
  )

  list(A = A, D = D, Sigma = Sigma, K = K, ndraws = ndraws,
       labels = var_process_labels(object, K))
}

#' Optionally parallelise a per-draw VAR post-processing lapply
#'
#' Internal helper shared by `irf.mvgam()`, `fevd.mvgam()` and
#' `stability.mvgam()`. Their per-draw loops run under the `future`
#' plan the caller set, as chain fitting does.
#'
#' When `future = FALSE` (the default) the loop is base `lapply()`,
#' with no `future` dependency and no runtime overhead. When
#' `future = TRUE`, `future::future()` runs every posterior draw under
#' the `plan()` the caller set (defaulting to `sequential()`, which
#' behaves like `lapply` with negligible extra overhead);
#' `plan(multisession, workers = N)` before the call spreads draws
#' across `N` R processes. Errors from any draw propagate.
#'
#' `future` is in Suggests. A request for parallel execution without
#' the package installed stops with an install hint.
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
  require_package("future")
  futures <- vector("list", n)
  for (i in seq_len(n)) {
    local_i <- i
    futures[[i]] <- future::future(fn(local_i), seed = TRUE)
  }
  lapply(futures, future::value)
}

#' Validate the processes a VAR surface plot selects
#'
#' The impulse response and variance decomposition plots select
#' processes by position or by name. A name is matched against the
#' process labels the surface carries. A selection by name survives a
#' change in the order of the series levels.
#'
#' @param ids `NULL`, an integer vector of positions or a character
#'   vector of process names. `NULL` selects every process.
#' @param labels The process labels, in the order the VAR numbers them.
#' @param arg The user-facing argument name (for error messages).
#' @return Sorted, de-duplicated integer vector of positions.
#' @noRd
validate_var_plot_ids <- function(ids, labels, arg) {
  checkmate::assert_character(labels, min.len = 1L, any.missing = FALSE)
  n_proc <- length(labels)
  if (is.null(ids)) return(seq_len(n_proc))
  if (is.character(ids)) {
    checkmate::assert_character(ids, min.len = 1L, any.missing = FALSE,
                                .var.name = arg)
    unknown <- setdiff(ids, labels)
    if (length(unknown)) {
      shown <- utils::head(labels, 6L)
      stop(insight::format_error(c(
        paste0("'", arg, "' names processes outside the fitted VAR."),
        x = paste0("Unknown: ", paste(unknown, collapse = ", "), "."),
        i = paste0(
          "Processes: ", paste(shown, collapse = ", "),
          if (n_proc > length(shown)) ", ..." else "", "."
        )
      )), call. = FALSE)
    }
    ids <- match(ids, labels)
  }
  checkmate::assert_integerish(
    ids, lower = 1L, upper = n_proc, min.len = 1L, any.missing = FALSE,
    .var.name = arg
  )
  sort(unique(as.integer(ids)))
}

#' The name each latent VAR process carries
#'
#' An impulse response and a variance decomposition show which series
#' drives which. Their labels name a series wherever a series names the
#' process. A VAR over the observed series numbers its processes by the
#' series axis. A factor VAR's processes are latent factors that no
#' series names. They are labelled by index.
#'
#' @param object A fitted `mvgam` object whose trend is a VAR.
#' @param K The VAR dimension.
#' @return A character vector of length `K`.
#' @noRd
var_process_labels <- function(object, K) {
  checkmate::assert_int(K, lower = 1L)
  generic <- paste0("Process_", seq_len(K))
  # `detect_factor_n_lv()` returns NULL when the processes are the
  # series, including a model with one factor per series.
  if (!is.null(detect_factor_n_lv(object))) {
    return(generic)
  }
  levs <- mvgam_axes(object)$series$levels
  if (length(levs) != K) {
    return(generic)
  }
  as.character(levs)
}

#' Posterior transition matrix for a VAR trend
#'
#' @description
#' Extract a VAR transition matrix $A_l$ from a fitted `mvgam`
#' object, either as a per-cell posterior summary (default) or as
#' raw per-draw values. Row $i$ of $A_l$ gives the linear dependence
#' of latent process $i$ at time $t$ on the vector of processes at
#' time $t - l$; the diagonal is self-persistence, off-diagonals
#' encode cross-outcome dependence at lag $l$.
#'
#' @details
#' The extractor dispatches on whether the fit is hierarchical
#' (`VAR(gr = <var>, ...)`) or single-panel.
#'
#' * On a **single-panel VAR** the routine returns the fitted
#'   `Phi_trend` array.
#' * On a **hierarchical VAR** with `groups = <int>` or
#'   `groups = <name>` it returns the panel-specific matrix
#'   `Phi_group_trend[c, l, , ]`, the transition matrix of the
#'   requested panel.
#' * On a **hierarchical VAR** with `groups = NULL` it returns the
#'   mean of the panels' matrices, computed in each posterior draw.
#'   The hyperparameters `Amu_trend` and `Aomega_trend` pool the
#'   unconstrained matrices that the stationarity map of Heaps
#'   (2023) turns into each panel's $A_c$. Their mean is no
#'   transition matrix. The mean over panels summarises the pool on
#'   the scale of $A_c$.
#'
#' With `summary = TRUE` (default) the per-cell summary uses the
#' same machinery as `residual_cor()`. The printed and plotted output
#' of the two extractors then agree:
#' point estimates default to the posterior mean; intervals use
#' `probs` on the raw draws; effective sample sizes come from
#' `posterior::ess_basic()`; and
#' `prob_positive` / `prob_negative` / `prob_nonzero` count the
#' fraction of draws with each entry above or below zero. See
#' the argument documentation for the median switch.
#'
#' @param object A fitted `mvgam` object whose trend is a VAR.
#' @param lag The lag whose transition matrix is returned, at most
#'   the order `p` of the fitted `VAR(p)`. Defaults to `1`.
#' @param groups Which panels of a hierarchical VAR fit to
#'   return. `NULL` (the default) gives the mean of the panels'
#'   matrices. A non-hierarchical fit accepts `NULL` alone. A single
#'   panel, named by level or by integer index into the levels of
#'   `gr`, returns that panel's matrix on its own. Several panels, or
#'   `"all"`, return a named list of `mvgam_var_matrix` objects
#'   classed as `mvgam_var_matrix_list`. Its `plot()` method draws a
#'   faceted heatmap grid or a pointrange of each outcome's
#'   self-persistence, see [plot.mvgam_var_matrix_list()].
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
#'   `probs`, `lag`. Has `print()` and `plot()` methods. With
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
                                         probs = c(0.025, 0.975),
                                         lag = 1L) {
  checkmate::assert_flag(summary)
  checkmate::assert_flag(robust)
  checkmate::assert_numeric(probs, len = 2L, lower = 0, upper = 1,
                            any.missing = FALSE, unique = TRUE)
  require_fitted_model(object, "posterior_transition_matrix")
  assert_var_trend(object, surface = "posterior_transition_matrix()")
  checkmate::assert_int(lag, lower = 1L)
  order <- length(object$trend_metadata$ar_lags %||% 1L)
  if (lag > order) {
    stop(insight::format_error(c(
      paste0("'lag' must be at most ", order, ", the order of the VAR."),
      x = paste0("Got 'lag' = ", lag, ".")
    )), call. = FALSE)
  }
  probs <- sort(probs)

  one_matrix <- function(group) {
    arr <- extract_transition_matrix_draws(object, group, lag)
    if (!summary) return(arr)
    finalise_transition_matrix(
      arr,
      group_label = attr(arr, "group_label"),
      robust      = robust,
      probs       = probs,
      lag         = lag
    )
  }

  # No selection asks for the mean over panels, or for the one matrix
  # a non-hierarchical fit has
  target_names <- resolve_var_groups(object, groups)
  if (is.null(target_names)) {
    return(one_matrix(NULL))
  }

  # One named panel returns that panel's matrix. `"all"` returns the
  # list that `plot.mvgam_var_matrix_list()` facets, whatever the
  # number of panels.
  if (length(target_names) == 1L && !identical(groups, "all")) {
    return(one_matrix(target_names))
  }

  out <- lapply(target_names, one_matrix)
  names(out) <- target_names
  structure(out, class = "mvgam_var_matrix_list")
}

#' The panels of a hierarchical VAR that a surface asks for
#'
#' @param object A fitted `mvgam` object whose trend is a VAR
#' @param groups `NULL`, `"all"`, panel names or panel indices
#' @return `NULL` when `groups` is `NULL`, or the names of the panels.
#'   A non-hierarchical fit accepts `NULL` alone.
#' @noRd
resolve_var_groups <- function(object, groups) {
  if (is.null(groups)) {
    return(NULL)
  }
  if (is.null(object$standata$N_groups_trend)) {
    stop(insight::format_error(c(
      "'groups' only applies to hierarchical VAR fits.",
      i = "Leave 'groups' at NULL on a non-hierarchical fit."
    )), call. = FALSE)
  }
  all_levels <- group_labels(object, var_dim(object, "N_groups_trend"))
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
    )), call. = FALSE)
  }
  groups
}

#' Extract raw `[ndraws, K, K]` posterior draws of the lag-`lag` VAR
#' transition matrix from a fitted mvgam. Returned array carries
#' `attr(., "group_label")` (`"mean over groups"`, a group name, or
#' `"single-panel"`) for downstream summarisers to label the object.
#' @noRd
extract_transition_matrix_draws <- function(object, group, lag) {
  draws_mat <- posterior::as_draws_matrix(object$fit)

  if (!is.null(object$standata$N_groups_trend)) {
    K <- var_dim(object, "N_subgroups_trend")
    n_groups <- var_dim(object, "N_groups_trend")
    group_matrix <- function(g) {
      extract_indexed_array_2d(
        draws_mat, "Phi_group_trend", K, K,
        prefix_ids   = c(g, lag),
        labels       = subgroup_labels(object, K),
        required_for = "a per-group VAR transition matrix"
      )
    }
    # The hyperparameters `Amu_trend` pool the raw matrices before the
    # stationarity map, which leaves no transition matrix at the
    # population level. The function returns the mean of the groups'
    # matrices in each draw for the pool.
    if (is.null(group)) {
      out <- Reduce(`+`, lapply(seq_len(n_groups), group_matrix)) /
        n_groups
      attr(out, "group_label") <- "mean over groups"
      return(out)
    }
    # `resolve_var_groups()` validated the name
    out <- group_matrix(match(group, group_labels(object, n_groups)))
    attr(out, "group_label") <- group
    return(out)
  }

  # Non-hierarchical VAR: one Phi_trend matrix per lag
  K <- var_dim(object, "N_lv_trend")
  labs <- var_process_labels(object, K)
  out <- extract_indexed_array_2d(
    draws_mat, "Phi_trend", K, K,
    prefix_ids   = lag,
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
                                        robust, probs, lag = 1L) {
  series_names <- dimnames(arr)[[2L]]
  stats <- summarise_unconstrained_array(
    arr, robust = robust, probs = probs, series_names = series_names
  )
  signs <- sign_probabilities(arr, series_names)
  structure(
    list(
      A             = stats$point,
      A_se          = stats$se,
      A_lower       = stats$lower,
      A_upper       = stats$upper,
      A_ess         = stats$ess,
      prob_positive = signs$prob_positive,
      prob_negative = signs$prob_negative,
      prob_nonzero  = signs$prob_nonzero,
      n_series      = dim(arr)[2L],
      series_names  = series_names,
      group_label   = group_label,
      probs         = probs,
      lag           = lag
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
#' the `A_lower` / `A_upper` slots. The header follows the layout of
#' [print.mvgam_residcor()].
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
  cat("Lag-", x$lag %||% 1L, " VAR transition matrix from an mvgam fit\n",
      sep = "")
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
#' Draws the point estimate of the transition matrix as a heatmap.
#' Row \eqn{i}, column \eqn{j} is the effect of process \eqn{j} at
#' time \eqn{t - 1} on process \eqn{i} at time \eqn{t}. A black
#' outline marks each cell whose credible interval excludes zero. The
#' colour scale is symmetric about zero and spans the largest absolute
#' cell.
#'
#' @param x An `mvgam_var_matrix` object returned by
#'   [posterior_transition_matrix()].
#' @param cluster Logical. If `TRUE`, reorder rows and columns
#'   using approximate Robinson clustering on `1 - A` (only
#'   meaningful when `A` is roughly symmetric).
#' @param ... Unused. Anything passed here is refused.
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
  rlang::check_dots_empty()
  idx <- if (cluster) cluster_cormat(x$A) else seq_len(x$n_series)
  panel <- var_matrix_panel(x, idx)
  var_matrix_heatmap(panel)
}

#' One transition matrix in long format, with its clear cells
#'
#' @param x An `mvgam_var_matrix`
#' @param idx Order of the processes
#' @return Frame of `Var1`, `Var2`, `value` and `clear`
#' @noRd
var_matrix_panel <- function(x, idx = seq_len(x$n_series)) {
  long <- gather_matrix(x$A[idx, idx, drop = FALSE],
                        drop_diag = FALSE, drop_upper = FALSE)
  clear <- interval_excludes_zero(x$A_lower, x$A_upper)
  long$clear <- as.vector(clear[idx, idx, drop = FALSE])
  long
}

#' Heatmap of transition matrices, rows responding to columns
#'
#' Fill limits are symmetric about the largest absolute cell. A
#' mostly positive matrix then keeps its negative cells visible.
#'
#' @param panel Frame from `var_matrix_panel()`, optionally with a
#'   `group` column
#' @param ncol Facet columns when `panel` has a `group` column
#' @return A `ggplot` object
#' @noRd
var_matrix_heatmap <- function(panel, ncol = NULL) {
  rng <- max(abs(range(panel$value, na.rm = TRUE, finite = TRUE)))
  matrix_heatmap(
    panel,
    mvgam_diverging_scale(name = "Posterior\nA", limits = c(-rng, rng)),
    layout = "matrix",
    x_lab = "Process at t - 1",
    y_lab = "Process at t",
    ncol = ncol
  )
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
  cat("Lag-", x[[1L]]$lag %||% 1L,
      " VAR transition matrices from an mvgam fit\n", sep = "")
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
#' * `type = "heatmap"` (default): one heatmap per group, drawn as
#'   by [plot.mvgam_var_matrix()]. The facets share fill limits.
#' * `type = "diagonal"`: self-persistence pointrange with one
#'   point per group per outcome (dodged by outcome). Up to three
#'   outcomes take the dark, mid and light shades of the red
#'   bayesplot scheme, as the fevd, irf and stability plots do.
#'
#' Both draw with [mvgam_theme()].
#'
#' @param x An `mvgam_var_matrix_list` returned by
#'   [posterior_transition_matrix()] with `groups` set.
#' @param type One of `"heatmap"` (default) or `"diagonal"`.
#' @param ncol Optional integer. Number of facet columns for the
#'   heatmap grid; defaults to `ceiling(sqrt(length(x)))`. Only
#'   used when `type = "heatmap"`.
#' @param ... Unused. Anything passed here is refused.
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
  rlang::check_dots_empty()
  if (type == "heatmap") return(plot_var_matrix_list_heatmap(x, ncol))
  plot_var_matrix_list_diagonal(x)
}

#' Faceted heatmap of per-group transition matrices. The facets share
#' fill limits. Cell magnitudes then compare across groups.
#' @noRd
plot_var_matrix_list_heatmap <- function(x, ncol) {
  panel <- do.call(rbind, lapply(names(x), function(g) {
    df <- var_matrix_panel(x[[g]])
    df$group <- g
    df
  }))
  var_matrix_heatmap(panel, ncol)
}

#' Per-outcome self-persistence pointrange: one row per group, one
#' point per outcome (diagonal cell) with the credible interval from
#' `A_lower` / `A_upper`. Outcomes take the shades of the red scheme
#' that the fevd, irf and stability plots draw with.
#' @noRd
plot_var_matrix_list_diagonal <- function(x) {
  set_color_scheme_local("red")
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
  # The first group is drawn at the top and the outcomes keep the
  # order of the levels of `subgr`
  df$group <- factor(df$group, levels = rev(names(x)))
  df$outcome <- factor(df$outcome, levels = outc)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$med, y = .data$group,
                                    colour = .data$outcome)) +
    reference_line(xintercept = 0) +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = .data$lo, xmax = .data$hi),
      position = ggplot2::position_dodge(width = 0.6)
    ) +
    mvgam_model_colour_scale(length(outc), name = "Outcome") +
    ggplot2::labs(x = "Self-persistence coefficient", y = NULL) +
    mvgam_theme()
}

#' A dimension of the VAR, as the fitted Stan program declared it
#' @noRd
var_dim <- function(object, standata_slot) {
  K <- object$standata[[standata_slot]]
  if (is.null(K)) {
    stop_missing_fields("The stored Stan data", standata_slot)
  }
  K <- as.integer(K)
  checkmate::assert_int(K, lower = 1L, .var.name = standata_slot)
  K
}

