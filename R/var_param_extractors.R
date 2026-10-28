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
  draws_mat <- fit_draws(object$fit,
                         c("Phi_trend", "Theta_trend", "Sigma_trend"))
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
#' `future = FALSE` (the default) loops with `lapply()`. Under
#' `future = TRUE`, `future::future()` runs each posterior draw under
#' the caller's `plan()`. The default `sequential()` plan runs them in
#' turn. `plan(multisession, workers = N)` spreads them across `N` R
#' processes. An error in any draw reaches the caller. Without the
#' future package, which is in Suggests, the loop uses `lapply()` and
#' says so once per session.
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
  if (!future || !future_available()) {
    return(lapply(seq_len(n), fn))
  }
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

#' Posterior transition matrix of a VAR trend
#'
#' @description
#' The coefficient matrix \eqn{A_l} of lag \eqn{l} of a `VAR()` trend.
#' Row \eqn{i} holds the effects of the processes at time \eqn{t - l}
#' on process \eqn{i} at time \eqn{t}. The diagonal holds each
#' process's persistence. The off-diagonal cells hold the effects of
#' one process on another.
#'
#' @details
#' A hierarchical VAR (`VAR(gr = ...)`) has one matrix per panel.
#' `groups` names a panel to return its matrix. With `groups = NULL`
#' the function returns the mean of the panels' matrices, computed in
#' each draw. The hyperparameters `Amu_trend` and `Aomega_trend` pool
#' unconstrained matrices, which the stationarity map of Heaps (2023)
#' turns into each panel's matrix. Their mean is no transition matrix.
#' The mean over panels summarises the pool on the scale of the
#' matrices.
#'
#' Each cell is summarised as [residual_cor()] summarises a
#' correlation. The estimate is the posterior mean, or the median with
#' `robust = TRUE`. The interval takes the quantiles `probs`. The
#' effective sample size comes from [posterior::ess_basic()].
#' `prob_positive`, `prob_negative` and `prob_nonzero` are the shares of
#' draws above zero, below zero and away from zero.
#'
#' @inheritParams predict.mvgam
#' @param object A fitted `mvgam` object whose trend is a VAR.
#' @param lag The lag of the matrix, at most the order `p` of the
#'   fitted `VAR(p)`.
#' @param groups The panels of a hierarchical VAR, by level or by
#'   index, or `"all"`. `NULL` gives the mean of the panels' matrices.
#'   A single-panel fit takes `NULL` alone. One panel returns
#'   its matrix. Several, or `"all"`, return a list whose `plot()`
#'   method is [plot.mvgam_var_matrix_list()].
#' @param summary Logical. `TRUE` returns the summary of each cell.
#'   `FALSE` returns the draws as a `[ndraws, K, K]` array, with the
#'   processes naming its second and third dimensions.
#'
#' @return With `summary = TRUE`, an `mvgam_var_matrix`: a list of the
#'   matrices `A`, `A_se`, `A_lower`, `A_upper`, `A_ess`,
#'   `prob_positive`, `prob_negative` and `prob_nonzero`, with
#'   `n_series`, `series_names`, `group_label`, `probs` and `lag`. It
#'   has `print()` and `plot()` methods. With `summary = FALSE`, the
#'   array of draws. Several panels give a list of either.
#'
#' @examples
#' \dontrun{
#' set.seed(0)
#' simdat <- sim_mvgam(family = gaussian(), n_series = 3L,
#'                     n_timepoints = 120L, trend_model = VAR(),
#'                     prop_trend = 0.95)
#' var_mod <- mvgam(y ~ 1, trend_formula = ~ VAR(p = 1),
#'                  data = simdat$data_train, family = gaussian(),
#'                  chains = 2, silent = 2)
#'
#' # Each series' persistence and its effects on the others
#' A <- posterior_transition_matrix(var_mod)
#' A
#' plot(A)
#'
#' # Next, irf(var_mod) traces how a shock to one series spreads
#' }
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
#' @family latent dynamics
#' @export
posterior_transition_matrix <- function(object, groups = NULL,
                                         summary = TRUE,
                                         robust = FALSE,
                                         probs = c(0.025, 0.975),
                                         lag = 1L) {
  probs <- assert_var_surface_args(NULL, NULL, summary, probs,
                                   future = FALSE)
  checkmate::assert_flag(robust)
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
  draws_mat <- fit_draws(object$fit, c("Phi_group_trend", "Phi_trend"))

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
  out <- extract_indexed_array_2d(
    draws_mat, "Phi_trend", K, K,
    prefix_ids   = lag,
    labels       = var_process_labels(object, K),
    required_for = "the VAR transition matrix"
  )
  attr(out, "group_label") <- "single-panel"
  out
}

#' Per-cell summary of VAR transition matrix draws
#'
#' The cells take the summary `residual_cor()` gives a covariance,
#' from `summarise_unconstrained_array()`, plus the sign
#' probabilities of `sign_probabilities()`.
#'
#' @param arr `[ndraws, K, K]` array of transition matrix draws
#' @return An `mvgam_var_matrix`
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

#' Print a VAR transition matrix
#'
#' Prints the panel, the dimension, the mean absolute off-diagonal
#' cell and the posterior estimate of the matrix. The interval of each
#' cell is in `A_lower` and `A_upper`.
#'
#' @param x An `mvgam_var_matrix` object returned by
#'   [posterior_transition_matrix()].
#' @param digits Integer. Digits used when rounding the printed
#'   matrix.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @seealso [posterior_transition_matrix()],
#'   [plot.mvgam_var_matrix()],
#'   [print.mvgam_var_matrix_list()].
#' @author Nicholas J Clark
#' @method print mvgam_var_matrix
#' @export
print.mvgam_var_matrix <- function(x, digits = 2L, ...) {
  cat("Lag-", x$lag, " VAR transition matrix from an mvgam fit\n",
      sep = "")
  cat("  Group      : ", x$group_label, "\n", sep = "")
  cat("  Dimension  : ", x$n_series, " x ", x$n_series, "\n",
      sep = "")
  cat(sprintf("  Mean |A_ij| (i != j): %.2f\n", mean_abs_offdiag(x$A)))
  cat("  Point estimate:\n")
  print(round(x$A, digits))
  cat("  ", sprintf("%.0f%% CI", 100 * (x$probs[2L] - x$probs[1L])),
      " on each cell available in `A_lower` / `A_upper`.\n",
      sep = "")
  invisible(x)
}

#' Plot a VAR transition matrix
#'
#' Draws the posterior estimate of the matrix as a heatmap. Row
#' \eqn{i}, column \eqn{j} is the effect of process \eqn{j} at time
#' \eqn{t - l} on process \eqn{i} at time \eqn{t}. A black outline
#' marks each cell whose interval excludes zero. The colour scale is
#' symmetric about zero and spans the largest absolute cell.
#'
#' @inheritParams print.mvgam_var_matrix
#' @param cluster Logical. `TRUE` orders the processes by a Robinson
#'   clustering of `1 - A`, which suits a matrix close to symmetric.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A [ggplot2::ggplot] object.
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
  var_matrix_heatmap(var_matrix_panel(x, idx), x$lag)
}

#' One transition matrix in long format, with its clear cells
#'
#' @param x An `mvgam_var_matrix`
#' @param idx Order of the processes
#' @return Frame of `Var1`, `Var2`, `value` and `clear`
#' @noRd
var_matrix_panel <- function(x, idx = seq_len(x$n_series)) {
  clear <- interval_excludes_zero(x$A_lower, x$A_upper)
  matrix_panel(x$A[idx, idx, drop = FALSE],
               clear = clear[idx, idx, drop = FALSE])
}

#' Heatmap of transition matrices, rows responding to columns
#'
#' Fill limits are symmetric about the largest absolute cell. A
#' mostly positive matrix then keeps its negative cells visible.
#'
#' @param panel Frame from `var_matrix_panel()`, optionally with a
#'   `group` column
#' @param lag Integer lag of the matrix, which labels the columns
#' @param ncol Facet columns when `panel` has a `group` column
#' @return A `ggplot` object
#' @noRd
var_matrix_heatmap <- function(panel, lag, ncol = NULL) {
  rng <- max(abs(range(panel$value, na.rm = TRUE, finite = TRUE)))
  matrix_heatmap(
    panel,
    mvgam_diverging_scale(name = "Lagged\neffect", limits = c(-rng, rng)),
    layout = "matrix",
    x_lab = paste("Process at t -", lag),
    y_lab = "Process at t",
    ncol = ncol
  )
}

#' Print the VAR transition matrices of several panels
#'
#' Prints the number of panels, the dimension and the mean absolute
#' off-diagonal cell of each panel. The panel with the largest mean
#' has the strongest effects between processes.
#'
#' @param x An `mvgam_var_matrix_list` returned by
#'   [posterior_transition_matrix()] with `groups` set.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @seealso [posterior_transition_matrix()],
#'   [plot.mvgam_var_matrix_list()],
#'   [print.mvgam_var_matrix()].
#' @author Nicholas J Clark
#' @method print mvgam_var_matrix_list
#' @export
print.mvgam_var_matrix_list <- function(x, ...) {
  cat("Lag-", x[[1L]]$lag,
      " VAR transition matrices from an mvgam fit\n", sep = "")
  cat("  Groups     : ", length(x), "\n", sep = "")
  cat("  Dimension  : ", x[[1L]]$n_series, " x ",
      x[[1L]]$n_series, "\n", sep = "")
  cat("  Mean |A_ij| (i != j) by group:\n")
  m <- vapply(x, function(g) mean_abs_offdiag(g$A), numeric(1L))
  print(round(m, 3))
  invisible(x)
}

#' Plot the VAR transition matrices of several panels
#'
#' `type = "heatmap"` draws one heatmap per panel, as
#' [plot.mvgam_var_matrix()] draws one, with fill limits shared across
#' panels. `type = "diagonal"` draws the persistence of each process in
#' each panel as a point with its interval.
#'
#' @inheritParams print.mvgam_var_matrix_list
#' @param type `"heatmap"` (default) or `"diagonal"`.
#' @param ncol The number of facet columns of the heatmaps, or `NULL`
#'   for `ceiling(sqrt(length(x)))`.
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
  var_matrix_heatmap(panel, x[[1L]]$lag, ncol)
}

#' Per-outcome self-persistence pointrange: one row per group, one
#' point per outcome (diagonal cell) with the credible interval from
#' `A_lower` / `A_upper`. Outcomes take the shades of the red scheme
#' that the fevd, irf and stability plots draw with.
#' @noRd
plot_var_matrix_list_diagonal <- function(x) {
  set_color_scheme_local()
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

