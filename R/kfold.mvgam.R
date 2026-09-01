# K-fold cross-validation for mvgam fits with optional group
# stratification and a selective-refit hybrid that mirrors the
# `lfo_cv.mvgam()` Bürkner-Gabry-Vehtari (2020) algorithm,
# generalised from rolling-time blocks to arbitrary groups.
#
# Reuses:
#   - `lfo_log_sum_exp` / `lfo_log_mean_exp` / `lfo_sum_rows`
#     (R/lfo_cv.mvgam.R) for numerically stable log-density math.
#   - `clean_ll` (R/loo.mvgam.R) for sanitising log-lik draws.
#   - `closure_unit_grouping` (R/families.R) for the default group
#     key on closure-unit families (occ / nmix variants).
#   - `mvgam_training_data` (R/brms_wrappers.mvgam.R) for the
#     parent fit's training frame.
#   - `update.mvgam` for per-fold refits: passing the prior arg
#     OMITTED inherits the parent fit's pinned prior table so the
#     compiled Stan binary is reused across all refits (per #188).
#   - `loo::kfold_split_grouped` / `loo::kfold_split_random` for
#     the fold partition logic.

#' K-fold cross-validation for `mvgam` fits
#'
#' Hierarchical / grouped k-fold cross-validation. Generalises the
#' PSIS-with-selective-refit pattern of [lfo_cv.mvgam()] (Bürkner,
#' Gabry and Vehtari 2020) from rolling time blocks to arbitrary
#' groups. One function covers three modes:
#'
#' \itemize{
#'   \item **Pure PSIS** (`pareto_k_threshold = Inf`): no refits.
#'     Cheapest, leave-one-group-out PSIS-LOO via `loo::loo()` on
#'     the group-aggregated log-lik matrix. Pareto-k diagnostics
#'     warn when the approximation is unreliable.
#'   \item **Hybrid** (default): PSIS first; refit only those
#'     folds whose Pareto-k exceeds `pareto_k_threshold`. Typical
#'     case is a handful of refits over K folds.
#'   \item **Exact** (`exact = TRUE`): refit every fold,
#'     brms-style. Most expensive; the gold standard when PSIS
#'     diagnostics are uniformly bad.
#' }
#'
#' Better than `brms::kfold` in two ways: (1) `group` accepts a
#' character vector (multi-column composite key, e.g.
#' `c("site", "season")`) and (2) the hybrid mode amortises the
#' refit cost. Both extensions sit cleanly on top of the existing
#' `loo` package fold helpers.
#'
#' @param x A fitted `mvgam` object.
#' @param K Integer fold count. When `group` is supplied and `K`
#'   is `NULL` (the default), uses one group per fold
#'   (leave-one-group-out). When `K` is set and smaller than the
#'   number of distinct groups, [loo::kfold_split_grouped()] bins
#'   groups into `K` folds. Without `group`, defaults to `K = 10`.
#' @param group Optional character vector naming columns in the
#'   training data that define the fold partitioning unit. All
#'   observations sharing a `group` tuple stay in the same fold.
#'   Default `NULL` partitions by closure unit on closure-unit
#'   families (via `closure_unit_grouping(family)`) and by row
#'   otherwise.
#' @param folds Optional integer vector with one fold assignment
#'   per row of the training data. Overrides `K`, `group`, and
#'   `fold_split`. Escape hatch for designs not expressible via
#'   the built-in splitters.
#' @param exact Logical. When `TRUE`, refit every fold and skip
#'   the PSIS path. Default `FALSE` uses the hybrid mode.
#' @param pareto_k_threshold Numeric in `[0, Inf]`. Folds whose
#'   PSIS Pareto-k exceeds this trigger a refit. `NULL` (default)
#'   uses the adaptive threshold `min(1 - 1 / log10(S), 0.7)`,
#'   where `S` is the number of posterior draws (Vehtari, Simpson,
#'   Gelman, Yao & Gabry 2024). Pass an explicit numeric to
#'   override, or `Inf` for pure PSIS (no refits ever).
#' @param fold_split One of `"grouped"`, `"stratified"`,
#'   `"random"`. The `loo::kfold_split_*` helper used when
#'   `K < n_groups`. Ignored when `K == n_groups` (one group per
#'   fold) or when `folds` is supplied. Default `"grouped"`.
#' @param seed Optional integer. Forwarded to `set.seed()` before
#'   the fold split so the partition is reproducible.
#' @param silent Integer in `\{0, 1, 2\}`. `0` prints per-fold
#'   progress; `1` (default) prints only the refit count; `2`
#'   silences output.
#' @param ... Ignored (reserved for future arguments).
#'
#' @return An object of class `c("mvgam_kfold", "kfold", "loo")`
#'   carrying `estimates` (elpd_kfold, p_kfold, kfoldic with SE),
#'   `pointwise` (per-group elpd contributions), `pareto_k`
#'   (per-group PSIS shape values, NA for refit folds), plus
#'   diagnostic slots:
#'   \itemize{
#'     \item `n_refits` --- number of folds refit.
#'     \item `refit_groups` --- character vector of refit fold
#'       labels.
#'     \item `K`, `group`, `pareto_k_threshold`, `exact` ---
#'       echoed call arguments.
#'   }
#'   The `loo` parent class lets [loo::loo_compare()] and
#'   `print.loo()` consume the result directly.
#'
#' @references
#' Bürkner P-C, Gabry J, Vehtari A (2020). Approximate
#'   leave-future-out cross-validation for Bayesian time series
#'   models. *Journal of Statistical Computation and Simulation*,
#'   90(14), 2499--2523. \doi{10.1080/00949655.2020.1783262}.
#'
#' Vehtari A, Gelman A, Gabry J (2017). Practical Bayesian model
#'   evaluation using leave-one-out cross-validation and WAIC.
#'   *Statistics and Computing*, 27, 1413--1432.
#'   \doi{10.1007/s11222-016-9696-4}.
#'
#' @seealso [loo.mvgam()], [lfo_cv.mvgam()], [log_lik.mvgam()],
#'   [update.mvgam()], [loo::kfold_split_grouped()]
#'
#' @examples
#' \dontrun{
#' # Use CAR() so the refit on fold-deleted rows still has a
#' # well-defined trend (AR refits would error on the resulting
#' # irregular time spacing).
#' set.seed(13)
#' simdat <- sim_mvgam(family = gaussian(), n_series = 1L,
#'                      n_timepoints = 120L, type = 6L)
#'
#' mod <- mvgam(y ~ s(season, bs = "cc"),
#'               trend_formula = ~ CAR(),
#'               data    = simdat$data_train,
#'               family  = gaussian(),
#'               chains  = 2, silent = 2)
#'
#' k <- kfold(mod, K = 3L, save_fits = FALSE)
#' k$estimates
#' }
#'
#' @author Nicholas J Clark
#' @method kfold mvgam
#' @importFrom loo kfold kfold_split_grouped kfold_split_random
#'   kfold_split_stratified psis pareto_k_values
#'   weights.importance_sampling
#' @export
kfold.mvgam <- function(x,
                        K = NULL,
                        group = NULL,
                        folds = NULL,
                        exact = FALSE,
                        pareto_k_threshold = NULL,
                        fold_split = c("grouped", "stratified",
                                       "random"),
                        seed = NULL,
                        silent = 1L,
                        ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_int(K, lower = 2L, null.ok = TRUE)
  checkmate::assert_character(group, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_integerish(folds, null.ok = TRUE,
                               any.missing = FALSE)
  checkmate::assert_flag(exact)
  checkmate::assert_number(pareto_k_threshold, lower = 0,
                            null.ok = TRUE)
  checkmate::assert_int(silent, lower = 0L, upper = 2L)
  fold_split <- match.arg(fold_split)
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }

  data <- mvgam_training_data(x)
  if (is.null(data)) {
    stop(insight::format_error(c(
      "Could not recover training data from the fit.",
      i = paste0(
        "kfold needs the original training data to partition ",
        "folds and refit. Refit with 'data = ...' and try again."
      )
    )))
  }

  # Resolve group columns + per-row group key. Closure-unit fits
  # default to closure_unit_grouping(family); other fits default
  # to row-level partitioning.
  group_info <- resolve_kfold_group(x, group, data)
  group_key <- group_info$key
  n_groups <- group_info$n_groups

  # Partition group_key into fold IDs (one per row of data).
  fold_ids <- build_kfold_partition(
    group_key = group_key, K = K, folds = folds,
    fold_split = fold_split
  )
  K_actual <- length(unique(fold_ids))

  # log_lik(x) on the full data: one matrix at the family's
  # natural grain (per-row or per-closure-unit). Reused for both
  # the PSIS path and as the diagnostic baseline.
  loglik_full <- log_lik(x)
  loglik_full <- clean_ll(x, loglik_full)

  # Map each log-lik column to its group + fold so the
  # aggregation is grain-aware (closure-unit fits already produce
  # per-unit log-lik; standard fits produce per-row).
  col_meta <- map_loglik_cols_to_groups(
    object = x, data = data,
    group_key = group_key, fold_ids = fold_ids,
    loglik_ncol = NCOL(loglik_full)
  )

  # Aggregate per-column log-lik into per-group log-lik
  # [draws x n_groups]. Sum log-lik across rows belonging to each
  # group (independence at the row grain).
  loglik_grouped <- aggregate_loglik_by_group(
    loglik_full, col_meta$col_group
  )
  group_labels <- colnames(loglik_grouped)

  # Resolve the numeric refit threshold. When the caller left
  # `pareto_k_threshold = NULL` (the default) apply the Vehtari
  # et al. (2024) adaptive rule from `S` posterior draws. See the
  # sibling logic in `lfo_cv.mvgam()` for the argument contract.
  pareto_k_threshold_used <- if (is.null(pareto_k_threshold)) {
    mvgam_ps_khat_threshold(nrow(loglik_grouped))
  } else {
    pareto_k_threshold
  }

  # Build the per-fold -> groups mapping (which group labels live
  # in which fold). Used for both the PSIS refit-trigger loop and
  # the exact refit loop.
  groups_per_fold <- split(group_labels, col_meta$group_to_fold)

  if (exact) {
    # Every-fold-refit mode: skip the PSIS pre-pass entirely.
    pointwise <- rep(NA_real_, length(group_labels))
    names(pointwise) <- group_labels
    pareto_k <- rep(NA_real_, length(group_labels))
    names(pareto_k) <- group_labels
    refit_groups <- group_labels
    pointwise_psis <- NULL
    pointwise <- exact_kfold_refit(
      object = x, data = data, fold_ids = fold_ids,
      group_key = group_key, pointwise = pointwise,
      silent = silent
    )
  } else {
    # Hybrid mode: PSIS-LOO on the aggregated matrix gives a
    # per-group elpd + Pareto-k. Refit only folds containing
    # high-k groups.
    psis_loo <- suppressWarnings(loo::loo(loglik_grouped))
    pointwise <- psis_loo$pointwise[, "elpd_loo"]
    names(pointwise) <- group_labels
    pareto_k <- psis_loo$diagnostics$pareto_k
    names(pareto_k) <- group_labels

    refit_folds <- which(vapply(
      groups_per_fold,
      function(g) any(pareto_k[g] > pareto_k_threshold_used),
      logical(1L)
    ))
    refit_groups <- unlist(groups_per_fold[refit_folds],
                           use.names = FALSE)

    # Snapshot the PSIS estimates BEFORE the refit splice so
    # `summary()` can report per-group lift (refit_elpd - psis_elpd)
    # and surface which folds the PSIS approximation got most wrong.
    pointwise_psis <- pointwise
    if (length(refit_folds) > 0L) {
      pointwise <- hybrid_kfold_refit(
        object = x, data = data, fold_ids = fold_ids,
        group_key = group_key, refit_fold_ids = refit_folds,
        pointwise = pointwise, silent = silent
      )
      # Refit ELPDs replace the PSIS estimate exactly, but we
      # KEEP the original PSIS Pareto-k values on `pareto_k` as a
      # diagnostic record. The plot then shows which folds had
      # high k (i.e. why the refit was triggered) instead of
      # silently dropping them from the Pareto-k panel.
    }
  }

  build_mvgam_kfold(
    pointwise = pointwise, pointwise_psis = pointwise_psis,
    pareto_k = pareto_k,
    refit_groups = refit_groups, K = K_actual,
    group = group_info$group_names,
    pareto_k_threshold = pareto_k_threshold,
    pareto_k_threshold_used = pareto_k_threshold_used,
    exact = exact
  )
}


# Internal: resolve user-supplied `group` arg into a per-row
# character key. Closure-unit families default to the family's
# `closure_unit_grouping()` attr; otherwise each row gets a unique
# key (= row index) for pure k-fold by row.
#
# @noRd
resolve_kfold_group <- function(object, group, data) {
  if (is.null(group)) {
    closure_cols <- closure_unit_grouping(object$family)
    if (!is.null(closure_cols)) {
      group <- closure_cols
    } else {
      return(list(
        key = as.character(seq_len(NROW(data))),
        n_groups = NROW(data),
        group_names = NULL
      ))
    }
  }
  missing_cols <- setdiff(group, names(data))
  if (length(missing_cols) > 0L) {
    stop(insight::format_error(c(
      "Some 'group' columns are missing from the training data.",
      x = paste0("Missing: ",
                 paste(missing_cols, collapse = ", "), "."),
      i = paste0("Available columns: ",
                 paste(names(data), collapse = ", "), ".")
    )))
  }
  vals <- lapply(group, function(g) as.character(data[[g]]))
  key <- do.call(paste, c(vals, list(sep = "_")))
  list(
    key = key,
    n_groups = length(unique(key)),
    group_names = group
  )
}


# Internal: build per-row fold IDs given a group key. Delegates
# to `loo::kfold_split_grouped` / `_random` / `_stratified` so the
# binning math is upstream-tested.
#
# @noRd
build_kfold_partition <- function(group_key, K = NULL,
                                  folds = NULL,
                                  fold_split = "grouped") {
  if (!is.null(folds)) {
    if (length(folds) != length(group_key)) {
      stop(insight::format_error(c(
        "'folds' length does not match the number of training rows.",
        x = paste0("folds: ", length(folds),
                   ", rows: ", length(group_key), ".")
      )))
    }
    return(as.integer(folds))
  }
  n_groups <- length(unique(group_key))
  if (is.null(K)) {
    K <- if (n_groups < NROW(group_key)) {
      n_groups
    } else {
      min(10L, n_groups)
    }
  }
  if (K > n_groups) {
    stop(insight::format_error(c(
      "'K' exceeds the number of distinct groups.",
      x = paste0("K: ", K, ", groups: ", n_groups, "."),
      i = "Reduce 'K' or partition by a finer 'group' key."
    )))
  }
  if (K < 2L) {
    stop(insight::format_error("'K' must be at least 2."))
  }
  # loo::kfold_split_* returns numeric; coerce to integer so
  # downstream vapply(..., integer(1L)) calls land on the
  # expected storage mode.
  as.integer(switch(
    fold_split,
    "grouped"    = loo::kfold_split_grouped(K = K, x = group_key),
    "stratified" = loo::kfold_split_stratified(
      K = K, x = group_key
    ),
    "random"     = loo::kfold_split_random(
      K = K, N = length(group_key)
    )
  ))
}


# Internal: map each log-lik column to its group label + fold ID.
# Standard families produce one log-lik column per row; closure-
# unit families produce one column per closure unit. The per-unit
# ordering is reproduced directly from `closure_unit_grouping`
# (first-appearance order over `data`), matching what
# `build_closure_unit_arrays()` does inside `log_lik.mvgam`.
#
# Returns:
#   col_group     - character vector of length `loglik_ncol`,
#                    each entry the group key for that column.
#   group_to_fold - integer vector of length n_groups, fold ID
#                    per unique group label.
#
# @noRd
map_loglik_cols_to_groups <- function(object, data, group_key,
                                       fold_ids, loglik_ncol) {
  if (loglik_ncol == NROW(data)) {
    col_group <- group_key
  } else {
    # closure_unit_grouping() returns the explicit grouping when
    # set (e.g. multi-season c("series","site","time")) and NULL
    # for default-mode closure-unit families. The default key in
    # build_closure_unit_arrays() is c("series", "time"); fall
    # back to that when the family is closure-unit but the attr
    # is unset.
    closure_cols <- closure_unit_grouping(object$family)
    if (is.null(closure_cols)) {
      if (is_closure_unit_family(object$family)) {
        closure_cols <- c("series", "time")
      } else {
        stop(insight::format_error(c(
          "Could not align log-lik columns with rows of data.",
          x = paste0("log_lik has ", loglik_ncol,
                     " cols; data has ", NROW(data), " rows."),
          i = paste0("Expected per-row or per-closure-unit log-",
                     "lik; the family is not closure-unit so the ",
                     "two should match.")
        )))
      }
    }
    # Mirror build_closure_unit_arrays() ordering: per-row unit
    # key, unique in first-appearance order. Each unit's group
    # key is read from the first row matching that unit, leaning
    # on closure_unit_grouping's invariance within a unit.
    row_unit_key <- do.call(paste, c(
      lapply(closure_cols,
             function(c) as.character(data[[c]])),
      list(sep = "_")
    ))
    unit_levels <- unique(row_unit_key)
    first_row_per_unit <- match(unit_levels, row_unit_key)
    col_group <- group_key[first_row_per_unit]
    if (length(col_group) != loglik_ncol) {
      stop(insight::format_error(c(
        "Closure-unit alignment count mismatch.",
        x = paste0("Reconstructed ", length(col_group),
                   " units from data; log_lik has ", loglik_ncol,
                   " columns."),
        i = paste0("The fit's training data may not match the ",
                   "closure-unit layout the model was fit with.")
      )))
    }
  }
  unique_groups <- unique(col_group)
  # For each unique group, look up its fold via the first row
  # carrying that group key. Group integrity (all rows of a group
  # in the same fold) is guaranteed by build_kfold_partition.
  group_to_fold <- vapply(
    unique_groups,
    function(g) fold_ids[match(g, group_key)],
    integer(1L)
  )
  names(group_to_fold) <- unique_groups
  list(col_group = col_group, group_to_fold = group_to_fold)
}


# Internal: collapse a [draws x n_cols] log-lik matrix into
# [draws x n_groups] by summing within each group (rows of a
# group are independent given the model, so log-densities add).
# Handles the single-group case directly (stats::model.matrix
# refuses to build contrasts for a 1-level factor).
#
# @noRd
aggregate_loglik_by_group <- function(loglik, col_group) {
  group_levels <- unique(col_group)
  if (length(group_levels) == 1L) {
    agg <- matrix(rowSums(loglik), ncol = 1L)
    colnames(agg) <- group_levels
    return(agg)
  }
  group_fac <- factor(col_group, levels = group_levels)
  indicator <- stats::model.matrix(~ 0 + group_fac)
  agg <- loglik %*% indicator
  colnames(agg) <- levels(group_fac)
  agg
}


# Internal: per-fold refit. Calls update.mvgam(object, newdata =
# train) WITHOUT a `prior` arg so the parent's pinned prior table
# is inherited (#188), preserving stancode and the compiled
# binary across refits. Returns per-group ELPD contributions for
# the held-out fold.
#
# @noRd
refit_score_one_fold <- function(object, data, fold_ids,
                                 group_key, fold_id, silent) {
  train_rows <- which(fold_ids != fold_id)
  held_rows <- which(fold_ids == fold_id)
  train_data <- data[train_rows, , drop = FALSE]
  held_data <- data[held_rows, , drop = FALSE]

  if (silent < 1L) {
    cat("kfold refit at fold", fold_id,
        "(", length(held_rows), "rows held out) ...\n")
  }

  refit <- update(object, newdata = train_data,
                  silent = max(silent, 1L))

  # Held data may contain factor levels the refit never saw (the
  # whole point of leave-one-group-out is that the group was
  # excluded from the refit). Two valid responses:
  #   re_formula = NA  -> drop random effects from prediction
  #                       (score the marginal / fixed-effects model)
  #   allow_new_levels + sample_new_levels -> sample new REs from
  #                       the population (more honest but brittle
  #                       for non-Gaussian RE structures; see brms
  #                       issue #1779).
  # Default to the safer marginal path; users can override via
  # the `...` plumbed through kfold -> here.
  held_loglik <- log_lik(
    refit, newdata = held_data,
    re_formula = NA,
    allow_new_levels = TRUE
  )
  held_loglik <- clean_ll(refit, held_loglik)

  # Align held log-lik columns to per-column group keys. Two
  # cases: log_lik columns map one-to-one to held rows (standard
  # families), OR one-to-one to held closure units (occ / nmix).
  if (NCOL(held_loglik) == NROW(held_data)) {
    held_col_group <- group_key[held_rows]
  } else {
    # See map_loglik_cols_to_groups() for the rationale.
    closure_cols <- closure_unit_grouping(object$family)
    if (is.null(closure_cols)) {
      if (is_closure_unit_family(object$family)) {
        closure_cols <- c("series", "time")
      } else {
        stop(insight::format_error(c(
          "Held log-lik shape does not match held data row count.",
          x = paste0(
            "log_lik returned ", NCOL(held_loglik),
            " cols for ", NROW(held_data), " held rows."
          )
        )))
      }
    }
    # Closure-unit grain: held units appear in first-appearance
    # order over held_data rows (matches build_closure_unit_arrays).
    held_unit_key <- do.call(paste, c(
      lapply(closure_cols,
             function(c) as.character(held_data[[c]])),
      list(sep = "_")
    ))
    unit_levels <- unique(held_unit_key)
    first_row_per_unit <- match(unit_levels, held_unit_key)
    held_col_group <- group_key[held_rows[first_row_per_unit]]
  }

  held_grouped <- aggregate_loglik_by_group(
    held_loglik, held_col_group
  )

  # Per-group ELPD: log_mean_exp over draws.
  apply(held_grouped, 2L, lfo_log_mean_exp)
}


# Internal: exact-mode wrapper. Refit every fold in fold_ids and
# splice the held-out ELPDs into `pointwise` keyed by group label.
#
# @noRd
exact_kfold_refit <- function(object, data, fold_ids, group_key,
                              pointwise, silent) {
  for (f in sort(unique(fold_ids))) {
    fold_elpds <- refit_score_one_fold(
      object = object, data = data, fold_ids = fold_ids,
      group_key = group_key, fold_id = f, silent = silent
    )
    pointwise[names(fold_elpds)] <- fold_elpds
  }
  pointwise
}


# Internal: hybrid-mode wrapper. Refit only the folds flagged by
# the PSIS Pareto-k pre-pass, splicing the exact ELPDs over the
# PSIS estimates for those folds' groups.
#
# @noRd
hybrid_kfold_refit <- function(object, data, fold_ids, group_key,
                               refit_fold_ids, pointwise, silent) {
  if (silent < 2L) {
    cat("kfold: refitting", length(refit_fold_ids),
        "fold(s) with Pareto-k above threshold ...\n")
  }
  for (f in refit_fold_ids) {
    fold_elpds <- refit_score_one_fold(
      object = object, data = data, fold_ids = fold_ids,
      group_key = group_key, fold_id = f, silent = silent
    )
    pointwise[names(fold_elpds)] <- fold_elpds
  }
  pointwise
}


#' Pretty-print an `mvgam_kfold` object
#'
#' One-screen summary: total ELPD with SE, fold count, refit
#' count, group key, and Pareto-k summary stats for the PSIS
#' folds that were NOT refit. Falls back to `print.loo()` style
#' for the headline numbers.
#'
#' @param x An `mvgam_kfold` object.
#' @param digits Integer; significant digits for ELPD / SE.
#'   Default `2`.
#' @param ... Ignored.
#'
#' @return `invisible(x)`.
#'
#' @method print mvgam_kfold
#' @export
print.mvgam_kfold <- function(x, digits = 2L, ...) {
  cat("mvgam k-fold cross-validation\n")
  cat(sprintf(
    "  elpd_kfold = %s  (SE %s)\n",
    formatC(x$elpd_kfold, digits = digits, format = "f"),
    formatC(x$se_elpd_kfold, digits = digits, format = "f")
  ))
  cat(sprintf(
    "  kfoldic    = %s  (SE %s)\n",
    formatC(-2 * x$elpd_kfold, digits = digits, format = "f"),
    formatC(2 * x$se_elpd_kfold, digits = digits, format = "f")
  ))
  cat(sprintf("  K          = %d folds\n", x$K))
  group_lbl <- if (is.null(x$group)) {
    "<per row>"
  } else {
    paste(x$group, collapse = " x ")
  }
  cat(sprintf("  group      = %s\n", group_lbl))
  cat(sprintf(
    "  mode       = %s\n",
    if (isTRUE(x$exact)) "exact (every fold refit)" else "hybrid"
  ))
  cat(sprintf(
    "  refits     = %d of %d folds\n",
    x$n_refits, x$K
  ))
  psis_k <- x$pareto_k[!is.na(x$pareto_k)]
  if (length(psis_k) > 0L) {
    # Adaptive-default fits stash the effective threshold on
    # `pareto_k_threshold_used`; older fits fall back to the
    # user-supplied `pareto_k_threshold`.
    threshold_val <- x$pareto_k_threshold_used %||%
      x$pareto_k_threshold
    threshold_lbl <- if (is.null(x$pareto_k_threshold)) {
      paste0(formatC(threshold_val, digits = digits, format = "f"),
             " (adaptive)")
    } else {
      formatC(threshold_val, digits = digits, format = "f")
    }
    cat(sprintf(
      "  Pareto-k   = max %s, median %s (PSIS folds, threshold %s)\n",
      formatC(max(psis_k), digits = digits, format = "f"),
      formatC(stats::median(psis_k),
              digits = digits, format = "f"),
      threshold_lbl
    ))
  }

  # Truncated diagnostic lists: top ~15% of folds (min 3, max 10)
  # by lowest ELPD contribution (worst-fit groups) and by largest
  # |refit lift| (where the cheap PSIS approximation would have
  # been most misleading). The second list is only shown when the
  # hybrid mode actually triggered refits.
  n_show <- max(3L, min(10L, ceiling(x$K * 0.15)))
  s <- summary(x)
  worst_idx <- order(s$elpd, na.last = TRUE)[seq_len(
    min(n_show, NROW(s))
  )]
  cat(sprintf(
    "\n  Worst-fit folds%s:\n",
    if (NROW(s) > n_show)
      sprintf(" (top %d of %d)", length(worst_idx), x$K)
    else ""
  ))
  for (i in worst_idx) {
    cat(sprintf(
      "    %-20s  elpd = %s  (%s%% of total)\n",
      s$group[i],
      formatC(s$elpd[i], digits = digits, format = "f"),
      formatC(100 * s$elpd_share[i], digits = 1L, format = "f")
    ))
  }
  # Lift is only defined when pointwise_psis was captured before
  # the refit splice; some build paths (tests, exact-mode refits)
  # omit it, so guard the loop on the actual count of non-NA lifts.
  n_lift <- sum(!is.na(s$lift))
  if (isTRUE(x$n_refits > 0L) && n_lift > 0L) {
    lift_idx <- order(abs(s$lift), decreasing = TRUE,
                      na.last = TRUE)
    lift_idx <- lift_idx[!is.na(s$lift[lift_idx])][
      seq_len(min(n_show, n_lift))
    ]
    cat(sprintf(
      "\n  Most-influential folds%s:\n",
      if (n_lift > n_show)
        sprintf(" (top %d of %d refits)",
                length(lift_idx), n_lift)
      else ""
    ))
    for (i in lift_idx) {
      sign_lift <- if (s$lift[i] >= 0) "+" else ""
      cat(sprintf(
        "    %-20s  lift = %s%s  (PSIS %s, refit %s)\n",
        s$group[i], sign_lift,
        formatC(s$lift[i], digits = digits, format = "f"),
        formatC(s$elpd_psis[i], digits = digits, format = "f"),
        formatC(s$elpd[i], digits = digits, format = "f")
      ))
    }
  }

  # Footnote-style definitions (mirrors the summary.mvgam Rhat /
  # Bulk_ESS footer pattern). Plain-language so an ecologist can
  # lift the wording into a manuscript without translation.
  group_word <- if (is.null(x$group)) "fold" else x$group[1L]
  cat("\n")
  footer <- c(
    paste0(
      "Worst-fit ", group_word, "s were the held-out groups the ",
      "model predicted least accurately and so contributed most ",
      "to the negative cross-validation score. These are the ",
      "groups whose data the current model structure cannot ",
      "reproduce well, suggesting unmeasured covariates or local ",
      "processes the global model is averaging over."
    ),
    paste0(
      "Most-influential ", group_word, "s were the groups whose ",
      "predictive score changed most when the model was refit ",
      "without them. The fitted model is noticeably tuned to ",
      "their data, so dropping them shifts the inference."
    )
  )
  for (para in footer) {
    cat(strwrap(para, width = 72), sep = "\n")
    cat("\n")
  }
  invisible(x)
}


#' Tibble summary of an `mvgam_kfold` object
#'
#' Long-format tibble with one row per group, carrying the
#' per-group ELPD, Pareto-k (NA for refit folds), and a refit
#' flag. Useful for downstream filtering / plotting.
#'
#' @param object An `mvgam_kfold` object.
#' @param ... Ignored.
#'
#' @return A tibble with columns `group`, `elpd`, `pareto_k`,
#'   `refit`.
#'
#' @method summary mvgam_kfold
#' @export
summary.mvgam_kfold <- function(object, ...) {
  group <- rownames(object$pointwise) %||%
    names(object$pareto_k)
  elpd <- as.numeric(object$pointwise[, "elpd_kfold"])
  total <- sum(elpd)
  elpd_psis <- if (!is.null(object$pointwise_psis)) {
    as.numeric(object$pointwise_psis[group])
  } else {
    rep(NA_real_, length(group))
  }
  # Lift = exact refit ELPD - PSIS ELPD. Only defined for folds
  # that triggered a refit; NA for everything else.
  refit_flag <- group %in% object$refit_groups
  lift <- ifelse(refit_flag, elpd - elpd_psis, NA_real_)
  tibble::tibble(
    group = group,
    elpd = elpd,
    elpd_share = if (total != 0) elpd / total else NA_real_,
    elpd_psis = elpd_psis,
    lift = lift,
    pareto_k = as.numeric(object$pareto_k),
    refit = refit_flag
  )
}


#' Diagnostic plot for an `mvgam_kfold` object
#'
#' Faceted ggplot matching the [plot.mvgam_lfo()] convention: one
#' panel per diagnostic with outlier folds highlighted. The Pareto-k
#' panel flags groups above `pareto_k_threshold` (PSIS approximation
#' unreliable, refit recommended); the ELPD panel flags groups below
#' the 15% quantile (poorest-fit folds). Inlier groups are drawn in
#' grey, outliers in the package red.
#'
#' @param x An `mvgam_kfold` object.
#' @param ... Ignored.
#'
#' @return A `ggplot` object.
#'
#' @seealso [plot.mvgam_lfo()] for the matching time-block CV plot.
#'
#' @method plot mvgam_kfold
#' @export
plot.mvgam_kfold <- function(x, ...) {
  # Pin the scheme the way every other mvgam figure does, so a
  # user's own `bayesplot::color_scheme_set()` does not leave this
  # panel the odd one out.
  set_color_scheme_local("red")
  group <- rownames(x$pointwise) %||% names(x$pareto_k)
  elpd <- as.numeric(x$pointwise[, "elpd_kfold"])
  elpd_threshold <- stats::quantile(
    elpd, probs = 0.15, na.rm = TRUE
  )
  elpd_dat <- data.frame(
    group = group, value = elpd,
    threshold = as.numeric(elpd_threshold),
    facet = "ELPD"
  )
  # Read the numeric threshold applied at the refit gate. Adaptive
  # fits carry it on `pareto_k_threshold_used`; older fits fall
  # back to `pareto_k_threshold`.
  threshold_val <- x$pareto_k_threshold_used %||%
    x$pareto_k_threshold
  k_dat <- data.frame(
    group = group,
    value = as.numeric(x$pareto_k),
    threshold = threshold_val,
    facet = "Pareto k"
  )
  long <- rbind(elpd_dat, k_dat)
  long$group <- factor(long$group, levels = group)
  long$colour <- ifelse(
    (long$facet == "Pareto k" &
       !is.na(long$value) & long$value > long$threshold) |
      (long$facet == "ELPD" & long$value < long$threshold),
    "outlier", "inlier"
  )

  ggplot2::ggplot(
    long,
    ggplot2::aes(x = .data$group, y = .data$value)
  ) +
    ggplot2::facet_wrap(~ .data$facet, ncol = 1L,
                        scales = "free_y") +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = .data$threshold),
      colour = mvgam_colour("mid_highlight"),
      linetype = "dashed", linewidth = 1,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(group = .data$facet),
      linewidth = 0.5, colour = "grey30", na.rm = TRUE
    ) +
    ggplot2::geom_point(shape = 16, colour = "white", size = 2,
                        na.rm = TRUE) +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data$colour),
      shape = 16, show.legend = FALSE, size = 1.5,
      na.rm = TRUE
    ) +
    ggplot2::scale_colour_manual(
      values = c(inlier = "grey30", outlier = mvgam_colour("dark"))
    ) +
    ggplot2::labs(
      x = if (is.null(x$group)) "row" else
        paste(x$group, collapse = " x "),
      y = NULL
    ) +
    mvgam_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


# Internal: build the `mvgam_kfold` return object. Inherits from
# the `loo` and `kfold` classes so `loo_compare()`, `print.loo`,
# and `loo_model_weights()` work without further glue code.
#
# @noRd
build_mvgam_kfold <- function(pointwise, pointwise_psis = NULL,
                              pareto_k, refit_groups,
                              K, group, pareto_k_threshold,
                              pareto_k_threshold_used = pareto_k_threshold,
                              exact) {
  elpd <- sum(pointwise)
  se_elpd <- sqrt(length(pointwise) * stats::var(pointwise))

  estimates <- matrix(
    c(elpd, se_elpd,
      -2 * elpd, 2 * se_elpd),
    nrow = 2L, byrow = TRUE,
    dimnames = list(
      c("elpd_kfold", "kfoldic"),
      c("Estimate", "SE")
    )
  )
  pointwise_mat <- cbind(elpd_kfold = pointwise,
                         kfoldic = -2 * pointwise)
  out <- list(
    estimates = estimates,
    pointwise = pointwise_mat,
    pointwise_psis = pointwise_psis,
    elpd_kfold = elpd,
    se_elpd_kfold = se_elpd,
    pareto_k = pareto_k,
    n_refits = length(refit_groups),
    refit_groups = refit_groups,
    K = K,
    group = group,
    pareto_k_threshold = pareto_k_threshold,
    pareto_k_threshold_used = pareto_k_threshold_used,
    exact = exact
  )
  class(out) <- c("mvgam_kfold", "kfold", "loo")
  out
}
