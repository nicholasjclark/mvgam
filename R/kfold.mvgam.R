# K-fold cross-validation for mvgam fits, with optional grouping and
# the selective refits of `lfo_cv.mvgam()` applied to groups of rows.

#' K-fold cross-validation for `mvgam` fits
#'
#' @description
#' Measures how well a model predicts groups of data it was not fitted
#' to, such as sites or species. Each fold of groups is held out in
#' turn and scored by its expected log predictive density (ELPD).
#' Higher is better. A fold that importance sampling approximates well takes the
#' PSIS estimate of [loo::loo()] on the fold's summed log likelihood.
#' The rest are refitted without the fold, as [lfo_cv.mvgam()] refits
#' the time blocks it cannot approximate. Three settings cover the
#' range:
#'
#' \itemize{
#'   \item `pareto_k_threshold = Inf` refits nothing. Pareto-k
#'     warnings mark folds the approximation fits poorly.
#'   \item The default refits the folds whose Pareto-k exceeds
#'     `pareto_k_threshold`, which is often a handful.
#'   \item `exact = TRUE` refits every fold, as `brms::kfold()` does.
#' }
#'
#' `group` takes several columns, such as `c("site", "season")`. Every
#' row of one combination stays in the same fold.
#'
#' @param x A fitted `mvgam` object.
#' @param K Integer number of folds. With `group` and `K = NULL` (the
#'   default), each group is a fold. A `K` below the number of groups
#'   bins the groups into `K` folds with [loo::kfold_split_grouped()].
#'   Without `group`, `K` defaults to 10.
#' @param group Optional character vector of training-data columns
#'   whose combinations define the groups. `NULL` (the default) takes
#'   the closure unit of an [occ()] or [nmix()] model and the row
#'   otherwise.
#' @param folds Optional integer vector of one fold per training row,
#'   replacing `K` and `fold_split`. Each fold is then held out and
#'   scored as one group. A fold never splits a group, which is a
#'   closure unit by default.
#' @param exact Logical. `TRUE` refits every fold. Defaults to `FALSE`.
#' @param pareto_k_threshold Numeric. A fold whose Pareto-k exceeds
#'   it is refitted. `NULL` (the default) takes
#'   `min(1 - 1 / log10(S), 0.7)` for `S` posterior draws. `Inf`
#'   refits nothing.
#' @param fold_split `"grouped"` (the default), `"stratified"` or
#'   `"random"`: the `loo::kfold_split_*()` function binning groups
#'   into `K` folds when there are more groups than folds.
#' @param seed Optional integer seed for the fold split and the refits.
#'   The session's random state is restored when the call returns.
#' @param silent Integer in `\{0, 1, 2\}`. `0` prints each refit
#'   and its sampler start and finish lines. `1` (default) prints
#'   the refit count. `2` prints nothing.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `c("mvgam_kfold", "kfold", "loo")` object, which
#'   [loo::loo_compare()] takes as it takes [loo.mvgam()] results. It
#'   holds:
#'   \itemize{
#'     \item `estimates`: `elpd_kfold`, `p_kfold` and `kfoldic` with
#'       their standard errors.
#'     \item `pointwise`: the ELPD of each group. `pointwise_psis` holds
#'       the PSIS estimates before any refit.
#'     \item `pareto_k`: the Pareto-k of each group's PSIS estimate, all
#'       `NA` under `exact = TRUE`.
#'     \item `n_refits` and `refit_groups`: the number of refitted folds
#'       and the groups they hold.
#'     \item `K` and `exact`: the number of folds and the mode.
#'     \item `group`: the grouping columns, `"fold"` when `folds` was
#'       given, or `NULL` for single rows.
#'     \item `pareto_k_threshold`: the threshold applied, with
#'       `pareto_k_threshold_adaptive` `TRUE` when the default rule set
#'       it.
#'   }
#'
#' @references
#' Burkner, P.-C., Gabry, J. and Vehtari, A. (2020). Approximate
#'   leave-future-out cross-validation for Bayesian time series
#'   models. *Journal of Statistical Computation and Simulation*,
#'   90(14):2499-2523. \doi{10.1080/00949655.2020.1783262}
#'
#' Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical Bayesian
#'   model evaluation using leave-one-out cross-validation and WAIC.
#'   *Statistics and Computing*, 27:1413-1432.
#'   \doi{10.1007/s11222-016-9696-4}
#'
#' @seealso [loo.mvgam()] approximates leave-one-out cross-validation.
#'   [lfo_cv.mvgam()] scores forecasts from rolling origins.
#'   [summary.mvgam_kfold()] and [plot.mvgam_kfold()] report each group.
#'   The
#'   `r mvgam_article("forecast_evaluation", "forecast evaluation article")`
#'   compares models by cross-validation.
#'
#' @examples
#' \dontrun{
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
#' # Neighbouring times share information through the trend. Folds of
#' # contiguous time blocks hold each block out whole. Folds that PSIS
#' # approximates poorly are refitted.
#' blocks <- cut(simdat$data_train$time, breaks = 4L, labels = FALSE)
#' k <- kfold(mod, folds = blocks)
#' k
#'
#' # The ELPD and Pareto k of each block
#' plot(k)
#'
#' # Next, lfo_cv() scores forecasts made from rolling origins
#' }
#'
#' @author Nicholas J Clark
#' @family model comparison
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
  rlang::check_dots_empty()
  local_seed(seed)

  data <- x$data
  if (is.null(data)) {
    stop(insight::format_error(c(
      "'kfold()' requires the fit's training data.",
      x = "The fitted object's training-data slot is empty.",
      i = "Refit with 'data = ...' and call 'kfold()' again."
    )), call. = FALSE)
  }

  group_info <- resolve_kfold_group(x, group, data)
  fold_ids <- build_kfold_partition(
    group_key = group_info$key, K = K, folds = folds,
    fold_split = fold_split
  )
  # A supplied fold is held out and scored as one group. PSIS then
  # approximates leaving the whole fold out.
  if (!is.null(folds)) {
    group_info <- list(key = as.character(fold_ids), group_names = "fold")
  }
  group_key <- group_info$key
  K_actual <- length(unique(fold_ids))

  # One column per row, or per unit for a closure-unit family. Columns
  # are mapped to groups and folds before `clean_ll()` drops any.
  loglik_full <- log_lik(x)
  col_meta <- map_loglik_cols_to_groups(
    object = x, data = data,
    group_key = group_key, fold_ids = fold_ids,
    loglik_ncol = NCOL(loglik_full)
  )

  # The column groups are narrowed to the columns `clean_ll()` keeps.
  # `group_to_fold` keeps one entry per group, named by the group. A
  # group whose rows all lack a response leaves no column. The
  # surviving groups take their folds by name.
  loglik_full <- clean_ll(loglik_full)
  col_meta$col_group <- narrow_to_scored(col_meta$col_group, loglik_full)

  loglik_grouped <- aggregate_loglik_by_group(
    loglik_full, col_meta$col_group
  )
  group_labels <- colnames(loglik_grouped)

  pareto_k_threshold_used <- resolve_pareto_k_threshold(
    pareto_k_threshold, nrow(loglik_grouped)
  )

  groups_per_fold <- split(group_labels,
                           col_meta$group_to_fold[group_labels])

  if (exact) {
    pointwise <- rep(NA_real_, length(group_labels))
    names(pointwise) <- group_labels
    pareto_k <- rep(NA_real_, length(group_labels))
    names(pareto_k) <- group_labels
    refit_groups <- group_labels
    refit_folds <- sort(unique(fold_ids))
    pointwise_psis <- NULL
    pointwise <- kfold_refit_folds(
      object = x, data = data, fold_ids = fold_ids,
      group_key = group_key,
      refit_fold_ids = refit_folds,
      pointwise = pointwise, silent = silent
    )
  } else {
    # PSIS on the group sums gives each group's ELPD and Pareto k. A
    # fold holding a group above the threshold is refitted. Its
    # Pareto k is kept for the plot.
    psis_loo <- with_pareto_k_handled(loo::loo(loglik_grouped, r_eff = 1))
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

    # `summary()` reports the change from the PSIS estimate to the
    # refit
    pointwise_psis <- pointwise
    if (length(refit_folds) > 0L) {
      pointwise <- kfold_refit_folds(
        object = x, data = data, fold_ids = fold_ids,
        group_key = group_key, refit_fold_ids = refit_folds,
        pointwise = pointwise, silent = silent
      )
    }
  }

  build_mvgam_kfold(
    pointwise = pointwise, pointwise_psis = pointwise_psis,
    pareto_k = pareto_k,
    refit_groups = refit_groups, K = K_actual,
    group = group_info$group_names,
    n_refit_folds = length(refit_folds),
    pareto_k_threshold = pareto_k_threshold_used,
    pareto_k_threshold_adaptive = is.null(pareto_k_threshold),
    exact = exact
  )
}


#' The fold key of each training row
#'
#' A closure-unit family keys by the unit its likelihood is written
#' over, holding out whole units. `closure_unit_key_vars()` supplies
#' the unit columns for every closure-unit family. Any other fit gives
#' each row its own key.
#'
#' @param object Fitted `mvgam` object
#' @param group Character vector of grouping columns, or `NULL`
#' @param data Data frame of training data
#' @return List holding the per-row character `key` and the grouping
#'   columns in `group_names`
#' @noRd
resolve_kfold_group <- function(object, group, data) {
  checkmate::assert_list(object)
  checkmate::assert_character(group, min.len = 1L, null.ok = TRUE)
  checkmate::assert_data_frame(data)
  if (is.null(group)) {
    closure_cols <- fit_closure_unit_keys(object)
    if (!is.null(closure_cols)) {
      group <- closure_cols
    } else {
      return(list(
        key = as.character(seq_len(NROW(data))),
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
    )), call. = FALSE)
  }
  vals <- lapply(group, function(g) as.character(data[[g]]))
  list(
    key = do.call(paste, c(vals, list(sep = "_"))),
    group_names = group
  )
}


#' The fold of each training row
#'
#' Binning goes through the `loo::kfold_split_*()` functions. Every row
#' of a group lands in the same fold. Supplied `folds` must keep each
#' group whole.
#'
#' @param group_key Character vector of per-row group keys
#' @param K Integer number of folds, or `NULL` for a default
#' @param folds Integer vector of user-supplied fold indices, or `NULL`
#' @param fold_split Character split method, `"grouped"`,
#'   `"stratified"` or `"random"`
#' @return Integer vector of fold indices, one per row
#' @noRd
build_kfold_partition <- function(group_key, K = NULL,
                                  folds = NULL,
                                  fold_split = "grouped") {
  checkmate::assert_character(group_key, min.len = 1L)
  checkmate::assert_int(K, null.ok = TRUE)
  checkmate::assert_integerish(folds, null.ok = TRUE)
  checkmate::assert_choice(fold_split, c("grouped", "stratified", "random"))
  if (!is.null(folds)) {
    if (length(folds) != length(group_key)) {
      stop(insight::format_error(c(
        "'folds' must give one fold per training row.",
        x = paste0("Got ", length(folds), " folds for ",
                   length(group_key), " rows.")
      )), call. = FALSE)
    }
    split_groups <- names(which(
      tapply(folds, group_key, function(f) length(unique(f))) > 1L
    ))
    if (length(split_groups)) {
      stop(insight::format_error(c(
        "'folds' must keep every row of a group in one fold.",
        x = paste0("Split across folds: ",
                   paste0("'", utils::head(split_groups, 5L), "'",
                          collapse = ", "), "."),
        i = paste0("An occupancy or N-mixture likelihood is written per ",
                   "site, which a fold holds out whole.")
      )), call. = FALSE)
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
    )), call. = FALSE)
  }
  if (K < 2L) {
    stop(insight::format_error(
      "Cross-validation needs at least 2 groups."
    ), call. = FALSE)
  }
  # `loo::kfold_split_*()` returns doubles
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


#' The group and fold of each log-likelihood column
#'
#' `loglik_col_values()` maps the per-row keys to the grain of the
#' family's log-likelihood columns.
#'
#' @param object Fitted `mvgam` object
#' @param data Data frame of training data
#' @param group_key Character vector of per-row group keys
#' @param fold_ids Integer vector of per-row fold indices
#' @param loglik_ncol Integer number of log-likelihood columns
#' @return List holding the group key of each column in `col_group`
#'   with the fold of each distinct group, named by its key, in
#'   `group_to_fold`
#' @noRd
map_loglik_cols_to_groups <- function(object, data, group_key,
                                       fold_ids, loglik_ncol) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(data)
  checkmate::assert_character(group_key)
  checkmate::assert_integerish(fold_ids, len = length(group_key))
  checkmate::assert_int(loglik_ncol, lower = 0L)
  col_group <- loglik_col_values(object, data, group_key, loglik_ncol)
  unique_groups <- unique(col_group)
  # `build_kfold_partition()` keeps each group in one fold. The fold
  # of its first row is the fold of the group.
  group_to_fold <- vapply(
    unique_groups,
    function(g) fold_ids[match(g, group_key)],
    integer(1L)
  )
  names(group_to_fold) <- unique_groups
  list(col_group = col_group, group_to_fold = group_to_fold)
}


#' Log-likelihood columns summed within each group
#'
#' Rows of a group are independent given the model, making their
#' log-densities additive. A single group is summed directly. The
#' `stats::model.matrix()` indicator refuses a one-level factor.
#'
#' @param loglik Numeric log-likelihood matrix, one row per draw
#' @param col_group Character vector of the group of each column
#' @return Numeric matrix with one column per group, named by group
#' @noRd
aggregate_loglik_by_group <- function(loglik, col_group) {
  checkmate::assert_matrix(loglik)
  checkmate::assert_character(col_group, len = ncol(loglik))
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


#' The held-out ELPD of each group in one refitted fold
#'
#' The refit goes through `update()` without a `prior` argument. The
#' refit then inherits the parent's prior table, keeping the Stan code
#' and the compiled model. Held-out rows are scored with
#' `re_formula = NA`, dropping group-level effects for levels the refit
#' never saw.
#'
#' @param object Fitted `mvgam` object
#' @param data Data frame of training data
#' @param fold_ids Integer vector of per-row fold indices
#' @param group_key Character vector of per-row group keys
#' @param fold_id Integer index of the fold to hold out
#' @param silent Integer verbosity level
#' @return Named numeric vector of ELPD contributions, one per held-out
#'   group
#' @noRd
refit_score_one_fold <- function(object, data, fold_ids,
                                 group_key, fold_id, silent) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(data)
  checkmate::assert_integerish(fold_ids, len = nrow(data))
  checkmate::assert_character(group_key, len = nrow(data))
  checkmate::assert_int(fold_id)
  checkmate::assert_int(silent, lower = 0L)
  held_rows <- which(fold_ids == fold_id)
  train_data <- mask_heldout_response(object, data, held_rows)
  held_data <- data[held_rows, , drop = FALSE]

  if (silent < 1L) {
    cat("kfold refit at fold", fold_id,
        "(", length(held_rows), "rows held out) ...\n")
  }

  refit <- refit_on_held_out(object, train_data, silent)

  # A held-out group may hold grouping levels the refit never saw.
  # `re_formula = NA` scores it without the group-level effects.
  held_loglik <- log_lik(
    refit, newdata = held_data,
    re_formula = NA,
    allow_new_levels = TRUE
  )
  # The groups are mapped at the width `log_lik()` returned and then
  # narrowed with the matrix, as on the full-data path above. A fold
  # with a missing response then scores the rows it observed.
  held_col_group <- loglik_col_values(
    object, held_data, group_key[held_rows], NCOL(held_loglik)
  )
  held_loglik <- clean_ll(held_loglik)
  held_col_group <- narrow_to_scored(held_col_group, held_loglik)

  held_grouped <- aggregate_loglik_by_group(
    held_loglik, held_col_group
  )

  apply(held_grouped, 2L, log_mean_exp)
}


#' The training frame for one fold
#'
#' A held-out observation is a missing response on an occasion that
#' stays in the frame. Deleting the row would open a gap in the time
#' axis. AR and CAR trends refuse such gaps. Masking the response
#' follows how `mvgam()` treats any gap. The likelihood covers the
#' non-missing rows. `N_time_trend` comes from the full frame, leaving
#' the time grid unchanged. Every response is masked. A response left
#' unmasked would keep held-out rows in the likelihood.
#'
#' @param object Fitted `mvgam` object
#' @param data Data frame of training data
#' @param held_rows Integer vector of held-out row indices
#' @return `data` with every response set to `NA` on `held_rows`
#' @noRd
mask_heldout_response <- function(object, data, held_rows) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(data)
  checkmate::assert_integerish(held_rows, lower = 1)
  if (!length(held_rows)) {
    return(data)
  }
  # Every response is masked or none is. A response left unmasked
  # would keep the held-out rows in the refit's likelihood. The fold
  # would then be scored on its own training data.
  resp <- insight::find_response(object)
  absent <- setdiff(resp, names(data))
  if (length(absent)) {
    stop(insight::format_error(c(
      "A response column is missing from the training data.",
      x = paste0("Missing: ", paste0("'", absent, "'", collapse = ", "),
                 "."),
      i = "Every response is masked in a held-out fold."
    )), call. = FALSE)
  }
  for (r in resp) {
    data[[r]][held_rows] <- NA
  }
  data
}


#' Pointwise ELPDs with the named folds refitted
#'
#' Exact mode passes every fold. Hybrid mode passes the folds holding a
#' group whose Pareto-k crossed the threshold.
#'
#' @param object Fitted `mvgam` object
#' @param data Data frame of training data
#' @param fold_ids Integer vector of per-row fold indices
#' @param group_key Character vector of per-row group keys
#' @param refit_fold_ids Integer vector of the folds to refit
#' @param pointwise Named numeric vector of per-group ELPDs
#' @param silent Integer verbosity level
#' @return `pointwise` with the refitted groups replaced
#' @noRd
kfold_refit_folds <- function(object, data, fold_ids, group_key,
                              refit_fold_ids, pointwise, silent) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(data)
  checkmate::assert_integerish(fold_ids)
  checkmate::assert_character(group_key)
  checkmate::assert_integerish(refit_fold_ids)
  checkmate::assert_numeric(pointwise)
  checkmate::assert_int(silent, lower = 0L)
  if (silent < 2L) {
    cat("kfold: refitting", length(refit_fold_ids),
        "fold(s) ...\n")
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


#' Print an `mvgam_kfold` object
#'
#' Prints the ELPD with its standard error, the folds and refits and
#' the Pareto k of the PSIS estimates. The first list below names the
#' groups with the lowest ELPD. The second names the refitted groups
#' whose ELPD moved most from the PSIS estimate.
#'
#' @param x An `mvgam_kfold` object.
#' @param digits Integer number of decimal places. Defaults to 2.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#'
#' @method print mvgam_kfold
#' @export
print.mvgam_kfold <- function(x, digits = 2L, ...) {
  checkmate::assert_int(digits, lower = 0L)
  fmt <- function(v) formatC(v, digits = digits, format = "f")
  s <- summary(x)
  rows <- c(
    elpd_kfold = paste0(fmt(x$elpd_kfold), " (SE ", fmt(x$se_elpd_kfold),
                        ")"),
    kfoldic = paste0(fmt(-2 * x$elpd_kfold), " (SE ",
                     fmt(2 * x$se_elpd_kfold), ")"),
    groups = paste0(NROW(s), " (",
                    if (is.null(x$group)) "row" else
                      paste(x$group, collapse = " x "), ")"),
    refits = paste0(x$n_refits, " of ", x$K, " folds",
                    if (isTRUE(x$exact)) " (exact)")
  )
  psis_k <- x$pareto_k[!is.na(x$pareto_k)]
  if (length(psis_k) > 0L) {
    rows["Pareto k"] <- paste0(
      "max ", fmt(max(psis_k)), ", median ", fmt(stats::median(psis_k)),
      ", threshold ", pareto_k_threshold_label(x, digits)
    )
  }
  cat_field_rows("mvgam k-fold cross-validation", rows)

  # About 15% of the groups, between 3 and 10
  n_show <- max(3L, min(10L, ceiling(NROW(s) * 0.15)))
  worst <- utils::head(order(s$elpd), n_show)
  cat("\nLowest ELPD (share of the total):\n")
  cat(sprintf("  %-20s %s (%s%%)\n", s$group[worst], fmt(s$elpd[worst]),
              formatC(100 * s$elpd_share[worst], digits = 1L,
                      format = "f")), sep = "")
  # An exact run has no PSIS estimate to compare with
  lifted <- which(!is.na(s$lift))
  if (length(lifted) > 0L) {
    moved <- utils::head(lifted[order(-abs(s$lift[lifted]))], n_show)
    cat("\nLargest change from PSIS to refit:\n")
    cat(sprintf("  %-20s %s -> %s\n", s$group[moved],
                fmt(s$elpd_psis[moved]), fmt(s$elpd[moved])), sep = "")
  }
  invisible(x)
}


#' Tibble summary of an `mvgam_kfold` object
#'
#' @param object An `mvgam_kfold` object.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A tibble with one row per held-out group.
#'   \describe{
#'     \item{`group`}{The group label.}
#'     \item{`elpd`, `elpd_share`}{The group's ELPD and its share of
#'       the total.}
#'     \item{`elpd_psis`, `lift`}{For a refitted group, the PSIS
#'       estimate before the refit and the refit's change from it.}
#'     \item{`pareto_k`, `refit`}{The Pareto k of the PSIS estimate and
#'       whether the group's fold was refitted.}
#'   }
#'
#' @method summary mvgam_kfold
#' @export
summary.mvgam_kfold <- function(object, ...) {
  rlang::check_dots_empty()
  group <- rownames(object$pointwise) %||%
    names(object$pareto_k)
  elpd <- as.numeric(object$pointwise[, "elpd_kfold"])
  total <- sum(elpd)
  elpd_psis <- if (!is.null(object$pointwise_psis)) {
    as.numeric(object$pointwise_psis[group])
  } else {
    rep(NA_real_, length(group))
  }
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
#' One panel for the ELPD of each group and one for its Pareto k, as
#' in [plot.mvgam_lfo()]. Dark red points mark a Pareto k above the
#' dashed threshold, where PSIS is unreliable. In the ELPD panel they
#' mark the groups below the 15% quantile.
#'
#' @param x An `mvgam_kfold` object.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [plot.mvgam_lfo()] for the matching time-block CV plot.
#'
#' @method plot mvgam_kfold
#' @export
plot.mvgam_kfold <- function(x, ...) {
  rlang::check_dots_empty()
  group <- rownames(x$pointwise) %||% names(x$pareto_k)
  group <- factor(group, levels = group)
  long <- rbind(
    cv_panel(group, as.numeric(x$pointwise[, "elpd_kfold"]), "ELPD",
             probs = 0.15, worse_above = FALSE),
    pareto_k_panel(group, as.numeric(x$pareto_k), x$pareto_k_threshold)
  )
  cv_diagnostic_plot(long) +
    ggplot2::labs(x = if (is.null(x$group)) "row" else
      paste(x$group, collapse = " x ")) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


#' The `mvgam_kfold` object `kfold()` returns
#'
#' Inherits from the `kfold` and `loo` classes for `loo_compare()`,
#' `print.loo()` and `loo_model_weights()`.
#'
#' @param pointwise Named numeric vector of per-group ELPDs
#' @param pointwise_psis Named numeric vector of the PSIS estimates
#'   before refitting, or `NULL`
#' @param pareto_k Named numeric vector of per-group Pareto-k values
#' @param refit_groups Character vector of the refitted groups
#' @param K Integer number of folds
#' @param group Character vector of grouping columns, or `NULL`
#' @param n_refit_folds Integer number of refitted folds
#' @param pareto_k_threshold Numeric Pareto-k threshold applied
#' @param pareto_k_threshold_adaptive Logical. `TRUE` for a threshold
#'   derived from the draw count
#' @param exact Logical. `TRUE` when every fold was refitted
#' @return Object of class `c("mvgam_kfold", "kfold", "loo")`
#' @noRd
build_mvgam_kfold <- function(pointwise, pointwise_psis = NULL,
                              pareto_k, refit_groups,
                              K, group, n_refit_folds,
                              pareto_k_threshold,
                              pareto_k_threshold_adaptive = FALSE,
                              exact) {
  checkmate::assert_numeric(pointwise, min.len = 1L)
  checkmate::assert_numeric(pointwise_psis, null.ok = TRUE)
  checkmate::assert_numeric(pareto_k)
  checkmate::assert_character(refit_groups, null.ok = TRUE)
  checkmate::assert_int(K, lower = 1L)
  checkmate::assert_character(group, null.ok = TRUE)
  checkmate::assert_int(n_refit_folds, lower = 0L)
  checkmate::assert_number(pareto_k_threshold)
  checkmate::assert_flag(pareto_k_threshold_adaptive)
  checkmate::assert_flag(exact)
  elpd <- sum(pointwise)
  se_elpd <- se_of_sum(pointwise)

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
    # Folds, as `print()` reports against `K`. `refit_groups` holds
    # the groups inside them.
    n_refits = n_refit_folds,
    refit_groups = refit_groups,
    K = K,
    group = group,
    pareto_k_threshold = pareto_k_threshold,
    pareto_k_threshold_adaptive = pareto_k_threshold_adaptive,
    exact = exact
  )
  class(out) <- c("mvgam_kfold", "kfold", "loo")
  out
}


#' Refit a model on a training window whose held-out rows are masked
#'
#' Every cross-validation method here holds a fold out by setting its
#' response to `NA`. Deleting the rows would move the trend grid off
#' the parent's. brms warns whenever it drops rows whose response is
#' missing. Those rows are the fold, which the method masked itself,
#' and the notice names nothing the caller can act on. The handler
#' muffles that one warning by its text. Every other warning from the
#' refit reaches the caller.
#'
#' A refit runs one verbosity level below the calling method. Its
#' sampler prints no iteration lines (`refresh = 0`, as brms sets for
#' its own refits). At the default `silent = 1` the method prints its
#' progress lines alone. At `silent = 0` each refit adds its sampler
#' start and finish lines.
#'
#' @param object The fitted model to refit.
#' @param train_data The training frame, held-out responses masked.
#' @param silent Verbosity of the calling method, `0` to `2`.
#' @return The refitted model.
#' @noRd
refit_on_held_out <- function(object, train_data, silent = 1L) {
  without_warnings_matching(
    update(object, newdata = train_data,
           silent = min(silent + 1L, 2L), refresh = 0),
    "Rows containing NAs", fixed = TRUE
  )
}
