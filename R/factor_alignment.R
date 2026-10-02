# Identification of sampled factor loadings.
#
# A factor model's likelihood depends on the loadings `Z` and the
# factor paths `lv_trend` only through their product. The transforms
# of the factors that leave the whole model unchanged decide how the
# posterior is identified:
#
#   * Factors with interchangeable dynamics (RW, VAR, ZMVN and AR
#     with one shared coefficient) are unchanged by any rotation. Stan
#     rotates each draw to a lower-triangular `Z_tilde` by a thin QR
#     decomposition in generated quantities.
#   * Factors with their own coefficients (AR) are unchanged only by
#     reordering the factors and flipping their signs. A rotation
#     would mix factors that follow different dynamics. Each draw is
#     relabelled here after sampling to agree with a reference, and
#     the loadings, the factor paths and the per-factor coefficients
#     move together.
#
# `factor_identification()` makes that choice once, for the Stan
# generators and for `relabel_factors()`.


#' How a model identifies its sampled loadings
#'
#' @param trend_spec A normalised trend specification, or a
#'   per-response list of them.
#' @param has_by_lv Does a `by = lv_axis()` term tie each factor to
#'   its own covariate effect?
#' @return `"rotation"` for the QR step in Stan, `"relabel"` for
#'   `relabel_factors()`, `"none"` where the loadings keep the
#'   orientation they were sampled in.
#' @noRd
factor_identification <- function(trend_spec, has_by_lv = FALSE) {
  checkmate::assert_flag(has_by_lv, null.ok = TRUE)
  spec <- trend_spec_head(trend_spec)
  if (!samples_any_loading(spec) || isTRUE(has_by_lv)) {
    return("none")
  }
  partial <- !is.null(spec$fixed_Z)
  if (has_factor_coefficients(spec)) {
    # A sampled correlation matrix is stored as a Cholesky factor,
    # which has no closed form under a reordering of its rows.
    if (partial && isTRUE(spec$cor)) {
      return("none")
    }
    return("relabel")
  }
  if (partial) "none" else "rotation"
}


#' How a fitted model identifies its sampled loadings
#'
#' @param object A fitted `mvgam` object.
#' @return As `factor_identification()`.
#' @noRd
fit_factor_identification <- function(object) {
  factor_identification(first_trend_spec(object),
                        object$trend_metadata$has_by_lv)
}

#' Does each factor of this trend take its own coefficients?
#'
#' @param trend_spec A normalised trend specification.
#' @return A single logical.
#' @noRd
has_factor_coefficients <- function(trend_spec) {
  spec <- trend_spec_head(trend_spec)
  trend_property(spec$trend, "per_factor_coefficients") &&
    !identical(ar_coef_sharing(spec), "shared")
}


# The stored parameters indexed by factor. `relabel_factors()` moves
# exactly these. A sign flip of a factor negates the `signed` ones.
# Their factor index comes last, and `Z_cols` holds it first. A new
# per-factor parameter of a trend with its own coefficients is added
# here. The multiplicative gamma process samples `varrho_inv`, whose
# running product is `Psi_diag`. `relabel_chain()` recomputes it from
# the relabelled `Psi_diag`.
factor_indexed_pars <- list(
  signed = c("Z", "lv_trend", "innovations_trend",
             "scaled_innovations_trend", "ma_innovations_trend",
             "init_innovations_trend"),
  signed_first = "Z_cols",
  unsigned = "^((ar[0-9]+(_pacf)?|theta[0-9]+|sigma)_trend|Psi_diag)$",
  recomputed = "varrho_inv"
)


#' Relabel the factors of a fit to one ordering and orientation
#'
#' Runs once, where the fit is assembled. The stored draws are
#' rewritten, and every post-fit method then uses one labelling.
#' `Z %*% t(lv_trend)` is identical in every draw before and after.
#'
#' @param object A fitted `mvgam` object.
#' @return `object`, with relabelled draws where
#'   `factor_identification()` gives `"relabel"`.
#' @noRd
relabel_factors <- function(object) {
  checkmate::assert_class(object, "mvgam")
  if (!identical(fit_factor_identification(object), "relabel")) {
    return(object)
  }
  spec <- first_trend_spec(object)
  n_lv <- spec_n_lv(spec)
  n_series <- mvgam_axes(object)$series$n
  samples <- object$fit@sim$samples
  loading_cols <- sprintf("Z[%d,%d]", rep(seq_len(n_series), n_lv),
                          rep(seq_len(n_lv), each = n_series))
  stacked <- do.call(rbind, lapply(samples, function(chain) {
    as.matrix(chain[c(loading_cols, "lp__")])
  }))
  alignment <- align_factor_draws(
    Z = array(stacked[, loading_cols], c(nrow(stacked), n_series, n_lv)),
    lp = stacked[, "lp__"],
    template = spec$fixed_Z,
    path_var = do.call(rbind, lapply(samples, factor_path_variance,
                                     n_lv = n_lv))
  )
  chain_rows <- split(
    seq_len(nrow(stacked)),
    rep(seq_along(samples), vapply(samples, nrow, integer(1L)))
  )
  object$fit@sim$samples <- Map(function(chain, rows) {
    relabel_chain(chain, alignment$perm[rows, , drop = FALSE],
                  alignment$sign[rows, , drop = FALSE], spec$fixed_Z)
  }, samples, chain_rows)
  object
}


#' The ordering and signs that align each draw's factors
#'
#' Each draw's loading columns are matched to a reference by the
#' assignment with the largest summed absolute inner product, and a
#' column takes the sign of its inner product. The reference starts at
#' the draw of highest log density and is replaced by the mean of the
#' aligned draws until the assignment stops changing. The factors are
#' then ordered by the variance they contribute to the series, largest
#' first, and signed so the largest mean loading of each is positive.
#' A factor's contribution is the sum of its squared mean loadings
#' times the variance of its path.
#'
#' A `template` restricts the moves to those that keep it intact.
#' Columns are exchanged only with columns of the same template, and a
#' column with a non-zero fixed entry keeps its sign.
#'
#' @param Z Loadings, `[draws, series, factors]`.
#' @param lp Log density of each draw.
#' @param template The `trend_map` matrix with `NA` for sampled
#'   entries, or `NULL` when every loading is sampled.
#' @param path_var Variance over time of each sampled factor path,
#'   `[draws, factors]`, or `NULL` to order by the loadings alone.
#' @return A list of two `[draws, factors]` matrices. `perm[s, j]` is
#'   the sampled column that becomes factor `j` in draw `s`, and
#'   `sign[s, j]` multiplies it.
#' @noRd
align_factor_draws <- function(Z, lp, template = NULL, path_var = NULL) {
  checkmate::assert_array(Z, mode = "numeric", d = 3L)
  checkmate::assert_numeric(lp, len = dim(Z)[1L])
  checkmate::assert_matrix(template, nrows = dim(Z)[2L], ncols = dim(Z)[3L],
                           null.ok = TRUE)
  n_draws <- dim(Z)[1L]
  n_series <- dim(Z)[2L]
  n_lv <- dim(Z)[3L]
  checkmate::assert_matrix(path_var, nrows = n_draws, ncols = n_lv,
                           null.ok = TRUE)
  template <- template %||% matrix(NA_real_, n_series, n_lv)
  sign_free <- apply(template, 2L, function(col) all(is.na(col) | col == 0))
  key <- apply(template, 2L, paste, collapse = "\r")
  classes <- split(seq_len(n_lv), match(key, unique(key)))

  match_to <- function(reference) {
    perm <- matrix(seq_len(n_lv), n_draws, n_lv, byrow = TRUE)
    sgn <- matrix(1, n_draws, n_lv)
    for (cols in classes) {
      # inner[s, j, k]: reference column j against sampled column k
      inner <- array(0, c(n_draws, length(cols), length(cols)))
      for (j in seq_along(cols)) {
        for (k in seq_along(cols)) {
          inner[, j, k] <- matrix(Z[, , cols[k]], n_draws) %*%
            reference[, cols[j]]
        }
      }
      pick <- best_assignment(abs(inner))
      perm[, cols] <- cols[pick]
      for (j in which(sign_free[cols])) {
        s <- sign(inner[cbind(seq_len(n_draws), j, pick[, j])])
        sgn[, cols[j]] <- ifelse(s == 0, 1, s)
      }
    }
    list(perm = perm, sign = sgn)
  }

  reference <- matrix(Z[which.max(lp), , ], n_series, n_lv)
  alignment <- NULL
  # The loop stops at a fixed point, which it reaches within a few
  # passes. The bound guards against a cycle between two assignments.
  for (pass in seq_len(20L)) {
    previous <- alignment
    alignment <- match_to(reference)
    if (identical(alignment, previous)) {
      break
    }
    reference <- vapply(seq_len(n_lv), function(j) {
      colMeans(aligned_factor(Z, alignment, j))
    }, numeric(n_series))
  }

  # One ordering and orientation for the reference itself
  slot <- seq_len(n_lv)
  size <- colSums(reference^2)
  if (!is.null(path_var)) {
    draws <- seq_len(n_draws)
    size <- size * vapply(seq_len(n_lv), function(j) {
      mean(path_var[cbind(draws, alignment$perm[, j])])
    }, numeric(1L))
  }
  for (cols in classes) {
    slot[cols] <- cols[order(size[cols], decreasing = TRUE)]
  }
  flip <- vapply(slot, function(j) {
    col <- reference[, j]
    if (sign_free[j] && col[which.max(abs(col))] < 0) -1 else 1
  }, numeric(1L))
  list(
    perm = alignment$perm[, slot, drop = FALSE],
    sign = sweep(alignment$sign[, slot, drop = FALSE], 2L, flip, `*`)
  )
}


# Internal: the variance over time of each factor path in every draw
# of a chain, `[draws, factors]`.
#'@noRd
factor_path_variance <- function(chain, n_lv) {
  vapply(seq_len(n_lv), function(k) {
    path <- as.matrix(chain[grepl(
      sprintf("^lv_trend\\[[0-9]+,%d\\]$", k), names(chain)
    )])
    rowMeans((path - rowMeans(path))^2)
  }, numeric(nrow(chain)))
}


# Internal: the loadings of factor `j` in every draw under an
# alignment, `[draws, series]`.
#'@noRd
aligned_factor <- function(Z, alignment, j) {
  n_draws <- dim(Z)[1L]
  n_series <- dim(Z)[2L]
  index <- cbind(rep(seq_len(n_draws), n_series),
                 rep(seq_len(n_series), each = n_draws),
                 rep(alignment$perm[, j], n_series))
  matrix(Z[index], n_draws, n_series) * alignment$sign[, j]
}


#' The assignment of columns to slots with the largest total score
#'
#' Every ordering is scored for up to six columns, which is exact.
#' Larger problems take the largest remaining score in turn.
#'
#' @param score Non-negative scores, `[draws, slots, columns]`.
#' @return An integer matrix `[draws, slots]` of chosen columns.
#' @noRd
best_assignment <- function(score) {
  checkmate::assert_array(score, mode = "numeric", d = 3L)
  n_draws <- dim(score)[1L]
  m <- dim(score)[2L]
  if (m == 1L) {
    return(matrix(1L, n_draws, 1L))
  }
  if (m <= 6L) {
    orders <- all_orderings(m)
    totals <- vapply(seq_len(nrow(orders)), function(p) {
      rowSums(vapply(seq_len(m), function(j) score[, j, orders[p, j]],
                     numeric(n_draws)))
    }, numeric(n_draws))
    return(orders[max.col(totals, ties.method = "first"), , drop = FALSE])
  }
  pick <- matrix(NA_integer_, n_draws, m)
  flat <- matrix(score, n_draws, m * m)
  cell_slot <- rep(seq_len(m), times = m)
  cell_column <- rep(seq_len(m), each = m)
  for (step in seq_len(m)) {
    cell <- max.col(flat, ties.method = "first")
    pick[cbind(seq_len(n_draws), cell_slot[cell])] <- cell_column[cell]
    taken <- outer(cell_slot[cell], cell_slot, `==`) |
      outer(cell_column[cell], cell_column, `==`)
    flat[taken] <- -Inf
  }
  pick
}


# Internal: every ordering of `1:m`, one per row.
#'@noRd
all_orderings <- function(m) {
  if (m == 1L) {
    return(matrix(1L, 1L, 1L))
  }
  shorter <- all_orderings(m - 1L)
  do.call(rbind, lapply(seq_len(m), function(first) {
    rest <- seq_len(m)[-first]
    cbind(first, matrix(rest[shorter], nrow(shorter)), deparse.level = 0L)
  }))
}


#' Apply an alignment to one chain's stored draws
#'
#' @param chain A data frame of draws, one column per scalar.
#' @param perm,sign The chain's rows of `align_factor_draws()`.
#' @param template The `trend_map` matrix, or `NULL`.
#' @return The chain with its factor-indexed columns relabelled.
#' @noRd
relabel_chain <- function(chain, perm, sign, template = NULL) {
  checkmate::assert_data_frame(chain)
  checkmate::assert_matrix(perm, nrows = nrow(chain))
  checkmate::assert_matrix(sign, nrows = nrow(chain), ncols = ncol(perm))
  n_lv <- ncol(perm)
  rows <- seq_len(nrow(chain))
  stems <- sub("\\[.*$", "", names(chain))
  # Each pattern captures the text before the factor index, the index
  # and the text after it. Columns agreeing on the first and third
  # differ only in their factor.
  factor_last <- "^(.*[\\[,])([0-9]+)(\\])$"
  factor_first <- "^([^\\[]*\\[)([0-9]+)(,.*\\])$"
  move <- function(columns, pattern, signed) {
    factor <- as.integer(sub(pattern, "\\2", columns))
    groups <- split(columns[order(factor)],
                    sub(pattern, "\\1\\3", columns)[order(factor)])
    for (cols in groups) {
      old <- as.matrix(chain[cols])
      for (j in seq_len(n_lv)) {
        values <- old[cbind(rows, perm[, j])]
        chain[[cols[j]]] <<- if (signed) values * sign[, j] else values
      }
    }
  }
  move(names(chain)[stems %in% factor_indexed_pars$signed],
       factor_last, signed = TRUE)
  move(names(chain)[stems %in% factor_indexed_pars$signed_first],
       factor_first, signed = TRUE)
  move(names(chain)[grepl(factor_indexed_pars$unsigned, stems)],
       factor_last, signed = FALSE)

  shrinkage <- sprintf("Psi_diag[%d]", seq_len(n_lv))
  if (all(shrinkage %in% names(chain))) {
    ratios <- as.matrix(chain[shrinkage])
    ratios[, -1L] <- ratios[, -1L] / ratios[, -n_lv]
    chain[sprintf("%s[%d]", factor_indexed_pars$recomputed,
                  seq_len(n_lv))] <- ratios
  }

  # A partial `trend_map` stores its sampled loadings a second time as
  # a vector, in the column-major order of the template's `NA` cells.
  if (anyNA(template)) {
    free <- which(is.na(template), arr.ind = TRUE)
    chain[sprintf("Z_free_vec[%d]", seq_len(nrow(free)))] <-
      chain[sprintf("Z[%d,%d]", free[, 1L], free[, 2L])]
  }
  chain
}
