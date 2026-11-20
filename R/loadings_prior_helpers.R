# Internal helpers of `normalise_loadings_prior()` (R/validations.R)
# and `make_loadings_prior_stanvars()` (R/stan_assembly.R). They
# encode the features as a numeric matrix in series-level order. They
# validate the pairwise distances. They warn on collinear length-scale
# dimensions and on one-hot columns too imbalanced to identify.


#' Encode per-series features as a numeric matrix in level order
#'
#' Encodes each column by its type:
#' - numeric, integer or logical: z-score by SD
#' - ordered factor: `as.numeric()` then z-score
#' - unordered factor: one-hot, keeping all levels
#' - character: coerce to factor, then one-hot
#'
#' `align_feature_rows()` puts the rows in the order of
#' `series_levels`. The structured loadings prior takes its ARD
#' distances from this matrix. Encoding stops with an error when any
#' entry is `NA`.
#'
#' @param features Numeric matrix or data frame with one row per
#'   training series
#' @param series_levels Character vector of the training series, in
#'   axis order
#' @return Numeric p x c matrix, one row per series named by its level.
#'   Columns take the name of their source column (numerics) or
#'   `source.level` (one-hots).
#' @noRd
encode_loadings_features <- function(features, series_levels) {
  checkmate::assert_character(series_levels, min.len = 1L,
                              any.missing = FALSE)
  if (is.matrix(features)) {
    if (!is.numeric(features)) {
      stop(insight::format_error(
        "'features' matrix must be numeric."
      ), call. = FALSE)
    }
    features <- as.data.frame(features)
  }
  checkmate::assert_data_frame(features, min.cols = 1L)
  features <- align_feature_rows(features, series_levels)
  feature_cols <- names(features)
  if (length(feature_cols) == 0L) {
    stop(insight::format_error(c(
      "'features' has no usable columns after dropping 'series'.",
      i = paste0("Supply at least one numeric, factor or character ",
                 "column other than 'series'.")
    )), call. = FALSE)
  }
  encoded_cols <- lapply(
    feature_cols,
    function(col) encode_feature_column(features[[col]], col)
  )
  encoded <- do.call(cbind, encoded_cols)
  rownames(encoded) <- series_levels
  if (anyNA(encoded)) {
    stop(insight::format_error(c(
      "Encoded 'features' matrix contains NA values.",
      i = paste0(
        "Drop / impute NA entries in 'features' before passing ",
        "to 'loadings_prior'."
      )
    )), call. = FALSE)
  }
  encoded
}


#' Rows of a per-series data frame in the order of the series levels
#'
#' A `series` column names each row's series. Without one, row names
#' name the series when any of them is a series level. Default row
#' names name none. The rows are then taken in level order.
#' `series_row_order()` refuses a missing, unknown or repeated series.
#'
#' @param df Data frame with one row per series
#' @param series_levels The training series, in axis order
#' @param subject The argument to name in a message
#' @return `df` in level order, without its `series` column
#' @noRd
align_feature_rows <- function(df, series_levels, subject = "features") {
  if ("series" %in% names(df)) {
    df <- df[series_row_order(df$series, series_levels,
                              paste0(subject, "$series")), , drop = FALSE]
    df$series <- NULL
    return(df)
  }
  if (any(rownames(df) %in% series_levels)) {
    return(df[series_row_order(rownames(df), series_levels,
                               paste0("rownames(", subject, ")")), ,
              drop = FALSE])
  }
  if (nrow(df) == length(series_levels)) {
    return(df)
  }
  stop(insight::format_error(c(
    paste0("'", subject, "' needs one row per training series."),
    x = paste0("Got ", nrow(df), " rows for ", length(series_levels),
               " series."),
    i = paste0("Add a 'series' column, name the rows by series or give ",
               "one row per series in level order.")
  )), call. = FALSE)
}


#' Encode one feature column as one or several numeric columns
#'
#' A numeric, logical or ordered column becomes one z-scored column. A
#' character or unordered factor column becomes one-hot columns. An
#' all-`NA` column or a column of any other type stops with an error.
#'
#' @param x Vector holding one feature column
#' @param name Character name of the source column
#' @return Numeric matrix with one row per element of `x`. A z-scored
#'   column takes the source feature name. A one-hot column takes
#'   `name.level`.
#' @noRd
encode_feature_column <- function(x, name) {
  if (all(is.na(x))) {
    stop(insight::format_error(c(
      paste0("Feature column '", name, "' is entirely NA."),
      i = "Drop the column or supply at least one observation."
    )), call. = FALSE)
  }
  if (is.character(x)) {
    x <- factor(x)
  }
  if (is.factor(x)) {
    if (is.ordered(x)) {
      return(zscore_matrix(as.numeric(x), name))
    }
    return(one_hot_columns(x, name))
  }
  if (is.logical(x)) {
    return(zscore_matrix(as.numeric(x), name))
  }
  if (is.numeric(x)) {
    return(zscore_matrix(as.numeric(x), name))
  }
  stop(insight::format_error(c(
    paste0(
      "Feature column '", name,
      "' has unsupported type '", class(x)[1L], "'."
    ),
    i = paste0(
      "Supported types: numeric, integer, logical, ordered ",
      "factor, unordered factor, character."
    )
  )), call. = FALSE)
}


#' One 0 / 1 column per level of a factor
#'
#' @param x A factor
#' @param name The column's name, which prefixes each level
#' @return Numeric matrix with one row per element of `x`
#' @noRd
one_hot_columns <- function(x, name) {
  checkmate::assert_factor(x)
  levs <- levels(x)
  matrix(vapply(levs, function(lvl) as.numeric(x == lvl),
                numeric(length(x))),
         nrow = length(x), dimnames = list(NULL, paste0(name, ".", levs)))
}


#' Z-score a numeric vector into a one-column matrix
#'
#' A zero SD is allowed. It leaves the column at mean-centred zero,
#' which contributes nothing to the ARD distance. The imbalance check
#' that follows reports such a column as uninformative.
#'
#' @param x Numeric vector
#' @param name Character column name
#' @return Numeric matrix with one column named `name` and one row per
#'   element of `x`
#' @noRd
zscore_matrix <- function(x, name) {
  mu <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  z <- if (is.na(s) || s == 0) {
    x - mu
  } else {
    (x - mu) / s
  }
  matrix(z, ncol = 1L, dimnames = list(NULL, name))
}


#' Validate and standardise a pairwise distance matrix
#'
#' Checks the matrix against the training-series count and the shape
#' requirements: square, symmetric, non-negative, zero diagonal. When
#' row and column names hold the series levels, the matrix is put in
#' level order. With `standardise = TRUE` it is rescaled to a maximum
#' off-diagonal entry of 1. Heaps & Jermyn (2024), Supplement S4.2.1,
#' standardise distances the same way, "by scaling to make the common
#' root to tip distance one". The default length-scale prior
#' `lognormal(0, 1)` sets its scale against the standardised distances.
#'
#' @param mat Numeric matrix or data frame of pairwise distances
#' @param n_series Integer number of training series
#' @param name Character name of the `loadings_prior$distances` entry.
#'   Error messages name it so users know which entry failed a check.
#' @param series_levels Character vector of the training series in
#'   axis order, or `NULL` to keep the matrix's own order
#' @param standardise Logical; rescale to a maximum entry of 1
#' @param tol Numeric tolerance for the sign, diagonal and symmetry
#'   checks
#' @return Symmetrised numeric `n_series` x `n_series` matrix with a
#'   zero diagonal
#' @noRd
validate_pairwise_distance <- function(mat, n_series, name,
                                       series_levels = NULL,
                                       standardise = TRUE,
                                       tol = 1e-8) {
  if (is.data.frame(mat)) {
    mat <- as.matrix(mat)
  }
  if (!is.matrix(mat) || !is.numeric(mat)) {
    stop(insight::format_error(c(
      paste0(
        "Distance matrix '", name,
        "' must be a numeric matrix."
      ),
      x = paste0("Got: ", class(mat)[1L], ".")
    )), call. = FALSE)
  }
  if (nrow(mat) != n_series || ncol(mat) != n_series) {
    stop(insight::format_error(c(
      paste0(
        "Distance matrix '", name,
        "' has the wrong shape."
      ),
      x = paste0(
        "Got ", nrow(mat), " x ", ncol(mat), ", expected ",
        n_series, " x ", n_series, "."
      ),
      i = "Rows and columns must correspond to training series."
    )), call. = FALSE)
  }
  if (!is.null(series_levels) &&
      !is.null(rownames(mat)) && !is.null(colnames(mat)) &&
      setequal(rownames(mat), series_levels) &&
      setequal(colnames(mat), series_levels)) {
    mat <- mat[series_levels, series_levels, drop = FALSE]
  }
  if (anyNA(mat)) {
    stop(insight::format_error(c(
      paste0(
        "Distance matrix '", name, "' contains NA values."
      )
    )), call. = FALSE)
  }
  if (any(mat < -tol)) {
    stop(insight::format_error(c(
      paste0(
        "Distance matrix '", name,
        "' contains negative entries."
      )
    )), call. = FALSE)
  }
  if (max(abs(diag(mat))) > tol) {
    stop(insight::format_error(c(
      paste0(
        "Distance matrix '", name,
        "' has a non-zero diagonal."
      ),
      x = paste0(
        "max(|diag|) = ",
        format(max(abs(diag(mat))), digits = 3), "."
      ),
      i = "Self-distance must be zero."
    )), call. = FALSE)
  }
  if (max(abs(mat - t(mat))) > tol) {
    stop(insight::format_error(c(
      paste0(
        "Distance matrix '", name, "' is not symmetric."
      ),
      x = paste0(
        "max(|mat - t(mat)|) = ",
        format(max(abs(mat - t(mat))), digits = 3), "."
      ),
      i = "Pairwise distances must satisfy d(i, j) == d(j, i)."
    )), call. = FALSE)
  }
  mat <- (mat + t(mat)) / 2
  diag(mat) <- 0
  if (standardise) {
    max_d <- max(mat)
    if (max_d > 0) {
      mat <- mat / max_d
    }
  }
  mat
}


#' Whether a fit used a structured `loadings_prior`
#'
#' The structured prior writes `N_features_trend` for trait features and
#' a `dist_<name>` matrix for each pairwise distance into the Stan data.
#' Either marks it.
#'
#' @param object A fitted `mvgam` object
#' @return Logical scalar
#' @noRd
uses_loadings_prior <- function(object) {
  sd <- object$standata
  if (is.null(sd)) return(FALSE)
  isTRUE(sd$N_features_trend > 0L) || any(grepl("^dist_", names(sd)))
}
