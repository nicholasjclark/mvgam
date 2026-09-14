# Helpers for the `loadings_prior` user surface. All functions
# here are internal and called from `normalise_loadings_prior()`
# (R/validations.R) and `make_loadings_prior_stanvars()`
# (R/stan_assembly.R). The helpers cover the four pieces of work
# the surface needs: feature encoding into a numeric matrix
# aligned to factor-level series ordering, pairwise-distance
# validation, collinearity warning across length-scale dimensions
# (feature-derived distance plus pairwise distances), and
# warning for imbalanced one-hot columns whose length-scales the
# data cannot identify.


# Encode a per-series feature matrix or data.frame into a numeric
# p x c matrix in factor-level order. Walks columns by type:
#   - numeric / integer: z-score by SD
#   - ordered factor: as.numeric() then z-score
#   - unordered factor: one-hot, keep all levels
#   - character: coerce to factor, then one-hot
#
# `features` can be a matrix (treated as all numeric) or a
# data.frame. `series_levels` is the levels(data$series) vector.
# Row alignment is decided by the first match of:
#   1. data.frame has a 'series' column -> reorder by that column
#   2. rownames match series_levels -> reorder by rownames
#   3. nrow equals length(series_levels) -> assume caller-ordered
# Returns the encoded p x c matrix with informative column names
# (source column for numerics; source.level for one-hots).
#'@noRd
encode_loadings_features <- function(features, series_levels) {
  checkmate::assert_character(series_levels, min.len = 1L, any.missing = FALSE)
  n_series <- length(series_levels)
  if (is.matrix(features)) {
    if (!is.numeric(features)) {
      stop(insight::format_error(
        "'features' matrix must be numeric."
      ))
    }
    features <- as.data.frame(features)
  }
  checkmate::assert_data_frame(features, min.cols = 1L)
  features <- align_feature_rows(features, series_levels)
  feature_cols <- setdiff(names(features), "series")
  if (length(feature_cols) == 0L) {
    stop(insight::format_error(c(
      "'features' has no usable columns after dropping 'series'.",
      i = paste0(
        "Supply at least one numeric / factor / character ",
        "column besides 'series'."
      )
    )))
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
    )))
  }
  encoded
}


# Align rows of a feature data.frame to the requested
# series_levels order. Three accepted layouts (probed in order):
# 1. df has a 'series' column matching series_levels
# 2. df has rownames matching series_levels
# 3. df has exactly nrow == length(series_levels) rows
#'@noRd
align_feature_rows <- function(df, series_levels) {
  n_series <- length(series_levels)
  if ("series" %in% names(df)) {
    s <- as.character(df$series)
    df <- df[
      series_row_order(s, series_levels, "features$series"), ,
      drop = FALSE
    ]
    df$series <- NULL
    return(df)
  }
  if (!is.null(rownames(df)) &&
      setequal(rownames(df), series_levels)) {
    return(df[series_levels, , drop = FALSE])
  }
  if (nrow(df) == n_series) {
    return(df)
  }
  stop(insight::format_error(c(
    "Cannot align 'features' rows to training series.",
    x = paste0(
      "Got nrow(features) = ", nrow(df), ", expected ",
      n_series, " (one row per series)."
    ),
    i = paste0(
      "Add a 'series' column, set rownames(features) to the ",
      "series levels or supply rows in factor-level order."
    )
  )))
}


# Encode one feature column into one (numeric) or several
# (one-hot) columns. Returns a numeric matrix; column names
# carry the source feature name (and level, for one-hots).
#'@noRd
encode_feature_column <- function(x, name) {
  if (all(is.na(x))) {
    stop(insight::format_error(c(
      paste0("Feature column '", name, "' is entirely NA."),
      i = "Drop the column or supply at least one observation."
    )))
  }
  if (is.character(x)) {
    x <- factor(x)
  }
  if (is.factor(x)) {
    if (is.ordered(x)) {
      return(zscore_matrix(as.numeric(x), name))
    }
    levs <- levels(x)
    mat <- vapply(
      levs,
      function(lvl) as.numeric(x == lvl),
      numeric(length(x))
    )
    colnames(mat) <- paste0(name, ".", levs)
    return(mat)
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
  )))
}


# z-score a numeric vector into a one-column numeric matrix. SD
# of zero leaves the column at mean-centred zero (which produces
# zero contribution to the ARD distance) and is allowed; the
# subsequent imbalance check will flag it as uninformative.
#'@noRd
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


# Validate a pairwise distance matrix against the training-series
# count and shape requirements (square, symmetric, non-negative,
# zero diagonal). Optionally rescales the matrix so its maximum
# off-diagonal entry is 1, matching Heaps & Jermyn (2024) Supps
# S4.2.1 (which standardises distances "by scaling to make the
# common root to tip distance one"). With the default
# `standardise = TRUE`, the per-distance length-scale prior
# `log(theta) ~ normal(0, 1)` lives on the standardised scale and
# is meaningful out of the box. `name` flows into error messages
# so users know which `loadings_prior$distances` entry tripped a
# check.
#'@noRd
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
    )))
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
    )))
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
      ),
      i = "All pairwise distances must be present."
    )))
  }
  if (any(mat < -tol)) {
    stop(insight::format_error(c(
      paste0(
        "Distance matrix '", name,
        "' contains negative entries."
      ),
      i = paste0(
        "Distances must be >= 0; check the matrix or apply ",
        "an absolute value if it encodes a signed quantity."
      )
    )))
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
    )))
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
    )))
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
