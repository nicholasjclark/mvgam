#' Process Error Innovation Sampling for Trend Models
#'
#' @description
#' Infrastructure for sampling process errors (innovations) from trend
#' model distributions. All mvgam trend models use multivariate normal
#' innovations with different covariance parameterizations.
#'
#' @details
#' This module implements a pattern-based sampling system where trend
#' types are mapped to one of four covariance patterns:
#'
#' \itemize{
#'   \item \code{none}: Deterministic trends (PW) - no innovations
#'   \item \code{diagonal}: Independent innovations per series (CAR)
#'   \item \code{cholesky_scaled}: Correlated via L_Sigma =
#'     diag(sigma) * L_Omega (RW, AR, ZMVN)
#'   \item \code{full_covariance}: Direct covariance matrix Sigma (VAR)
#' }
#'
#' New trend types only need to declare their covariance pattern to
#' integrate with this sampling infrastructure.
#'
#' @name sample_innovations
#' @keywords internal
NULL


#' Covariance Pattern Constants
#'
#' Maps trend types to their covariance parameterization pattern.
#' New trend types should be added here with their appropriate pattern.
#'
#' @noRd
trend_covariance_patterns <- list(
  # Cholesky-scaled patterns: L_Sigma = diag(sigma_trend) %*% L_Omega_trend
  RW = "cholesky_scaled",
  AR = "cholesky_scaled",
  ZMVN = "cholesky_scaled",

  # Full covariance patterns: direct Sigma_trend matrix
  VAR = "full_covariance",

  # Diagonal patterns: independent innovations, sigma_trend only
  CAR = "diagonal",

  # No innovations: deterministic trends
  PW = "none",
  None = "none"
)


#' Get Covariance Pattern for Trend Type
#'
#' Retrieves the covariance pattern associated with a trend type.
#' Falls back to "cholesky_scaled" for unknown types (most common).
#'
#' @param trend_type Character string specifying the trend type
#'   (e.g., "AR", "RW", "VAR", "ZMVN", "CAR", "PW")
#'
#' @return Character string: one of "none", "diagonal",
#'   "cholesky_scaled", or "full_covariance"
#'
#' @noRd
get_covariance_pattern <- function(trend_type) {
  checkmate::assert_string(trend_type, min.chars = 1)

  # Reason: match dictionary keys case-insensitively after stripping
  # any numeric suffix ("AR1" -> "AR"). The dictionary keys keep
  # their natural case so adding a new trend type does not require
  # knowing the lookup convention.
  base_type <- gsub("[0-9]+$", "", trend_type)
  key_idx <- match(
    tolower(base_type),
    tolower(names(trend_covariance_patterns))
  )

  pattern <- if (!is.na(key_idx)) {
    trend_covariance_patterns[[key_idx]]
  } else {
    NULL
  }

  if (is.null(pattern)) {
    # Default to cholesky_scaled for unknown types (most common pattern)
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        insight::format_message(
          c(
            paste0("Unknown trend type '", trend_type, "' encountered."),
            i = "Defaulting to 'cholesky_scaled' covariance pattern.",
            i = "Register custom trends in trend_covariance_patterns."
          )
        ),
        .frequency = "once",
        .frequency_id = paste0("unknown_trend_cov_", trend_type)
      )
    }
    pattern <- "cholesky_scaled"
  }

  pattern
}


#' Prepare a frame the way the fit's own series identity was built
#'
#' Both the training frame and any prediction frame have to be read
#' through the same preparation, or the series each names is spelled
#' one way on one side and another way on the other and no comparison
#' between them means anything.
#'
#' @param object A fitted `mvgam` object
#' @param data A data frame to prepare
#' @return `data` carrying the `mvgam_time` / `mvgam_series` attributes
#'
#' @noRd
prepare_mvgam_frame <- function(object, data) {
  meta <- object$trend_metadata
  ensure_mvgam_variables(
    data = data,
    parsed_trend = NULL,
    time_var = meta$variables$time_var %||% "time",
    series_var = meta$variables$series_var %||% "series",
    response_vars = object$response_names,
    metadata = meta
  )
}


#' The series label each row of a frame carries
#'
#' The same identity `fitted_series_index()` is keyed on, read per
#' row rather than per level, so a caller naming its output by one
#' and subsetting by the other cannot end up comparing a derived
#' series against the column it superseded.
#'
#' @param object A fitted `mvgam` object
#' @param data A frame to label, defaulting to the training data
#' @return Character vector of series labels, one per row
#'
#' @noRd
training_series_labels <- function(object, data = NULL) {
  data <- data %||% mvgam_training_data(object)
  as.character(get_series_for_grouping(
    prepare_mvgam_frame(object, data)
  ))
}


#' The index each series carries in the fitted trend matrix
#'
#' `trend[t, s]` numbers its second index over the series the model
#' was fitted on, and `standata$obs_trend_series` records the index
#' the fit gave each training row. That record is what Stan sampled
#' against, so it settles the mapping outright.
#'
#' Deriving the order instead, by sorting the levels the series
#' column happens to hold, answers a different question: a fit whose
#' series is rebuilt from a `gr` / `subgr` pair sorts region-major
#' while the column it superseded sorted otherwise, and the two
#' orders are a permutation of one another. Both carry every label,
#' so the mismatch costs no error and no missing value, and every
#' series simply reads another series' state.
#'
#' A fit predating the record, or one whose trend has no series axis,
#' falls back to the derived order.
#'
#' @param object A fitted `mvgam` object
#' @return Named integer vector mapping series label to trend column,
#'   or NULL when neither source is available
#'
#' @noRd
fitted_series_index <- function(object) {
  train <- mvgam_training_data(object)
  if (is.null(train)) {
    return(NULL)
  }
  labels <- as.character(get_series_for_grouping(
    prepare_mvgam_frame(object, train)
  ))
  recorded <- object$standata$obs_trend_series
  if (!is.null(recorded) && length(recorded) == length(labels)) {
    per_label <- tapply(as.integer(recorded), labels, unique)
    # A label spanning two columns would mean the preparation and the
    # fit disagree about what a series is, which no mapping can
    # reconcile; the derived order is then the honest answer.
    if (all(lengths(per_label) == 1L)) {
      out <- unlist(per_label)
      return(sort(out))
    }
  }
  levs <- sort(unique(labels))
  stats::setNames(seq_along(levs), levs)
}


#' Get Observation Structure for Innovation Sampling
#'
#' Extracts time and series indices for each observation in newdata.
#' Uses existing mvgam infrastructure for handling all series creation
#' strategies (explicit, hierarchical, multivariate).
#'
#' @param object mvgam object with fitted trend model
#' @param newdata Data frame with prediction covariates. If NULL, uses
#'   training data from object.
#'
#' @return List with components:
#'   \itemize{
#'     \item \code{time}: Integer vector of time indices (1-based)
#'     \item \code{series}: Factor/character vector of series identifiers
#'     \item \code{series_int}: Integer vector of series indices (1-based)
#'     \item \code{n_obs}: Number of observations
#'     \item \code{n_times}: Number of unique time points
#'     \item \code{n_series}: Number of unique series
#'     \item \code{unique_times}: Sorted unique time indices
#'   }
#'
#' @noRd
get_observation_structure <- function(object, newdata = NULL) {
  checkmate::assert_class(object, "mvgam")

  # Use training data if newdata not provided
  if (is.null(newdata)) {
    newdata <- object$obs_data
    if (is.null(newdata)) {
      newdata <- object$data
    }
  }

  checkmate::assert_data_frame(newdata, min.rows = 1)

  # Get variable names from stored metadata
  metadata <- object$trend_metadata

  if (is.null(metadata)) {
    # Fallback for models without trend_metadata
    time_var <- "time"
    series_var <- "series"
  } else {
    time_var <- metadata$variables$time_var
    if (is.null(time_var)) time_var <- "time"
    series_var <- metadata$variables$series_var
    if (is.null(series_var)) series_var <- "series"
  }

  # The trained model already knows its series structure
  # (object$standata$N_series_trend). For single-series models the
  # newdata typically has no `series` column at all (e.g. brms drops
  # columns the formula doesn't reference). Treat single-series as a
  # first-class shortcut here: build the observation structure
  # directly from object metadata + newdata's time column, without
  # invoking ensure_mvgam_variables.
  n_series_trained <- object$standata$N_series_trend %||%
    object$trend_metadata$dimensions$n_series %||%
    length(levels(as.factor(
      attr(mvgam_training_data(object), "mvgam_series")
    )))

  is_single_series <- !is.null(n_series_trained) && n_series_trained == 1L
  has_explicit_series <- series_var %in% names(newdata)

  if (is_single_series && !has_explicit_series) {
    # Pull the trained level so series indices line up with the
    # fitted model's encoding.
    train_series <- attr(mvgam_training_data(object), "mvgam_series")
    level_label <- if (!is.null(train_series)) {
      as.character(train_series[1L])
    } else {
      "1"
    }
    return(build_single_series_observation_structure(
      newdata, time_var, level_label
    ))
  }

  # Otherwise: prepare data with standardized time/series attributes.
  # For multivariate fits the data may not carry an explicit `series`
  # column (mvbind builds it implicitly); pass response_names so
  # ensure_mvgam_variables can recreate the multivariate series.
  data_prepared <- prepare_mvgam_frame(object, newdata)

  # Validate that data preparation succeeded
  checkmate::assert_data_frame(data_prepared, min.rows = 1)
  if (is.null(attr(data_prepared, "mvgam_time"))) {
    stop(insight::format_error(c(
      "Failed to prepare data with mvgam time attributes.",
      i = cli::format_inline(
        "Check that {.field {time_var}} exists in data."
      )
    )))
  }
  if (is.null(attr(data_prepared, "mvgam_series"))) {
    stop(insight::format_error(c(
      "Failed to prepare data with mvgam series attributes.",
      i = cli::format_inline(
        "Check that {.field {series_var}} exists in data."
      )
    )))
  }

  # Extract indices using existing accessor functions
  time_indices <- get_time_for_grouping(data_prepared)
  series_indices <- get_series_for_grouping(data_prepared)

  # The integer is an index into the fitted trend matrix, so it has to
  # be read against the series the model was fitted on rather than the
  # ones this frame happens to contain. Factoring `newdata` on its own
  # values numbers them from one within the frame: a caller asking
  # about a single series, which is what the per-series hindcast arms
  # do, would get index 1 for every series and read the first series'
  # latent state throughout. A frame carrying a `series` column keeps
  # its full level set through subsetting and so survives that; one
  # whose series is rebuilt from a `gr` / `subgr` pair does not,
  # because the rebuild sees only the levels present.
  index <- fitted_series_index(object)
  if (is.null(index)) {
    levs <- sort(unique(as.character(series_indices)))
    index <- stats::setNames(seq_along(levs), levs)
  }
  series_levels <- names(index)
  series_int <- unname(index[as.character(series_indices)])
  unmatched <- unique(as.character(series_indices)[is.na(series_int)])
  if (length(unmatched)) {
    stop(insight::format_error(c(
      "newdata names series the model was not fitted on.",
      x = cli::format_inline("Unknown: {.val {unmatched}}."),
      i = cli::format_inline("Fitted series are {.val {series_levels}}.")
    )))
  }

  unique_times <- sort(unique(time_indices))

  list(
    time = time_indices,
    series = series_indices,
    series_int = series_int,
    series_levels = series_levels,
    n_obs = nrow(newdata),
    n_times = length(unique_times),
    n_series = length(series_levels),
    unique_times = unique_times
  )
}


#' Build a single-series observation structure directly
#'
#' Shortcut for trained models with `n_series == 1`. The trained model
#' already encodes the (single) series identity in its standata, so we
#' don't need to round-trip through `ensure_mvgam_variables`. We just
#' read newdata's time column, repeat the trained series label across
#' all rows, and produce the same structure
#' `get_observation_structure()` returns for the multi-series path.
#'
#' @param newdata Data frame; must have a column matching `time_var`.
#' @param time_var Character; name of the time column.
#' @param level_label Character; the single trained series level used
#'   for the integer/factor mapping. Pulled from
#'   `attr(object$obs_data, "mvgam_series")\[1\]`.
#'
#' @noRd
build_single_series_observation_structure <- function(newdata, time_var,
                                                       level_label) {
  if (!time_var %in% names(newdata)) {
    stop(insight::format_error(c(
      paste0("Required time variable '", time_var,
             "' not found in newdata."),
      i = "Add a time column or pass newdata that retains it."
    )))
  }
  time_indices <- newdata[[time_var]]
  unique_times <- sort(unique(time_indices))
  n_obs <- nrow(newdata)
  series_factor <- factor(rep(level_label, n_obs), levels = level_label)
  list(
    time = time_indices,
    series = series_factor,
    series_int = rep(1L, n_obs),
    series_levels = level_label,
    n_obs = n_obs,
    n_times = length(unique_times),
    n_series = 1L,
    unique_times = unique_times
  )
}


#' Get Trend Type from mvgam Object
#'
#' Extracts the base trend type from a fitted mvgam object.
#'
#' @param object mvgam object with fitted trend model
#'
#' @return Character string with trend type (e.g., "AR", "RW", "VAR"),
#'   or "None" if no trend model present.
#'
#' @details
#' Lookup order:
#' 1. `trend_components$types\[1\]` - primary source used by summary/print
#' 2. `trend_metadata$trend$trend_type` - fallback metadata source
#' 3. `trend_formula` existence check - issues warning if type unclear
#' 4. Returns "None" if no trend model detected
#'
#' @noRd
get_trend_type <- function(object) {
  checkmate::assert_class(object, "mvgam")

  # Primary source: trend_components$types (used by summary/print)
  trend_comps <- object$trend_components
  if (!is.null(trend_comps) && !is.null(trend_comps$types)) {
    trend_type <- trend_comps$types[1]
    if (!is.null(trend_type) && !is.na(trend_type)) {
      return(trend_type)
    }
  }

  # Secondary source: trend_metadata$trend$trend_type. The guard
  # tests for a list rather than for non-NULL: `$` on an atomic
  # vector is an error, not NULL, so a fit whose `trend$` slot
  # holds a bare character vector took the fallback path into a
  # stop() instead of through it.
  metadata <- object$trend_metadata
  if (!is.null(metadata) && is.list(metadata$trend)) {
    trend_type <- metadata$trend$trend_type
    if (!is.null(trend_type) && !is.na(trend_type)) {
      return(trend_type)
    }
  }

  # Tertiary: check trend_formula existence (indicates trends present)
  if (!is.null(object$trend_formula)) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        insight::format_message(
          c(
            "Could not determine trend type from object structure.",
            i = paste0(
              "Trend formula present but type not stored in ",
              "'trend_components' or 'trend_metadata'."
            )
          )
        ),
        .frequency = "once",
        .frequency_id = "unknown_trend_type"
      )
    }
  }

  # Default to None if no trend model present
  "None"
}


#' Check if Model Has Stochastic Trend
#'
#' Determines if the model has a trend component that produces
#' stochastic innovations (i.e., not PW or None).
#'
#' @param object mvgam object
#'
#' @return Logical indicating if model has stochastic innovations
#'
#' @noRd
has_stochastic_trend <- function(object) {
  trend_type <- get_trend_type(object)
  pattern <- get_covariance_pattern(trend_type)
  pattern != "none"
}


#' Sample Process Errors for a Fitted mvgam Object
#'
#' Top-level entry point for innovation sampling. Composes the
#' observation structure, the trend covariance structure, and the
#' pattern-specific transform into a single matrix of process-error
#' draws aligned with the observation grid.
#'
#' @param object A fitted `mvgam` object.
#' @param ndraws Optional number of posterior draws to use. Mutually
#'   exclusive with `draw_ids`. Defaults to all available draws.
#' @param newdata Optional data frame of prediction covariates. If
#'   `NULL`, the observation grid is taken from the training data.
#' @param draw_ids Optional integer vector of specific draw indices.
#'   Mutually exclusive with `ndraws`.
#'
#' @return Numeric matrix \[ndraws x n_obs\] of sampled innovations,
#'   in the same column order as `newdata` (or training data) rows.
#'   Returns a matrix of zeros for deterministic-trend models (PW,
#'   None) so callers can add it unconditionally.
#'
#' @details
#' For deterministic trends the routine short-circuits and returns a
#' single-row zero matrix unless `ndraws` or `draw_ids` is supplied,
#' in which case the requested row count is returned.
#'
#' @noRd
sample_process_errors <- function(object, ndraws = NULL, newdata = NULL,
                                   draw_ids = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  if (!is.null(ndraws) && !is.null(draw_ids)) {
    stop(insight::format_error(
      "Cannot specify both 'ndraws' and 'draw_ids'."
    ))
  }

  obs_structure <- get_observation_structure(object, newdata)

  if (!has_stochastic_trend(object)) {
    n_rows <- if (!is.null(draw_ids)) {
      length(draw_ids)
    } else if (!is.null(ndraws)) {
      ndraws
    } else {
      1L
    }
    return(matrix(0, n_rows, obs_structure$n_obs))
  }

  cov_structure <- get_trend_covariance_structure(
    object, ndraws = ndraws, draw_ids = draw_ids
  )
  innov <- sample_innovations(cov_structure, obs_structure)
  # Strip posterior::draws_matrix class so downstream callers (notably
  # marginaleffects, which type-checks @draws against matrixOrNULL)
  # see a plain numeric matrix matching brms's posterior_* return.
  if (!is.null(innov)) {
    dn <- dim(innov)
    innov <- as.numeric(innov)
    dim(innov) <- dn
  }
  innov
}


#' Required Parameter Names by Covariance Pattern
#'
#' Defines which posterior parameters are needed for each pattern type.
#'
#' @noRd
covariance_param_specs <- list(
  cholesky_scaled = list(
    simple = c("sigma_trend", "L_Omega_trend"),
    hierarchical = c("L_Omega_global_trend", "L_deviation_group_trend",
                     "alpha_cor_trend", "sigma_group_trend")
  ),
  full_covariance = list(
    simple = c("Sigma_trend"),
    hierarchical = c("Sigma_group_trend")
  ),
  diagonal = list(
    simple = c("sigma_trend"),
    hierarchical = c("sigma_group_trend")
  ),
  none = list(simple = character(0), hierarchical = character(0))
)


#' Name the covariance structure innovations are drawn from
#'
#' A hierarchical trend carries its correlations as a population
#' Cholesky factor plus per-group deviations, whatever the trend
#' constructor was, so a hierarchical `VAR(gr = ...)` is parameterised
#' exactly as a hierarchical `AR(gr = ...)`. Naming the structure once
#' keeps the parameter extraction and the innovation transform from
#' disagreeing about which shape they are working with: reading it two
#' ways is how a hierarchical VAR came to be handed the parameters of
#' one structure and then asked for the covariance of another.
#'
#' @param hierarchical Whether the trend is grouped
#' @param effective_pattern Covariance pattern after the
#'   no-correlations collapse to `"diagonal"`
#' @return A single key of the form `"hier.cholesky_scaled"`
#'
#' @noRd
covariance_structure_key <- function(hierarchical, effective_pattern) {
  if (isTRUE(hierarchical) &&
      effective_pattern %in% c("cholesky_scaled", "full_covariance")) {
    return("hier.cholesky_scaled")
  }
  paste0(if (isTRUE(hierarchical)) "hier" else "flat", ".",
         effective_pattern)
}


#' Get Trend Covariance Structure from Posterior
#'
#' Extracts covariance parameters from posterior draws based on the
#' trend model's covariance pattern. Uses trend_metadata as ground truth
#' for structure determination.
#'
#' @param object mvgam object with fitted trend model
#' @param ndraws Number of posterior draws to extract (NULL = all).
#'   Mutually exclusive with draw_ids.
#' @param draw_ids Specific draw indices to use. Mutually exclusive
#'   with ndraws.
#'
#' @return List with components:
#'   \itemize{
#'     \item \code{pattern}: Covariance pattern type
#'     \item \code{n_series}: Number of series
#'     \item \code{hierarchical}: Whether model uses hierarchical structure
#'     \item \code{has_correlations}: Whether cor=TRUE in trend constructor
#'     \item \code{ndraws}: Number of posterior draws extracted
#'     \item \code{params}: List of extracted parameter matrices
#'     \item \code{group_info}: Group structure (if hierarchical)
#'   }
#'   Cholesky factors are kept as flat per-draw vectors (in row-major
#'   order from posterior sort); reshape via
#'   matrix(vec, dim, dim, byrow = TRUE) to get L.
#'
# Per-draw innovation degrees of freedom for forecasting.
#
# Returns NULL when the trend has Gaussian innovations, a length-ndraws
# vector of posterior draws when they were estimated, and a repeated
# constant when they were fixed at a finite value on the constructor.
#'@noRd
extract_nu_trend_draws <- function(draws_mat, object) {
  if ("nu_trend" %in% colnames(draws_mat)) {
    return(as.numeric(draws_mat[, "nu_trend"]))
  }
  df <- object$trend_metadata$df %||% Inf
  if (is_gaussian_df(df) || is.na(df)) {
    return(NULL)
  }
  rep(as.numeric(df), nrow(draws_mat))
}

#' @noRd
get_trend_covariance_structure <- function(object, ndraws = NULL,
                                           draw_ids = NULL) {
  validate_covariance_inputs(object, ndraws, draw_ids)

  metadata <- object$trend_metadata
  if (is.null(metadata)) {
    stop(insight::format_error(c(
      "This 'mvgam' fit has no latent trend, so it has no trend covariance.",
      i = paste0(
        "A trend is declared through 'trend_formula', for example ",
        "trend_formula = ~ AR(cor = TRUE)."
      )
    )))
  }

  trend_type <- get_trend_type(object)
  pattern <- get_covariance_pattern(trend_type)

  specs <- object$trend_components$specifications
  has_correlations <- if (inherits(specs, "mvgam_trend")) {
    isTRUE(specs$cor)
  } else {
    isTRUE(specs[[1]]$cor)
  }

  if (pattern == "none") {
    return(list(
      pattern = "none",
      n_series = 0L,
      hierarchical = FALSE,
      has_correlations = FALSE,
      ndraws = 0L,
      params = list(),
      group_info = NULL
    ))
  }

  # Trend topology comes from the user's trend constructor spec.
  # `detect_factor_n_lv` returns n_lv (or NULL); a non-NA `$gr`
  # marks a hierarchical (grouped) trend. Same checks the
  # stan-assembly / validation layers use.
  spec <- trend_spec_for_residcor(object)
  n_lv <- detect_factor_n_lv(object)
  is_lv <- !is.null(n_lv)
  hierarchical <- !is.null(spec$gr) && spec$gr != "NA"
  n_obs_series <- object$series_info$n_series %||%
    object$trend_components$n_trends
  n_series <- if (is_lv) as.integer(n_lv) else
    as.integer(n_obs_series)

  draws_mat <- posterior::as_draws_matrix(object$fit)
  draw_indices <- resolve_draw_indices(nrow(draws_mat), ndraws, draw_ids)
  draws_mat <- draws_mat[draw_indices, , drop = FALSE]

  effective_pattern <- pattern
  if (pattern == "cholesky_scaled" && !has_correlations) {
    effective_pattern <- "diagonal"
  }

  group_info <- if (hierarchical) get_group_info(object$standata) else NULL
  n_series_int <- as.integer(n_series)

  # Dispatch posterior-parameter extraction on (hierarchical, pattern).
  # Multi-index params (Cholesky factors, full covariance, hierarchical
  # diagonal) require explicit named-column lookup; extract_named_params()
  # sorts only by first index, which is brittle for 2D+ arrays where the
  # secondary order then depends on the source storage convention.
  # The fallback covers patterns that are single-index (e.g. simple
  # diagonal: just "sigma_trend").
  # Hierarchical VAR (`VAR(gr = ..., subgr = ..., cor = TRUE)`) emits
  # the same population-vs-deviation parameter shapes as the
  # hierarchical Cholesky-scaled case: `alpha_cor_trend`,
  # `L_Omega_global_trend`, `L_deviation_group_trend`, and
  # `sigma_group_trend`. See `generate_hierarchical_correlation_parameters()`
  # in R/stan_assembly.R. One extractor populates both, and
  # `compute_residcor_hierarchical()` reads the same fields downstream.
  dispatch_key <- covariance_structure_key(hierarchical, effective_pattern)
  params <- switch(
    dispatch_key,
    "hier.cholesky_scaled"  = extract_hierarchical_cholesky_params(
                                draws_mat, group_info),
    "hier.diagonal"         = extract_hierarchical_diagonal_params(
                                draws_mat, group_info),
    "flat.cholesky_scaled"  = extract_simple_cholesky_params(
                                draws_mat, n_series_int),
    "flat.full_covariance"  = extract_simple_full_cov_params(
                                draws_mat, n_series_int),
    {
      param_names <- covariance_param_specs[[effective_pattern]][[
        if (hierarchical) "hierarchical" else "simple"
      ]]
      extract_named_params(draws_mat, param_names)
    }
  )

  # A marginal prediction integrates over the state's own spread, not
  # over one innovation, so the covariance the transforms read is the
  # stationary one wherever the kernel settles at all.
  params <- rescale_params_to_stationary(
    params = params, object = object, draws_mat = draws_mat,
    group_info = group_info
  )

  list(
    pattern = pattern,
    n_series = as.integer(n_series),
    hierarchical = hierarchical,
    has_correlations = has_correlations,
    ndraws = length(draw_indices),
    params = params,
    group_info = group_info,
    is_lv = is_lv,
    n_obs_series = as.integer(n_obs_series),
    draws_mat = if (is_lv) draws_mat else NULL,
    # Per-draw innovation degrees of freedom, NULL for Gaussian trends.
    # Carried here so `sample_innovations()` draws from the same
    # distribution the model was fitted with.
    nu_trend = extract_nu_trend_draws(draws_mat, object),
    # Threaded to `resolve_factor_loadings()` so fixed-Z fits
    # skip the Z[i, j] posterior lookup that does not exist when
    # Z is supplied as Stan data.
    fixed_Z = object$trend_metadata$fixed_Z
  )
}


#' @noRd
validate_covariance_inputs <- function(object, ndraws, draw_ids) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)

  if (!is.null(ndraws) && !is.null(draw_ids)) {
    stop(insight::format_error(
      "Cannot specify both 'ndraws' and 'draw_ids'."
    ))
  }

  if (is.null(object$fit)) {
    stop(insight::format_error(
      "mvgam object has no fitted posterior draws."
    ))
  }

  invisible(NULL)
}


#' Take the rows of a draws matrix belonging to the chosen draws
#'
#' The companion to `resolve_draw_indices()`, and the shape every
#' extractor in the package ends in: find the columns holding a
#' quantity, then keep the rows holding the iterations the rest of the
#' answer is built from. Going through one reader is what keeps a
#' second extractor from choosing its own draws.
#'
#' @param x A draws matrix, or any matrix whose rows are draws
#' @param ndraws Requested number of draws, or `NULL`
#' @param draw_ids Draw indices the caller already has, or `NULL`
#' @return `x` restricted to the chosen rows
#'
#' @noRd
subset_draws_rows <- function(x, ndraws = NULL, draw_ids = NULL) {
  x[resolve_draw_indices(nrow(x), ndraws, draw_ids), , drop = FALSE]
}


#' Turn a draw count into draw indices
#'
#' The single place a requested number of draws becomes a set of
#' rows. Everything a post-fit method assembles has to be read out of
#' the same iterations, and it only takes two extractions choosing
#' differently for a mean to be paired with a dispersion, a trend or a
#' set of innovations that belong to another draw. A count therefore
#' resolves to indices once and those indices are passed on, rather
#' than each extraction being handed the count and choosing again.
#'
#' A count covering the whole posterior still resolves, because the
#' subsample is random: handed the bare count, one extraction would
#' return the draws shuffled and another would not.
#'
#' @param total_draws Number of draws the posterior holds
#' @param ndraws Requested number of draws, or `NULL`
#' @param draw_ids Draw indices the caller already has, or `NULL`
#' @return An integer vector of indices
#'
#' @noRd
resolve_draw_indices <- function(total_draws, ndraws, draw_ids) {
  if (!is.null(draw_ids)) {
    if (max(draw_ids) > total_draws) {
      stop(insight::format_error(c(
        "Requested 'draw_ids' exceed available draws.",
        x = paste0("Max requested: ", max(draw_ids),
                   ", available: ", total_draws, ".")
      )))
    }
    return(as.integer(draw_ids))
  }

  if (!is.null(ndraws)) {
    if (ndraws > total_draws) {
      stop(insight::format_error(c(
        "Requested more draws than the posterior holds.",
        x = paste0("Asked for ", ndraws, "; the fit has ",
                   total_draws, "."),
        i = "Lower 'ndraws', or leave it unset to use every draw."
      )))
    }
    if (ndraws == total_draws) {
      return(seq_len(total_draws))
    }
    return(sort(sample.int(total_draws, ndraws)))
  }

  seq_len(total_draws)
}


#' @noRd
extract_named_params <- function(draws_mat, param_names) {
  all_cols <- colnames(draws_mat)
  params <- lapply(param_names, function(name) {
    extract_posterior_param(draws_mat, all_cols, name)
  })
  names(params) <- param_names
  params
}


#' @noRd
extract_posterior_param <- function(draws_mat, all_cols, param_name) {
  # Match indexed params (param[...]) or scalar (param)
  indexed_pattern <- paste0("^", param_name, "\\[")
  scalar_pattern <- paste0("^", param_name, "$")

  indexed_cols <- grep(indexed_pattern, all_cols, value = TRUE)

  if (length(indexed_cols) > 0) {
    # Sort by numeric index for consistent ordering
    idx_nums <- as.integer(gsub(".*\\[(\\d+).*", "\\1", indexed_cols))
    indexed_cols <- indexed_cols[order(idx_nums)]
    return(as.matrix(draws_mat[, indexed_cols, drop = FALSE]))
  }

  scalar_cols <- grep(scalar_pattern, all_cols, value = TRUE)
  if (length(scalar_cols) > 0) {
    return(as.matrix(draws_mat[, scalar_cols[1], drop = FALSE]))
  }

  stop(insight::format_error(c(
    paste0("Parameter '", param_name, "' not found in posterior."),
    i = "Check that model was fitted with expected covariance structure."
  )))
}


#' Extract Simple Cholesky Parameters as Structured Arrays
#'
#' Builds a per-draw \[ndraws, n, n\] array for `L_Omega_trend` and
#' \[ndraws, n\] matrix for `sigma_trend` via direct column-name
#' lookup. Mirrors `extract_hierarchical_cholesky_params()` for the
#' non-hierarchical case so reconstruction is independent of any
#' sort-order assumption in `extract_named_params()`.
#'
#' @noRd
NULL

#' Pull an indexed Stan matrix parameter into a per-draw
#' `[ndraws, nrow, ncol]` array. Handles matrix-valued (`name[i, j]`)
#' or array-of-matrix (`name[c, i, j]`, `name[c, l, i, j]`) Stan
#' declarations via `prefix_ids`, applies an optional per-draw
#' matrix `transform` (e.g. `stats::cov2cor`, `tcrossprod`) and
#' can label dims 2-3 for user-facing return values. Shared by
#' Cholesky / covariance / factor-loading extractors that build
#' `[d, i, j]` arrays from posterior draws, and by
#' `posterior_innovation_cor()` / `posterior_transition_matrix()`.
#'
#' @param draws_mat A posterior draws matrix
#'   (`posterior::as_draws_matrix()`).
#' @param name Character. Base parameter name (e.g. `"Sigma_trend"`,
#'   `"A_group_trend"`).
#' @param nrow,ncol Positive integers. Matrix dimensions.
#' @param prefix_ids Integer vector of leading (fixed) indices.
#'   Empty for a bare `name[i, j]` matrix; `c(group)` for
#'   `name[group, i, j]`; `c(group, lag)` for
#'   `name[group, lag, i, j]`.
#' @param transform Optional function taking a `nrow x ncol` matrix
#'   and returning a matrix of the same shape (or `NULL` for the
#'   identity). Applied per draw.
#' @param labels Character vector of length `max(nrow, ncol)` used
#'   to name dims 2-3 (or `NULL` to leave unnamed). Only sensible
#'   when `nrow == ncol`.
#' @param required_for Human-readable hint used in the missing-cell
#'   error message.
#' @return `[ndraws, nrow, ncol]` numeric array.
#'
#' @noRd
extract_indexed_array_2d <- function(draws_mat, name, nrow, ncol,
                                      prefix_ids = integer(0),
                                      transform = NULL,
                                      labels = NULL,
                                      required_for = name) {
  checkmate::assert_matrix(draws_mat, min.rows = 1, min.cols = 1)
  checkmate::assert_string(name, min.chars = 1)
  checkmate::assert_int(nrow, lower = 1)
  checkmate::assert_int(ncol, lower = 1)
  checkmate::assert_integerish(prefix_ids, lower = 1L, min.len = 0L)
  ndraws <- base::nrow(draws_mat)
  all_cols <- colnames(draws_mat)
  # Build column names in column-major order (i varies fastest so
  # the flat vector maps directly onto a [ndraws, nrow, ncol]
  # array via R's default column-major fill), then take the whole
  # block in one matrix slice. The earlier per-cell loop cost one
  # named-column lookup per (nrow * ncol) iterations; a 24 x 24
  # `Sigma_trend` block on a wide draws matrix took long enough to
  # dominate `irf()` wall-clock on hierarchical VAR fits.
  ij <- expand.grid(i = seq_len(nrow), j = seq_len(ncol))
  if (length(prefix_ids)) {
    prefix_str <- paste(as.integer(prefix_ids), collapse = ",")
    col_names <- sprintf(
      paste0(name, "[", prefix_str, ",%d,%d]"), ij$i, ij$j
    )
  } else {
    col_names <- sprintf(paste0(name, "[%d,%d]"), ij$i, ij$j)
  }
  missing_cols <- setdiff(col_names, all_cols)
  if (length(missing_cols)) {
    stop(insight::format_error(c(
      paste0("Posterior parameter '", missing_cols[1L], "' not found."),
      i = paste0("Required for ", required_for, ".")
    )))
  }
  # Fast path when no per-draw transform is requested: one array()
  # call over the whole column-major slice.
  if (is.null(transform)) {
    out <- array(as.numeric(draws_mat[, col_names, drop = FALSE]),
                 dim = c(ndraws, nrow, ncol))
  } else {
    flat <- draws_mat[, col_names, drop = FALSE]
    stacked <- vapply(seq_len(ndraws), function(d) {
      as.numeric(transform(matrix(as.numeric(flat[d, ]), nrow, ncol)))
    }, numeric(nrow * ncol))
    out <- aperm(array(stacked, dim = c(nrow, ncol, ndraws)),
                 c(3, 1, 2))
  }
  if (!is.null(labels)) {
    dimnames(out) <- list(NULL, labels, labels)
  }
  out
}


extract_simple_cholesky_params <- function(draws_mat, n_series) {
  checkmate::assert_matrix(draws_mat, min.rows = 1, min.cols = 1)
  checkmate::assert_int(n_series, lower = 1)
  list(
    sigma_trend = read_draws_vector(draws_mat, "sigma_trend", n_series),
    L_Omega_trend = read_draws_matrix(draws_mat, "L_Omega_trend",
                                       n_series, n_series)
  )
}


#' Extract Simple Full Covariance Parameters as Structured Arrays
#'
#' Builds a \[ndraws, n, n\] array for `Sigma_trend` via direct
#' column-name lookup so reconstruction is order-safe.
#'
#' @noRd
extract_simple_full_cov_params <- function(draws_mat, n_series) {
  checkmate::assert_matrix(draws_mat, min.rows = 1, min.cols = 1)
  checkmate::assert_int(n_series, lower = 1)
  ndraws <- nrow(draws_mat)
  all_cols <- colnames(draws_mat)

  pull_col <- function(name) {
    if (!name %in% all_cols) {
      stop(insight::format_error(c(
        paste0("Posterior parameter '", name, "' not found."),
        i = "Required for full-covariance trend."
      )))
    }
    as.numeric(draws_mat[, name])
  }

  Sigma <- array(0, c(ndraws, n_series, n_series))
  for (j in seq_len(n_series)) {
    for (i in seq_len(n_series)) {
      Sigma[, i, j] <- pull_col(sprintf("Sigma_trend[%d,%d]", i, j))
    }
  }

  list(Sigma_trend = Sigma)
}


#' Extract Hierarchical Cholesky Parameters as Structured Arrays
#'
#' Builds named arrays for hierarchical Cholesky covariance from posterior
#' draws via direct column-name lookup. Unlike `extract_named_params()`
#' (which sorts only by first index), this preserves multi-index ordering
#' so downstream code can use `arr\[d, g, i, j\]` directly.
#'
#' @return List with components:
#'   - `alpha_cor_trend`: numeric vector length ndraws
#'   - `L_Omega_global_trend`: array \[ndraws, n_sub, n_sub\]
#'   - `L_deviation_group_trend`: array \[ndraws, n_groups, n_sub, n_sub\]
#'   - `sigma_group_trend`: array \[ndraws, n_groups, n_sub\]
#'
#' @noRd
extract_hierarchical_cholesky_params <- function(draws_mat, group_info) {
  checkmate::assert_matrix(draws_mat, min.rows = 1, min.cols = 1)
  checkmate::assert_list(group_info)
  checkmate::assert_names(
    names(group_info),
    must.include = c("n_groups", "n_subgroups")
  )

  ndraws <- nrow(draws_mat)
  n_groups <- as.integer(group_info$n_groups)
  n_sub <- as.integer(group_info$n_subgroups)
  all_cols <- colnames(draws_mat)

  pull_col <- function(name) {
    if (!name %in% all_cols) {
      stop(insight::format_error(c(
        paste0("Posterior parameter '", name, "' not found."),
        i = "Required for hierarchical Cholesky covariance."
      )))
    }
    as.numeric(draws_mat[, name])
  }

  # alpha_cor_trend: scalar per draw
  alpha <- pull_col(HIER_COV_PARS$alpha)

  # L_Omega_global_trend: 2D matrix per draw, shape [n_sub, n_sub]
  L_global <- array(0, c(ndraws, n_sub, n_sub))
  for (j in seq_len(n_sub)) {
    for (i in seq_len(n_sub)) {
      L_global[, i, j] <- pull_col(
        paste0(HIER_COV_PARS$global, "[", i, ",", j, "]")
      )
    }
  }

  # L_deviation_group_trend: per-group matrix, shape [n_groups, n_sub, n_sub]
  L_dev <- array(0, c(ndraws, n_groups, n_sub, n_sub))
  for (g in seq_len(n_groups)) {
    for (j in seq_len(n_sub)) {
      for (i in seq_len(n_sub)) {
        L_dev[, g, i, j] <- pull_col(
          paste0(HIER_COV_PARS$deviation, "[", g, ",", i, ",", j, "]")
        )
      }
    }
  }

  # sigma_group_trend: per-group SDs, shape [n_groups, n_sub]
  sigma_grp <- array(0, c(ndraws, n_groups, n_sub))
  for (g in seq_len(n_groups)) {
    for (s in seq_len(n_sub)) {
      sigma_grp[, g, s] <- pull_col(
        paste0(HIER_COV_PARS$sigma, "[", g, ",", s, "]")
      )
    }
  }

  list(
    alpha_cor_trend = alpha,
    L_Omega_global_trend = L_global,
    L_deviation_group_trend = L_dev,
    sigma_group_trend = sigma_grp
  )
}


#' Extract Hierarchical Diagonal Parameters Broadcast to Per-Series Sigma
#'
#' For hierarchical models with `cor = FALSE` (RW(gr=), AR(gr=), ZMVN(gr=)
#' with diagonal innovation covariance), the posterior holds per-group SDs
#' as `sigma_group_trend\[g, k\]`. The innovation sampler operates on a flat
#' per-series `sigma_trend\[d, s\]` matrix, so we broadcast each series to
#' its group's `k`-th entry, where `k` is the series' index within its
#' group as ordered in the Stan codegen (matches the `if
#' (group_inds_trend\[s\] == g_idx) { k += 1; ... }` loop in
#' `generate_hierarchical_correlation_parameters()`).
#'
#' @return List with `sigma_trend`: matrix \[ndraws x n_series\]
#'
#' @noRd
extract_hierarchical_diagonal_params <- function(draws_mat, group_info) {
  checkmate::assert_matrix(draws_mat, min.rows = 1, min.cols = 1)
  checkmate::assert_list(group_info)
  checkmate::assert_names(
    names(group_info),
    must.include = c("n_groups", "n_subgroups", "group_inds")
  )

  ndraws <- nrow(draws_mat)
  n_groups <- as.integer(group_info$n_groups)
  n_sub <- as.integer(group_info$n_subgroups)
  group_inds <- as.integer(group_info$group_inds)
  n_series <- length(group_inds)
  all_cols <- colnames(draws_mat)

  pull_col <- function(name) {
    if (!name %in% all_cols) {
      stop(insight::format_error(c(
        paste0("Posterior parameter '", name, "' not found."),
        i = "Required for hierarchical diagonal covariance."
      )))
    }
    as.numeric(draws_mat[, name])
  }

  # Sub-index of each series within its group, matching Stan loop order.
  sub_idx <- integer(n_series)
  group_counter <- integer(n_groups)
  for (s in seq_len(n_series)) {
    g <- group_inds[s]
    group_counter[g] <- group_counter[g] + 1L
    sub_idx[s] <- group_counter[g]
  }

  sigma_trend <- matrix(0, ndraws, n_series)
  for (s in seq_len(n_series)) {
    sigma_trend[, s] <- pull_col(
      sprintf("sigma_group_trend[%d,%d]", group_inds[s], sub_idx[s])
    )
  }

  list(sigma_trend = sigma_trend)
}


#' @noRd
get_group_info <- function(standata) {
  list(
    n_groups = standata$N_groups_trend,
    n_subgroups = standata$N_subgroups_trend,
    group_inds = standata$group_inds_trend
  )
}




# ============================================================================
# Pattern-Specific Innovation Samplers
# ============================================================================
#
# All trend models sample from MVN with different covariance
# parameterizations. These functions transform standard normal samples
# z ~ N(0,I) into innovations with the appropriate covariance structure.
#
# Design: Vectorize where possible, minimize memory allocations.
# Per-draw loops only when unavoidable (different covariance per draw).


#' Sample Innovations from Trend Model
#'
#' Main entry point for innovation sampling. Dispatches to pattern-specific
#' transform functions based on covariance structure.
#'
#' @param cov_structure List from `get_trend_covariance_structure()`
#'   containing: pattern, n_series, ndraws, params, has_correlations.
#' @param obs_structure List from `get_observation_structure()` containing:
#'   time, series_int, n_obs, n_times, n_series, unique_times.
#'
#' @return Matrix \[ndraws x n_obs\] of sampled innovations, where each
#'   observation gets the innovation for its (time, series) combination.
#'
#' @details
#' Algorithm:
#' 1. Generate standard normals for (time, series) grid
#' 2. Transform by covariance pattern
#' 3. Map grid innovations to observations via linear indexing
#'
#' @noRd
sample_innovations <- function(cov_structure, obs_structure) {
  # Validate input structures
  checkmate::assert_list(cov_structure, min.len = 1)
  checkmate::assert_names(
    names(cov_structure),
    must.include = c("pattern", "ndraws", "n_series", "params")
  )
  checkmate::assert_list(obs_structure, min.len = 1)
  checkmate::assert_names(
    names(obs_structure),
    must.include = c("n_obs", "n_times", "series_int", "time", "unique_times")
  )

  pattern <- cov_structure$pattern
  ndraws <- cov_structure$ndraws

  checkmate::assert_string(pattern)
  checkmate::assert_int(ndraws, lower = 0)

  # Return zeros for deterministic trends (pattern="none") or when
  # no draws requested (ndraws=0, which can occur during validation)
  if (pattern == "none" || ndraws == 0L) {
    effective_ndraws <- if (ndraws == 0L) 1L else ndraws
    return(matrix(0, effective_ndraws, obs_structure$n_obs))
  }

  n_times <- obs_structure$n_times
  n_series <- cov_structure$n_series

  checkmate::assert_int(n_times, lower = 1)
  checkmate::assert_int(n_series, lower = 1)

  # When cor=FALSE, use diagonal path (independent innovations per series)
  effective_pattern <- pattern
  if (pattern == "cholesky_scaled" && !isTRUE(cov_structure$has_correlations)) {
    effective_pattern <- "diagonal"
  }

  # Standardised innovations for the entire (time, series) grid, drawn
  # from whichever distribution the model was fitted with. The mixing
  # value is shared across series within a (draw, time) cell, matching
  # the multivariate t in the Stan model rather than giving each series
  # its own independent tail.
  nu <- cov_structure$nu_trend
  z <- matrix(0, ndraws, n_times * n_series)
  for (d in seq_len(ndraws)) {
    df_d <- if (is.null(nu)) Inf else nu[[d]]
    z[d, ] <- as.numeric(draw_trend_innovations(n_times, n_series, df_d))
  }

  # Transform by covariance structure. The hierarchical case uses a
  # per-group convex combination of correlations, so it has its own
  # transform. The structure is named by the same helper the parameter
  # extraction used, so the two cannot disagree about which shape the
  # parameters in hand describe.
  is_hier_chol <- identical(
    covariance_structure_key(cov_structure$hierarchical, effective_pattern),
    "hier.cholesky_scaled"
  )

  if (is_hier_chol) {
    innovations_flat <- transform_hierarchical_cholesky_innovations(
      z, cov_structure$params, n_times, n_series, ndraws,
      cov_structure$group_info
    )
  } else {
    innovations_flat <- switch(
      effective_pattern,
      diagonal = transform_diagonal_innovations(
        z, cov_structure$params, n_times, n_series, ndraws
      ),
      cholesky_scaled = transform_cholesky_innovations(
        z, cov_structure$params, n_times, n_series, ndraws
      ),
      full_covariance = transform_full_cov_innovations(
        z, cov_structure$params, n_times, n_series, ndraws
      ),
      stop(insight::format_error(
        paste0("Unknown covariance pattern: '", effective_pattern, "'.")
      ))
    )
  }

  # Latent-factor map: innovations were sampled at the LV level
  # (`n_series == n_lv` here). Convert to per-series innovations
  # via the loadings `Z[s, lv]` before mapping to obs. The
  # resolver returns sampled Z draws or the broadcast fixed Z
  # transparently.
  if (isTRUE(cov_structure$is_lv)) {
    # The model basis. The innovations were drawn from the latent
    # covariance, which is stated for the columns the model
    # sampled, so they have to be mapped through those same
    # columns. Using the QR-identified loadings pairs a rotated
    # basis with an unrotated covariance and changes the spread
    # these innovations carry to the series.
    Z <- resolve_factor_loadings(
      draws_mat = cov_structure$draws_mat,
      fixed_Z = cov_structure$fixed_Z,
      n_series = cov_structure$n_obs_series,
      n_lv = n_series,
      basis = "model"
    )
    innovations_flat <- map_lv_to_series_innovations(
      innovations_flat, Z, n_times, n_lv = n_series,
      n_obs_series = cov_structure$n_obs_series
    )
    n_series <- cov_structure$n_obs_series
  }

  # Map (time, series) grid to observations
  map_innovations_to_obs(innovations_flat, n_times, n_series, obs_structure)
}


# Shared regex selectors for factor-model parameter names.
# Free-Z factor fits emit identified `Z_tilde[i, j]` and rotated
# `lv_trend_tilde[t, k]` in generated quantities (Heaps and Jermyn
# 2024). Partial-Z fits skip the QR rotation and keep the user-
# supplied pattern on `Z[i, j]` / `lv_trend[t, k]` directly. These
# helpers centralise the prefer-identified-then-fall-back rule so
# `extract_Z_loadings`, `extract_lv_trend_matrices`,
# `match_z_loadings` (summary.mvgam.R), and
# `categorize_mvgam_parameters` (index-mvgam.R) stay in lockstep.
#' @param pars Parameter names from the fit.
#' @param basis `"identified"` for the QR-rotated `Z_tilde`, which
#'   is what anything reporting or plotting loadings wants;
#'   `"model"` for the raw `Z` the model sampled, which is what
#'   anything combining loadings with `sigma_trend`,
#'   `Sigma_trend` or `Omega_trend` needs, since those live in the
#'   unrotated basis. `Z Z'` is invariant to the rotation, so the
#'   distinction only bites once a non-isotropic covariance sits
#'   between the loadings.
#'@noRd
factor_loading_param_pattern <- function(pars,
                                         basis = c("identified",
                                                   "model")) {
  basis <- match.arg(basis)
  if (identical(basis, "model")) return("^Z\\[")
  if (has_identified_loadings(pars)) "^Z_tilde\\[" else "^Z\\["
}


#' Does this fit carry QR-identified loadings?
#'
#' A free-loading factor fit is rotated to a canonical form and the
#' result is emitted as `Z_tilde`, alongside the raw `Z` it came from.
#' Several places need to know whether that happened, and the answer
#' has to be the same in all of them: the name of the parameter that
#' settles it lives here, and nowhere else.
#'
#' @param pars Character vector of parameter names
#' @return A single logical
#'
#' @noRd
has_identified_loadings <- function(pars) {
  any(grepl("^Z_tilde\\[", pars))
}


#' Does this fit carry QR-identified factor states?
#'
#' The companion to `has_identified_loadings()` for the factor paths
#' the loadings multiply.
#'
#' @param pars Character vector of parameter names
#' @return A single logical
#'
#' @noRd
has_identified_factor_states <- function(pars) {
  any(grepl("^lv_trend_tilde\\[", pars))
}

#'@noRd
factor_state_param_pattern <- function(pars) {
  if (has_identified_factor_states(pars)) {
    "^lv_trend_tilde\\["
  } else {
    "^lv_trend\\["
  }
}

# Returns the regex matching parameter-draws that the summary
# / tidy classifiers should hide because their rotation- or
# sign-indeterminate raw form has an identified counterpart in
# the posterior. Covers:
#   * Raw loadings `Z[i, j]` when `Z_tilde[i, j]` is present
#     (free-Z factor fits with the Heaps & Jermyn QR rotation).
#   * Raw factor paths `lv_trend[t, k]` and the upstream
#     `innovations_trend[t, k]` / `scaled_innovations_trend[t, k]`
#     when `lv_trend_tilde[t, k]` is present (same condition).
#   * Unrotated VAR dynamics `A_trend[lag][i, j]` when
#     `A_trend_tilde[lag][i, j]` is present.
# Each raw family is rotation-indeterminate by design and shows
# poor Rhat / low ESS while the identified counterpart is well
# behaved; hiding the raw form here keeps convergence diagnostics,
# `summary.mvgam()` print, `posterior_summary.mvgam()` and the
# variable-keyword machinery focused on the identified params.
# Returns NULL when nothing needs hiding.
#'@noRd
hidden_unrotated_factor_pars <- function(pars) {
  patterns <- character(0L)
  if (any(grepl("^A_trend_tilde\\[", pars))) {
    patterns <- c(patterns, "^A_trend\\[")
  }
  if (has_identified_loadings(pars)) {
    # Free-Z factor fit detected. The raw loadings `Z[i, j]`,
    # raw factor paths `lv_trend[t, k]`, the innovations driving
    # them, the rotation orthogonal matrix `Q_tilde[i, j]`, and
    # the latent-factor-level variance-block parameters
    # (`sigma_trend[k]`, `L_Omega_trend[i, j]`,
    # `Sigma_trend[i, j]`) are all rotation- or sign-indeterminate
    # because the QR step in generated quantities absorbs the
    # rotation orbit; gate them all together on the `Z_tilde[`
    # signal so the hide pattern survives the latent-state filter
    # upstream of `summary.mvgam()`. The `^sigma_trend\\[` /
    # `^L_Omega_trend\\[` / `^Sigma_trend\\[` patterns are safe
    # against hierarchical-trend false positives: the grouped
    # variants emit as `sigma_group_trend[g, s]` /
    # `L_Omega_global_trend[i, j]` etc., which do not match
    # these prefixes and remain visible.
    patterns <- c(
      patterns,
      "^Z\\[",
      "^lv_trend\\[",
      "^innovations_trend\\[",
      "^scaled_innovations_trend\\[",
      "^Q_tilde\\[",
      "^sigma_trend\\[",
      "^L_Omega_trend\\[",
      "^Sigma_trend\\["
    )
  }
  if (length(patterns) == 0L) return(NULL)
  paste(patterns, collapse = "|")
}

# Return the input vector with rotation- / sign-indeterminate raw
# factor-model parameter names dropped, using the regex from
# `hidden_unrotated_factor_pars()`. Centralises the filter used by
# `variables.mvgam()`, `extract_mvgam_draws()` and `summary.mvgam()`
# so name-side filtering lives in one place.
#'@noRd
filter_hidden_unrotated <- function(pars) {
  hide_pat <- hidden_unrotated_factor_pars(pars)
  if (is.null(hide_pat)) return(pars)
  pars[!grepl(hide_pat, pars)]
}

# Internal: return a `[ndraws, n_series, n_lv]` loading array
# for any post-fit method that needs Z across draws. Two
# branches:
#   1. Free-loadings fits (default factor model, jsdgam under
#      the Heaps identification, any trend_map = NULL or
#      all-NA matrix) sample Z. The posterior carries
#      `Z_tilde[s, k]` (post-hoc QR) or raw `Z[s, k]` columns
#      that `extract_Z_loadings()` pulls per draw.
#   2. Fully-fixed trend_map fits (any user shorthand or
#      numeric matrix with all finite entries) never sample Z.
#      The deterministic loading matrix lives on
#      `object$mv_spec$trend_specs$fixed_Z` and is broadcast
#      across draws here so callers see the same three-way
#      array shape they would get from the sampled branch.
# Callers: forecast.mvgam:propagate_one_draw for the LV-space
# forecast projection, active_factors for column-norm
# summaries. Both need per-draw Z either way.
#'@noRd
resolve_Z_loadings <- function(object, draws_mat, n_series, n_lv,
                               basis = c("identified", "model")) {
  checkmate::assert_matrix(draws_mat)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(n_lv, lower = 1L)
  basis <- match.arg(basis)
  has_free_Z <- any(grepl(
    "^Z(_tilde)?\\[", colnames(draws_mat)
  ))
  if (has_free_Z) {
    return(extract_Z_loadings(draws_mat, n_series, n_lv, basis = basis))
  }
  fixed_Z <- object$mv_spec$trend_specs$fixed_Z
  if (is.null(fixed_Z)) {
    stop(insight::format_error(c(
      "Cannot resolve loading matrix for factor fit.",
      x = "Neither posterior Z / Z_tilde columns nor a fixed_Z on mv_spec.",
      i = paste0(
        "Expected the fit to carry `mv_spec$trend_specs$fixed_Z` ",
        "(fully-fixed trend_map) or free-Z posterior columns."
      )
    )))
  }
  checkmate::assert_matrix(fixed_Z, nrows = n_series, ncols = n_lv)
  ndraws <- nrow(draws_mat)
  # Broadcast the deterministic [n_series, n_lv] loading matrix
  # across draws so the caller's per-draw slicing sees the same
  # array shape it would get from `extract_Z_loadings()`.
  array(rep(as.numeric(fixed_Z), each = ndraws),
        dim = c(ndraws, n_series, n_lv))
}


# Internal: extract factor-loading draws from the posterior.
# Returns array [ndraws, n_obs_series, n_lv] sorted by series
# index (outer) then by lv index (inner), matching Stan's
# column-major storage convention. Selects `Z_tilde` or `Z` via
# `factor_loading_param_pattern()`. For a resolver that also
# handles fully-fixed trend_map fits (Z as data), use
# `resolve_Z_loadings()` above.
#'@noRd
extract_Z_loadings <- function(draws_mat, n_obs_series, n_lv,
                               basis = c("identified", "model")) {
  checkmate::assert_matrix(draws_mat)
  checkmate::assert_int(n_obs_series, lower = 1L)
  checkmate::assert_int(n_lv, lower = 1L)
  basis <- match.arg(basis)
  ndraws <- nrow(draws_mat)
  pattern <- factor_loading_param_pattern(colnames(draws_mat), basis)
  param_name <- if (pattern == "^Z_tilde\\[") "Z_tilde" else "Z"
  cols <- grep(pattern, colnames(draws_mat), value = TRUE)
  expected_cols <- n_obs_series * n_lv
  if (length(cols) != expected_cols) {
    stop(insight::format_error(c(
      paste0(
        "Expected ", expected_cols, " ", param_name,
        " loading columns, found ", length(cols), "."
      ),
      i = paste0(
        "Latent-factor model needs ", param_name,
        "[s, lv] for s in 1..", n_obs_series,
        ", lv in 1..", n_lv, "."
      ),
      i = paste0(
        "Read draws from the stanfit itself: mvgam's 'as_draws_*' ",
        "methods hide the unrotated loadings."
      )
    )))
  }
  # Stan stores the loading matrix column-major:
  # [1,1], [2,1], ..., [N_series,1], [1,2], ...
  array(
    as.numeric(draws_mat[, cols, drop = FALSE]),
    dim = c(ndraws, n_obs_series, n_lv)
  )
}


# Internal: vectorised LV->series mapping for innovations.
# `lv_innov` is `[ndraws, n_times * n_lv]` laid out
# time-fastest-within-lv (matching `transform_diagonal_innovations`).
# `Z` is `[ndraws, n_obs_series, n_lv]`. Output is
# `[ndraws, n_times * n_obs_series]` laid out
# time-fastest-within-series, matching what
# `map_innovations_to_obs` expects. Per draw d, for each time t
# and series s: out[d, t + (s-1)*n_times] =
#   sum_lv Z[d, s, lv] * lv_innov[d, t + (lv-1)*n_times].
#'@noRd
map_lv_to_series_innovations <- function(lv_innov, Z, n_times,
                                          n_lv, n_obs_series) {
  ndraws <- nrow(lv_innov)
  checkmate::assert_matrix(lv_innov,
                            ncols = n_times * n_lv)
  checkmate::assert_array(Z, d = 3L)
  out <- matrix(0, ndraws, n_times * n_obs_series)
  for (d in seq_len(ndraws)) {
    # [n_times x n_lv] for this draw, time varies fastest
    lv_d <- matrix(lv_innov[d, ], nrow = n_times, ncol = n_lv,
                    byrow = FALSE)
    # [n_obs_series x n_lv] for this draw
    Z_d <- Z[d, , , drop = TRUE]
    # [n_times x n_obs_series] per-series innovations
    series_d <- lv_d %*% t(Z_d)
    out[d, ] <- as.numeric(series_d)
  }
  out
}


#' Transform Innovations: Diagonal Pattern
#'
#' For models with independent innovations per series (CAR, or RW/AR/ZMVN
#' with cor=FALSE). Vectorized implementation without per-draw loops.
#'
#' @param z Matrix `[ndraws x (n_times * n_series)]` of standard normals
#' @param params List with `sigma_trend` matrix \[ndraws x n_series\]
#' @param n_times Number of unique time points
#' @param n_series Number of series
#' @param ndraws Number of posterior draws
#'
#' @return Matrix `[ndraws x (n_times * n_series)]` of transformed innovations
#'
#' @details
#' Layout: columns ordered as (t1_s1, t2_s1, ..., tT_s1, t1_s2, ..., tT_sS).
#' Each series block of n_times columns gets scaled by that series' sigma.
#'
#' @noRd
transform_diagonal_innovations <- function(z, params, n_times, n_series,
                                           ndraws) {
  checkmate::assert_matrix(z, nrows = ndraws, ncols = n_times * n_series)
  checkmate::assert_int(n_times, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(ndraws, lower = 1)

  sigma <- params$sigma_trend
  checkmate::assert_matrix(sigma, nrows = ndraws, ncols = n_series)

  # Create column indices: each series index repeated n_times
  # Result: (1,1,...,1, 2,2,...,2, ..., S,S,...,S) with n_times repetitions
  sigma_expanded <- sigma[
    ,
    rep(seq_len(n_series), each = n_times),
    drop = FALSE
  ]

  z * sigma_expanded
}


#' Transform Innovations: Cholesky-Scaled Pattern
#'
#' For correlated innovations via L_Sigma = diag(sigma) %*% L_Omega.
#' Requires per-draw loop because covariance matrices vary across draws.
#'
#' @param z Matrix `[ndraws x (n_times * n_series)]` of standard normals
#' @param params List from `extract_simple_cholesky_params()`:
#'   - `sigma_trend`: matrix \[ndraws x n_series\] of innovation SDs
#'   - `L_Omega_trend`: array \[ndraws x n_series x n_series\]
#' @param n_times Number of unique time points
#' @param n_series Number of series
#' @param ndraws Number of posterior draws
#'
#' @return Matrix `[ndraws x (n_times * n_series)]` of transformed innovations
#'
#' @details
#' For each draw d:
#' 1. Slice L_Omega from the per-draw 3D array
#' 2. Compute L_Sigma = diag(sigma) %*% L_Omega via row scaling
#' 3. Transform: innovations = z %*% t(L_Sigma)
#'
#' @noRd
transform_cholesky_innovations <- function(z, params, n_times, n_series,
                                           ndraws) {
  checkmate::assert_matrix(z, nrows = ndraws, ncols = n_times * n_series)
  checkmate::assert_int(n_times, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(ndraws, lower = 1)

  sigma <- params$sigma_trend
  L_omega_arr <- params$L_Omega_trend

  checkmate::assert_matrix(sigma, nrows = ndraws, ncols = n_series)
  if (!identical(dim(L_omega_arr),
                 as.integer(c(ndraws, n_series, n_series)))) {
    stop(insight::format_error(c(
      "Unexpected dimensions for 'L_Omega_trend'.",
      x = paste0("Got: ", paste(dim(L_omega_arr), collapse = "x"),
                 ", expected: ", ndraws, "x", n_series, "x", n_series, ".")
    )))
  }

  # Pre-allocate result matrix
  result <- matrix(0, ndraws, n_times * n_series)

  for (d in seq_len(ndraws)) {
    L_omega <- L_omega_arr[d, , ]

    # L_Sigma = diag(sigma) %*% L_Omega via row scaling
    L_sigma <- L_omega * as.numeric(sigma[d, ])

    # Reshape z for this draw: [n_times x n_series]
    z_d <- matrix(z[d, ], n_times, n_series, byrow = FALSE)

    # Transform: x = z %*% t(L) gives MVN with cov L %*% t(L)
    result[d, ] <- as.vector(z_d %*% t(L_sigma))
  }

  result
}


#' Transform Innovations: Full Covariance Pattern
#'
#' For VAR models with direct Sigma matrix. Requires Cholesky decomposition
#' per draw, which is O(n^3) where n = n_series.
#'
#' @param z Matrix `[ndraws x (n_times * n_series)]` of standard normals
#' @param params List from `extract_simple_full_cov_params()`:
#'   - `Sigma_trend`: array \[ndraws x n_series x n_series\]
#' @param n_times Number of unique time points
#' @param n_series Number of series
#' @param ndraws Number of posterior draws
#'
#' @return Matrix `[ndraws x (n_times * n_series)]` of transformed innovations
#'
#' @details
#' For each draw d:
#' 1. Slice Sigma from the per-draw 3D array
#' 2. Compute L = chol(Sigma) using R's LAPACK-based implementation
#' 3. Transform: innovations = z %*% t(L)
#'
#' @noRd
transform_full_cov_innovations <- function(z, params, n_times, n_series,
                                           ndraws) {
  checkmate::assert_matrix(z, nrows = ndraws, ncols = n_times * n_series)
  checkmate::assert_int(n_times, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(ndraws, lower = 1)

  Sigma_arr <- params$Sigma_trend
  if (!identical(dim(Sigma_arr),
                 as.integer(c(ndraws, n_series, n_series)))) {
    stop(insight::format_error(c(
      "Unexpected dimensions for 'Sigma_trend'.",
      x = paste0("Got: ", paste(dim(Sigma_arr), collapse = "x"),
                 ", expected: ", ndraws, "x", n_series, "x", n_series, ".")
    )))
  }

  # Pre-allocate result matrix
  result <- matrix(0, ndraws, n_times * n_series)

  for (d in seq_len(ndraws)) {
    Sigma_d <- Sigma_arr[d, , ]

    # R's chol() returns upper triangular; transpose for lower.
    L_d <- t(chol(Sigma_d))

    # Reshape z for this draw
    z_d <- matrix(z[d, ], n_times, n_series, byrow = FALSE)

    # Transform
    result[d, ] <- as.vector(z_d %*% t(L_d))
  }

  result
}


#' Map Grid Innovations to Observations
#'
#' Maps innovations from (time, series) grid to observation vector using
#' linear indexing. Multiple observations at the same (time, series)
#' receive identical innovations.
#'
#' @param innovations_flat Matrix `[ndraws x (n_times * n_series)]` with
#'   layout: columns ordered as (t1_s1, t2_s1, ..., tT_s1, t1_s2, ..., tT_sS)
#' @param n_times Number of unique time points
#' @param n_series Number of series
#' @param obs_structure List from `get_observation_structure()`
#'
#' @return Matrix \[ndraws x n_obs\] of innovations for each observation
#'
#' @details
#' Linear indexing converts (time, series) to grid position. For example,
#' (time=2, series=3) with n_times=10 becomes grid position 2 + (3-1)*10 = 22.
#' Grid layout: time varies fastest within each series block.
#'
#' @noRd
map_innovations_to_obs <- function(innovations_flat, n_times, n_series,
                                   obs_structure) {
  checkmate::assert_matrix(
    innovations_flat,
    ncols = n_times * n_series,
    any.missing = FALSE
  )
  checkmate::assert_int(n_times, lower = 1)
  checkmate::assert_int(n_series, lower = 1)

  # Convert observation (time, series) to grid indices
  time_idx <- match(obs_structure$time, obs_structure$unique_times)
  series_idx <- obs_structure$series_int

  # Validate time matching succeeded
  if (any(is.na(time_idx))) {
    stop(insight::format_error(
      "Failed to match observation times to unique time points."
    ))
  }

  # Validate series indices are within bounds
  checkmate::assert_integerish(
    series_idx,
    lower = 1,
    upper = n_series,
    any.missing = FALSE
  )

  # Linear index into flattened (time, series) grid
  # Grid layout: time varies fastest within each series block
  grid_idx <- time_idx + (series_idx - 1L) * n_times

  # Column selection
  innovations_flat[, grid_idx, drop = FALSE]
}


# The Stan names a grouped trend's covariance is read from. Both
# readers of that structure -- the innovation transform here and the
# forecast state reader in extract_last_state.R -- name it through
# these, so neither can spell a parameter the other does not.
HIER_COV_PARS <- list(
  alpha = "alpha_cor_trend",
  global = "L_Omega_global_trend",
  deviation = "L_deviation_group_trend",
  sigma = "sigma_group_trend"
)


#' Stan names of a matrix parameter's elements
#'
#' Stan writes a matrix element as `name[i,j]` and a per-group matrix
#' as `name[g,i,j]`. Names come back in column-major order, so the
#' values they pull fill a matrix directly.
#'
#' @param prefix Parameter name
#' @param n_row,n_col Matrix dimensions
#' @param lead Leading index for a per-group matrix, or `NULL`
#' @return Character vector of length `n_row * n_col`
#'
#' @noRd
stan_matrix_names <- function(prefix, n_row, n_col, lead = NULL) {
  head <- if (is.null(lead)) "" else paste0(lead, ",")
  paste0(prefix, "[", head,
         rep(seq_len(n_row), times = n_col), ",",
         rep(seq_len(n_col), each = n_row), "]")
}


#' Stan names of a vector parameter's elements
#'
#' @param prefix Parameter name
#' @param n Vector length
#' @param lead Leading index for a per-group vector, or `NULL`
#' @return Character vector of length `n`
#'
#' @noRd
stan_vector_names <- function(prefix, n, lead = NULL) {
  head <- if (is.null(lead)) "" else paste0(lead, ",")
  paste0(prefix, "[", head, seq_len(n), "]")
}


#' Read a vector parameter's draws
#'
#' Pairs the naming convention with the read, so a caller states the
#' parameter once rather than spelling its Stan names again.
#'
#' @param required Whether a missing name is an error. `FALSE` returns
#'   `NULL`, for a parameter only some trend kernels carry.
#' @return `[ndraws x n]` matrix, or `NULL`
#'
#' @noRd
read_draws_vector <- function(draws_mat, prefix, n, lead = NULL,
                              required = TRUE) {
  nm <- stan_vector_names(prefix, n, lead = lead)
  missing <- setdiff(nm, colnames(draws_mat))
  if (length(missing) > 0L) {
    if (!isTRUE(required)) {
      return(NULL)
    }
    stop(insight::format_error(c(
      paste0("Posterior parameter '", prefix, "' is incomplete."),
      x = paste0("Missing: ", paste(utils::head(missing, 3L),
                                    collapse = ", "), ".")
    )))
  }
  matrix(as.numeric(draws_mat[, nm]), nrow = nrow(draws_mat))
}


#' Read a matrix parameter's draws
#'
#' @inheritParams read_draws_vector
#' @return `[ndraws x n_row x n_col]` array, or `NULL`
#'
#' @noRd
read_draws_matrix <- function(draws_mat, prefix, n_row, n_col,
                              lead = NULL, required = TRUE) {
  nm <- stan_matrix_names(prefix, n_row, n_col, lead = lead)
  missing <- setdiff(nm, colnames(draws_mat))
  if (length(missing) > 0L) {
    if (!isTRUE(required)) {
      return(NULL)
    }
    stop(insight::format_error(c(
      paste0("Posterior parameter '", prefix, "' is incomplete."),
      x = paste0("Missing: ", paste(utils::head(missing, 3L),
                                    collapse = ", "), ".")
    )))
  }
  array(as.numeric(draws_mat[, nm]),
        dim = c(nrow(draws_mat), n_row, n_col))
}


#' Cholesky factor of one group's trend covariance
#'
#' A grouped trend gives every group the same population correlation
#' pulled part of the way towards its own, then scales by that group's
#' standard deviations. Both the innovation transform and the forecast
#' reader need the resulting factor, and reading it two ways is what let
#' them disagree about the structure before.
#'
#' @param alpha Weight on the population correlation, one draw
#' @param L_global Lower Cholesky factor of the population correlation
#' @param L_deviation Lower Cholesky factor of the group's own
#' @param sigma Numeric vector of the group's standard deviations
#' @return Lower-triangular matrix `L` with `L %*% t(L)` the group's
#'   covariance
#'
#' @noRd
hierarchical_group_cholesky <- function(alpha, L_global, L_deviation,
                                        sigma) {
  combined <- alpha * tcrossprod(L_global) +
    (1 - alpha) * tcrossprod(L_deviation)
  # R's chol() is upper-triangular; transpose for Stan's lower-triangular
  # cholesky_decompose() convention. Row-scaling by sigma is
  # diag(sigma) %*% L.
  t(chol(combined)) * sigma
}


#' Transform Innovations: Hierarchical Cholesky Pattern
#'
#' For models declared with `gr=` grouping. The per-group covariance is
#' assembled via Stan's `combine_cholesky()` (see R/stan_assembly.R) as a
#' convex combination of the global correlation and per-group deviations,
#' then re-Choleskied and scaled by per-group SDs:
#' \itemize{
#'   \item C_global = L_g L_g^T
#'   \item C_local\[g\] = L_d\[g\] L_d\[g\]^T
#'   \item L_grp\[g\] = chol(alpha * C_global + (1 - alpha) * C_local\[g\])
#'   \item L_full\[g\] = diag(sigma_grp\[g\]) %*% L_grp\[g\]
#' }
#' Different groups are independent (block-diagonal full covariance).
#'
#' @param z Matrix `[ndraws x (n_times * n_series)]` of standard normals.
#' @param params List of structured arrays from
#'   `extract_hierarchical_cholesky_params()`:
#'   `alpha_cor_trend`, `L_Omega_global_trend`,
#'   `L_deviation_group_trend`, `sigma_group_trend`.
#' @param n_times Number of unique time points.
#' @param n_series Total number of series across all groups.
#' @param ndraws Number of posterior draws.
#' @param group_info List with `n_groups`, `n_subgroups`, `group_inds`.
#'
#' @return Matrix `[ndraws x (n_times * n_series)]` of transformed
#'   innovations laid out in the same column order as the diagonal /
#'   simple-Cholesky transforms.
#'
#' @noRd
transform_hierarchical_cholesky_innovations <- function(z, params, n_times,
                                                        n_series, ndraws,
                                                        group_info) {
  checkmate::assert_matrix(z, nrows = ndraws, ncols = n_times * n_series)
  checkmate::assert_int(n_times, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(ndraws, lower = 1)
  checkmate::assert_list(group_info)
  checkmate::assert_names(
    names(group_info),
    must.include = c("n_groups", "n_subgroups", "group_inds")
  )

  n_groups <- as.integer(group_info$n_groups)
  n_sub <- as.integer(group_info$n_subgroups)
  group_inds <- as.integer(group_info$group_inds)

  checkmate::assert_int(n_groups, lower = 1)
  checkmate::assert_int(n_sub, lower = 1)
  checkmate::assert_integerish(
    group_inds,
    lower = 1, upper = n_groups,
    len = n_series, any.missing = FALSE
  )

  alpha <- params$alpha_cor_trend
  L_glob_arr <- params$L_Omega_global_trend
  L_dev_arr <- params$L_deviation_group_trend
  sigma_arr <- params$sigma_group_trend

  checkmate::assert_numeric(alpha, len = ndraws, any.missing = FALSE)
  # Local helper: dim() returns integer; build expected as integer too
  # so identical() doesn't trip on a numeric/integer mismatch.
  check_dims <- function(arr, name, expected) {
    expected <- as.integer(expected)
    if (!identical(dim(arr), expected)) {
      stop(insight::format_error(c(
        paste0("Unexpected dimensions for '", name, "'."),
        x = paste0("Got: ", paste(dim(arr), collapse = "x"),
                   ", expected: ", paste(expected, collapse = "x"), ".")
      )))
    }
  }

  # L_Omega_global: [ndraws, n_sub, n_sub]
  checkmate::assert_array(
    L_glob_arr, d = 3,
    any.missing = FALSE
  )
  check_dims(L_glob_arr, "L_Omega_global_trend",
             c(ndraws, n_sub, n_sub))
  # L_deviation_group: [ndraws, n_groups, n_sub, n_sub]
  check_dims(L_dev_arr, "L_deviation_group_trend",
             c(ndraws, n_groups, n_sub, n_sub))
  # sigma_group: [ndraws, n_groups, n_sub]
  check_dims(sigma_arr, "sigma_group_trend",
             c(ndraws, n_groups, n_sub))

  # Each series's rank within its group (1-based). Vectorized via ave().
  within_pos <- as.integer(
    ave(seq_along(group_inds), group_inds, FUN = seq_along)
  )

  # Pre-compute series indices grouped by group_id
  series_by_group <- split(seq_len(n_series), group_inds)

  # Pre-compute (start, end) column ranges per series in z / result
  series_col_start <- (seq_len(n_series) - 1L) * n_times + 1L

  result <- matrix(0, ndraws, n_times * n_series)

  for (d in seq_len(ndraws)) {
    # drop = FALSE not needed for 3D->2D slice; R returns a matrix.
    L_glob_d <- L_glob_arr[d, , ]
    alpha_d <- alpha[d]

    for (g in seq_len(n_groups)) {
      L_full <- hierarchical_group_cholesky(
        alpha = alpha_d,
        L_global = L_glob_d,
        L_deviation = L_dev_arr[d, g, , ],
        sigma = sigma_arr[d, g, ]
      )

      series_g <- series_by_group[[g]]

      # Stack z columns for these series into [n_times, n_sub] in
      # within-group order. z layout: cols (s-1)*n_times + 1:n_times.
      z_g <- matrix(0, n_times, n_sub)
      for (k in seq_along(series_g)) {
        s <- series_g[k]
        col_range <- series_col_start[s] + (0:(n_times - 1L))
        z_g[, within_pos[s]] <- z[d, col_range]
      }

      # Transform: x = z %*% t(L) gives MVN with cov L %*% t(L)
      innov_g <- z_g %*% t(L_full)

      # Place innovations back into result
      for (k in seq_along(series_g)) {
        s <- series_g[k]
        col_range <- series_col_start[s] + (0:(n_times - 1L))
        result[d, col_range] <- innov_g[, within_pos[s]]
      }
    }
  }

  result
}


# Draw standardised trend innovations for forecasting.
#
# Mirrors the Stan sampling statement built by
# `innovation_sampling_code()`. Draws are at the identity scale and the
# caller applies the covariance transform, exactly as the Stan
# transformed-parameters block does, so the two sides cannot drift
# apart in the distribution they assume.
#
# A multivariate t is drawn as a scale mixture with one mixing value
# per row, shared across series. Sharing the scale makes large
# innovations tend to occur together in time; it does not make the
# series move together, since each still draws its own standard normal
# and so keeps its own direction and magnitude.
#
# `df` here is always `Inf` or a finite value: when the degrees of
# freedom are estimated the caller passes the posterior draw, never NA.
#'@noRd
draw_trend_innovations <- function(n_draws, n_series, df = Inf) {
  checkmate::assert_int(n_draws, lower = 0L)
  checkmate::assert_int(n_series, lower = 1L)
  z <- matrix(stats::rnorm(n_draws * n_series), n_draws, n_series)
  if (is_gaussian_df(df)) {
    return(z)
  }
  # Repeated from `assert_trend_df()` on purpose. That runs once at the
  # constructor; this runs on every forecast draw, where the value
  # arrives from a posterior column rather than from the user, so a
  # corrupted or out-of-range draw is caught before it silently
  # produces innovations with no finite variance.
  checkmate::assert_number(df, finite = TRUE)
  if (df <= 2) {
    stop(insight::format_error(
      "Innovation 'df' must be greater than 2 to have finite variance."
    ))
  }
  u <- 1 / stats::rgamma(n_draws, shape = df / 2, rate = df / 2)
  z * sqrt(u)
}


#' Variance of the latent state a marginal prediction integrates over
#'
#' A marginal prediction answers for a latent state drawn from its own
#' distribution, not from a single innovation. A stationary
#' autoregression settles wider than the innovations driving it: an
#' AR(1) at `sigma^2 / (1 - ar^2)`, which is the very scaling the Stan
#' model uses to draw its first state. Sampling the innovation
#' covariance instead leaves every marginal prediction too narrow and,
#' through a non-identity link, its mean biased with it.
#'
#' Three assumptions are stated rather than solved. A random walk has
#' no stationary distribution, a `ZMVN()` trend has no dynamics to
#' settle into, and a `CAR()` trend decays by `ar^gap`, so under the
#' irregular gaps it exists for there is no single stationary
#' variance. All three keep the innovation covariance. So does any
#' draw whose autoregression is jointly explosive, which `p > 1`
#' allows even though each coefficient is bounded to the unit
#' interval.
#'
#' @param object A fitted `mvgam` object
#' @param draws_mat Posterior draws, already subset to the draws in play
#' @param n_series Number of series or latent factors
#' @return A `[ndraws x n_series]` matrix of variance multipliers, or
#'   `NULL` when the trend keeps its innovation covariance
#'
#' @noRd
ar_stationary_multiplier <- function(object, draws_mat, n_series) {
  spec <- trend_spec_for_residcor(object)
  lags <- resolve_active_lags(spec$p)
  if (length(lags) == 0L) {
    return(NULL)
  }
  phi <- lapply(lags, function(l) {
    read_draws_vector(draws_mat, paste0("ar", l, "_trend"), n_series,
                      required = FALSE)
  })
  if (any(vapply(phi, is.null, logical(1L)))) {
    return(NULL)
  }
  mult <- if (identical(as.integer(lags), 1L)) {
    denom <- 1 - phi[[1L]]^2
    ifelse(denom > .Machine$double.eps, 1 / denom, 1)
  } else {
    ar_companion_multiplier(phi, as.integer(lags))
  }
  # An `ma` term filters the innovations before the recursion sees
  # them, widening what the autoregression then settles around.
  theta <- if (isTRUE(spec$ma)) {
    read_draws_vector(draws_mat, "theta1_trend", n_series,
                      required = FALSE)
  } else {
    NULL
  }
  if (!is.null(theta)) {
    d_ma <- 1 - theta^2
    mult <- mult * ifelse(d_ma > .Machine$double.eps, 1 / d_ma, 1)
  }
  mult
}


#' Stationary variance of a scalar autoregression of order above one
#'
#' Solves the state's own Lyapunov equation on the companion form,
#' which covers a sparse lag set such as `p = c(1, 12)` without
#' special casing: the lags the user did not ask for simply carry a
#' zero coefficient. A draw the doubling solver cannot settle is
#' explosive, and keeps its innovation variance.
#'
#' @noRd
ar_companion_multiplier <- function(phi, lags) {
  ndraws <- nrow(phi[[1L]])
  n_series <- ncol(phi[[1L]])
  max_lag <- max(lags)
  out <- matrix(1, ndraws, n_series)
  innov <- matrix(0, max_lag, max_lag)
  innov[1L, 1L] <- 1
  sub_rows <- if (max_lag > 1L) seq.int(2L, max_lag) else integer(0)
  for (d in seq_len(ndraws)) {
    for (s in seq_len(n_series)) {
      companion <- matrix(0, max_lag, max_lag)
      for (li in seq_along(lags)) {
        companion[1L, lags[li]] <- phi[[li]][d, s]
      }
      if (length(sub_rows) > 0L) {
        companion[cbind(sub_rows, sub_rows - 1L)] <- 1
      }
      v <- solve_dlyap(companion, innov)[1L, 1L]
      if (is.finite(v) && v > 0 && v < 1e8) {
        out[d, s] <- v
      }
    }
  }
  out
}


#' Rescale a trend's innovation covariance to its stationary spread
#'
#' Applied once, on the parameters every innovation transform reads,
#' so the diagonal, correlated, grouped and factor paths all inherit
#' it without restating the rule.
#'
#' A `VAR()` fit needs no rescaling here: its Stan model already
#' carries `Omega_trend`, the stationary joint variance of the
#' companion state, and the leading block of that is what a marginal
#' prediction integrates over.
#'
#' The multiplier scales each series' own variance exactly. Any
#' correlation between series rides through unchanged, which is exact
#' when they share an autoregressive coefficient and an approximation
#' when they do not, since a stationary cross-covariance carries
#' `1 / (1 - ar_i * ar_j)` rather than the geometric mean of the two
#' series' own factors.
#'
#' @noRd
rescale_params_to_stationary <- function(params, object, draws_mat,
                                          group_info = NULL) {
  # A multivariate fit names its trend type after the response, and a
  # shared trend still answers for every one of them, so the name is
  # dropped before the kernel is identified.
  trend_type <- unname(as.character(get_trend_type(object)))[1L]
  if (identical(trend_type, "VAR")) {
    # Sized from the covariance being replaced, since a shared or
    # factor trend carries fewer latent series than the fit has
    # responses.
    k <- dim(params$Sigma_trend)[2L]
    omega <- if (is.null(k)) {
      NULL
    } else {
      read_draws_matrix(draws_mat, "Omega_trend", k, k, required = FALSE)
    }
    if (!is.null(omega)) {
      params$Sigma_trend <- omega
    }
    return(params)
  }
  if (!identical(trend_type, "AR")) {
    return(params)
  }
  # The multiplier is sized from the scales it multiplies rather than
  # from the fit's series count: a trend shared across responses, a
  # factor trend and a `trend_map` fit all carry fewer latent series
  # than the observation model has, and reading the wrong width would
  # silently leave the covariance unscaled.
  if (!is.null(params$sigma_trend)) {
    n <- ncol(params$sigma_trend)
    # Correlated series settle at `Sigma[i, j] / (1 - ar_i * ar_j)`,
    # which is not the geometric mean of each series' own factor. The
    # gap grows with the spread of the coefficients, reaching a fifth
    # of the cross-covariance on a fitted pair, so it is computed
    # rather than approximated wherever the lag-one coefficients are
    # the whole autoregression.
    phi <- ar_lag_one_draws(object, draws_mat, n)
    if (!is.null(params$L_Omega_trend) && !is.null(phi)) {
      return(stationary_correlated_params(params, phi))
    }
    mult <- ar_stationary_multiplier(object, draws_mat, n)
    if (!is.null(mult) && identical(dim(mult), dim(params$sigma_trend))) {
      params$sigma_trend <- params$sigma_trend * sqrt(mult)
    }
    return(params)
  }
  sg <- params[[HIER_COV_PARS$sigma]]
  if (is.null(sg) || length(dim(sg)) != 3L) {
    return(params)
  }
  n_groups <- dim(sg)[2L]
  n_sub <- dim(sg)[3L]
  mult <- ar_stationary_multiplier(object, draws_mat, n_groups * n_sub)
  if (is.null(mult) || is.null(group_info$group_inds)) {
    return(params)
  }
  scale <- sqrt(mult)
  # Which series a group's scale belongs to is read from the fit's own
  # `group_inds_trend`, the same mapping the innovation transform
  # aligns on. Assuming the series run group-major would agree with it
  # only when the groups happen to be contiguous.
  group_inds <- as.integer(group_info$group_inds)
  within_pos <- as.integer(
    stats::ave(seq_along(group_inds), group_inds, FUN = seq_along)
  )
  for (sr in seq_along(group_inds)) {
    if (sr <= ncol(scale)) {
      sg[, group_inds[sr], within_pos[sr]] <-
        sg[, group_inds[sr], within_pos[sr]] * scale[, sr]
    }
  }
  params[[HIER_COV_PARS$sigma]] <- sg
  params
}


#' Lag-one coefficients when they are the whole autoregression
#'
#' Returns `NULL` for a higher-order or sparse lag set, where the
#' stationary cross-covariance no longer reduces to a lag-one form.
#'
#' @noRd
ar_lag_one_draws <- function(object, draws_mat, n_series) {
  spec <- trend_spec_for_residcor(object)
  lags <- resolve_active_lags(spec$p)
  if (!identical(as.integer(lags), 1L) || isTRUE(spec$ma)) {
    return(NULL)
  }
  read_draws_vector(draws_mat, "ar1_trend", n_series, required = FALSE)
}


#' Stationary covariance of correlated AR(1) series
#'
#' `Gamma0[i, j] = Sigma[i, j] / (1 - ar_i * ar_j)`, returned in the
#' scale-and-correlation form the innovation transform reads. A draw
#' whose result is not a covariance keeps its innovations.
#'
#' @noRd
stationary_correlated_params <- function(params, phi) {
  sigma <- params$sigma_trend
  L <- params$L_Omega_trend
  ndraws <- nrow(sigma)
  n <- ncol(sigma)
  if (n == 1L) {
    # One series has no cross-covariance to get right, so it takes the
    # scalar factor without a decomposition per draw.
    denom <- 1 - phi[, 1L]^2
    keep <- denom > .Machine$double.eps
    sigma[keep, 1L] <- sigma[keep, 1L] / sqrt(denom[keep])
    params$sigma_trend <- sigma
    return(params)
  }
  for (d in seq_len(ndraws)) {
    m <- 1 / (1 - outer(phi[d, ], phi[d, ]))
    if (any(!is.finite(m)) || any(diag(m) <= 0)) {
      next
    }
    Ld <- matrix(L[d, , ], n, n)
    omega <- tcrossprod(Ld) * m
    root <- sqrt(diag(m))
    omega <- omega / outer(root, root)
    diag(omega) <- 1
    # A draw can leave `omega` a hair outside the positive-definite
    # cone through rounding, and it is one draw of many rather than a
    # fault to report. It keeps its innovations and the rest proceed.
    chol_omega <- tryCatch(t(chol(omega)), error = function(e) NULL)
    if (is.null(chol_omega)) {
      next
    }
    sigma[d, ] <- sigma[d, ] * root
    L[d, , ] <- chol_omega
  }
  params$sigma_trend <- sigma
  params$L_Omega_trend <- L
  params
}
