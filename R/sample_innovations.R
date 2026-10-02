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
#' Each trend declares its pattern in the trend registry.
#'
#' @name sample_innovations
#' @keywords internal
NULL


#' Get Covariance Pattern for Trend Type
#'
#' Each trend registers how its innovations are parameterised in
#' `<name>_trend_properties()`. A fit with no trend, which
#' `get_trend_type()` reports as `"None"`, has no innovations.
#'
#' @param trend_type A registered trend type name, or `"None"`
#'
#' @return Character string: one of "none", "diagonal",
#'   "cholesky_scaled", or "full_covariance"
#'
#' @noRd
get_covariance_pattern <- function(trend_type) {
  trend_property(trend_type, "covariance_pattern")
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
    time_var = axis_vars(object)$time_var,
    series_var = axis_vars(object)$series_var,
    response_vars = response_columns(object),
    metadata = meta
  )
}


#' The series label each row of a frame carries
#'
#' The same identity `fitted_series_levels()` names, read per
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


#' The series a model was fitted on
#'
#' In the order `trend[t, s]` numbers its columns, from the axis record.
#' A hierarchical fit derives its series from `gr` and `subgr`, and
#' sorting the column it superseded gives a permutation of that order.
#' Every label stays valid under a permutation. Each series would then
#' take another series' state and nothing would fail.
#'
#' @param object A fitted `mvgam` object or prefit
#' @return Character vector, or `NULL` for a model whose frame names no
#'   axis
#' @noRd
fitted_series_levels <- function(object) {
  levels <- mvgam_axes(object)$series$levels
  if (!is.null(levels)) as.character(levels)
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
#' @param resp The response this structure is for. Required on a fit
#'   whose series are its responses, where it is what names the trend
#'   column a row reads; ignored otherwise.
#'
#' @return List with components:
#'   \itemize{
#'     \item \code{time}: Integer vector of time indices (1-based)
#'     \item \code{series}: Factor/character vector of series identifiers
#'     \item \code{series_int}: Integer vector, the column of
#'       `trend[t, s]` each row reads (1-based)
#'     \item \code{n_obs}: Number of observations
#'     \item \code{n_times}: Number of unique time points
#'     \item \code{n_series}: Number of unique series
#'     \item \code{unique_times}: Sorted unique time indices
#'   }
#'
#' @noRd
get_observation_structure <- function(object, newdata = NULL,
                                      resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_string(resp, null.ok = TRUE)

  # Use training data if newdata not provided
  if (is.null(newdata)) {
    newdata <- object$obs_data
    if (is.null(newdata)) {
      newdata <- object$data
    }
  }

  checkmate::assert_data_frame(newdata, min.rows = 1)

  vars <- axis_vars(object)
  time_var <- vars$time_var
  series_var <- vars$series_var

  # The trained model already knows its series structure
  # (object$standata$N_series_trend). For single-series models the
  # newdata typically has no `series` column at all (e.g. brms drops
  # columns the formula doesn't reference). Treat single-series as a
  # first-class shortcut here: build the observation structure
  # directly from object metadata + newdata's time column, without
  # invoking ensure_mvgam_variables.
  n_series_trained <- object$standata$N_series_trend %||%
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
  # column (mvbind builds it implicitly), and the preparation
  # rebuilds the multivariate series from the fit's own responses.
  data_prepared <- prepare_mvgam_frame(object, newdata)

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
  series_levels <- fitted_series_levels(object) %||%
    observed_levels(series_indices)
  refuse_unseen_levels("Series", unique(as.character(series_indices)),
                       series_levels)
  series_int <- match(as.character(series_indices), series_levels)

  # Every reader of this structure indexes the trend, or the
  # innovations drawn for it, with `series_int`, so the rule for a
  # wide frame is applied once here. Left to each reader, three of
  # four forgot it and read the first response's column for all of
  # them.
  series_int <- trend_series_index(object, series_int, resp)
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
  assert_axis_column(newdata, time_var, "time")
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
#' @param object An `mvgam` object or prefit
#' @return The base trend type (e.g. "AR", "RW", "VAR"), or "None"
#'   for a model without a trend
#' @noRd
get_trend_type <- function(object) {
  object$trend_metadata$trend_type %||% "None"
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
                                   draw_ids = NULL, resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_character(resp, min.len = 1L, any.missing = FALSE,
                              null.ok = TRUE)
  validate_draw_selectors(ndraws, draw_ids)

  if (!has_stochastic_trend(object)) {
    n_rows <- if (!is.null(draw_ids)) {
      length(draw_ids)
    } else if (!is.null(ndraws)) {
      ndraws
    } else {
      1L
    }
    n_obs <- NROW(newdata %||% mvgam_training_data(object))
    return(matrix(0, n_rows, n_obs))
  }

  cov_structure <- get_trend_covariance_structure(
    object, ndraws = ndraws, draw_ids = draw_ids
  )
  # One structure per response the noise is read for. Every response
  # reads the same joint draw, so on a wide frame they keep the
  # correlation the trend was fitted with; each reads its own column,
  # so each keeps its own scale.
  structures <- lapply(resp %||% list(NULL), function(r) {
    get_observation_structure(object, newdata, resp = r)
  })
  grid <- draw_innovation_grid(cov_structure, structures[[1L]]$n_times)
  innov <- lapply(structures, function(os) {
    m <- map_innovations_to_obs(grid$innovations, os$n_times,
                                grid$n_series, os)
    # A plain numeric matrix, as brms's posterior_* methods return;
    # marginaleffects type-checks its draws against `matrixOrNULL`.
    dn <- dim(m)
    m <- as.numeric(m)
    dim(m) <- dn
    m
  })
  if (length(resp) > 1L) stats::setNames(innov, resp) else innov[[1L]]
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
    simple = c("sigma_trend")
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
#'     \item \code{has_correlations}: Whether \code{cor} resolved to
#'       TRUE on the trend spec. A grouping resolves it to TRUE.
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
      "A trend covariance requires a fit with a latent trend.",
      i = paste0("Declare a trend through 'trend_formula', e.g. ",
                 "'trend_formula = ~ AR(cor = TRUE)'.")
    )), call. = FALSE)
  }

  trend_type <- get_trend_type(object)
  pattern <- get_covariance_pattern(trend_type)
  # `create_mvgam_trend()` resolves `cor` to TRUE for a grouped trend,
  # matching the program, which declares the group correlation
  # parameters whenever `gr` is named.
  spec <- first_trend_spec(object)
  has_correlations <- isTRUE(spec$cor)

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

  # `detect_factor_n_lv` returns n_lv (or NULL). A named `gr` marks a
  # hierarchical (grouped) trend.
  n_lv <- detect_factor_n_lv(object)
  is_lv <- !is.null(n_lv)
  hierarchical <- named_var(spec$gr)
  n_obs_series <- mvgam_axes(object)$series$n
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
  # Multi-index params (Cholesky factors, full covariance) require
  # explicit named-column lookup; extract_named_params()
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
    # Carried here so `draw_innovation_grid()` draws from the same
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
  validate_draw_selectors(ndraws, draw_ids)

  # A prefit is the user-facing shape of this condition and carries
  # the message every other method gives it. The slot test stays for
  # the classed stubs the tests build, which a user cannot construct
  # and which that message would describe wrongly.
  require_fitted_model(object, "residual_cor")
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
        "Requested more draws than the posterior has.",
        x = paste0("Asked for ", ndraws, " from a fit with ", total_draws,
                   ".")
      )), call. = FALSE)
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

  stop_missing_draws(param_name, c(param_name, paste0(param_name, "[...]")))
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
#'   `"Phi_group_trend"`).
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
  lead <- if (length(prefix_ids)) {
    paste(as.integer(prefix_ids), collapse = ",")
  }
  out <- read_draws_matrix(draws_mat, name, nrow, ncol, lead = lead,
                           needed_for = required_for)
  if (!is.null(transform)) {
    for (d in seq_len(base::nrow(draws_mat))) {
      out[d, , ] <- transform(matrix(out[d, , ], nrow, ncol))
    }
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
    L_Omega_trend = if (stores_innovation_cov(draws_mat)) {
      extract_indexed_array_2d(
        draws_mat, "Sigma_trend", n_series, n_series,
        required_for = "Sigma_trend (innovation covariance)",
        transform = innovation_cor_factor
      )
    } else {
      read_draws_matrix(draws_mat, "L_Omega_trend", n_series, n_series)
    }
  )
}


#' Whether the draws hold the innovation covariance `Sigma_trend`
#'
#' `Sigma_trend` is the covariance of the innovations over one step.
#' `CAR()` places `L_Omega_trend` on the correlation of the shocks over
#' an instant, and its `Sigma_trend` is then a different matrix from
#' `sigma_trend` and `L_Omega_trend` multiplied out. Every post-fit
#' path takes the innovation correlation from `Sigma_trend` where the
#' fit stores it.
#'
#' @param draws Draws with one named column per parameter element
#' @return `TRUE` or `FALSE`
#' @noRd
stores_innovation_cov <- function(draws) {
  "Sigma_trend[1,1]" %in% colnames(draws)
}


#' Lower Cholesky factor of a covariance matrix's correlation
#' @noRd
innovation_cor_factor <- function(Sigma) {
  t(chol(stats::cov2cor(Sigma)))
}


#' Extract Simple Full Covariance Parameters as Structured Arrays
#'
#' A \[ndraws, n, n\] array of `Sigma_trend`, located by name.
#'
#' @noRd
extract_simple_full_cov_params <- function(draws_mat, n_series) {
  checkmate::assert_matrix(draws_mat, min.rows = 1, min.cols = 1)
  checkmate::assert_int(n_series, lower = 1)
  list(Sigma_trend = read_draws_matrix(draws_mat, "Sigma_trend",
                                       n_series, n_series))
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


#' @noRd
get_group_info <- function(standata) {
  group_inds <- as.integer(standata$group_inds_trend)
  list(
    n_groups = standata$N_groups_trend,
    n_subgroups = standata$N_subgroups_trend,
    group_inds = group_inds,
    members = group_members(group_inds, standata$N_subgroups_trend)
  )
}


#' The member series of each group of a grouped trend
#'
#' Ascending series order within each group. The Stan data
#' `group_members_trend` is built from this, and every R path that
#' takes a group's series takes them from it. A group's coefficients,
#' variates and covariance block then fall on the same series on both
#' sides.
#'
#' @param group_inds Group index of each series
#' @param n_subgroups Series per group. The Stan program sizes every
#'   group's arrays by it.
#' @return A list with one integer vector per group
#'
#' @noRd
group_members <- function(group_inds, n_subgroups) {
  checkmate::assert_integerish(group_inds, lower = 1, any.missing = FALSE,
                               min.len = 1L)
  checkmate::assert_int(n_subgroups, lower = 1L)
  members <- lapply(seq_len(max(group_inds)),
                    function(g) which(group_inds == g))
  sizes <- lengths(members)
  if (any(sizes != n_subgroups)) {
    stop(insight::format_error(c(
      "Every group of a grouped trend needs the same number of series.",
      x = paste0("Group sizes are ", paste(sizes, collapse = ", "),
                 " and each group needs ", n_subgroups, ".")
    )), call. = FALSE)
  }
  members
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


#' One joint draw of a trend's innovations over its whole grid
#'
#' The innovations every series receives at a time are drawn together,
#' from the covariance the trend was fitted with, so a correlated or
#' factor trend moves its series jointly. Drawing the grid is kept
#' apart from reading it row by row because one draw can be read more
#' than once: the responses of a wide frame are its series, and each
#' reads its own column of the same draw. Drawing once per response
#' instead would lose the correlation between them, and reading one
#' column for all of them gave every response the first one's noise.
#'
#' @param cov_structure List from `get_trend_covariance_structure()`.
#' @param n_times Number of occasions the grid spans.
#' @return List with `innovations`, a `[ndraws x (n_times * n_series)]`
#'   matrix laid out time-fastest within each series block, and
#'   `n_series`, the series count that layout uses.
#' @noRd
draw_innovation_grid <- function(cov_structure, n_times) {
  checkmate::assert_list(cov_structure, min.len = 1)
  checkmate::assert_names(
    names(cov_structure),
    must.include = c("pattern", "ndraws", "n_series", "params")
  )
  pattern <- cov_structure$pattern
  ndraws <- cov_structure$ndraws

  checkmate::assert_string(pattern)
  checkmate::assert_int(ndraws, lower = 0)
  checkmate::assert_int(n_times, lower = 1)

  # Zeros for deterministic trends (pattern = "none") or when no draws
  # were requested (ndraws = 0, which can occur during validation).
  if (pattern == "none" || ndraws == 0L) {
    effective_ndraws <- if (ndraws == 0L) 1L else ndraws
    # The width a drawn grid has once it reaches the series, which on a
    # factor trend is past the loadings.
    n_series <- as.integer(if (isTRUE(cov_structure$is_lv)) {
      cov_structure$n_obs_series
    } else {
      cov_structure$n_series
    })
    return(list(
      innovations = matrix(0, effective_ndraws, n_times * n_series),
      n_series = n_series
    ))
  }

  n_series <- cov_structure$n_series
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

  list(innovations = innovations_flat, n_series = n_series)
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

# Returns the regex matching parameter-draws the summary / tidy
# classifiers hide, each a raw form of a quantity the posterior
# reports under another name. Covers:
#   * Raw loadings `Z[i, j]` when `Z_tilde[i, j]` is present
#     (free-Z factor fits with the Heaps & Jermyn QR rotation).
#   * Raw factor paths `lv_trend[t, k]` and the upstream
#     `innovations_trend[t, k]` / `scaled_innovations_trend[t, k]`
#     when `lv_trend_tilde[t, k]` is present (same condition).
#   * Unrotated VAR dynamics `Phi_trend[lag][i, j]` when
#     `Phi_trend_tilde[lag][i, j]` is present.
#   * Per-series copies of a shared AR coefficient when the
#     sampled `shared_ar{k}_trend[.]` scalar is present.
# The rotation- and sign-indeterminate families show poor Rhat /
# low ESS while the identified counterpart is well behaved, and
# the shared copies repeat one sampled value. Hiding the raw form
# here keeps convergence diagnostics, `summary.mvgam()` print,
# `posterior_summary.mvgam()` and the variable-keyword machinery
# on the reported names. Returns NULL when the pattern set is
# empty.
#'@noRd
hidden_par_pattern <- function(pars) {
  patterns <- character(0L)
  if (any(grepl("^Phi_trend_tilde\\[", pars))) {
    patterns <- c(patterns, "^Phi_trend\\[")
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
  # `coef_sharing = "shared"` samples one coefficient per lag and
  # broadcasts it across the series. The per-series copies repeat the
  # sampled scalar's value, and reporting all of them presents one
  # quantity as several. The sampled name stays, and the copies are
  # reachable by name.
  shared <- grep("^shared_ar[0-9]+(_pacf)?_trend\\[", pars, value = TRUE)
  if (length(shared) > 0L) {
    stems <- unique(sub("^shared_(.*)\\[.*$", "\\1", shared))
    patterns <- c(patterns, paste0("^", stems, "\\["))
  }
  if (length(patterns) == 0L) return(NULL)
  paste(patterns, collapse = "|")
}

# Which of `pars` the default parameter view hides, as a logical
# mask. Callers need two shapes: a mask to combine with other
# conditions, and a filtered vector. The regex from
# `hidden_par_pattern()` is applied here once, and
# `filter_hidden_pars()` is defined in terms of it. A mask
# rebuilt inline at each call site drifts: one such copy covered
# a single parameter bucket, which let `L_Omega_trend` reach
# `tidy()` while every other accessor hid it.
#'@noRd
is_hidden_par <- function(pars) {
  hide_pat <- hidden_par_pattern(pars)
  if (is.null(hide_pat)) return(rep(FALSE, length(pars)))
  grepl(hide_pat, pars)
}

# Return the input vector with the names `is_hidden_par()` marks
# dropped. Used by `variables.mvgam()`, `extract_mvgam_draws()`
# and `summary.mvgam()`.
#'@noRd
filter_hidden_pars <- function(pars) {
  pars[!is_hidden_par(pars)]
}

# Internal: extract factor-loading draws from the posterior.
# Returns array [ndraws, n_obs_series, n_lv] sorted by series
# index (outer) then by lv index (inner), matching Stan's
# column-major storage convention. Selects `Z_tilde` or `Z` via
# `factor_loading_param_pattern()`. `resolve_factor_loadings()`
# also returns the loadings of a fully fixed trend_map fit, whose Z
# is data.
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
    stop_mvgam_fault(
      paste0("Expected ", expected_cols, " ", param_name,
             " loading columns, found ", length(cols), "."),
      paste0("A factor model stores ", param_name, "[s, lv] for s in 1..",
             n_obs_series, " and lv in 1..", n_lv, ".")
    )
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
  expected <- c(ndraws, n_series, n_series)
  if (!identical(dim(L_omega_arr), as.integer(expected))) {
    stop_shape_fault("'L_Omega_trend'", dim(L_omega_arr), expected)
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
  expected <- c(ndraws, n_series, n_series)
  if (!identical(dim(Sigma_arr), as.integer(expected))) {
    stop_shape_fault("'Sigma_trend'", dim(Sigma_arr), expected)
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

  if (anyNA(time_idx)) {
    stop_mvgam_fault(
      "Every observation time must be one of the grid's times.",
      paste0(sum(is.na(time_idx)), " observation times matched none.")
    )
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
#' @param rows The rows to name, all of them by default. A caller
#'   taking the last few occasions of a long matrix names those alone.
#' @return Character vector of length `length(rows) * n_col`
#'
#' @noRd
stan_matrix_names <- function(prefix, n_row, n_col, lead = NULL,
                              rows = seq_len(n_row)) {
  head <- if (is.null(lead)) "" else paste0(lead, ",")
  paste0(prefix, "[", head,
         rep(rows, times = n_col), ",",
         rep(seq_len(n_col), each = length(rows)), "]")
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


#' Draws at a set of Stan names, the core both readers below share
#'
#' One `match()` both locates the columns and finds any the posterior
#' lacks. A missing name is refused with the parameter it belongs to,
#' or returns `NULL` for a parameter only some trend kernels carry.
#'
#' @param draws_mat A draws matrix, one row per draw. A single draw is
#'   a one-row matrix.
#' @param nms Stan names, from `stan_vector_names()` or
#'   `stan_matrix_names()`
#' @param prefix The parameter the names belong to, for the refusal
#' @param required Whether a missing name is an error. `FALSE` returns
#'   `NULL`.
#' @param needed_for What the parameter is needed for, named in the
#'   refusal, or `NULL`
#' @return A numeric vector running down `nms` within each draw, draws
#'   varying fastest, or `NULL`
#'
#' @noRd
draws_columns <- function(draws_mat, nms, prefix, required = TRUE,
                          needed_for = NULL) {
  idx <- match(nms, colnames(draws_mat))
  if (anyNA(idx)) {
    if (!isTRUE(required)) {
      return(NULL)
    }
    stop(insight::format_error(c(
      paste0("Posterior parameter '", prefix, "' is incomplete."),
      x = paste0("Missing: ", paste(utils::head(nms[is.na(idx)], 3L),
                                    collapse = ", "), "."),
      i = if (is.null(needed_for)) {
        "Refit keeping it in the saved draws."
      } else {
        paste0("Required for ", needed_for, ".")
      }
    )), call. = FALSE)
  }
  as.numeric(draws_mat[, idx, drop = FALSE])
}


#' Draws of a vector parameter
#'
#' Pairs the naming convention with the lookup. A caller names the
#' parameter once, and this builds its Stan names.
#'
#' @inheritParams draws_columns
#' @param prefix Parameter name
#' @param n Vector length
#' @param lead Leading index for a per-group vector, or `NULL`
#' @return `[ndraws x n]` matrix, or `NULL`
#'
#' @noRd
read_draws_vector <- function(draws_mat, prefix, n, lead = NULL,
                              required = TRUE) {
  vals <- draws_columns(draws_mat, stan_vector_names(prefix, n, lead = lead),
                        prefix, required)
  if (is.null(vals)) NULL else matrix(vals, nrow = nrow(draws_mat))
}


#' Draws of a matrix parameter
#'
#' @inheritParams read_draws_vector
#' @param n_row,n_col Matrix dimensions
#' @param rows The rows to return, all of them by default
#' @return `[ndraws x length(rows) x n_col]` array, or `NULL`
#'
#' @noRd
read_draws_matrix <- function(draws_mat, prefix, n_row, n_col,
                              lead = NULL, required = TRUE,
                              rows = seq_len(n_row), needed_for = NULL) {
  nms <- stan_matrix_names(prefix, n_row, n_col, lead = lead, rows = rows)
  vals <- draws_columns(draws_mat, nms, prefix, required, needed_for)
  if (is.null(vals)) {
    return(NULL)
  }
  array(vals, dim = c(nrow(draws_mat), length(rows), n_col))
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


#' The Cholesky factor of one group's trend covariance, for one draw
#'
#' The rescale stores a stationary factor in `L_group_stationary`
#' wherever it reached one. Absent that, the factor recomposes from
#' the population correlation, the group's own deviation and the
#' group's scales. The innovation transform and `residual_cor()` both
#' call this, which states the preference once.
#'
#' @param params Parameter list from `get_trend_covariance_structure()`
#' @param d Draw index
#' @param g Group index
#' @param n_sub Number of subgroups, the block dimension
#' @return Lower-triangular matrix `L` with `L %*% t(L)` the group's
#'   covariance
#'
#' @noRd
group_trend_factor <- function(params, d, g, n_sub) {
  L_stat <- params$L_group_stationary
  if (!is.null(L_stat)) {
    return(matrix(L_stat[d, g, , ], n_sub, n_sub))
  }
  hierarchical_group_cholesky(
    alpha = params[[HIER_COV_PARS$alpha]][d],
    L_global = matrix(params[[HIER_COV_PARS$global]][d, , ],
                      n_sub, n_sub),
    L_deviation = matrix(params[[HIER_COV_PARS$deviation]][d, g, , ],
                         n_sub, n_sub),
    sigma = params[[HIER_COV_PARS$sigma]][d, g, ]
  )
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
    must.include = c("n_groups", "n_subgroups", "group_inds", "members")
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
  # dim() returns integers and the expected shape is compared as integers
  check_dims <- function(arr, name, expected) {
    if (!identical(dim(arr), as.integer(expected))) {
      stop_shape_fault(paste0("'", name, "'"), dim(arr), expected)
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

  # The columns of `z` and of the result holding one group's series,
  # `[n_times, n_sub]` with column k the group's k-th member. Series
  # `s` occupies columns `(s - 1) * n_times + 1:n_times`.
  group_cols <- lapply(group_info$members, function(members) {
    outer(seq_len(n_times), (members - 1L) * n_times, "+")
  })

  result <- matrix(0, ndraws, n_times * n_series)
  for (d in seq_len(ndraws)) {
    for (g in seq_len(n_groups)) {
      cols <- group_cols[[g]]
      # `z L'` has covariance `L L'` in each row.
      result[d, cols] <- matrix(z[d, cols], n_times, n_sub) %*%
        t(group_trend_factor(params, d, g, n_sub))
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


#' Coefficients of a trend's autoregression, one matrix per lag
#'
#' One collection for every consumer of the stationary distribution.
#' The lag set comes from the trend spec, each lag's coefficients from
#' the draws under the name the generator emitted, and a
#' moving-average term from `theta1_trend`. A fit with no
#' autoregression, or missing a coefficient the spec names, returns
#' `NULL`, and its caller keeps the innovation covariance.
#'
#' @param object A fitted `mvgam` object
#' @param draws_mat Posterior draws, already subset to the draws in play
#' @param n_series Number of series or latent factors
#' @return A list with `lags`, `phi` (one `[ndraws, n_series]` matrix
#'   per lag) and `theta`, or `NULL`
#'
#' @noRd
ar_coef_draws <- function(object, draws_mat, n_series) {
  spec <- first_trend_spec(object)
  lags <- as.integer(resolve_active_lags(spec$p))
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
  theta <- if (isTRUE(spec$ma)) {
    read_draws_vector(draws_mat, "theta1_trend", n_series,
                      required = FALSE)
  } else {
    NULL
  }
  list(lags = lags, phi = phi, theta = theta)
}


#' Stationary covariance factors of diagonal-coefficient AR series
#'
#' Each series of an `AR()` trend is a scalar autoregression, with a
#' moving-average term of order one where the trend has one. Its
#' moving-average weights run `psi_0 = 1` and
#' `psi_j = sum_k ar_k psi_{j - lag_k} + theta [j == 1]`, and two series
#' with innovation covariance `Sigma[a, b]` settle at
#' `Sigma[a, b] * F[a, b]` for `F[a, b] = sum_j psi_{a, j} psi_{b, j}`.
#' One sum covers every lag set, contiguous or sparse, with or without
#' a moving-average term, and gives the cross-covariances as exactly as
#' the variances.
#'
#' The weights are advanced for every draw at once, and a draw stops
#' once each of its series has held below `tol` for a full cycle
#' of the largest lag. A weight passing `1 / tol` marks the draw
#' explosive. A draw near a unit root settles slowly, and one still
#' running at `max_terms` takes `companion_factor()`, which needs no
#' truncation. A sparse lag set bounds its coefficients one at a time
#' and admits an explosive draw; the partial autocorrelations of a
#' contiguous set do not.
#'
#' @param ar_coefs The collected coefficients from `ar_coef_draws()`
#' @param cross Whether to sum the cross-covariances. Without them the
#'   off-diagonal cells hold zero.
#' @param tol Weight size below which a series has settled
#' @param max_terms Weights summed before a draw takes the companion
#'   solve
#' @return `[ndraws, n, n]` array of `F`, with `NA` throughout the
#'   draws that have no stationary distribution
#'
#' @noRd
ar_stationary_factor <- function(ar_coefs, cross = TRUE, tol = 1e-10,
                                 max_terms = 2000L) {
  checkmate::assert_list(ar_coefs)
  checkmate::assert_integerish(ar_coefs$lags, lower = 1, min.len = 1,
                               any.missing = FALSE)
  checkmate::assert_list(ar_coefs$phi, types = "matrix",
                         len = length(ar_coefs$lags))
  checkmate::assert_matrix(ar_coefs$theta, null.ok = TRUE)
  checkmate::assert_flag(cross)
  checkmate::assert_number(tol, lower = 0)
  checkmate::assert_int(max_terms, lower = 1L)
  phi <- ar_coefs$phi
  lags <- ar_coefs$lags
  theta <- ar_coefs$theta
  ndraws <- nrow(phi[[1L]])
  n <- ncol(phi[[1L]])
  max_lag <- max(lags)
  # The pairs summed, one column each: the upper triangle, or the
  # diagonal alone when no cross-covariance is asked for.
  pairs <- if (cross) {
    which(upper.tri(diag(n), diag = TRUE), arr.ind = TRUE)
  } else {
    cbind(seq_len(n), seq_len(n))
  }
  pair_a <- pairs[, 1L]
  pair_b <- pairs[, 2L]
  out <- matrix(0, ndraws, length(pair_a))
  bad <- rep(FALSE, ndraws)
  # The working set: `rows` names its draws, `acc` their running sums,
  # `past[[k]]` their `psi_{j - k}`, the most recent first. A settled
  # draw stays in it with its weights zeroed until a fifth of the set
  # has settled, and the set is then compacted.
  rows <- seq_len(ndraws)
  psi <- matrix(1, ndraws, n)
  acc <- out + 1
  past <- rep(list(matrix(0, ndraws, n)), max_lag)
  last_large <- integer(ndraws)
  explosive <- rep(FALSE, ndraws)
  live <- rep(TRUE, ndraws)
  for (j in seq_len(max_terms)) {
    past <- c(list(psi), past[-max_lag])
    psi <- Reduce(`+`, Map(function(ph, ps) ph[rows, , drop = FALSE] * ps,
                           phi, past[lags]))
    if (j == 1L && !is.null(theta)) {
      psi <- psi + theta
    }
    psi[!live, ] <- 0
    acc <- acc + psi[, pair_a, drop = FALSE] * psi[, pair_b, drop = FALSE]
    size <- abs(psi[, 1L])
    for (k in seq_len(n)[-1L]) {
      size <- pmax(size, abs(psi[, k]))
    }
    last_large[size >= tol] <- j
    explosive <- explosive | size > 1 / tol
    live <- live & !explosive & (j - last_large < max_lag)
    if (!any(live)) {
      break
    }
    if (sum(live) <= 0.8 * length(live)) {
      out[rows, ] <- acc
      bad[rows[explosive]] <- TRUE
      rows <- rows[live]
      acc <- acc[live, , drop = FALSE]
      psi <- psi[live, , drop = FALSE]
      past <- lapply(past, function(ps) ps[live, , drop = FALSE])
      last_large <- last_large[live]
      explosive <- explosive[live]
      live <- live[live]
    }
  }
  out[rows, ] <- acc
  bad[rows[explosive]] <- TRUE
  out[bad, ] <- NA_real_
  factor <- array(if (cross) NA_real_ else 0, dim = c(ndraws, n, n))
  for (k in seq_along(pair_a)) {
    factor[, pair_a[k], pair_b[k]] <- out[, k]
    factor[, pair_b[k], pair_a[k]] <- out[, k]
  }
  # Draws still live ran out of terms before settling.
  for (d in rows[live & j == max_terms]) {
    exact <- companion_factor(
      lapply(phi, function(ph) ph[d, ]), lags,
      if (is.null(theta)) NULL else theta[d, ]
    )
    factor[d, , ] <- if (cross) exact else diag(diag(exact), n)
  }
  factor
}


#' Stationary covariance factor of one draw from its companion
#'
#' The joint companion of every series, with unit innovation weight on
#' every pair, settles at `X = sum_k A^k Q A'^k`, whose leading block is
#' `F`. The doubling `X <- X + A X A'`, `A <- A^2` sums `2^k` terms at
#' step `k`. For spectral radius `r` it needs `log2(1 / (1 - r))` steps
#' where the weight sum needs `1 / (1 - r)` terms. Powers of `A` that stop shrinking mark a spectral
#' radius of one or more.
#'
#' @param phi Coefficient vectors, one per lag, one value per series
#' @param lags The lags `phi` belongs to
#' @param theta Moving-average coefficients, or `NULL`
#' @return `n x n` matrix of `F`, or `NA` throughout for a draw with no
#'   stationary distribution
#'
#' @noRd
companion_factor <- function(phi, lags, theta) {
  n <- length(phi[[1L]])
  has_ma <- !is.null(theta)
  width <- max(lags) + has_ma
  side <- width * n
  block <- function(i) (i - 1L) * n + seq_len(n)
  A <- matrix(0, side, side)
  for (k in seq_along(lags)) {
    A[cbind(block(1L), block(lags[k]))] <- phi[[k]]
  }
  if (max(lags) > 1L) {
    shifted <- seq_len((max(lags) - 1L) * n)
    A[cbind(shifted + n, shifted)] <- 1
  }
  noise <- block(1L)
  if (has_ma) {
    A[cbind(block(1L), block(width))] <- theta
    noise <- c(noise, block(width))
  }
  X <- matrix(0, side, side)
  X[noise, noise] <- 1
  # 64 doublings sum 2^64 terms. A draw still unsettled there has a
  # spectral radius of one to working precision.
  for (step in seq_len(64L)) {
    X <- X + A %*% tcrossprod(X, A)
    A <- A %*% A
    size <- max(abs(A))
    if (size < 1e-15) {
      return(X[block(1L), block(1L), drop = FALSE])
    }
    if (!is.finite(size) || size > 1e12) {
      break
    }
  }
  matrix(NA_real_, n, n)
}


#' Say how many draws kept their innovation covariance
#'
#' A draw with no stationary distribution keeps its innovation form in
#' a marginal prediction and in `residual_cor()`. Only a sparse lag set
#' produces one, and the count tells a user how much of the posterior
#' that covers.
#'
#' @param factor The array from `ar_stationary_factor()`
#' @return `NULL`, invisibly
#'
#' @noRd
warn_explosive_draws <- function(factor) {
  checkmate::assert_array(factor, d = 3L)
  n_bad <- sum(is.na(factor[, 1L, 1L]))
  if (n_bad > 0L) {
    warn_once(
      c(
        paste0(n_bad, " of ", dim(factor)[1L], " posterior draws have ",
               "an explosive autoregression."),
        i = paste0("The explosive draws keep their innovation ",
                   "covariance in marginal predictions and ",
                   "'residual_cor()'. A sparse lag set bounds each ",
                   "coefficient on its own and admits them.")
      ),
      "mvgam_explosive_ar_draws"
    )
  }
  invisible(NULL)
}


#' Is this matrix a covariance with a Cholesky factor?
#'
#' Symmetrises the rounding asymmetry of a stored draw, then tests
#' the result for positive definiteness against its own largest
#' eigenvalue. A draw failing the test keeps its innovation form,
#' which is one draw of many.
#'
#' @param x A square numeric matrix
#' @return `x`, symmetrised, or `NULL`
#'
#' @noRd
as_stationary_cov <- function(x) {
  if (any(!is.finite(x))) {
    return(NULL)
  }
  x <- (x + t(x)) / 2
  ev <- eigen(x, symmetric = TRUE, only.values = TRUE)$values
  if (min(ev) <= sqrt(.Machine$double.eps) * max(ev)) {
    return(NULL)
  }
  x
}


#' Rescale a trend's innovation covariance to its stationary spread
#'
#' Applied once, on the parameters every innovation transform uses,
#' which gives the diagonal, correlated, grouped and factor paths the
#' rule without restating it.
#'
#' A `VAR()` fit needs no rescaling here. Its Stan model already
#' supplies `Omega_trend`, the stationary joint variance of the
#' companion state, whose leading block is the covariance a marginal
#' prediction integrates over.
#'
#' An `AR()` trend takes `ar_stationary_factor()`, exact for every lag
#' set in the cross-covariances as well as the variances.
#'
#' @noRd
rescale_params_to_stationary <- function(params, object, draws_mat,
                                          group_info = NULL) {
  checkmate::assert_list(params)
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_matrix(draws_mat)
  checkmate::assert_list(group_info, null.ok = TRUE)
  # A multivariate fit names its trend type after the response, and a
  # shared trend still answers for every one of them, so the name is
  # dropped before the kernel is identified.
  trend_type <- unname(as.character(get_trend_type(object)))[1L]
  # Which kernels settle, and how each one supplies the covariance, is
  # one registry fact.
  stat_source <- trend_stationary_source(trend_type)
  if (identical(stat_source, "none")) {
    return(params)
  }
  if (identical(stat_source, "omega")) {
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
      return(params)
    }
    # A grouped VAR gives the extraction the per-group correlation
    # parameters, which leaves `Sigma_trend` out of `params` and `k`
    # empty above. The program gives its first state `Omega_trend`
    # over every series, and each group's block becomes one factor.
    return(stationary_group_var_params(params, draws_mat, group_info))
  }
  # The factor is sized from the scales it lifts. A trend shared across
  # responses, a factor trend and a `trend_map` fit all carry fewer
  # latent series than the observation model has, and a series count
  # would overstate them.
  sg <- params[[HIER_COV_PARS$sigma]]
  grouped <- is.null(params$sigma_trend)
  n <- if (grouped) dim(sg)[2L] * dim(sg)[3L] else ncol(params$sigma_trend)
  ar_coefs <- ar_coef_draws(object, draws_mat, n)
  if (is.null(ar_coefs)) {
    return(params)
  }
  # Independent series use the variances alone.
  independent <- !grouped && is.null(params$L_Omega_trend)
  factor <- ar_stationary_factor(ar_coefs, cross = !independent)
  warn_explosive_draws(factor)
  if (grouped) {
    return(stationary_group_params(params, factor, group_info))
  }
  if (!independent) {
    return(stationary_correlated_params(params, factor))
  }
  # The diagonal of each draw's factor. A draw with no stationary
  # distribution keeps its innovation scale.
  nd <- dim(factor)[1L]
  cell <- rep(seq_len(n), each = nd)
  mult <- matrix(factor[cbind(rep(seq_len(nd), n), cell, cell)], nd, n)
  mult[is.na(mult)] <- 1
  params$sigma_trend <- params$sigma_trend * sqrt(mult)
  params
}


#' Stationary covariance of correlated AR series
#'
#' `Gamma = Sigma * F` elementwise, returned in the scale-and-correlation
#' form every innovation transform takes. A diagonal scaling commutes
#' with the elementwise product, and the lift takes the correlation
#' factor with the scales multiplied in after. A draw with no
#' stationary covariance, or one rounding leaves outside the
#' positive-definite cone, keeps its innovations.
#'
#' @param params Extracted trend parameters
#' @param factor The array from `ar_stationary_factor()`
#' @return `params`, with `sigma_trend` and `L_Omega_trend` lifted
#'
#' @noRd
stationary_correlated_params <- function(params, factor) {
  checkmate::assert_matrix(params$sigma_trend)
  checkmate::assert_array(params$L_Omega_trend, d = 3L)
  checkmate::assert_array(factor, d = 3L)
  sigma <- params$sigma_trend
  L <- params$L_Omega_trend
  n <- ncol(sigma)
  for (d in seq_len(nrow(sigma))) {
    # `L_Omega_trend` is the correlation factor. A stored draw rounds
    # its rows off unit norm, and normalising by the result's own
    # diagonal absorbs that, which keeps the scale and the correlation
    # reconstructing the covariance they describe.
    gamma_d <- as_stationary_cov(tcrossprod(matrix(L[d, , ], n, n)) *
                                   matrix(factor[d, , ], n, n))
    if (is.null(gamma_d)) {
      next
    }
    root <- sqrt(diag(gamma_d))
    omega <- gamma_d / outer(root, root)
    diag(omega) <- 1
    sigma[d, ] <- sigma[d, ] * root
    L[d, , ] <- t(chol(omega))
  }
  params$sigma_trend <- sigma
  params$L_Omega_trend <- L
  params
}


#' Stationary Cholesky factors of a grouped trend, one per group
#'
#' Groups are independent, which makes each group's block of the
#' stationary covariance its own problem. A draw whose block has no
#' factor keeps its innovation factor.
#'
#' @param params Extracted trend parameters
#' @param group_info The fit's grouping, from `get_group_info()`
#' @param ndraws Number of draws
#' @param block Function of the draw, the group's member series and the
#'   group's innovation factor, returning that group's block
#' @return `params`, with `L_group_stationary` added
#'
#' @noRd
stationary_group_factors <- function(params, group_info, ndraws, block) {
  checkmate::assert_list(params)
  checkmate::assert_list(group_info$members, types = "integerish",
                         min.len = 1L)
  checkmate::assert_int(ndraws, lower = 1L)
  checkmate::assert_function(block, nargs = 3L)
  n_groups <- as.integer(group_info$n_groups)
  n_sub <- as.integer(group_info$n_subgroups)
  L_stat <- array(0, dim = c(ndraws, n_groups, n_sub, n_sub))
  for (d in seq_len(ndraws)) {
    for (g in seq_len(n_groups)) {
      L_g <- group_trend_factor(params, d, g, n_sub)
      gamma_g <- as_stationary_cov(block(d, group_info$members[[g]], L_g))
      L_stat[d, g, , ] <- if (is.null(gamma_g)) L_g else t(chol(gamma_g))
    }
  }
  params$L_group_stationary <- L_stat
  params
}


#' Stationary covariance of a grouped VAR, one factor per group
#'
#' The program solves `Omega = A Omega A' + Sigma` over every series
#' and gives its first latent state that covariance. Each group's block
#' of it becomes that group's factor.
#'
#' @noRd
stationary_group_var_params <- function(params, draws_mat, group_info) {
  checkmate::assert_list(params)
  checkmate::assert_matrix(draws_mat)
  checkmate::assert_list(group_info)
  n_lv <- length(group_info$group_inds)
  omega <- read_draws_matrix(draws_mat, "Omega_trend", n_lv, n_lv,
                             required = FALSE)
  if (is.null(omega)) {
    return(params)
  }
  stationary_group_factors(
    params, group_info, dim(omega)[1L],
    function(d, members, L_g) {
      matrix(omega[d, members, members], length(members), length(members))
    }
  )
}


#' Stationary covariance of a grouped AR, one factor per group
#'
#' `Gamma = Sigma_g * F[members, members]` over the member series of
#' one group.
#'
#' @param params Extracted trend parameters
#' @param factor The array from `ar_stationary_factor()`
#' @param group_info The fit's grouping, from `get_group_info()`
#' @return `params`, with `L_group_stationary` added
#'
#' @noRd
stationary_group_params <- function(params, factor, group_info) {
  checkmate::assert_array(factor, d = 3L)
  stationary_group_factors(
    params, group_info, dim(factor)[1L],
    function(d, members, L_g) {
      n_sub <- length(members)
      tcrossprod(L_g) * matrix(factor[d, members, members], n_sub, n_sub)
    }
  )
}
