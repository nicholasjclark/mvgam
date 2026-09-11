# Data shape helpers for users arriving from other multi-species
# packages. mvgam's closure-unit and JSDM pipelines all consume a
# long-format `data.frame` keyed by `(series, time, ...)`. Other
# packages ship multi-dimensional arrays:
#
#   unmarked / ubms (single-season):
#     y = matrix[J sites, K max visits]                (Occu / PCount)
#     y = list of matrix[J, K], one per species         (OccuMulti)
#     siteCovs = data.frame[J, p]
#     obsCovs  = list of matrix[J, K] (one per visit-varying covariate)
#                OR data.frame[J * K, p] long-format
#
#   unmarked (multi-season, colext / unmarkedMultFrame):
#     y = matrix[J, T*K] (year-major blocks of K visits)
#     yearlySiteCovs = data.frame[J*T, p] or list of matrix[J, T]
#
#   spOccupancy (single-season multi-species):
#     y        = array[N species, J sites, K max visits]
#     occ.covs = data.frame[J, p_occ]
#     det.covs = list of matrix[J, K]
#
#   spOccupancy (multi-season multi-species, tMsPGOcc):
#     y        = array[N, J, T, K]
#     occ.covs = named list of [J] or [J, T] entries
#     det.covs = named list of [J], [J, T], or [J, T, K] entries
#
#   flocker:
#     "single":    obs = [J, K]                 unit_covs df[J]
#     "multi":     obs = [J, K, T_seasons]      unit_covs list of K df[J]
#     "augmented": obs = [L, K, N_species]      unit_covs df[L]
#
#   Hmsc (multi-species single-visit JSDM):
#     Y       = matrix[J sites, N species]
#     XData   = data.frame[J, p]
#     TrData  = data.frame[N, q]   (per-species traits)
#     phyloTree = phylo object
#
# These helpers pivot all these layouts into the long-format
# `data.frame` mvgam expects, broadcasting site / season / species
# covariates across the (species, site, season, visit) grid.

#' Pivot a detection array to mvgam long-format
#'
#' Convert the multi-dimensional detection histories used by
#' `unmarked`, `ubms`, `flocker`, and `spOccupancy` into the
#' long-format closure-unit layout `mvgam(family = occ() / nmix())`
#' and `jsdgam(family = occ() / nmix())` expect. One row per
#' `(species, site, season, visit)` tuple. Single-species data
#' drops the `series` axis; single-season data sets `season = 1`
#' for every row and `time = site`. Multi-season data has two
#' modes selected via `multi_season`:
#'
#' \itemize{
#'   \item `"fused"` (default): `time = (site - 1) * T + season`,
#'     so each (site, season) pair is a unique closure-unit
#'     identifier. Use when seasons are independent given covariates
#'     (single-season analysis applied per season, no cross-season
#'     dynamics modelled).
#'   \item `"hierarchical"`: `time = season`. Pair with
#'     `occ(multi_season = TRUE)` / `nmix(multi_season = TRUE)` and
#'     reference the `site` column in the observation `formula`
#'     (e.g. `s(site, bs = "re")`) so per-site variation enters the
#'     observation model while the trend pipeline operates on the
#'     season axis. See `?occ` for the multi-season scope and
#'     identification caveats (the model parameterizes joint
#'     cross-species occupancy dynamics, NOT explicit colonization
#'     and extinction rates).
#' }
#'
#' @param y One of four layouts:
#'   \itemize{
#'     \item **Single-species, single-season**: a `[J_sites,
#'       K_max_visits]` matrix of detections (0 / 1 for `occ()`,
#'       non-negative counts for `nmix()`). The
#'       `unmarked::unmarkedFrameOccu` / `unmarkedFramePCount` `y`
#'       slot and the `flocker::flock` `obs` argument with
#'       `type = "single"`.
#'     \item **Multi-species, single-season (array)**: a
#'       `[N_species, J_sites, K_max_visits]` array. The
#'       `spOccupancy::lfMsPGOcc` / `msPGOcc` `y` slot.
#'     \item **Multi-species, single-season (list)**: a named list
#'       of `[J_sites, K_max_visits]` matrices, one per species.
#'       The `unmarked::unmarkedFrameOccuMulti` `y` slot; list
#'       names become species labels.
#'     \item **Multi-species, multi-season**: a `[N_species, J_sites,
#'       T_seasons, K_max_visits]` 4D array. The
#'       `spOccupancy::tMsPGOcc` / `svcTMsPGOcc` `y` slot. For
#'       single-species multi-season data, wrap in a length-1 list
#'       (`list(only_species = matrix_or_3d_array)`) or pass as a
#'       `[1, J, T, K]` array.
#'   }
#'   `NA` entries mark missing visits / seasons and are dropped
#'   from the output.
#' @param site_covs Optional `data.frame` of site-level covariates
#'   with `nrow == J_sites`. Broadcast across visits, seasons, and
#'   species. The `unmarked::unmarkedFrameOccu` `siteCovs` slot,
#'   the spOccupancy `occ.covs`, and the `flocker` `unit_covs` map
#'   here.
#' @param season_covs Optional `data.frame` of season-level
#'   covariates with `nrow == T_seasons`. Broadcast across sites,
#'   visits, and species. Use for year-of-study effects,
#'   season-of-year covariates, or any covariate that varies across
#'   primary periods but is constant within a season.
#' @param site_season_covs Optional named list of
#'   `[J_sites, T_seasons]` matrices for covariates that vary
#'   across the `(site, season)` interaction (e.g., site-level
#'   habitat that changes between seasons).
#' @param obs_covs Optional visit-level covariates:
#'   \itemize{
#'     \item Single-season: named list of `[J_sites, K_max_visits]`
#'       matrices, OR a long-format `data.frame` with
#'       `J_sites * K_max_visits` rows in site-major / visit-fastest
#'       order (the `unmarked` `obsCovs` convention).
#'     \item Multi-season: named list of
#'       `[J_sites, T_seasons, K_max_visits]` arrays.
#'   }
#' @param species Optional character vector of species names with
#'   `length == N_species`. Only meaningful for multi-species `y`.
#'   When `y` is a named list, list names take precedence. Defaults
#'   to `sp1, sp2, ...`.
#' @param site_col Name of the site identifier in the output.
#'   Defaults to `"site"`.
#' @param season_col Name of the season identifier in the output.
#'   Defaults to `"season"`.
#' @param visit_col Name of the visit identifier. Defaults to
#'   `"visit"`.
#' @param time_col Name of the closure-unit time identifier.
#'   Defaults to `"time"`. Single-season: `time = site`.
#'   Multi-season + `multi_season = "fused"`:
#'   `time = (site - 1) * T + season` (one closure unit per
#'   (site, season)). Multi-season + `multi_season =
#'   "hierarchical"`: `time = season` (one closure unit per
#'   (species, site, season), with site referenced separately in
#'   the observation `formula`).
#' @param series_col Name of the species column. Defaults to
#'   `"series"`.
#' @param y_col Name of the detection / count column. Defaults to
#'   `"y"`.
#' @param multi_season Character scalar controlling the
#'   multi-season `time` encoding. `"fused"` (default) fuses
#'   `(site, season)` into a single `time` axis so each
#'   `(site, season)` pair is a unique closure unit. This matches
#'   the existing single-season closure-unit grouping
#'   `(series, time)` and works with `occ()` / `nmix()` unchanged.
#'   `"hierarchical"` keeps `time = season` and exposes `site` as
#'   a side-car covariate for use in the observation `formula`; pair
#'   with `occ(multi_season = TRUE)` or `nmix(multi_season = TRUE)`
#'   so the 3-axis closure-unit grouping `(series, site, time)` is
#'   activated. Ignored for single-season inputs (2D matrix, 3D
#'   `[N, J, K]` array, or 4D array with `T = 1`).
#'
#' @return A long-format `data.frame` with columns
#'   `(series, time, site, season, visit, y, <site_covs>,
#'   <season_covs>, <site_season_covs>, <obs_covs>)`. Rows where
#'   `y` is `NA` are dropped so closure units can vary in the
#'   number of observed visits / seasons.
#'
#' @author Nicholas J Clark
#' @export
pivot_detection_array <- function(y,
                                    site_covs = NULL,
                                    season_covs = NULL,
                                    site_season_covs = NULL,
                                    obs_covs = NULL,
                                    species = NULL,
                                    site_col = "site",
                                    season_col = "season",
                                    visit_col = "visit",
                                    time_col = "time",
                                    series_col = "series",
                                    y_col = "y",
                                    multi_season = c("fused",
                                                       "hierarchical")) {
  checkmate::assert_string(site_col)
  checkmate::assert_string(season_col)
  checkmate::assert_string(visit_col)
  checkmate::assert_string(time_col)
  checkmate::assert_string(series_col)
  checkmate::assert_string(y_col)
  multi_season <- match.arg(multi_season)

  # Coerce `y` to a canonical 4D `[N, J, T, K]` array. Single-species
  # and single-season cases slot in via singleton axes that drop /
  # reduce trivially in the output.
  resolved <- resolve_y_layout(y, species)
  y_arr <- resolved$y_arr
  drop_species_axis <- resolved$drop_species_axis
  species <- resolved$species

  N <- dim(y_arr)[1L]
  J <- dim(y_arr)[2L]
  T_ <- dim(y_arr)[3L]
  K <- dim(y_arr)[4L]
  # Local flag for the "is there >1 season in the data?" question.
  # Distinct from the user-facing `multi_season` mode arg.
  is_multi_season <- T_ > 1L

  reserved_cols <- c(series_col, site_col, season_col, visit_col,
                      time_col, y_col)

  # Validate covariate layouts before constructing the long grid.
  if (!is.null(site_covs)) {
    checkmate::assert_data_frame(site_covs, nrows = J)
    assert_no_reserved_cols(colnames(site_covs), reserved_cols,
                              "site_covs")
  }
  if (!is.null(season_covs)) {
    checkmate::assert_data_frame(season_covs, nrows = T_)
    assert_no_reserved_cols(colnames(season_covs), reserved_cols,
                              "season_covs")
  }
  if (!is.null(site_season_covs)) {
    checkmate::assert_list(site_season_covs, names = "unique")
    for (nm in names(site_season_covs)) {
      checkmate::assert_matrix(
        site_season_covs[[nm]], nrows = J, ncols = T_
      )
    }
    assert_no_reserved_cols(names(site_season_covs), reserved_cols,
                              "site_season_covs")
  }
  if (!is.null(obs_covs)) {
    obs_covs <- normalise_obs_covs(obs_covs, J = J, T_ = T_, K = K,
                                     multi_season = is_multi_season)
    assert_no_reserved_cols(names(obs_covs), reserved_cols, "obs_covs")
  }

  # (species, site, season, visit) grid, site-major to match the
  # mvgam canonical `arrange(time, series)` layout.
  grid <- expand.grid(
    visit   = seq_len(K),
    series  = seq_len(N),
    season  = seq_len(T_),
    site    = seq_len(J),
    KEEP.OUT.ATTRS   = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[, c("series", "site", "season", "visit"),
                drop = FALSE]

  out <- data.frame(matrix(NA, nrow = nrow(grid), ncol = 0L))
  out[[series_col]] <- factor(species[grid$series], levels = species)
  out[[site_col]]   <- grid$site
  out[[season_col]] <- grid$season
  out[[visit_col]]  <- grid$visit
  # Two branches. Hierarchical multi-season puts site on its own
  # axis: `time = season`, so the 3-axis closure-unit grouping
  # `(series, site, time)` inside `occ(multi_season = TRUE)` /
  # `nmix(multi_season = TRUE)` reads site separately for the
  # obs-formula. The other branch fuses (site, season) into a
  # single closure-unit time slot via `(site - 1) * T + season`,
  # which automatically reduces to `time = site` when T = 1
  # because season = 1 there. So single-season inputs and
  # fused-multi-season inputs share one branch.
  out[[time_col]] <- if (is_multi_season &&
                          identical(multi_season, "hierarchical")) {
    grid$season
  } else {
    (grid$site - 1L) * T_ + grid$season
  }
  out[[y_col]]      <- y_arr[cbind(grid$series, grid$site,
                                     grid$season, grid$visit)]

  if (!is.null(site_covs)) {
    for (col in colnames(site_covs)) {
      out[[col]] <- site_covs[[col]][grid$site]
    }
  }
  if (!is.null(season_covs)) {
    for (col in colnames(season_covs)) {
      out[[col]] <- season_covs[[col]][grid$season]
    }
  }
  if (!is.null(site_season_covs)) {
    for (nm in names(site_season_covs)) {
      out[[nm]] <- site_season_covs[[nm]][cbind(grid$site, grid$season)]
    }
  }
  if (!is.null(obs_covs)) {
    for (nm in names(obs_covs)) {
      out[[nm]] <- obs_covs[[nm]][cbind(grid$site, grid$season,
                                          grid$visit)]
    }
  }

  out <- out[!is.na(out[[y_col]]), , drop = FALSE]
  rownames(out) <- NULL

  if (drop_species_axis) {
    out[[series_col]] <- NULL
  }
  if (!is_multi_season) {
    out[[season_col]] <- NULL
  }
  out
}


# Resolve any of the four accepted `y` layouts to a canonical
# `[N, J, T, K]` 4D array. Returns the array, the resolved species
# vector, and a flag for whether the species axis should be dropped
# from the output.
#
# @noRd
resolve_y_layout <- function(y, species) {
  drop_species_axis <- FALSE
  resolved_species <- species

  if (is.matrix(y)) {
    # Single-species single-season: [J, K] -> [1, J, 1, K].
    drop_species_axis <- TRUE
    y_arr <- array(y, dim = c(1L, nrow(y), 1L, ncol(y)))
  } else if (is.array(y) && length(dim(y)) == 3L) {
    # Multi-species single-season: [N, J, K] -> [N, J, 1, K].
    d <- dim(y)
    y_arr <- array(y, dim = c(d[1L], d[2L], 1L, d[3L]))
    if (is.null(resolved_species) && !is.null(dimnames(y)[[1L]])) {
      resolved_species <- dimnames(y)[[1L]]
    }
  } else if (is.array(y) && length(dim(y)) == 4L) {
    # Multi-species multi-season: [N, J, T, K] direct.
    y_arr <- y
    if (is.null(resolved_species) && !is.null(dimnames(y)[[1L]])) {
      resolved_species <- dimnames(y)[[1L]]
    }
  } else if (is.list(y) && !is.data.frame(y) &&
              all(vapply(y, is.matrix, logical(1L)))) {
    # `unmarkedFrameOccuMulti`-style named list of `[J, K]` matrices.
    if (length(y) == 0L) {
      stop(insight::format_error(
        "'y' is an empty list; supply at least one species matrix."
      ))
    }
    nm <- names(y)
    if (is.null(nm) || any(!nzchar(nm))) {
      stop(insight::format_error(c(
        "List 'y' must be fully named (one name per species).",
        i = paste0(
          "The `unmarked::unmarkedFrameOccuMulti` convention uses ",
          "list names as species labels."
        )
      )))
    }
    J_first <- nrow(y[[1L]])
    K_first <- ncol(y[[1L]])
    for (i in seq_along(y)) {
      checkmate::assert_matrix(
        y[[i]], nrows = J_first, ncols = K_first
      )
    }
    y_arr <- array(NA, dim = c(length(y), J_first, 1L, K_first))
    for (i in seq_along(y)) {
      y_arr[i, , 1L, ] <- y[[i]]
    }
    if (is.null(resolved_species)) {
      resolved_species <- nm
    }
  } else {
    stop(insight::format_error(c(
      "'y' has an unsupported layout.",
      x = paste0("Got class ", paste(class(y), collapse = "/"),
                  if (is.array(y)) paste0(" with dim ",
                    paste(dim(y), collapse = " x ")) else ""),
      i = paste0(
        "Accepted: '[J, K]' matrix, '[N, J, K]' array, ",
        "'[N, J, T, K]' array or named list of '[J, K]' matrices."
      )
    )))
  }

  N <- dim(y_arr)[1L]
  if (is.null(resolved_species)) {
    resolved_species <- paste0("sp", seq_len(N))
  }
  checkmate::assert_character(
    resolved_species, len = N, any.missing = FALSE, unique = TRUE
  )
  list(
    y_arr = y_arr,
    species = resolved_species,
    drop_species_axis = drop_species_axis
  )
}


# Coerce `obs_covs` (a named list of `[J, K]` matrices, named list
# of `[J, T, K]` arrays, or long-format data.frame with `J*K` rows
# in site-major / visit-fastest order) into a uniform named list of
# `[J, T, K]` arrays so the downstream `cbind()` index lookup is
# layout-agnostic.
#
# @noRd
normalise_obs_covs <- function(obs_covs, J, T_, K, multi_season) {
  if (is.data.frame(obs_covs)) {
    if (multi_season) {
      stop(insight::format_error(c(
        "A data.frame 'obs_covs' holds single-season data only.",
        i = paste0(
          "Multi-season data must pass 'obs_covs' as a named list ",
          "of '[J, T, K]' arrays."
        )
      )))
    }
    expected_rows <- J * K
    checkmate::assert_data_frame(obs_covs, nrows = expected_rows)
    # unmarked stores `obsCovs` site-major + visit-fastest, i.e.
    # `as.vector(t(matrix_M_by_J))`. Reverse: each column becomes a
    # `[J, K]` matrix filled BY ROW. The transposed form has K rows
    # times J cols, then we `t()` to get J rows by K cols.
    out <- list()
    for (col in colnames(obs_covs)) {
      vals <- obs_covs[[col]]
      mat <- matrix(vals, nrow = J, ncol = K, byrow = TRUE)
      arr <- array(NA, dim = c(J, T_, K))
      arr[, 1L, ] <- mat
      out[[col]] <- arr
    }
    return(out)
  }
  checkmate::assert_list(obs_covs, names = "unique")
  out <- list()
  for (nm in names(obs_covs)) {
    x <- obs_covs[[nm]]
    if (multi_season) {
      checkmate::assert_array(x, d = 3L)
      checkmate::assert(
        identical(dim(x), c(J, T_, K)),
        .var.name = paste0("dim(obs_covs[['", nm, "']])")
      )
      out[[nm]] <- x
    } else {
      checkmate::assert_matrix(x, nrows = J, ncols = K)
      arr <- array(NA, dim = c(J, T_, K))
      arr[, 1L, ] <- x
      out[[nm]] <- arr
    }
  }
  out
}


# Internal: error if any name in `nms` collides with the reserved
# output column set. `arg_name` is the user-facing argument name to
# put in the error.
#
# @noRd
assert_no_reserved_cols <- function(nms, reserved, arg_name) {
  hits <- intersect(nms, reserved)
  if (length(hits)) {
    stop(insight::format_error(c(
      paste0("'", arg_name,
              "' must not contain the reserved output columns."),
      x = paste0("Overlapping: ",
                  paste(sprintf("'%s'", hits), collapse = ", "),
                  "."),
      i = paste0(
        "Rename or pass non-default '*_col' arguments to ",
        "`pivot_detection_array()`."
      )
    )))
  }
  invisible(TRUE)
}


#' Pivot a wide species matrix to mvgam long-format
#'
#' Convert the wide `[J sites, N species]` response matrix used by
#' `Hmsc` (and any single-visit JSDM workflow) into the long-format
#' `(series, time, y, ...)` layout that `mvgam()` / `jsdgam()` with
#' a simplex (`diri()`, `multi()`, `categ()`), continuous-response
#' (`mvn()`, `mvt()`), or bernoulli / binomial / poisson family
#' expects.
#'
#' @param Y A `[J_sites, N_species]` matrix or `data.frame`. Column
#'   names become the `series` factor levels; missing column names
#'   default to `sp1, sp2, ...`.
#' @param site_covs Optional `data.frame` of site-level covariates
#'   with `nrow == J_sites`. The `Hmsc` `XData` slot maps here.
#' @param traits Optional `data.frame` of per-species traits with
#'   `nrow == N_species`. Broadcast across sites. The `Hmsc`
#'   `TrData` slot maps here.
#' @param site_id_name Name of the site identifier in the output.
#'   Defaults to `"time"`.
#' @param series_name Name of the species column. Defaults to
#'   `"series"`.
#' @param values_to Name of the response column. Defaults to `"y"`.
#'
#' @return A long-format `data.frame` with `J * N` rows, each
#'   carrying `series`, `time`, `y`, site covariates broadcast
#'   across species, and species traits broadcast across sites.
#'
#' @author Nicholas J Clark
#' @export
pivot_species_matrix <- function(Y,
                                   site_covs = NULL,
                                   traits = NULL,
                                   site_id_name = "time",
                                   series_name = "series",
                                   values_to = "y") {
  checkmate::assert_string(site_id_name)
  checkmate::assert_string(series_name)
  checkmate::assert_string(values_to)
  # Validate input shape BEFORE coercing data.frame -> matrix so a
  # malformed input fails with a clear message instead of being
  # silently widened.
  checkmate::assert(
    checkmate::check_data_frame(Y, min.rows = 1L, min.cols = 1L),
    checkmate::check_matrix(Y, min.rows = 1L, min.cols = 1L)
  )
  if (is.data.frame(Y)) {
    Y <- as.matrix(Y)
  }

  J <- nrow(Y)
  N <- ncol(Y)
  if (!is.null(colnames(Y)) && anyDuplicated(colnames(Y))) {
    stop(insight::format_error(c(
      "Column names of 'Y' must be unique.",
      x = paste0(
        "Duplicated: ",
        paste(unique(colnames(Y)[duplicated(colnames(Y))]),
              collapse = ", "),
        "."
      ),
      i = "Each column of 'Y' is one species; names become factor levels."
    )))
  }
  species <- colnames(Y) %||% paste0("sp", seq_len(N))

  if (!is.null(site_covs)) {
    checkmate::assert_data_frame(site_covs, nrows = J)
    if (any(c(series_name, site_id_name, values_to)
            %in% colnames(site_covs))) {
      stop(insight::format_error(c(
        "'site_covs' must not contain the reserved output columns.",
        i = paste0("Rename overlaps with: '", series_name,
                   "', '", site_id_name, "', '", values_to, "'.")
      )))
    }
  }
  if (!is.null(traits)) {
    checkmate::assert_data_frame(traits, nrows = N)
    if (any(c(series_name, site_id_name, values_to)
            %in% colnames(traits))) {
      stop(insight::format_error(c(
        "'traits' must not contain the reserved output columns.",
        i = paste0("Rename overlaps with: '", series_name,
                   "', '", site_id_name, "', '", values_to, "'.")
      )))
    }
  }

  grid <- expand.grid(
    series = seq_len(N),
    site   = seq_len(J),
    KEEP.OUT.ATTRS   = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[, c("series", "site"), drop = FALSE]
  out <- data.frame(matrix(NA, nrow = nrow(grid), ncol = 0L))
  out[[series_name]]    <- factor(species[grid$series], levels = species)
  out[[site_id_name]]   <- grid$site
  out[[values_to]]      <- Y[cbind(grid$site, grid$series)]
  if (!is.null(site_covs)) {
    for (col in colnames(site_covs)) {
      out[[col]] <- site_covs[[col]][grid$site]
    }
  }
  if (!is.null(traits)) {
    for (col in colnames(traits)) {
      out[[col]] <- traits[[col]][grid$series]
    }
  }
  rownames(out) <- NULL
  out
}
