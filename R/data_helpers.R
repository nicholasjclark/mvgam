# Data shape helpers for users arriving from other multi-species
# packages. mvgam's closure-unit and JSDM pipelines all consume a
# long-format `data.frame` keyed by `(series, time, ...)`. Other
# packages ship multi-dimensional arrays:
#
#   unmarked / ubms / flocker:
#     y = matrix[J sites, K max visits]
#     siteCovs = data.frame[J, p]
#     obsCovs  = list of matrix[J, K] (one per visit-varying covariate)
#                OR data.frame[J * K, p] long-format
#
#   spOccupancy (multi-species):
#     y        = array[N species, J sites, K max visits]
#     occ.covs = data.frame[J, p_occ]
#     det.covs = list of matrix[J, K]
#
#   Hmsc (multi-species single-visit JSDM):
#     Y       = matrix[J sites, N species]
#     XData   = data.frame[J, p]
#     TrData  = data.frame[N, q]   (per-species traits)
#     phyloTree = phylo object
#
# These helpers pivot all three layouts into the long-format
# `data.frame` mvgam expects, broadcasting site / species covariates
# across the (species, site, visit) grid.

#' Pivot a detection array to mvgam long-format
#'
#' Convert the multi-dimensional detection histories used by
#' `unmarked`, `ubms`, `flocker`, and `spOccupancy` into the
#' long-format closure-unit layout `mvgam(family = occ() / nmix())`
#' and `jsdgam(family = occ() / nmix())` expect. One row per
#' `(species, site, visit)` triple (or `(site, visit)` when `y` is
#' single-species).
#'
#' @param y One of three layouts:
#'   \itemize{
#'     \item **Single-species**: a `[J_sites, K_max_visits]` matrix
#'       of detections (0 / 1 for `occ()`, non-negative counts for
#'       `nmix()`). This is the `unmarked::unmarkedFrameOccu` /
#'       `unmarkedFramePCount` `y` slot and the `flocker::flock`
#'       `obs` argument with `type = "single"`.
#'     \item **Multi-species (array)**: a
#'       `[N_species, J_sites, K_max_visits]` array. This is the
#'       `spOccupancy::lfMsPGOcc` / `msPGOcc` `y` slot.
#'     \item **Multi-species (list)**: a named list of
#'       `[J_sites, K_max_visits]` matrices, one per species. This
#'       is the `unmarked::unmarkedFrameOccuMulti` `y` slot.
#'       List names become species labels; matrices must share `J`
#'       and `K`.
#'   }
#'   `NA` entries mark missing visits and are dropped from the
#'   output, matching the `unmarked` / `flocker` ragged-visit
#'   convention.
#' @param site_covs Optional `data.frame` of site-level covariates
#'   with `nrow == J_sites`. Broadcast across visits and species.
#'   The `unmarked::unmarkedFrameOccu` `siteCovs` slot, the
#'   spOccupancy `occ.covs`, and the `flocker` `unit_covs` map
#'   here. Column names appear unchanged in the output.
#' @param obs_covs Optional visit-level covariates. Two layouts
#'   are accepted:
#'   \itemize{
#'     \item A **named list** of `[J_sites, K_max_visits]` matrices
#'       (the spOccupancy `det.covs`, `flocker` `event_covs`, and
#'       the list-form of `unmarked::unmarkedFrameOccu` `obsCovs`).
#'       List entries must be named.
#'     \item A **long-format data.frame** with `J_sites * K_max_visits`
#'       rows, site-major and visit-fastest (matching the
#'       `unmarked` `obsCovs` data.frame convention,
#'       `as.vector(t(matrix))`).
#'   }
#'   Broadcast across species in the multi-species cases.
#' @param species Optional character vector of species names with
#'   `length == N_species`. Only meaningful for multi-species `y`.
#'   Defaults to `sp1, sp2, ...`.
#' @param site_id_name Name of the site identifier in the output.
#'   Defaults to `"time"` so the result drops directly into mvgam
#'   (which expects `time`); set to `"site"` if you intend to pass
#'   the result through `jsdgam(unit = site)`.
#' @param visit_id_name Name of the visit identifier in the output.
#'   Defaults to `"visit"`.
#' @param series_name Name of the species column. Defaults to
#'   `"series"`. Set to `"species"` if you intend to pass the
#'   result through `jsdgam(species = species)`.
#' @param values_to Name of the detection / count column. Defaults
#'   to `"y"`.
#'
#' @return A long-format `data.frame` with columns
#'   `(series, time, visit, y, site_covs..., obs_covs...)` ready to
#'   feed `mvgam(family = occ() / nmix())` or `jsdgam(family =
#'   occ() / nmix())`. Rows where `y` is `NA` are dropped so closure
#'   units can vary in the number of visits.
#'
#' @author Nicholas J Clark
#' @export
pivot_detection_array <- function(y,
                                    site_covs = NULL,
                                    obs_covs = NULL,
                                    species = NULL,
                                    site_id_name = "time",
                                    visit_id_name = "visit",
                                    series_name = "series",
                                    values_to = "y") {
  checkmate::assert_string(site_id_name)
  checkmate::assert_string(visit_id_name)
  checkmate::assert_string(series_name)
  checkmate::assert_string(values_to)

  if (is.matrix(y)) {
    # Single-species: lift to a 1 x J x K array so the multi-species
    # path handles both cases. Strip the species axis at the end.
    drop_species_axis <- TRUE
    y_arr <- array(y, dim = c(1L, nrow(y), ncol(y)))
  } else if (is.array(y) && length(dim(y)) == 3L) {
    drop_species_axis <- FALSE
    y_arr <- y
  } else {
    stop(insight::format_error(c(
      "'y' must be a 2D matrix or a 3D array.",
      x = paste0("Got class ", paste(class(y), collapse = "/"),
                 " with dim ", paste(dim(y), collapse = " x "), "."),
      i = paste0(
        "Single-species: '[J_sites, K_visits]' matrix. Multi-species: ",
        "'[N_species, J_sites, K_visits]' array."
      )
    )))
  }

  N <- dim(y_arr)[1L]
  J <- dim(y_arr)[2L]
  K <- dim(y_arr)[3L]

  if (is.null(species)) {
    species <- paste0("sp", seq_len(N))
  }
  checkmate::assert_character(species, len = N, any.missing = FALSE)

  # Site covs: J rows, broadcast across (species, visit).
  if (!is.null(site_covs)) {
    checkmate::assert_data_frame(site_covs, nrows = J)
    if (any(c(series_name, site_id_name, visit_id_name, values_to)
            %in% colnames(site_covs))) {
      stop(insight::format_error(c(
        "'site_covs' must not contain the reserved output columns.",
        i = paste0(
          "Rename overlaps with: '", series_name, "', '",
          site_id_name, "', '", visit_id_name, "', '",
          values_to, "'."
        )
      )))
    }
  }

  # Obs covs: list of [J, K] matrices.
  if (!is.null(obs_covs)) {
    checkmate::assert_list(obs_covs, names = "unique")
    for (nm in names(obs_covs)) {
      mat <- obs_covs[[nm]]
      checkmate::assert_matrix(mat, nrows = J, ncols = K)
    }
    if (any(c(series_name, site_id_name, visit_id_name, values_to)
            %in% names(obs_covs))) {
      stop(insight::format_error(c(
        "'obs_covs' must not contain the reserved output columns.",
        i = paste0(
          "Rename overlaps with: '", series_name, "', '",
          site_id_name, "', '", visit_id_name, "', '",
          values_to, "'."
        )
      )))
    }
  }

  # Build the (species, site, visit) row index. Order is
  # site-fastest within species to match mvgam's canonical
  # `arrange(time, series, visit)` layout downstream.
  grid <- expand.grid(
    visit  = seq_len(K),
    series = seq_len(N),
    site   = seq_len(J),
    KEEP.OUT.ATTRS   = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[, c("series", "site", "visit"), drop = FALSE]
  out <- data.frame(
    matrix(NA, nrow = nrow(grid), ncol = 0L)
  )
  out[[series_name]]    <- factor(species[grid$series], levels = species)
  out[[site_id_name]]   <- grid$site
  out[[visit_id_name]]  <- grid$visit
  out[[values_to]]      <- y_arr[cbind(grid$series, grid$site, grid$visit)]

  if (!is.null(site_covs)) {
    for (col in colnames(site_covs)) {
      out[[col]] <- site_covs[[col]][grid$site]
    }
  }
  if (!is.null(obs_covs)) {
    for (nm in names(obs_covs)) {
      out[[nm]] <- obs_covs[[nm]][cbind(grid$site, grid$visit)]
    }
  }

  # Drop rows where y is NA (missing visits).
  out <- out[!is.na(out[[values_to]]), , drop = FALSE]
  rownames(out) <- NULL

  if (drop_species_axis) {
    # Single-species: drop the redundant `series` factor (all rows
    # share the same level). Users who pass a single 2D matrix into
    # `mvgam()` typically don't carry a species axis.
    out[[series_name]] <- NULL
  }
  out
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
