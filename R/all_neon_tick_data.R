#' NEON Amblyomma and Ixodes tick abundance survey data
#'
#' A dataset containing timeseries of Amblyomma americanum and Ixodes scapularis nymph abundances at NEON sites
#'
#' @format A tibble/dataframe containing covariate information alongside the main fields of:
#' \describe{
#' \item{Year}{Year of sampling}
#' \item{epiWeek}{Epidemiological week of sampling}
#'   \item{plot_ID}{NEON plot ID for survey location}
#'   \item{siteID}{NEON site ID for survey location}
#'   \item{amblyomma_americanum}{Counts of A. americanum nymphs}
#'   \item{ixodes_scapularis}{Counts of I. scapularis nymphs}
#' }
#' @source \url{https://www.neonscience.org/data}
"all_neon_tick_data"
