#' Portal Project rodent capture survey data
#'
#' A dataset containing timeseries of total captures (across all control plots) for select rodent species from the Portal Project
#'
#' @format A dataframe containing the following fields:
#' \describe{
#' \item{moon}{time of sampling in lunar cycles}
#' \item{DM}{Total captures of species Dipodomys merriami}
#' \item{DO}{Total captures of species Dipodomys ordii}
#' \item{PP}{Total captures of species Chaetodipus penicillatus}
#' \item{OT}{Total captures of species Onychomys torridus}
#' \item{year}{Sampling year}
#' \item{month}{Sampling month}
#' \item{mintemp}{Monthly mean minimum temperature}
#' \item{precipitation}{Monthly mean precipitation}
#' \item{ndvi}{Monthly mean Normalised Difference Vegetation Index}
#' }
#' @source \url{https://github.com/weecology/PortalData/blob/main/SiteandMethods/Methods.md}
"portal_data"
