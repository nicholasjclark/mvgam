#' Portal Project rodent capture survey data
#'
#' A dataset containing timeseries of total captures (across all control plots) for select rodent species from the Portal Project
#'
#' @format A `data.frame` containing the following fields:
#' \describe{
#' \item{time}{time of sampling, in lunar monthly cycles}
#' \item{series}{factor indicator of the time series, i.e. the species}
#' \item{captures}{total captures across all control plots}
#' \item{mintemp}{monthly mean minimum temperature}
#' \item{ndvi_ma12}{12-month moving average of the mean Normalised Difference Vegetation Index}
#' \item{mintemp}{monthly mean of minimum temperature}
#' }
#' @source \url{https://github.com/weecology/PortalData/blob/main/SiteandMethods/Methods.md}
"portal_data"
