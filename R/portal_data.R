#' Portal Project rodent capture survey data
#'
#' Monthly time series of rodent captures for selected species of the
#' Portal Project, a long-term study of a Chihuahuan Desert ecosystem
#' near Portal, Arizona. Captures are summed across all control plots.
#'
#' @format A `data.frame` containing the following fields:
#' \describe{
#'   \item{time}{time of sampling, in lunar monthly cycles}
#'   \item{series}{factor identifying the species}
#'   \item{captures}{total captures across all control plots at each
#'     time}
#'   \item{ndvi_ma12}{12-month moving average of the mean Normalised
#'     Difference Vegetation Index, a measure of plant greenness}
#'   \item{mintemp}{monthly mean of the minimum temperature}
#' }
#' @source \url{https://github.com/weecology/PortalData/blob/main/SiteandMethods/Methods.md}
#'
#' @seealso [mvgam()] for a state-space model of these counts and
#'   [mvgam_data()] to check the data. The
#'   `r mvgam_article("mvgam_overview", "overview article")` fits one.
#'
#' @examples
#' data(portal_data)
#' mvgam_data(portal_data, y = "captures", family = poisson())
"portal_data"
