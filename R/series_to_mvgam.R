# Convert timeseries object to format necessary for mvgam
#'
#'This function converts univariate or multivariate time series (\code{xts} or \code{ts} objects)
#'to the format necessary for \code{\link{mvgam}}
#'
#'@importFrom stats is.ts ts start time frequency
#'@importFrom utils head
#'@param series \code{\link[xts]{xts}} or \code{\link[stats]{ts}} object to be converted to \code{\link{mvgam}} format
#'@param freq \code{integer}. The seasonal frequency of the series
#'@param train_prop \code{numeric} stating the proportion of data to use for training. Should be between \code{0.25} and \code{0.95}
#'@return A \code{list} object containing outputs needed for \code{\link{mvgam}},
#'including 'data_train' and 'data_test'
#'
#'@examples
#'# A ts object example
#'data("sunspots")
#'series <- cbind(sunspots, sunspots)
#'colnames(series) <- c('blood', 'bone')
#'head(series)
#'series_to_mvgam(series, frequency(series), 0.85)
#'
#'# An xts object example
#'library(xts)
#'dates <- seq(as.Date("2001-05-01"), length=30, by="quarter")
#'data  <- cbind(c(gas = rpois(30, cumprod(1+rnorm(30, mean = 0.01, sd = 0.001)))),
#'c(oil = rpois(30, cumprod(1+rnorm(30, mean = 0.01, sd = 0.001)))))
#'series <- xts(x = data, order.by = dates)
#'colnames(series) <- c('gas', 'oil')
#'head(series)
#'series_to_mvgam(series, freq = 4, train_prop = 0.85)
#'
#'@export
#'
series_to_mvgam <- function(series, freq, train_prop = 0.85){

  # Check for xts and lubridate packages
  insight::check_if_installed("xts")
  insight::check_if_installed("lubridate")

  # Check series format
  type <- 'wrong'
  if(is.ts(series)) {
    type <- 'ts'
  }

  if(xts::is.xts(series)) {
    type <- 'xts'
  }

  if(type == 'wrong'){
    stop("series must be either a ts or xts object")
  }

  # Extract information on years and seasons from the series object
  if(type == 'ts'){
    dates <- lubridate::date_decimal(as.numeric(time(series)))
    years <- lubridate::year(dates)
    seasons <- as.vector(1 + ((time(series) %% 1) * frequency(series)))
  }

  # Function to convert xts to ts object
  xts.to.ts <- function(x, freq = 52) {
    start_time <-  head(1 +
                          (round((lubridate::yday(lubridate::date(time(series))) / 365) *
                                   freq, 0)), 1)
    ts(as.numeric(x),
       start = c(lubridate::year(start(x)),
                 start_time), frequency = freq)
  }

  if(type == 'xts'){
    dates <- lubridate::date(time(series))
    years <- lubridate::year(time(series))
    seasons <- as.vector(1 + ((time(xts.to.ts(series[,1], freq = freq)) %% 1) *
                                freq))
  }

  # Extract remaining information and put into correct format
  n_series <- NCOL(series)
  T <- NROW(series)
  series_names <- factor(colnames(series), levels = colnames(series))
  if(length(levels(series_names)) == 0){
    series_names <- factor(paste0('series_', seq(1, n_series)),
                           levels = paste0('series_', seq(1, n_series)))
  }

  mvgam_data = data.frame(y = as.vector(series),
                          season = rep(seasons, n_series),
                          year = rep(years, n_series),
                          date = rep(dates, n_series),
                          series = as.factor(sort(rep(series_names, T)))) %>%
    dplyr::arrange(year, season, series)


  mvgam_data %>%
    dplyr::left_join(mvgam_data %>%
                       dplyr::select(year, season) %>%
                       dplyr::distinct() %>%
                       dplyr::arrange(year, season) %>%
                       dplyr::mutate(time = dplyr::row_number()),
                     by = c('season', 'year')) -> mvgam_data

  # Split into training and testing and return
  last_time <- floor(max(mvgam_data$time) * train_prop)

  return(list(data_train = mvgam_data %>%
                dplyr::filter(time <= last_time),
              data_test = mvgam_data %>%
                dplyr::filter(time > last_time)))
}
