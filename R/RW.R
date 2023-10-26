#' Specify autoregressive dynamic processes
#'
#' Set up a autoregressive or autoregressive moving average trend models
#' in \code{mvgam}. These functions do not evaluate their arguments –
#' they exist purely to help set up a model with particular autoregressive
#' trend models.
#' @param ma \code{Logical} Include moving average terms of order \code{1}?
#' Default is \code{FALSE}. Note, this option is only currently operational
#' for fitting VARMA models. Support for other models (AR and RW) is upcoming.
#' @param cor \code{Logical} Include correlated process errors as part of a
#' multivariate normal process model? If \code{TRUE} and if \code{n_series > 1}
#' in the supplied data, a fully structured covariance matrix will be estimated
#' for the process errors. Default is \code{FALSE}. Note, this option is only currently operational
#' for fitting VAR / VARMA models. Support for other models (AR and RW) is upcoming.
#' @param p A non-negative integer specifying the autoregressive (AR) order.
#' Default is \code{1}. Cannot currently be larger than \code{3}
#' @return An object of class \code{mvgam_trend}, which contains a list of
#' arguments to be interpreted by the parsing functions in \code{mvgam}
#' @rdname RW
#' @export
RW = function(ma = FALSE, cor = FALSE){
  if(ma){
    stop('Moving average terms not yet supported for RW models',
         call. = FALSE)
  }
  if(cor){
    stop('Correlated errors not yet supported for RW models',
         call. = FALSE)
  }
  out <- structure(list(trend_model = 'RW',
                        ma = ma,
                        cor = cor,
                        label = match.call()),
                   class = 'mvgam_trend')
}

#' @rdname RW
#' @export
AR = function(p = 1, ma = FALSE, cor = FALSE){
  if(ma){
    stop('Moving average terms not yet supported for AR models',
         call. = FALSE)
  }
  if(cor){
    stop('Correlated errors not yet supported for AR models',
         call. = FALSE)
  }
  validate_pos_integer(p)
  if(p > 3){
    stop("Argument 'p' must be <= 3",
         call. = FALSE)
  }
  out <- structure(list(trend_model = paste0('AR', p),
                        ma = ma,
                        cor = cor,
                        label = match.call()),
                   class = 'mvgam_trend')
}

#' @rdname RW
#' @export
VAR = function(ma = FALSE, cor = FALSE){
  out <- structure(list(trend_model = 'VAR',
                        ma = ma,
                        cor = cor,
                        label = match.call()),
                   class = 'mvgam_trend')
}
