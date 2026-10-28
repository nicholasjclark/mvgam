#' @section Trend state:
#' By default the trend enters through its covariate part, without the
#' state it took at each observed time. The \pkg{brms} counterparts of
#' this method use that fitted state. `incl_autocor = TRUE` uses it
#' here too, as [fitted.mvgam()] and [pp_check.mvgam()] do on the
#' training data. The section 'Which prediction function?' of
#' [predict.mvgam()] compares the prediction functions.
