#' Details of formula specifications in `mvgam`
#' @details \code{\link{mvgam}} will accept an observation model formula and an optional
#' process model formula (via the argument `trend_formula`). Neither of these formulae can
#' be specified as lists, contrary to the accepted behaviour in some `mgcv` or `brms` models.
#' \cr
#' \cr
#' Note that it is possible to supply an empty formula where
#' there are no predictors or intercepts in the observation model (i.e. `y ~ 0` or `y ~ -1`).
#' In this case, an intercept-only observation model will be set up but the intercept coefficient
#' will be fixed at zero. This can be handy if you wish to fit pure State-Space models where
#' the variation in the dynamic trend controls the average expectation, and/or where intercepts
#' are non-identifiable.
#' \cr
#' \cr
#' The formulae supplied to \code{\link{mvgam}} are exactly like those supplied to
#' \code{\link{glm}} except that smooth terms,
#' \code{\link[mgcv]{s}},
#' \code{\link[mgcv]{te}},
#' \code{\link[mgcv]{ti}} and
#' \code{\link[mgcv]{t2}},
#' time-varying effects using \code{\link{dynamic}},
#' monotonically increasing (using `s(x, bs = 'moi')`) or
#' or decreasing splines (using `s(x, bs = 'mod')`; see \code{\link{monotonic}} for
#' details), as well as
#' Gaussian Process functions using \code{\link[brms]{gp}},
#' can be added to the right hand side (and \code{.} is not supported in \code{mvgam} formulae).
#' \cr
#' \cr
#' Further details on specifying different kinds of smooth functions, and how to control their behaviours
#' by modifying their potential complexities and / or how the penalties behave, can be found in the
#' extensive documentation for the `mgcv` package.
#' @seealso \code{\link{mvgam}},
#' \code{\link[mgcv]{formula.gam}},
#' \code{\link[mgcv]{gam.models}},
#' \code{\link[mgcv]{jagam}},
#' \code{\link[mgcv]{gam}},
#' \code{\link[mgcv]{s}},
#' \code{\link[stats]{formula}},
#' \code{\link{monotonic}}
#' @author Nicholas J Clark
#' @name mvgam_formulae
NULL
