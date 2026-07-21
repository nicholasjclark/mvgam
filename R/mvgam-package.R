#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats coef fitted getCall model.frame nobs predict vcov
#' @importFrom stats acf as.formula ave cov2cor delete.response gaussian
#' @importFrom stats lag mad median plogis qlogis rnorm sd setNames
#' @importFrom stats terms.formula
#' @importFrom graphics pairs
#' @importFrom utils tail
## usethis namespace: end
NULL

# Column names introduced by data-masking (dplyr / ggplot2 / data.table)
# and posterior-summary helpers. Declared here so R CMD check does not
# report them as undefined global variables.
utils::globalVariables(c(
  ".fitted", ".resid", ".resid.se", ".se.fit", "analysis", "draw__",
  "effect", "estimate__", "hi", "lo", "lower", "lower__", "q025",
  "q100", "q250", "q750", "q900", "q975", "trait_name", "upper", "upper__"
))
