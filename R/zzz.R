#' Package attachment hook
#'
#' Called when mvgam is loaded via library() or require()
#' @param libname Library name
#' @param pkgname Package name
#' @noRd
.onAttach <- function(libname, pkgname) {
  # marginaleffects refuses a model class it does not list unless this
  # option names it
  options("marginaleffects_model_classes" = "mvgam")
  packageStartupMessage(insight::format_message(paste0(
    "Loading 'mvgam' (version ", utils::packageVersion("mvgam"), "). ",
    "Useful instructions can be found by typing help('mvgam'). ",
    "A more detailed introduction to the package is available through ",
    "vignette('mvgam_overview')."
  )))
}

#' Package loading hook
#'
#' Called when mvgam namespace is loaded
#' @param libname Library name
#' @param pkgname Package name
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Register the core trend types
  ensure_registry_initialized()
  invisible()
}
