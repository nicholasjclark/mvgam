#' Package startup and loading functions
#'
#' Custom startup message and dependency loading following tidyverse patterns
#' @importFrom utils packageVersion
#' @name startup
#' @keywords internal
NULL

# Core dependencies that mvgam loads
core_deps <- c(
  "brms"
)


#' Package attachment hook
#'
#' Called when mvgam is loaded via library() or require()
#' @param libname Library name
#' @param pkgname Package name
#' @noRd
.onAttach <- function(libname, pkgname) {
  # Set marginaleffects option
  options("marginaleffects_model_classes" = "mvgam")
  
  # Show mvgam startup message (brms is auto-loaded as dependency)
  version <- utils::packageVersion("mvgam")
  
  startup_msg <- insight::format_message(
    paste0(
      "Loading 'mvgam' (version ",
      version,
      "). Useful instructions can be found by typing help('mvgam'). ",
      "A more detailed introduction to the package is available through vignette('mvgam_overview')."
    )
  )
  
  packageStartupMessage(startup_msg)
}

#' Package loading hook
#'
#' Called when mvgam namespace is loaded
#' @param libname Library name
#' @param pkgname Package name
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Initialize trend registry with core trend types
  ensure_registry_initialized()

  invisible()
}
