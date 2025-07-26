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

#' Check which core dependencies are not yet loaded
#' @noRd
core_unloaded <- function() {
  search <- paste0("package:", core_deps)
  core_deps[!search %in% search()]
}

#' Quietly attach core dependencies without startup messages
#' @noRd
mvgam_attach <- function() {
  to_load <- core_unloaded()
  
  # For dependencies, attach them quietly using attachNamespace
  # This prevents startup messages while making functions available
  for (pkg in to_load) {
    if (pkg %in% loadedNamespaces()) {
      # Namespace already loaded, just attach
      suppressWarnings(attachNamespace(pkg))
    } else {
      # Load and attach namespace quietly
      suppressPackageStartupMessages({
        requireNamespace(pkg, quietly = TRUE)
        attachNamespace(pkg)
      })
    }
  }
  
  invisible(to_load)
}


#' Package attachment hook
#' 
#' Called when mvgam is loaded via library() or require()
#' @param libname Library name
#' @param pkgname Package name
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
      "A more detailed introduction to the package is available through vignette('mvgam_overview'). ",
      "Full brms compatibility is now enabled."
    )
  )
  
  packageStartupMessage(startup_msg)
}

#' Package loading hook
#' 
#' Called when mvgam namespace is loaded
#' @param libname Library name  
#' @param pkgname Package name
.onLoad <- function(libname, pkgname) {
  # Any namespace-level initialization can go here
  invisible()
}