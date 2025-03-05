.onAttach = function(libname, pkgname) {
  options("marginaleffects_model_classes" = "mvgam")
  version <- utils::packageVersion("mvgam")
  packageStartupMessage(
    insight::format_message(
      paste0("Loading 'mvgam' (version ", version, "). Useful instructions can be found by typing help('mvgam'). A more detailed introduction to the package is available through vignette('mvgam_overview')."
  )))
}
