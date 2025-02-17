.onAttach = function(libname, pkgname){
  options("marginaleffects_model_classes" = "mvgam")
  packageStartupMessage(
    paste0('Welcome to mvgam. Please cite as:\n',
    'Clark, NJ, and Wells, K. 2022. Dynamic Generalized Additive Models (DGAMs)\n',
    'for forecasting discrete ecological time series. Methods in Ecology and\n',
    'Evolution, 2022, https://doi.org/10.1111/2041-210X.13974')
    )
}
