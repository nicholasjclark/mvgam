# mvgam 1.1.3 (development version; not yet on CRAN)
## New functionalities
* Allow intercepts to be included in process models when `trend_formula` is supplied. This breaks the assumption that the process has to be zero-centred, adding flexibility but also potentially inducing nonidentifiabilities with respect to any observation model intercepts. Thoughtful priors are a must for these models
* Added `stancode.mvgam` and `stancode.mvgam_prefit` methods
* Added 'gratia' to *Enhancements* to allow popular methods such as `draw()` to be used for 'mvgam' models if 'gratia' is already installed

## Deprecations
* The `drift` argument has been deprecated. It is now recommended for users to include parametric fixed effects of "time" in their respective GAM formulae to capture any expected drift effects

## Bug fixes
* Added a new check to ensure that exception messages are only suppressed by the `silent` argument if the user's version of 'cmdstanr' is adequate

# mvgam 1.1.2
## New functionalities
* Added options for silencing some of the 'Stan' compiler and modeling messages using the `silent` argument in `mvgam()`
* Moved a number of packages from 'Depends' to 'Imports' for simpler package loading and fewer potential masking conflicts
* Improved efficiency of the model initialisation by tweaking parameters of the underlying 'mgcv' `gam` object's convergence criteria, resulting in much faster model setups
* Added an option to use `trend_model = 'None'` in State-Space models, increasing flexibility by ensuring the process error evolves as white noise (#51)
* Added an option to use the non-centred parameterisation for some autoregressive trend models,
which speeds up mixing most of the time
* Updated support for multithreading so that all observation families (apart from `nmix()`) can now be modeled with multiple threads
* Changed default priors on autoregressive coefficients (AR1, AR2, AR3) to enforce
stationarity, which is a much more sensible prior in the majority of contexts

## Bug fixes
* Fixed a small bug that prevented `conditional_effects.mvgam()` from handling effects with three-way interactions

# mvgam 1.1.1
## New functionalities
* Changed indexing of an internal c++ function after Prof Brian Ripley’s   
  email: Dear maintainer, Please see the problems shown on 
  https://cran.r-project.org/web/checks/check_results_mvgam.html. Please correct   before 2024-05-22 to safely retain your package on CRAN. The CRAN Team
  
# mvgam 1.1.0
* First release of `mvgam` to CRAN
