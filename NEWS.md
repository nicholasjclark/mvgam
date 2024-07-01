# mvgam 1.1.2
* Added options for silencing some of the 'Stan' compiler and modeling messages using the `silent` argument in `mvgam()`
* Moved a number of packages from 'Depends' to 'Imports' for simpler package loading and fewer potential masking conflicts
* Improved efficiency of the model initialisation by tweaking parameters of the underlying 'mgcv' `gam` object's convergence criteria, resulting in much faster model setups
* Added an option to use `trend_model = 'None'` in State-Space models, increasing flexibility by ensuring the process error evolves as white noise (#51)
* Added an option to use the non-centred parameterisation for some autoregressive trend models,
which speeds up mixing most of the time
* Updated support for multithreading so that all observation families (apart from `nmix()`) can now be modeled with multiple threads
* Changed default priors on autoregressive coefficients (AR1, AR2, AR3) to enforce
stationarity, which is a much more sensible prior in the majority of contexts
* Fixed a small bug that prevented `conditional_effects.mvgam()` from handling effects with three-way interactions

# mvgam 1.1.1
* Changed indexing of an internal c++ function after Prof Brian Ripley’s   
  email: Dear maintainer, Please see the problems shown on 
  https://cran.r-project.org/web/checks/check_results_mvgam.html. Please correct   before 2024-05-22 to safely retain your package on CRAN. The CRAN Team
  
# mvgam 1.1.0
* First release of `mvgam` to CRAN
