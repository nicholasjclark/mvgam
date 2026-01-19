# Package Dependency Map

**Generated:** 2026-01-28 16:05:27
**Package:** mvgam v2.0.0
**Commit:** fe41ec2

## Summary

- **Total Files:** 65
- **Total Functions:** 654
- **Exported Functions:** 57
- **Internal Functions:** 597
- **S3 Methods:** 79
- **S3 Classes:** 32

## External Dependencies
- bayesplot
- brms
- dplyr
- generics
- ggplot2
- insight
- loo
- magrittr
- marginaleffects
- methods
- mgcv
- mvnfast
- patchwork
- posterior
- purrr
- Rcpp
- rlang
- rstan
- rstantools
- tibble

## User Interface (Exported Functions)

### Main Entry Points
- **Core Functions**: mvgam,series_to_mvgam,sim_mvgam
- **Trend Types**: AR,CAR,GP,PW,RW,VAR,ZMVN
- **Analysis Tools**: fevd,irf,lfo_cv,loo,ppc,pp_check,stability
- **Plotting Functions**: plot_mvgam_factors,plot_mvgam_fc,plot_mvgam_pterms,plot_mvgam_randomeffects,plot_mvgam_resids,plot_mvgam_series,plot_mvgam_smooth,plot_mvgam_trend,plot_mvgam_uncertainty

## Core Data Structures
- **mvgam object**: Fitted model with data, parameters, and predictions
- **mvgam_prefit**: Pre-compilation model structure
- **mvgam_forecast**: Forecast results with uncertainty
- **trend_param**: Trend model specifications

## File Organization & Function Locations

### Core Files
- **`R/mvgam_core.R`** (13 functions)
- **`R/series_to_mvgam.R`** (2 functions)
- **`R/sim_mvgam.R`** (5 functions)


### Stan/Modeling Files
- **`R/brms_integration.R`** (17 functions)
- **`R/make_stan.R`** (3 functions)
- **`R/mock-stanfit.R`** (6 functions)
- **`R/stan_assembly.R`** (81 functions)
- **`R/stan_polish.R`** (11 functions)


### Validation Files
- **`R/validations.R`** (75 functions)


### S3 Methods Files
- **`R/add_residuals.R`** (2 functions)
- **`R/mvgam_residcor-class.R`** (4 functions)
- **`R/posterior_predict.R`** (12 functions)
- **`R/predictions.R`** (20 functions)
- **`R/predict.R`** (2 functions)
- **`R/print.mvgam.R`** (11 functions)
- **`R/residual_cor.R`** (3 functions)
- **`R/residuals.mvgam.R`** (1 functions)
- **`R/summary.mvgam.R`** (24 functions)


### Plotting Files
- **`R/mcmc_plot.mvgam.R`** (1 functions)
- **`R/plot_mvgam_factors.R`** (1 functions)
- **`R/plot_mvgam_fc.R`** (2 functions)
- **`R/plot_mvgam_pterms.R`** (1 functions)
- **`R/plot.mvgam.R`** (2 functions)
- **`R/plot_mvgam_randomeffects.R`** (1 functions)
- **`R/plot_mvgam_resids.R`** (1 functions)
- **`R/plot_mvgam_series.R`** (6 functions)
- **`R/plot_mvgam_smooth.R`** (1 functions)
- **`R/plot_mvgam_trend.R`** (1 functions)
- **`R/plot_mvgam_uncertainty.R`** (2 functions)


### Utilities Files
- **`R/globals.R`** (0 functions)
- **`R/utils-pipe.R`** (0 functions)


### Data Files
- **`R/all_neon_tick_data.R`** (0 functions)
- **`R/as.data.frame.mvgam.R`** (10 functions)
- **`R/portal_data.R`** (0 functions)


### Other Files
- **`R/backends.R`** (26 functions)
- **`R/cpp_funs.R`** (0 functions)
- **`R/dynamic.R`** (1 functions)
- **`R/ensemble.R`** (4 functions)
- **`R/fevd.mvgam.R`** (4 functions)
- **`R/glm_analysis.R`** (27 functions)
- **`R/how_to_cite.R`** (3 functions)
- **`R/index-mvgam.R`** (6 functions)
- **`R/irf.mvgam.R`** (5 functions)
- **`R/lfo_cv.mvgam.R`** (7 functions)
- **`R/loo.mvgam.R`** (6 functions)
- **`R/lv_correlations.R`** (1 functions)
- **`R/monotonic.R`** (5 functions)
- **`R/mu_expression_analysis.R`** (26 functions)
- **`R/mvgam-class.R`** (0 functions)
- **`R/mvgam_fevd-class.R`** (3 functions)
- **`R/mvgam_forecast-class.R`** (1 functions)
- **`R/mvgam_irf-class.R`** (2 functions)
- **`R/mvgam-package.R`** (0 functions)
- **`R/ordinate.jsdgam.R`** (2 functions)
- **`R/pairs.mvgam.R`** (1 functions)
- **`R/posterior_epred.R`** (78 functions)
- **`R/posterior_linpred.R`** (2 functions)
- **`R/ppc.mvgam.R`** (5 functions)
- **`R/priors.R`** (28 functions)
- **`R/RcppExports.R`** (3 functions)
- **`R/sample_innovations.R`** (16 functions)
- **`R/stability.R`** (2 functions)
- **`R/tidier_methods.R`** (3 functions)
- **`R/trend_system.R`** (64 functions)
- **`R/zzz.R`** (2 functions)


## Function Location Quick Reference

### Exported Functions by File
- **`R/add_residuals.R`**: add_residuals
- **`R/dynamic.R`**: dynamic
- **`R/ensemble.R`**: ensemble
- **`R/fevd.mvgam.R`**: fevd
- **`R/how_to_cite.R`**: how_to_cite
- **`R/irf.mvgam.R`**: irf
- **`R/lfo_cv.mvgam.R`**: lfo_cv
- **`R/lv_correlations.R`**: lv_correlations
- **`R/mvgam_core.R`**: mvgam, mvgam_multiple
- **`R/ordinate.jsdgam.R`**: ordinate
- **`R/plot_mvgam_factors.R`**: plot_mvgam_factors
- **`R/plot_mvgam_fc.R`**: plot_mvgam_fc
- **`R/plot_mvgam_pterms.R`**: plot_mvgam_pterms
- **`R/plot_mvgam_randomeffects.R`**: plot_mvgam_randomeffects
- **`R/plot_mvgam_resids.R`**: plot_mvgam_resids
- **`R/plot_mvgam_series.R`**: plot_mvgam_series
- **`R/plot_mvgam_smooth.R`**: plot_mvgam_smooth
- **`R/plot_mvgam_trend.R`**: plot_mvgam_trend
- **`R/plot_mvgam_uncertainty.R`**: plot_mvgam_uncertainty
- **`R/ppc.mvgam.R`**: ppc
- **`R/priors.R`**: get_prior, mvgam_formula
- **`R/residual_cor.R`**: residual_cor
- **`R/series_to_mvgam.R`**: series_to_mvgam
- **`R/sim_mvgam.R`**: sim_mvgam
- **`R/stability.R`**: stability
- **`R/trend_system.R`**: AR, CAR, create_mvgam_trend, custom_trend, GP, is.trend_param, list_trend_types, mvgam_trend_choices, PW, register_custom_trend, register_trend_type, RW, trend_param, VAR, ZMVN
- **`R/validations.R`**: is.mvgam_trend

## Notes for LLM Agents
- **Primary workflow**: `mvgam()` -> prior/trend system setup -> Stan assembly -> model fitting -> S3 methods
- **Prior system**: `mvgam_formula()`, `get_prior()`, `set_prior()` for Bayesian prior specification
- **Trend system**: `trend_param()`, `register_trend_type()` for time series trend modeling
- **Stan assembly**: `stancode()`, `standata()` for Stan model compilation and data preparation
- **Stan integration**: Package heavily relies on Stan/brms for Bayesian computation
- **S3 system**: Extensive use of S3 classes (`mvgam`, `mvgam_forecast`, etc.) with method dispatch
- **File patterns**: `R/priors.R` (prior system), `R/trend_system.R` (trends), `R/stan_assembly.R` (Stan code)
- **Core functions often call**: validation functions, Stan code generation, data preprocessing utilities
