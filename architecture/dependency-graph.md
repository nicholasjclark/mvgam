# Package Dependency Map

**Generated:** 2025-08-29 11:05:47  
**Package:** mvgam v2.0.0  
**Commit:** test-de  

## Summary

- **Total Files:** 57
- **Total Functions:** 425
- **Exported Functions:** 68
- **Internal Functions:** 383
- **S3 Methods:** 44
- **S3 Classes:** 25

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
- **Core Functions**: mvgam, series_to_mvgam, sim_mvgam
- **Trend Types**: RW, GP, PW, VAR, CAR, ZMVN, AR
- **Analysis Tools**: stability, fevd, ppc, irf, lfo_cv
- **Plotting Functions**: plot_mvgam_series, plot_mvgam_smooth, plot_mvgam_factors, plot_mvgam_fc, plot_mvgam_trend, plot_mvgam_pterms, plot_mvgam_randomeffects, plot_mvgam_resids, plot_mvgam_uncertainty

## S3 Object System

### S3 Classes
- array.mvgam, brmsformula, construct.mod.smooth.spec, construct.moi.smooth.spec, data.frame.mvgam, default, formula, how_to_cite, jsdgam, matrix.mod.smooth, matrix.moi.smooth, matrix.mvgam, mvgam, mvgam_conditional_effects, mvgam_fevd, mvgam_forecast, mvgam_formula, mvgam_irf, mvgam_lfo, mvgam_prefit, mvgam_residcor, mvgam_trend, mvgamstancode, to.ts, trend_param

### Key S3 Methods
- **as()**: array.mvgam, data.frame.mvgam, matrix.mvgam
- **find_predictors()**: mvgam, mvgam_prefit
- **get_data()**: mvgam, mvgam_prefit
- **get_prior()**: brmsformula, default, formula, mvgam_formula
- **is()**: mvgam_trend, trend_param
- **plot()**: mvgam, mvgam_conditional_effects, mvgam_fevd, mvgam_forecast, mvgam_irf, mvgam_lfo, mvgam_residcor
- **Predict()**: matrix.mod.smooth, matrix.moi.smooth
- **print()**: how_to_cite, mvgam, mvgam_conditional_effects, mvgam_formula, mvgam_prefit, mvgam_trend, mvgamstancode, trend_param
- **residual_cor()**: jsdgam, mvgam
- **residuals()**: mvgam
- **smooth()**: construct.mod.smooth.spec, construct.moi.smooth.spec
- **stancode()**: mvgam, mvgam_formula, mvgam_prefit
- **summary()**: mvgam_fevd, mvgam_forecast, mvgam_irf

## Core Data Structures
- **mvgam object**: Fitted model with data, parameters, and predictions
- **mvgam_prefit**: Pre-compilation model structure
- **mvgam_forecast**: Forecast results with uncertainty
- **trend_param**: Trend model specifications

## Function Dependencies & Architecture

### Core Workflow Functions
- **mvgam()** (`R/mvgam_core.R`) - No internal dependencies tracked
- **sim_mvgam()** (`R/sim_mvgam.R`) - No internal dependencies tracked
- **plot.mvgam()** (`R/plot.mvgam.R`) - No internal dependencies tracked
- **lfo_cv.mvgam()** (`R/lfo_cv.mvgam.R`) - No internal dependencies tracked
- **series_to_mvgam()** (`R/series_to_mvgam.R`) - No internal dependencies tracked

### Most Connected Internal Functions

## Key Function Signatures

### `mvgam`:
```r
mvgam <- function(formula, trend_formula = NULL, data = NULL, backend = getOption("brms.backend", "cmdstanr"), combine = TRUE, family = poisson(), ...
```
**Arguments**: formula, trend_formula, data, backend

### `sim_mvgam`:
```r
sim_mvgam = function( T = 100, n_series = 3, seasonality = 'shared', use_lv = FALSE, n_lv = 0, trend_model = RW(), drift = FALSE, prop_trend = 0.2,...
```
**Arguments**: T, n_series, seasonality, use_lv, n_lv, trend_model

## File Organization & Function Locations

### Utilities Files
- **`R/utils-pipe.R`** (0 functions) - Utility functions
- **`R/globals.R`** (0 functions)

### Data Files
- **`R/portal_data.R`** (0 functions) - Data documentation and loading
- **`R/all_neon_tick_data.R`** (0 functions) - Data documentation and loading

### Validation Files
- **`R/validations.R`** (75 functions) - Input validation and checks
  - Key functions: is.mvgam_trend (+ 74 more)

### Core Files
- **`R/mvgam_core.R`** (25 functions) - Core package functionality
  - Key functions: mvgam (+ 24 more)
- **`R/marginaleffects.mvgam.R`** (10 functions) - Marginal effects
- **`R/as.data.frame.mvgam.R`** (10 functions) - Data documentation and loading
- **`R/lfo_cv.mvgam.R`** (7 functions)
  - Key functions: lfo_cv, lfo_cv.mvgam (+ 5 more)
- **`R/loo.mvgam.R`** (6 functions)
- **`R/print.mvgam.R`** (6 functions) - Print methods for objects
- **`R/sim_mvgam.R`** (5 functions)
  - Key functions: sim_mvgam (+ 4 more)
- **`R/irf.mvgam.R`** (5 functions)
  - Key functions: irf (+ 4 more)
- **`R/ppc.mvgam.R`** (5 functions)
  - Key functions: ppc (+ 4 more)
- **`R/fevd.mvgam.R`** (4 functions)
  - Key functions: fevd (+ 3 more)
- **`R/interpret_mvgam.R`** (3 functions)
- **`R/plot.mvgam.R`** (2 functions) - Plotting and visualization
  - Key functions: plot.mvgam (+ 1 more)
- **`R/series_to_mvgam.R`** (2 functions)
  - Key functions: series_to_mvgam (+ 1 more)
- **`R/pairs.mvgam.R`** (1 functions)
- **`R/mcmc_plot.mvgam.R`** (1 functions) - Plotting and visualization
- **`R/residuals.mvgam.R`** (1 functions) - Residual analysis
- **`R/index-mvgam.R`** (1 functions)

### Other Files
- **`R/trend_system.R`** (70 functions) - Trend modeling
  - Key functions: register_trend_type, list_trend_types, register_custom_trend, trend_param, is.trend_param (+ 65 more)
- **`R/priors.R`** (33 functions) - Prior specification
  - Key functions: mvgam_formula, get_prior (+ 31 more)
- **`R/backends.R`** (6 functions)
- **`R/conditional_effects.R`** (6 functions) - Conditional effects
- **`R/monotonic.R`** (5 functions)
- **`R/ensemble.R`** (4 functions)
  - Key functions: ensemble (+ 3 more)
- **`R/mvgam_residcor-class.R`** (4 functions) - Class definitions and methods
- **`R/how_to_cite.R`** (3 functions)
  - Key functions: how_to_cite (+ 2 more)
- **`R/residual_cor.R`** (3 functions) - Residual analysis
  - Key functions: residual_cor (+ 2 more)
- **`R/tidier_methods.R`** (3 functions) - Method implementations
- **`R/mvgam_fevd-class.R`** (3 functions) - Class definitions and methods
- **`R/RcppExports.R`** (3 functions) - Exported functions
- **`R/ordinate.jsdgam.R`** (2 functions)
  - Key functions: ordinate (+ 1 more)
- **`R/mvgam_irf-class.R`** (2 functions) - Class definitions and methods
- **`R/zzz.R`** (2 functions) - Package startup and attachment
- **`R/stability.R`** (2 functions)
  - Key functions: stability (+ 1 more)
- **`R/lv_correlations.R`** (1 functions)
  - Key functions: lv_correlations
- **`R/mvgam_forecast-class.R`** (1 functions) - Class definitions and methods
- **`R/dynamic.R`** (1 functions)
  - Key functions: dynamic
- **`R/cpp_funs.R`** (0 functions)
- **`R/mvgam-class.R`** (0 functions) - Class definitions and methods
- **`R/mvgam-package.R`** (0 functions)

### S3 Methods Files
- **`R/add_residuals.R`** (2 functions) - Residual analysis
  - Key functions: add_residuals (+ 1 more)

### Stan/Modeling Files
- **`R/stan_assembly.R`** (64 functions) - Stan model integration
- **`R/brms_integration.R`** (18 functions) - brms integration
- **`R/make_stan.R`** (3 functions) - Stan model integration

### Plotting Files
- **`R/plot_mvgam_series.R`** (6 functions) - Plotting and visualization
  - Key functions: plot_mvgam_series (+ 5 more)
- **`R/plot_mvgam_uncertainty.R`** (2 functions) - Plotting and visualization
  - Key functions: plot_mvgam_uncertainty (+ 1 more)
- **`R/plot_mvgam_fc.R`** (2 functions) - Plotting and visualization
  - Key functions: plot_mvgam_fc (+ 1 more)
- **`R/plot_mvgam_pterms.R`** (1 functions) - Plotting and visualization
  - Key functions: plot_mvgam_pterms
- **`R/plot_mvgam_resids.R`** (1 functions) - Plotting and visualization
  - Key functions: plot_mvgam_resids
- **`R/plot_mvgam_smooth.R`** (1 functions) - Plotting and visualization
  - Key functions: plot_mvgam_smooth
- **`R/plot_mvgam_randomeffects.R`** (1 functions) - Plotting and visualization
  - Key functions: plot_mvgam_randomeffects
- **`R/plot_mvgam_trend.R`** (1 functions) - Trend modeling
  - Key functions: plot_mvgam_trend
- **`R/plot_mvgam_factors.R`** (1 functions) - Plotting and visualization
  - Key functions: plot_mvgam_factors

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
- **`R/mvgam_core.R`**: mvgam
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

## External Package Integration

### Bayesian/MCMC
- **brms**: Bayesian regression model framework integration
- **rstan**: Stan probabilistic programming interface
- **rstantools**: Package functionality
- **posterior**: Tools for working with posterior distributions
- **loo**: Leave-one-out cross-validation and model comparison
- **bayesplot**: Package functionality

### Data Manipulation
- **dplyr**: Data frame manipulation and transformation
- **purrr**: Package functionality
- **tibble**: Package functionality
- **magrittr**: Package functionality

### Modeling/Statistics
- **mgcv**: GAM (Generalized Additive Model) functionality
- **mvnfast**: Package functionality
- **marginaleffects**: Marginal effects and predictions
- **generics**: Package functionality

### Infrastructure
- **methods**: Package functionality
- **Rcpp**: R and C++ integration for performance
- **rlang**: Package functionality
- **insight**: Unified interface to model information

### Visualization
- **ggplot2**: Grammar of graphics plotting system
- **patchwork**: Package functionality

## Notes for LLM Agents
- **Primary workflow**: `mvgam()` -> model fitting -> `plot()`, `predict()`, `summary()` methods
- **Stan integration**: Package heavily relies on Stan/brms for Bayesian computation
- **S3 system**: Extensive use of S3 classes (`mvgam`, `mvgam_forecast`, etc.) with method dispatch
- **Time series focus**: Specialized for multivariate time series with trend modeling
- **File patterns**: `R/plot_*.R` for visualization, `R/*_mvgam.R` for S3 methods, `R/validations.R` for input checking
- **Core functions often call**: validation functions, Stan code generation, data preprocessing utilities

