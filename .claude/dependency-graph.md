# Package Dependency Map

**Generated:** 2025-08-27 14:04:49  
**Package:** mvgam v2.0.0  
**Commit:**   

## Summary

- **Total Files:** 56
- **Total Functions:** 418
- **Exported Functions:** 69
- **Internal Functions:** 375
- **S3 Methods:** 42
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
- array.mvgam, brmsformula, brmsprior, construct.mod.smooth.spec, construct.moi.smooth.spec, data.frame.mvgam, default, formula, how_to_cite, jsdgam, matrix.mod.smooth, matrix.moi.smooth, matrix.mvgam, mvgam, mvgam_conditional_effects, mvgam_fevd, mvgam_forecast, mvgam_formula, mvgam_irf, mvgam_lfo, mvgam_prefit, mvgam_residcor, mvgam_trend, to.ts, trend_param

### Key S3 Methods
- **as()**: array.mvgam, data.frame.mvgam, matrix.mvgam
- **c()**: brmsprior, trend_param
- **find_predictors()**: mvgam, mvgam_prefit
- **get_data()**: mvgam, mvgam_prefit
- **get_prior()**: brmsformula, default, formula, mvgam_formula
- **is()**: mvgam_trend, trend_param
- **plot()**: mvgam, mvgam_conditional_effects, mvgam_fevd, mvgam_forecast, mvgam_irf, mvgam_lfo, mvgam_residcor
- **Predict()**: matrix.mod.smooth, matrix.moi.smooth
- **print()**: brmsprior, how_to_cite, mvgam, mvgam_conditional_effects, mvgam_formula, mvgam_prefit, mvgam_trend, trend_param
- **residual_cor()**: jsdgam, mvgam
- **residuals()**: mvgam
- **smooth()**: construct.mod.smooth.spec, construct.moi.smooth.spec
- **summary()**: mvgam_fevd, mvgam_forecast, mvgam_irf

## Core Data Structures
- **mvgam object**: Fitted model with data, parameters, and predictions
- **mvgam_prefit**: Pre-compilation model structure
- **mvgam_forecast**: Forecast results with uncertainty
- **trend_param**: Trend model specifications

## Key Function Dependencies
- **mvgam()** calls: mvgam_multiple, mvgam_single_dataset
- **plot.mvgam()** calls: AR, mvgam, plot_mvgam_factors, plot_mvgam_fc, plot_mvgam_pterms, ...
- **sim_mvgam()** calls: GP, lkj_corr, periodic_gp, random_Sigma, RW, ...
- **lfo_cv.mvgam()** calls: cv_split, log_mean_exp, log_sum_exp, sum_rows, validate_pos_integer, ...

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

## File Organization
- **R/mvgam_core.R**: Core package functionality
- **R/sim_mvgam.R**
- **Validation**: R/validations.R
- **Plotting**: R/plot_mvgam_factors.R, R/plot.mvgam.R, R/plot_mvgam_uncertainty.R, ...
- **Stan/brms**: R/stan_assembly.R, R/brms_integration.R

