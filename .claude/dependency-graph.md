# Package Dependency Map

**Generated:** 2025-08-25 22:02:37  
**Package:** mvgam v2.0.0  
**Commit:** test789  

## Summary

- **Total Files:** 56
- **Total Functions:** 399
- **Exported Functions:** 66
- **Internal Functions:** 359
- **S3 Methods:** 41
- **S3 Classes:** 20

## User Interface (Exported Functions)

### Main Entry Points
- **Core Functions**: mvgam, series_to_mvgam, sim_mvgam
- **Trend Types**: RW, GP, PW, VAR, CAR, ZMVN, AR
- **Analysis Tools**: stability, fevd, ppc, irf, lfo_cv
- **Plotting Functions**: plot_mvgam_series, plot_mvgam_smooth, plot_mvgam_factors, plot_mvgam_fc, plot_mvgam_trend, plot_mvgam_pterms, plot_mvgam_randomeffects, plot_mvgam_resids, plot_mvgam_uncertainty

## S3 Object System

### S3 Classes
- array.mvgam, construct.mod.smooth.spec, construct.moi.smooth.spec, data.frame.mvgam, how_to_cite, jsdgam, matrix.mod.smooth, matrix.moi.smooth, matrix.mvgam, mvgam, mvgam_conditional_effects, mvgam_fevd, mvgam_forecast, mvgam_irf, mvgam_lfo, mvgam_prefit, mvgam_residcor, mvgam_trend, to.ts, trend_param

### S3 Methods
- **add_residuals()**: mvgam
- **as()**: array.mvgam, data.frame.mvgam, matrix.mvgam
- **as_draws()**: mvgam
- **as_draws_array()**: mvgam
- **as_draws_df()**: mvgam
- **as_draws_list()**: mvgam
- **as_draws_matrix()**: mvgam
- **as_draws_rvars()**: mvgam
- **augment()**: mvgam
- **c()**: trend_param
- **conditional_effects()**: mvgam
- **ensemble()**: mvgam_forecast
- **fevd()**: mvgam
- **find_predictors()**: mvgam, mvgam_prefit
- **get_coef()**: mvgam
- **get_data()**: mvgam, mvgam_prefit
- **get_predict()**: mvgam
- **get_vcov()**: mvgam
- **how_to_cite()**: mvgam
- **irf()**: mvgam
- **is()**: mvgam_trend, trend_param
- **lfo_cv()**: mvgam
- **loo()**: mvgam
- **loo_compare()**: mvgam
- **mcmc_plot()**: mvgam
- **ordinate()**: jsdgam
- **pairs()**: mvgam
- **plot()**: mvgam, mvgam_conditional_effects, mvgam_fevd, mvgam_forecast, mvgam_irf, mvgam_lfo, mvgam_residcor
- **pp_check()**: mvgam
- **ppc()**: mvgam
- **Predict()**: matrix.mod.smooth, matrix.moi.smooth
- **print()**: how_to_cite, mvgam, mvgam_conditional_effects, mvgam_prefit, mvgam_trend, trend_param
- **residual_cor()**: jsdgam, mvgam
- **residuals()**: mvgam
- **set_coef()**: mvgam
- **smooth()**: construct.mod.smooth.spec, construct.moi.smooth.spec
- **stability()**: mvgam
- **summary()**: mvgam_fevd, mvgam_forecast, mvgam_irf
- **tidy()**: mvgam
- **variables()**: mvgam
- **xts()**: to.ts

## Key Function Signatures

### `mvgam`:
```r
mvgam <- function(formula, trend_formula = NULL, data = NULL, backend = getOption("brms.backend", "cmdstanr"), combine = TRUE, family = poisson(), ...) {
```
**Arguments**: formula, trend_formula, data, backend

### `plot.mvgam`:
```r
plot.mvgam = function( x, type = 'residuals', series = 1, residuals = FALSE, newdata, data_test, trend_effects = FALSE, ... ) {
```
**Arguments**: x, type, series, residuals, newdata, data_test, trend_effects
**S3 Method**: plot for class mvgam

### `conditional_effects.mvgam`:
```r
conditional_effects.mvgam = function( x, effects = NULL, type = 'expected', points = FALSE, rug = FALSE, ... ) {
```
**Arguments**: x, effects, type, points, rug
**S3 Method**: conditional_effects for class mvgam

### `sim_mvgam`:
```r
sim_mvgam = function( T = 100, n_series = 3, seasonality = 'shared', use_lv = FALSE, n_lv = 0, trend_model = RW(), drift = FALSE, prop_trend = 0.2, trend_rel, freq = 12, family = poisson(), phi, shape, sigma, nu, mu, prop_missing = 0, prop_train = 0.85 ) {
```
**Arguments**: T, n_series, seasonality, use_lv, n_lv, trend_model

### `lfo_cv.mvgam`:
```r
lfo_cv.mvgam = function( object, data, min_t, fc_horizon = 1, pareto_k_threshold = 0.7, silent = 1, ... ) {
```
**Arguments**: object, data, min_t, fc_horizon, pareto_k_threshold, silent
**S3 Method**: lfo_cv for class mvgam


## Function Definitions by File

### R/tidier_methods.R

tidy.mvgam, split_hier_Sigma, augment.mvgam

### R/stability.R

stability, stability.mvgam

### R/mvgam_forecast-class.R

summary.mvgam_forecast

### R/plot_mvgam_factors.R

plot_mvgam_factors

### R/validations.R

validate_nonlinear_trend_compatibility, apply_validation_rules, process_trend_validation_rules, dispatch_validation_rule, get_validation_rule_dispatch_table, validate_trend_grouping, validate_trend_correlation, validate_trend_time_intervals, validate_trend_factor_compatibility, validate_trend_hierarchical_structure, validate_factor_compatibility, validate_grouping_arguments, validate_correlation_requirements, validate_time_variable, validate_series_variable, validate_regular_time_intervals, deparse0, is_nonlinear_formula, validate_brms_formula, formula2str_mvgam, get_trend_validation_patterns, validate_obs_formula_brms, validate_trend_formula_brms, validate_bf_trend_formula, extract_all_bf_formulas, validate_list_trend_formula, validate_single_trend_formula, check_brms_autocor_usage, validate_no_brms_autocor_in_trends, validate_no_offsets_in_trends, validate_offsets_in_obs, validate_multivariate_trend_constraints, validate_autocor_separation, validate_setup_components, validate_time_series_for_trends, is.mvgam_trend, validate_trend_components, extract_time_series_dimensions, warn_default_time_variable, warn_default_series_variable, validate_mvgam_trend, validate_proportional, validate_pos_integer, validate_pos_real, eval_silent, is_try_error, is_multivariate_trend_specs, validate_factor_levels, validate_stan_code_structure, are_braces_balanced, validate_stan_code_fragment, validate_combined_stancode, validate_data_code_compatibility, check_semicolon_syntax, check_block_semicolons, validate_stan_code, validate_stan_data_structure, is_valid_stanvar, validate_trend_specs, validate_and_process_trend_parameters, process_lag_parameters, process_capacity_parameter, validate_no_factor_hierarchical, validate_series_time, all_times_avail, as_one_logical, validate_grouping_structure

### R/plot.mvgam.R

plot.mvgam, plottable

### R/loo.mvgam.R

loo.mvgam, loo_compare.mvgam, split_mod_dots, named_list, clean_ll, samp_noinf

### R/lfo_cv.mvgam.R

lfo_cv, log_sum_exp, log_mean_exp, lfo_cv.mvgam, plot.mvgam_lfo, cv_split, sum_rows

### R/zzz.R

core_unloaded, mvgam_attach

### R/stan_assembly.R

apply_response_suffix_to_stanvars, apply_suffix_to_name, apply_suffix_to_stan_code, generate_combined_stancode, generate_base_stancode_with_stanvars, prepare_mvgam_stancode, prepare_stan_data, extract_code_block, find_matching_brace, prepare_stanvars_for_brms, extract_trend_stanvars_from_setup, inject_trend_into_linear_predictor, inject_multivariate_trends_into_linear_predictors, assemble_mvgam_stan_code, assemble_mvgam_stan_data, generate_base_brms_stancode, generate_base_brms_standata, append_if_not_null, combine_stanvars, generate_shared_innovation_stanvars, generate_innovation_model, extract_hierarchical_info, generate_common_trend_data, generate_matrix_z_parameters, generate_matrix_z_tdata, generate_matrix_z_multiblock_stanvars, generate_factor_model, generate_trend_computation_tparameters, generate_hierarchical_functions, generate_hierarchical_correlation_parameters, generate_hierarchical_correlation_model, generate_trend_injection_stanvars, generate_rw_trend_stanvars, generate_trend_priors_stanvar, generate_ar_trend_stanvars, generate_var_trend_stanvars, calculate_car_time_distances, generate_car_trend_stanvars, generate_zmvn_trend_stanvars, generate_pw_trend_stanvars, parse_model_rstan, parse_model_cmdstanr, parse_data_declarations, extract_and_rename_trend_parameters, is_multivariate_brmsfit, extract_response_names_from_brmsfit, extract_and_rename_stan_blocks, extract_non_likelihood_from_model_block, extract_stan_block, rename_parameters_in_block, get_stan_reserved_words, extract_stan_identifiers, filter_renameable_identifiers, apply_safe_parameter_replacement, rename_univariate_parameters, rename_multivariate_parameters, extract_and_rename_standata_objects, extract_univariate_standata, extract_multivariate_standata, generate_times_trend_matrices, create_times_trend_matrix, generate_stan_array_declaration, format_matrix_for_stan_array

### R/plot_mvgam_uncertainty.R

plot_mvgam_uncertainty, intersect_hist

### R/fevd.mvgam.R

fevd, fevd.mvgam, gen_fevd, var_fecov

### R/mvgam_fevd-class.R

summary.mvgam_fevd, plot.mvgam_fevd, fevd_df

### R/ppc.mvgam.R

ppc, ppc.mvgam, ecdf_plotdat, pp_check.mvgam, is_like_factor

### R/plot_mvgam_fc.R

plot_mvgam_fc, plot.mvgam_forecast

### R/plot_mvgam_trend.R

plot_mvgam_trend

### R/backends.R

repair_names, repair_stanfit, repair_variable_names, is_equal, ulapply, seq_rows

### R/pairs.mvgam.R

pairs.mvgam

### R/plot_mvgam_series.R

plot_mvgam_series, validate_plot_data, plot_time_series, plot_histogram, plot_acf, plot_ecdf

### R/brms_integration.R

setup_brms_lightweight, extract_prior_from_setup, extract_brmsterms_from_setup, parse_multivariate_trends, is_multivariate_formula, has_mvbind_response, extract_response_names, extract_mvbind_responses, extract_variable_name, extract_response_trends, create_trend_base_formula, handle_nonlinear_model, extract_nonlinear_components, parse_nonlinear_manually, determine_trend_injection_point, extract_response_from_formula, modify_stancode_for_nonlinear, integrate_nonlinear_with_assembly

### R/priors.R

has_trend_priors, extract_trend_priors_only, extract_observation_priors_only, add_trend_component_attr, extract_observation_priors, extract_trend_priors, generate_trend_priors, generate_trend_priors_from_monitor_params, create_trend_parameter_prior, get_default_trend_parameter_prior, get_parameter_type_default_prior, create_empty_brmsprior, get_ar_parameter_prior, get_car_parameter_prior, combine_obs_trend_priors, get_trend_prior_spec, build_ar_prior_spec, map_prior_to_stan_string, extract_prior_string, get_best_prior_match, map_trend_priors, get_trend_parameter_prior

### R/RcppExports.R

ar3_recursC, var1_recursC, varma_recursC

### R/mvgam_irf-class.R

summary.mvgam_irf, plot.mvgam_irf

### R/plot_mvgam_smooth.R

plot_mvgam_smooth

### R/residuals.mvgam.R

residuals.mvgam

### R/irf.mvgam.R

irf, irf.mvgam, gen_irf, var_phi, var_psi

### R/marginaleffects.mvgam.R

get_coef.mvgam, set_coef.mvgam, get_vcov.mvgam, get_predict.mvgam, get_data.mvgam, error, get_data.mvgam_prefit, error, find_predictors.mvgam, find_predictors.mvgam_prefit

### R/conditional_effects.R

decimalplaces, print.mvgam_conditional_effects, conditional_effects.mvgam, plot.mvgam_conditional_effects, roundlabs, split_termlabs

### R/how_to_cite.R

how_to_cite, print.how_to_cite, how_to_cite.mvgam

### R/lv_correlations.R

lv_correlations

### R/series_to_mvgam.R

series_to_mvgam, xts.to.ts

### R/mvgam_core.R

mvgam, mvgam_single_dataset, generate_combined_stancode_and_data, fit_mvgam_model, create_mvgam_from_combined_fit, create_observation_brmsfit, create_trend_brmsfit, extract_observation_parameters, extract_trend_parameters, subset_stanfit_parameters, extract_mvgam_components, extract_time_information, extract_series_information, extract_trend_component_info, mvgam_multiple, validate_multiple_imputation_datasets, validate_missing_patterns, fit_multiple_imputation_models, mvgam_single_imputation, pool_mvgam_fits, extract_fit_estimates, apply_rubins_rules, pool_parameter_estimates, create_pooled_mvgam, extract_pooling_diagnostics

### R/mcmc_plot.mvgam.R

mcmc_plot.mvgam

### R/plot_mvgam_resids.R

plot_mvgam_resids

### R/ensemble.R

ensemble, ensemble.mvgam_forecast, allsame, split_fc_dots

### R/ordinate.jsdgam.R

ordinate, ordinate.jsdgam

### R/index-mvgam.R

variables.mvgam

### R/interpret_mvgam.R

interpret_mvgam, dyn_to_gpspline, dyn_to_gphilbert

### R/mvgam_residcor-class.R

gather_matrix, cluster_cormat, reorder_clusters, plot.mvgam_residcor

### R/dynamic.R

dynamic

### R/as.data.frame.mvgam.R

as_draws.mvgam, as_draws_matrix.mvgam, as_draws_df.mvgam, as_draws_array.mvgam, as_draws_list.mvgam, as_draws_rvars.mvgam, as.data.frame.mvgam, as.matrix.mvgam, as.array.mvgam, validate_variables

### R/print.mvgam.R

print_model_specification_simple, print.mvgam, print.mvgam_prefit

### R/trend_system.R

register_trend_type, get_trend_info, list_trend_types, get_factor_compatible_trends, get_default_incompatibility_reason, auto_register_trend_types, register_core_trends, validate_trend_properties, ar_trend_properties, rw_trend_properties, var_trend_properties, zmvn_trend_properties, car_trend_properties, pw_trend_properties, register_custom_trend, is_registry_initialized, ensure_registry_initialized, trend_param, c.trend_param, is.trend_param, print.trend_param, evaluate_param_conditions, generate_monitor_params, generate_rw_monitor_params, generate_ar_monitor_params, generate_var_monitor_params, generate_car_monitor_params, generate_zmvn_monitor_params, generate_pw_monitor_params, normalize_trend_type, add_monitor_params, generate_forecast_metadata, generate_forecast_required_params, filter_rw_forecast_params, filter_ar_forecast_params, filter_var_forecast_params, filter_car_forecast_params, filter_zmvn_forecast_params, filter_pw_forecast_params, add_forecast_metadata, generate_summary_labels, generate_parameter_label, add_summary_labels, add_complete_metadata, process_trend_params, mvgam_trend_registry, mvgam_trend_choices, mvgam_trend_pattern, build_trend_label, custom_trend, find_trend_terms, extract_regular_terms, parse_trend_formula, eval_trend_constructor, print.mvgam_trend, get_mvgam_trend_defaults, apply_mvgam_trend_defaults, get_default_validation_rules, create_mvgam_trend, process_arg, validate_trend_dispatch_consistency, get_trend_dispatch_function, add_consistent_dispatch_metadata, RW, AR, CAR, VAR, GP, PW, ZMVN

### R/residual_cor.R

residual_cor, residual_cor.mvgam, residual_cor.jsdgam

### R/plot_mvgam_pterms.R

plot_mvgam_pterms

### R/add_residuals.R

add_residuals, add_residuals.mvgam

### R/sim_mvgam.R

periodic_gp, lkj_corr, sim_mvgam, sim_seasonal, random_Sigma

### R/monotonic.R

smooth.construct.moi.smooth.spec, smooth.construct.mod.smooth.spec, Predict.matrix.moi.smooth, Predict.matrix.mod.smooth, add_mono_model_file

### R/plot_mvgam_randomeffects.R

plot_mvgam_randomeffects

