# Package Dependency Map

**Generated:** 2025-09-14 09:45:29  
**Package:** mvgam v2.0.0  
**Commit:** pending  

## Summary

- **Total Files:** 58
- **Total Functions:** 479
- **Exported Functions:** 68
- **Internal Functions:** 437
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
- **Plotting Functions**: plot_mvgam_series, plot_mvgam_smooth, plot_mvgam_pterms, plot_mvgam_factors, plot_mvgam_fc, plot_mvgam_trend, plot_mvgam_randomeffects, plot_mvgam_resids, plot_mvgam_uncertainty

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

### Priority Integration Functions (Stan/brms/Core/Validation)
- **insert_into_stan_block()** (`R/stan_assembly.R`)
  - Internal calls: detect_glm_usage, extract_and_rename_stan_blocks, extract_mapping_arrays, find_stan_block, find_trend_computation_end, generate_obs_trend_mapping, generate_trend_injection_code, inject_trend_into_glm_predictor, inject_trend_into_linear_predictor, insert_after_mu_lines_in_model_block, validate_mapping_arrays
- **generate_base_stancode_with_stanvars()** (`R/stan_assembly.R`)
  - Internal calls: sort_stanvars
- **prepare_mvgam_stancode()** (`R/stan_assembly.R`)
  - Internal calls: extract_response_names_from_brmsfit, generate_combined_stancode, validate_time_series_for_trends
- **inject_trend_into_glm_predictor()** (`R/stan_assembly.R`)
  - Internal calls: append_if_not_null, apply_response_suffix_to_stanvars, apply_safe_parameter_replacement, AR, calculate_car_time_distances, CAR, combine_stanvars, create_times_trend_matrix, detect_glm_usage, eval_silent, extract_and_rename_stan_blocks, extract_and_rename_standata_objects, extract_computed_variable_name, extract_computed_variables, extract_dependencies_from_declaration, extract_hierarchical_info, extract_mapping_arrays, extract_mu_construction_with_classification, extract_multivariate_standata, extract_non_likelihood_from_model_block, extract_response_names_from_brmsfit, extract_stan_block_content, extract_stan_functions_block, extract_stan_identifiers, extract_time_series_dimensions, extract_univariate_standata, filter_block_content, filter_renameable_identifiers, find_stan_block, find_trend_computation_end, find_variable_declarations, format_matrix_for_stan_array, generate_ar_trend_stanvars, generate_car_trend_stanvars, generate_combined_stancode, generate_common_trend_data, generate_factor_model, generate_hierarchical_correlation_model, generate_hierarchical_correlation_parameters, generate_hierarchical_functions, generate_innovation_model, generate_matrix_z_multiblock_stanvars, generate_matrix_z_parameters, generate_matrix_z_tdata, generate_obs_trend_mapping, generate_pw_trend_stanvars, generate_rw_trend_stanvars, generate_shared_innovation_stanvars, generate_times_trend_matrices, generate_trend_computation_tparameters, generate_trend_injection_code, generate_trend_priors_stanvar, generate_var_trend_stanvars, generate_zmvn_trend_stanvars, get_prior, get_stan_reserved_words, get_trend_parameter_prior, inject_trend_into_linear_predictor, insert_after_mu_lines_in_model_block, is_multivariate_brmsfit, normalize_function_body, normalize_function_signature, parse_glm_parameters, parse_stan_functions, PW, reconstruct_functions_block, reconstruct_mu_trend_with_renamed_vars, register_core_trends, remove_duplicate_functions, rename_parameters_in_block, replace_stan_functions_block, RW, should_include_in_transformed_parameters, transform_glm_call, validate_mapping_arrays, validate_no_factor_hierarchical, VAR, ZMVN
- **validate_mapping_arrays()** (`R/stan_assembly.R`)
  - Internal calls: generate_obs_trend_mapping
- **prepare_stan_data()** (`R/stan_assembly.R`)
  - Internal calls: validate_factor_levels

### Core Workflow Functions (Prior/Trend/Stan Assembly Systems)
- **mvgam_formula()** (`R/priors.R`) - No internal dependencies tracked
- **get_prior()** (`R/priors.R`) - No internal dependencies tracked
- **trend_param()** (`R/trend_system.R`) - No internal dependencies tracked
- **register_trend_type()** (`R/trend_system.R`) - No internal dependencies tracked
- **create_mvgam_trend()** (`R/trend_system.R`) - No internal dependencies tracked
- **mvgam()** (`R/mvgam_core.R`) - No internal dependencies tracked
- **sim_mvgam()** (`R/sim_mvgam.R`) - No internal dependencies tracked

### Most Connected Internal Functions
- **extract_code_block()** (`R/stan_assembly.R`) - 83 dependencies
  - Calls: append_if_not_null, apply_response_suffix_to_stanvars, apply_safe_parameter_replacement, AR, calculate_car_time_distances, CAR, combine_stanvars, create_times_trend_matrix, detect_glm_usage, eval_silent, extract_and_rename_stan_blocks, extract_and_rename_standata_objects, extract_and_rename_trend_parameters, extract_computed_variable_name, extract_computed_variables, extract_dependencies_from_declaration, extract_hierarchical_info, extract_mapping_arrays, extract_mu_construction_with_classification, extract_multivariate_standata, extract_non_likelihood_from_model_block, extract_response_names_from_brmsfit, extract_stan_block_content, extract_stan_functions_block, extract_stan_identifiers, extract_time_series_dimensions, extract_univariate_standata, filter_block_content, filter_renameable_identifiers, find_matching_brace, find_stan_block, find_trend_computation_end, find_variable_declarations, format_matrix_for_stan_array, generate_ar_trend_stanvars, generate_car_trend_stanvars, generate_combined_stancode, generate_common_trend_data, generate_factor_model, generate_hierarchical_correlation_model, generate_hierarchical_correlation_parameters, generate_hierarchical_functions, generate_innovation_model, generate_matrix_z_multiblock_stanvars, generate_matrix_z_parameters, generate_matrix_z_tdata, generate_obs_trend_mapping, generate_pw_trend_stanvars, generate_rw_trend_stanvars, generate_shared_innovation_stanvars, generate_times_trend_matrices, generate_trend_computation_tparameters, generate_trend_injection_code, generate_trend_priors_stanvar, generate_trend_specific_stanvars, generate_var_trend_stanvars, generate_zmvn_trend_stanvars, get_prior, get_stan_reserved_words, get_trend_parameter_prior, inject_trend_into_glm_predictor, inject_trend_into_linear_predictor, insert_after_mu_lines_in_model_block, is_multivariate_brmsfit, is_valid_stanvar, normalize_function_body, normalize_function_signature, parse_glm_parameters, parse_stan_functions, PW, reconstruct_functions_block, reconstruct_mu_trend_with_renamed_vars, register_core_trends, remove_duplicate_functions, rename_parameters_in_block, replace_stan_functions_block, RW, should_include_in_transformed_parameters, transform_glm_call, validate_mapping_arrays, validate_no_factor_hierarchical, VAR, ZMVN
- **inject_trend_into_glm_predictor()** (`R/stan_assembly.R`) - 78 dependencies
  - Calls: append_if_not_null, apply_response_suffix_to_stanvars, apply_safe_parameter_replacement, AR, calculate_car_time_distances, CAR, combine_stanvars, create_times_trend_matrix, detect_glm_usage, eval_silent, extract_and_rename_stan_blocks, extract_and_rename_standata_objects, extract_computed_variable_name, extract_computed_variables, extract_dependencies_from_declaration, extract_hierarchical_info, extract_mapping_arrays, extract_mu_construction_with_classification, extract_multivariate_standata, extract_non_likelihood_from_model_block, extract_response_names_from_brmsfit, extract_stan_block_content, extract_stan_functions_block, extract_stan_identifiers, extract_time_series_dimensions, extract_univariate_standata, filter_block_content, filter_renameable_identifiers, find_stan_block, find_trend_computation_end, find_variable_declarations, format_matrix_for_stan_array, generate_ar_trend_stanvars, generate_car_trend_stanvars, generate_combined_stancode, generate_common_trend_data, generate_factor_model, generate_hierarchical_correlation_model, generate_hierarchical_correlation_parameters, generate_hierarchical_functions, generate_innovation_model, generate_matrix_z_multiblock_stanvars, generate_matrix_z_parameters, generate_matrix_z_tdata, generate_obs_trend_mapping, generate_pw_trend_stanvars, generate_rw_trend_stanvars, generate_shared_innovation_stanvars, generate_times_trend_matrices, generate_trend_computation_tparameters, generate_trend_injection_code, generate_trend_priors_stanvar, generate_var_trend_stanvars, generate_zmvn_trend_stanvars, get_prior, get_stan_reserved_words, get_trend_parameter_prior, inject_trend_into_linear_predictor, insert_after_mu_lines_in_model_block, is_multivariate_brmsfit, normalize_function_body, normalize_function_signature, parse_glm_parameters, parse_stan_functions, PW, reconstruct_functions_block, reconstruct_mu_trend_with_renamed_vars, register_core_trends, remove_duplicate_functions, rename_parameters_in_block, replace_stan_functions_block, RW, should_include_in_transformed_parameters, transform_glm_call, validate_mapping_arrays, validate_no_factor_hierarchical, VAR, ZMVN
- **find_stan_block()** (`R/stan_assembly.R`) - 76 dependencies
  - Calls: append_if_not_null, apply_response_suffix_to_stanvars, apply_safe_parameter_replacement, AR, calculate_car_time_distances, CAR, combine_stanvars, create_times_trend_matrix, detect_glm_usage, eval_silent, extract_and_rename_stan_blocks, extract_and_rename_standata_objects, extract_computed_variable_name, extract_computed_variables, extract_dependencies_from_declaration, extract_hierarchical_info, extract_mapping_arrays, extract_mu_construction_with_classification, extract_multivariate_standata, extract_non_likelihood_from_model_block, extract_response_names_from_brmsfit, extract_stan_block_content, extract_stan_functions_block, extract_stan_identifiers, extract_time_series_dimensions, extract_univariate_standata, filter_block_content, filter_renameable_identifiers, find_trend_computation_end, find_variable_declarations, format_matrix_for_stan_array, generate_ar_trend_stanvars, generate_car_trend_stanvars, generate_combined_stancode, generate_common_trend_data, generate_factor_model, generate_hierarchical_correlation_model, generate_hierarchical_correlation_parameters, generate_hierarchical_functions, generate_innovation_model, generate_matrix_z_multiblock_stanvars, generate_matrix_z_parameters, generate_matrix_z_tdata, generate_obs_trend_mapping, generate_pw_trend_stanvars, generate_rw_trend_stanvars, generate_shared_innovation_stanvars, generate_times_trend_matrices, generate_trend_computation_tparameters, generate_trend_injection_code, generate_trend_priors_stanvar, generate_var_trend_stanvars, generate_zmvn_trend_stanvars, get_prior, get_stan_reserved_words, get_trend_parameter_prior, inject_trend_into_glm_predictor, inject_trend_into_linear_predictor, insert_after_mu_lines_in_model_block, is_multivariate_brmsfit, normalize_function_body, normalize_function_signature, parse_stan_functions, PW, reconstruct_functions_block, reconstruct_mu_trend_with_renamed_vars, register_core_trends, remove_duplicate_functions, rename_parameters_in_block, replace_stan_functions_block, RW, should_include_in_transformed_parameters, validate_mapping_arrays, validate_no_factor_hierarchical, VAR, ZMVN
- **generate_combined_stancode()** (`R/stan_assembly.R`) - 13 dependencies
  - Calls: AR, deduplicate_stan_functions, detect_shared_trends, extract_response_names, extract_trend_stanvars_from_setup, generate_base_brms_standata, generate_base_stancode_with_stanvars, inject_multivariate_trends_into_linear_predictors, inject_trend_into_linear_predictor, is_multivariate_trend_specs, RW, sort_stanvars, validate_stan_code
- **insert_into_stan_block()** (`R/stan_assembly.R`) - 11 dependencies
  - Calls: detect_glm_usage, extract_and_rename_stan_blocks, extract_mapping_arrays, find_stan_block, find_trend_computation_end, generate_obs_trend_mapping, generate_trend_injection_code, inject_trend_into_glm_predictor, inject_trend_into_linear_predictor, insert_after_mu_lines_in_model_block, validate_mapping_arrays
- **extract_trend_stanvars_from_setup()** (`R/stan_assembly.R`) - 5 dependencies
  - Calls: combine_stanvars, detect_glm_usage, extract_and_rename_trend_parameters, extract_time_series_dimensions, generate_trend_specific_stanvars

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
- **`R/all_neon_tick_data.R`** (0 functions) - Data documentation and loading
- **`R/portal_data.R`** (0 functions) - Data documentation and loading

### Validation Files
- **`R/validations.R`** (78 functions) - Input validation and checks
  - Key functions: is.mvgam_trend, validate_nonlinear_trend_compatibility, process_trend_validation_rules, validate_trend_grouping, validate_trend_correlation, validate_trend_time_intervals, validate_trend_factor_compatibility, validate_trend_hierarchical_structure, validate_factor_compatibility, validate_grouping_arguments, validate_correlation_requirements, validate_time_variable, validate_series_variable, validate_regular_time_intervals, validate_brms_formula (+ 63 more)

### Core Files
- **`R/mvgam_core.R`** (25 functions) - Core package functionality
  - Key functions: mvgam, generate_combined_stancode_and_data, create_mvgam_from_combined_fit, create_observation_brmsfit, create_trend_brmsfit, extract_trend_parameters, subset_stanfit_parameters, extract_trend_component_info, validate_multiple_imputation_datasets, validate_missing_patterns, create_pooled_mvgam (+ 14 more)
- **`R/marginaleffects.mvgam.R`** (10 functions) - Marginal effects
  - Key functions: get_coef.mvgam, set_coef.mvgam, get_vcov.mvgam, get_predict.mvgam, get_data.mvgam, error, get_data.mvgam_prefit, error, find_predictors.mvgam, find_predictors.mvgam_prefit
- **`R/as.data.frame.mvgam.R`** (10 functions) - Data documentation and loading
  - Key functions: validate_variablesas_draws.mvgam as_draws_matrix.mvgam as_draws_df.mvgam as_draws_array.mvgam as_draws_list.mvgam as_draws_rvars.mvgam as.data.frame.mvgam as.matrix.mvgam as.array.mvgam (+ 9 more)
- **`R/lfo_cv.mvgam.R`** (7 functions)
  - Key functions: lfo_cvlog_sum_exp log_mean_exp lfo_cv.mvgam plot.mvgam_lfo cv_split sum_rows (+ 6 more)
- **`R/loo.mvgam.R`** (6 functions)
  - Key functions: loo.mvgam, loo_compare.mvgam, split_mod_dots, named_list, clean_ll, samp_noinf
- **`R/print.mvgam.R`** (6 functions) - Print methods for objects
  - Key functions: stancode.mvgam, stancode.mvgam_prefit, print.mvgamstancode, print_model_specification_simple, print.mvgam, print.mvgam_prefit
- **`R/sim_mvgam.R`** (5 functions)
  - Key functions: sim_mvgamperiodic_gp lkj_corr sim_seasonal random_Sigma (+ 4 more)
- **`R/irf.mvgam.R`** (5 functions)
  - Key functions: irfirf.mvgam gen_irf var_phi var_psi (+ 4 more)
- **`R/ppc.mvgam.R`** (5 functions)
  - Key functions: ppc, pp_check.mvgam, ppc.mvgam, ecdf_plotdat, is_like_factor
- **`R/fevd.mvgam.R`** (4 functions)
  - Key functions: fevdfevd.mvgam gen_fevd var_fecov (+ 3 more)
- **`R/interpret_mvgam.R`** (3 functions)
  - Key functions: interpret_mvgam, dyn_to_gpspline, dyn_to_gphilbert
- **`R/plot.mvgam.R`** (2 functions) - Plotting and visualization
  - Key functions: plot.mvgam, plottable
- **`R/series_to_mvgam.R`** (2 functions)
  - Key functions: series_to_mvgamxts.to.ts (+ 1 more)
- **`R/pairs.mvgam.R`** (1 functions)
  - Key functions: pairs.mvgam
- **`R/mcmc_plot.mvgam.R`** (1 functions) - Plotting and visualization
  - Key functions: mcmc_plot.mvgam
- **`R/residuals.mvgam.R`** (1 functions) - Residual analysis
  - Key functions: residuals.mvgam
- **`R/index-mvgam.R`** (1 functions)
  - Key functions: variables.mvgam

### Other Files
- **`R/trend_system.R`** (70 functions) - Trend modeling
  - Key functions: register_trend_type, list_trend_types, register_custom_trend, trend_param, is.trend_param, mvgam_trend_choices, custom_trend, create_mvgam_trend, RW, AR, CAR, VAR, GP, PW, ZMVN (+ 55 more)
- **`R/priors.R`** (33 functions) - Prior specification
  - Key functions: mvgam_formula, get_prior, has_trend_priors, extract_trend_priors_only, extract_trend_priors_from_enhanced, extract_observation_priors_from_enhanced, extract_observation_priors_only, add_trend_component_attr, extract_observation_priors, extract_trend_priors, generate_trend_priors, generate_trend_priors_from_monitor_params, create_trend_parameter_prior, get_default_trend_parameter_prior, get_parameter_type_default_prior (+ 18 more)
- **`R/mu_expression_analysis.R`** (25 functions)
  - Key functions: create_empty_mu_result, create_analysis_context, check_loop_requirement, build_execution_dependency_plan, extract_mu_construction_with_classification, extract_mu_assignment_lines, classify_mu_expressions_structurally, extract_declared_functions, classify_single_mu_expression_structurally, normalize_mu_expression (+ 15 more)
- **`R/backends.R`** (6 functions)
  - Key functions: repair_stanfitrepair_names repair_variable_names is_equal ulapply seq_rows (+ 5 more)
- **`R/conditional_effects.R`** (6 functions) - Conditional effects
  - Key functions: decimalplaces, print.mvgam_conditional_effects, conditional_effects.mvgam, plot.mvgam_conditional_effects, roundlabs, split_termlabs
- **`R/monotonic.R`** (5 functions)
  - Key functions: smooth.construct.moi.smooth.spec, smooth.construct.mod.smooth.spec, Predict.matrix.moi.smooth, Predict.matrix.mod.smooth, add_mono_model_file
- **`R/mvgam_residcor-class.R`** (4 functions) - Class definitions and methods
  - Key functions: gather_matrix, cluster_cormat, reorder_clusters, plot.mvgam_residcor
- **`R/ensemble.R`** (4 functions)
  - Key functions: ensembleensemble.mvgam_forecast allsame split_fc_dots (+ 3 more)
- **`R/mvgam_fevd-class.R`** (3 functions) - Class definitions and methods
  - Key functions: summary.mvgam_fevd, plot.mvgam_fevd, fevd_df
- **`R/how_to_cite.R`** (3 functions)
  - Key functions: how_to_citeprint.how_to_cite how_to_cite.mvgam (+ 2 more)
- **`R/residual_cor.R`** (3 functions) - Residual analysis
  - Key functions: residual_corresidual_cor.mvgam residual_cor.jsdgam (+ 2 more)
- **`R/tidier_methods.R`** (3 functions) - Method implementations
  - Key functions: tidy.mvgam, split_hier_Sigma, augment.mvgam
- **`R/RcppExports.R`** (3 functions) - Exported functions
  - Key functions: ar3_recursC, var1_recursC, varma_recursC
- **`R/stability.R`** (2 functions)
  - Key functions: stabilitystability.mvgam (+ 1 more)
- **`R/zzz.R`** (2 functions) - Package startup and attachment
  - Key functions: core_unloaded, mvgam_attach
- **`R/mvgam_irf-class.R`** (2 functions) - Class definitions and methods
  - Key functions: summary.mvgam_irf, plot.mvgam_irf
- **`R/ordinate.jsdgam.R`** (2 functions)
  - Key functions: ordinateordinate.jsdgam (+ 1 more)
- **`R/dynamic.R`** (1 functions)
  - Key functions: dynamic
- **`R/mvgam_forecast-class.R`** (1 functions) - Class definitions and methods
  - Key functions: summary.mvgam_forecast
- **`R/lv_correlations.R`** (1 functions)
  - Key functions: lv_correlations
- **`R/cpp_funs.R`** (0 functions)
- **`R/mvgam-package.R`** (0 functions)
- **`R/mvgam-class.R`** (0 functions) - Class definitions and methods

### S3 Methods Files
- **`R/add_residuals.R`** (2 functions) - Residual analysis
  - Key functions: add_residualsadd_residuals.mvgam (+ 1 more)

### Stan/Modeling Files
- **`R/stan_assembly.R`** (90 functions) - Stan model integration
  - Key functions: apply_response_suffix_to_stanvars, generate_combined_stancode, generate_base_stancode_with_stanvars, prepare_mvgam_stancode, prepare_stan_data, extract_code_block, prepare_stanvars_for_brms, extract_trend_stanvars_from_setup, inject_trend_into_glm_predictor, validate_mapping_arrays, find_stan_block, insert_into_stan_block, apply_suffix_to_stan_code, detect_shared_trends, find_trend_computation_end (+ 75 more)
- **`R/brms_integration.R`** (18 functions) - brms integration
  - Key functions: setup_brms_lightweight, extract_prior_from_setup, extract_brmsterms_from_setup, parse_multivariate_trends, extract_response_trends, create_trend_base_formula, determine_trend_injection_point, modify_stancode_for_nonlinear, is_multivariate_formula, has_mvbind_response (+ 8 more)
- **`R/make_stan.R`** (3 functions) - Stan model integration
  - Key functions: generate_stan_components_mvgam_formula, stancode.mvgam_formula, standata.mvgam_formula

### Plotting Files
- **`R/plot_mvgam_series.R`** (6 functions) - Plotting and visualization
  - Key functions: plot_mvgam_series, validate_plot_data, plot_time_series, plot_histogram, plot_acf, plot_ecdf
- **`R/plot_mvgam_uncertainty.R`** (2 functions) - Plotting and visualization
  - Key functions: plot_mvgam_uncertaintyintersect_hist (+ 1 more)
- **`R/plot_mvgam_fc.R`** (2 functions) - Plotting and visualization
  - Key functions: plot_mvgam_fcplot.mvgam_forecast (+ 1 more)
- **`R/plot_mvgam_smooth.R`** (1 functions) - Plotting and visualization
  - Key functions: plot_mvgam_smooth
- **`R/plot_mvgam_resids.R`** (1 functions) - Plotting and visualization
  - Key functions: plot_mvgam_resids
- **`R/plot_mvgam_pterms.R`** (1 functions) - Plotting and visualization
  - Key functions: plot_mvgam_pterms
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

## System-Specific Function Breakdowns

### Prior System Functions
- **build_ar_prior_spec()** (`R/priors.R`)
- **combine_obs_trend_priors()** (`R/priors.R`)
- **create_empty_brmsprior()** (`R/priors.R`)
- **create_trend_parameter_prior()** (`R/priors.R`)
- **extract_observation_priors()** (`R/priors.R`)
- **extract_observation_priors_from_enhanced()** (`R/priors.R`)
- **extract_observation_priors_only()** (`R/priors.R`)
- **extract_prior_string()** (`R/priors.R`)

### Trend System Functions
- **apply_mvgam_trend_defaults()** (`R/trend_system.R`)
- **ar_trend_properties()** (`R/trend_system.R`)
- **auto_register_trend_types()** (`R/trend_system.R`)
- **build_trend_label()** (`R/trend_system.R`)
- **c.trend_param()** (`R/trend_system.R`)
- **car_trend_properties()** (`R/trend_system.R`)
- **create_mvgam_trend()** (`R/trend_system.R`)
- **create_trend_brmsfit()** (`R/mvgam_core.R`)

### Stan Assembly System Functions
- **apply_response_suffix_to_stanvars()** (`R/stan_assembly.R`) - calls: apply_suffix_to_name, apply_suffix_to_stan_code
- **apply_suffix_to_stan_code()** (`R/stan_assembly.R`)
- **calculate_car_time_distances()** (`R/stan_assembly.R`)
- **combine_stanvars()** (`R/stan_assembly.R`)
- **deduplicate_stan_functions()** (`R/stan_assembly.R`)
- **extract_and_rename_stan_blocks()** (`R/stan_assembly.R`)
- **extract_and_rename_standata_objects()** (`R/stan_assembly.R`)
- **extract_code_block()** (`R/stan_assembly.R`) - calls: append_if_not_null, apply_response_suffix_to_stanvars, apply_safe_parameter_replacement, ...

## Notes for LLM Agents
- **Primary workflow**: `mvgam()` -> prior/trend system setup -> Stan assembly -> model fitting -> S3 methods
- **Prior system**: `mvgam_formula()`, `get_prior()`, `set_prior()` for Bayesian prior specification
- **Trend system**: `trend_param()`, `register_trend_type()` for time series trend modeling
- **Stan assembly**: `stancode()`, `standata()` for Stan model compilation and data preparation
- **Stan integration**: Package heavily relies on Stan/brms for Bayesian computation
- **S3 system**: Extensive use of S3 classes (`mvgam`, `mvgam_forecast`, etc.) with method dispatch
- **File patterns**: `R/priors.R` (prior system), `R/trend_system.R` (trends), `R/stan_assembly.R` (Stan code)
- **Core functions often call**: validation functions, Stan code generation, data preprocessing utilities

