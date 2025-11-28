# Package Dependency Map

**Generated:** 2025-11-28 14:34:05  
**Package:** mvgam v2.0.0  
**Commit:** pending  

## Summary

- **Total Files:** 60
- **Total Functions:** 525
- **Exported Functions:** 57
- **Internal Functions:** 482
- **S3 Methods:** 41
- **S3 Classes:** 31

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
- **Analysis Tools**: ppc, stability, fevd, irf, lfo_cv
- **Plotting Functions**: plot_mvgam_fc, plot_mvgam_series, plot_mvgam_smooth, plot_mvgam_pterms, plot_mvgam_factors, plot_mvgam_trend, plot_mvgam_randomeffects, plot_mvgam_resids, plot_mvgam_uncertainty

## S3 Object System

### S3 Classes
- array.mvgam, brmsformula, brmsopencl, brmsthreads, construct.mod.smooth.spec, construct.moi.smooth.spec, data.frame.mvgam, default, formula, how_to_cite, jsdgam, matrix.mod.smooth, matrix.moi.smooth, matrix.mvgam, mock_stanfit, mvgam, mvgam_fevd, mvgam_forecast, mvgam_formula, mvgam_irf, mvgam_lfo, mvgam_pooled, mvgam_pooled_summary, mvgam_prefit, mvgam_residcor, mvgam_summary, mvgam_trend, mvgamstancode, stanfit, to.ts, trend_param

### Key S3 Methods
- **as()**: array.mvgam, data.frame.mvgam, matrix.mvgam
- **as_draws_matrix()**: mock_stanfit, mvgam
- **get_prior()**: brmsformula, default, formula, mvgam_formula
- **is()**: brmsopencl, brmsthreads, mvgam, mvgam_trend, stanfit, trend_param
- **plot()**: mvgam, mvgam_fevd, mvgam_forecast, mvgam_irf, mvgam_lfo, mvgam_residcor
- **Predict()**: matrix.mod.smooth, matrix.moi.smooth
- **print()**: how_to_cite, mvgam, mvgam_formula, mvgam_pooled_summary, mvgam_prefit, mvgam_summary, mvgam_trend, mvgamstancode, trend_param
- **residual_cor()**: jsdgam, mvgam
- **residuals()**: mvgam
- **smooth()**: construct.mod.smooth.spec, construct.moi.smooth.spec
- **stancode()**: mvgam, mvgam_formula, mvgam_prefit
- **summary()**: mvgam, mvgam_fevd, mvgam_forecast, mvgam_irf, mvgam_pooled

## Core Data Structures
- **mvgam object**: Fitted model with data, parameters, and predictions
- **mvgam_prefit**: Pre-compilation model structure
- **mvgam_forecast**: Forecast results with uncertainty
- **trend_param**: Trend model specifications

## Function Dependencies & Architecture

### Priority Integration Functions (Stan/brms/Core/Validation)
- **extract_response_names()** (`R/brms_integration.R`)
  - Internal calls: extract_mvbind_responses
- **extract_prior_from_setup()** (`R/brms_integration.R`)
  - Internal calls: get_prior
- **extract_prior_string()** (`R/priors.R`)
  - Internal calls: get_best_prior_match
- **extract_mvgam_components()** (`R/mvgam_core.R`)
  - Internal calls: extract_series_information, extract_time_information, extract_trend_component_info
- **validate_obs_formula_brms()** (`R/validations.R`)
  - Internal calls: formula2str_mvgam, get_trend_validation_patterns, mvgam, RW
- **mvgam_formula()** (`R/priors.R`)
  - Internal calls: validate_single_trend_formula

### Core Workflow Functions (Prior/Trend/Stan Assembly Systems)
- **mvgam_formula()** (`R/priors.R`)
  - Internal calls: validate_single_trend_formula
- **get_prior()** (`R/priors.R`) - No internal dependencies tracked
- **trend_param()** (`R/trend_system.R`) - No internal dependencies tracked
- **register_trend_type()** (`R/trend_system.R`)
  - Internal calls: get_default_incompatibility_reason
- **create_mvgam_trend()** (`R/trend_system.R`) - No internal dependencies tracked
- **mvgam()** (`R/mvgam_core.R`)
  - Internal calls: mvgam_multiple, mvgam_single
- **sim_mvgam()** (`R/sim_mvgam.R`) - No internal dependencies tracked

### Most Connected Internal Functions
- **find_stan_block()** (`R/stan_assembly.R`) - 81 dependencies
  - Calls: add_hierarchical_support, append_if_not_null, apply_correct_transformation_order, apply_response_suffix_to_stanvars, apply_safe_parameter_replacement, AR, calculate_car_time_distances, CAR, combine_stanvars, convert_glm_to_standard_form, create_times_trend_matrix, detect_glm_usage, extract_and_rename_stan_blocks, extract_and_rename_standata_objects, extract_computed_variables, extract_dependencies_from_declaration, extract_hierarchical_info, extract_mapping_arrays, extract_mu_construction_with_classification, extract_multivariate_standata, extract_non_likelihood_from_model_block, extract_response_names_from_brmsfit, extract_stan_block_content, extract_stan_functions_block, extract_stan_identifiers, extract_time_series_dimensions, extract_univariate_standata, filter_block_content, filter_renameable_identifiers, find_prior_only_insertion_point, find_trend_computation_end, find_variable_declarations, generate_ar_trend_stanvars, generate_car_trend_stanvars, generate_common_trend_data, generate_factor_model, generate_hierarchical_correlation_model, generate_hierarchical_correlation_parameters, generate_hierarchical_data_structures, generate_hierarchical_functions, generate_innovation_model, generate_matrix_z_multiblock_stanvars, generate_matrix_z_parameters, generate_matrix_z_tdata, generate_obs_trend_mapping, generate_pw_trend_stanvars, generate_rw_trend_stanvars, generate_shared_innovation_stanvars, generate_times_trend_matrices, generate_trend_computation_tparameters, generate_trend_injection_code, generate_trend_priors_stanvar, generate_var_trend_stanvars, generate_zmvn_trend_stanvars, get_prior, get_stan_reserved_words, get_trend_parameter_prior, handle_nonlinear_trend_injection, handle_response_trend_injection, inject_trend_into_linear_predictor, insert_after_mu_lines_in_model_block, is_multivariate_brmsfit, normalize_function_body, normalize_function_signature, parse_stan_functions, process_variable, PW, reconstruct_functions_block, reconstruct_mu_trend_with_renamed_vars, register_core_trends, remove_duplicate_functions, rename_parameters_in_block, replace_stan_functions_block, RW, should_include_in_transformed_parameters, transform_glm_call, transform_glm_code, validate_mapping_arrays, validate_no_factor_hierarchical, VAR, ZMVN
- **detect_glm_usage()** (`R/stan_assembly.R`) - 81 dependencies
  - Calls: add_hierarchical_support, append_if_not_null, apply_correct_transformation_order, apply_response_suffix_to_stanvars, apply_safe_parameter_replacement, AR, calculate_car_time_distances, CAR, combine_stanvars, convert_glm_to_standard_form, create_times_trend_matrix, extract_and_rename_stan_blocks, extract_and_rename_standata_objects, extract_computed_variables, extract_dependencies_from_declaration, extract_hierarchical_info, extract_mapping_arrays, extract_mu_construction_with_classification, extract_multivariate_standata, extract_non_likelihood_from_model_block, extract_response_names_from_brmsfit, extract_stan_block_content, extract_stan_functions_block, extract_stan_identifiers, extract_time_series_dimensions, extract_univariate_standata, filter_block_content, filter_renameable_identifiers, find_prior_only_insertion_point, find_stan_block, find_trend_computation_end, find_variable_declarations, generate_ar_trend_stanvars, generate_car_trend_stanvars, generate_common_trend_data, generate_factor_model, generate_hierarchical_correlation_model, generate_hierarchical_correlation_parameters, generate_hierarchical_data_structures, generate_hierarchical_functions, generate_innovation_model, generate_matrix_z_multiblock_stanvars, generate_matrix_z_parameters, generate_matrix_z_tdata, generate_obs_trend_mapping, generate_pw_trend_stanvars, generate_rw_trend_stanvars, generate_shared_innovation_stanvars, generate_times_trend_matrices, generate_trend_computation_tparameters, generate_trend_injection_code, generate_trend_priors_stanvar, generate_var_trend_stanvars, generate_zmvn_trend_stanvars, get_prior, get_stan_reserved_words, get_trend_parameter_prior, handle_nonlinear_trend_injection, handle_response_trend_injection, inject_trend_into_linear_predictor, insert_after_mu_lines_in_model_block, is_multivariate_brmsfit, normalize_function_body, normalize_function_signature, parse_stan_functions, process_variable, PW, reconstruct_functions_block, reconstruct_mu_trend_with_renamed_vars, register_core_trends, remove_duplicate_functions, rename_parameters_in_block, replace_stan_functions_block, RW, should_include_in_transformed_parameters, transform_glm_call, transform_glm_code, validate_mapping_arrays, validate_no_factor_hierarchical, VAR, ZMVN
- **find_prior_only_insertion_point()** (`R/stan_assembly.R`) - 15 dependencies
  - Calls: apply_correct_transformation_order, convert_glm_to_standard_form, detect_glm_usage, extract_mapping_arrays, find_stan_block, find_trend_computation_end, generate_obs_trend_mapping, generate_trend_injection_code, handle_nonlinear_trend_injection, handle_response_trend_injection, inject_trend_into_linear_predictor, insert_after_mu_lines_in_model_block, transform_glm_call, transform_glm_code, validate_mapping_arrays
- **generate_combined_stancode()** (`R/stan_assembly.R`) - 13 dependencies
  - Calls: AR, deduplicate_stan_functions, detect_shared_trends, extract_response_names, extract_trend_stanvars_from_setup, generate_base_brms_standata, generate_base_stancode_with_stanvars, inject_multivariate_trends_into_linear_predictors, inject_trend_into_linear_predictor, is_multivariate_trend_specs, RW, sort_stanvars, validate_stan_code
- **mvgam_single()** (`R/mvgam_core.R`) - 9 dependencies
  - Calls: compile_model, create_mvgam_from_combined_fit, fit_model, generate_stan_components_mvgam_formula, mvgam_formula, parse_model, validate_opencl, validate_silent, validate_threads
- **extract_mu_construction_with_classification()** (`R/mu_expression_analysis.R`) - 7 dependencies
  - Calls: build_execution_dependency_plan, classify_mu_expressions_structurally, create_empty_mu_result, extract_comprehensive_variable_references, extract_mu_assignment_lines, extract_stan_block_content, find_variable_declarations
- **parse_multivariate_trends()** (`R/brms_integration.R`) - 7 dependencies
  - Calls: cache_formula_latent_params, create_trend_base_formula, extract_response_names, extract_response_trends, is_multivariate_formula, parse_trend_formula, validate_trend_formula_brms
- **setup_brms_lightweight()** (`R/brms_integration.R`) - 7 dependencies
  - Calls: extract_brmsterms_from_setup, extract_prior_from_setup, extract_trend_data, parse_multivariate_trends, should_trend_formula_have_intercept, validate_brms_formula, validate_setup_components

## Key Function Signatures

### `mvgam`:
```r
mvgam <- function(formula, trend_formula = NULL, data = NULL, backend = getOption("brms.backend", "cmdstanr"), combine = TRUE, family = gaussian(),...
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
- **`R/validations.R`** (72 functions) - Input validation and checks
  - Key functions: is.mvgam_trend, apply_validation_rules, process_trend_validation_rules, dispatch_validation_rule, validate_trend_grouping, validate_trend_time_intervals, validate_regular_time_intervals, validate_brms_formula, validate_obs_formula_brms, validate_trend_formula_brms, validate_bf_trend_formula, validate_list_trend_formula, validate_single_trend_formula, validate_nonlinear_trend_compatibility, validate_required_variables (+ 57 more)

### Core Files
- **`R/summary.mvgam.R`** (24 functions) - Summary methods
  - Key functions: match_trend_pars, match_trend_fixed_pars, match_trend_smooth_pars, match_trend_random_pars, match_trend_specific_pars, check_mvgam_convergence, summary.mvgam, compute_all_summaries, rename_summary_cols, match_fixed_pars (+ 14 more)
- **`R/mvgam_core.R`** (13 functions) - Core package functionality
  - Key functions: mvgam, mvgam_multiple, mvgam_single, generate_combined_stancode_and_data, create_mvgam_from_combined_fit, extract_mvgam_components, validate_multiple_imputation_datasets, fit_multiple_imputation_models, extract_trend_component_info, validate_missing_patterns (+ 3 more)
- **`R/print.mvgam.R`** (11 functions) - Print methods for objects
  - Key functions: stancode.mvgam, stancode.mvgam_prefit, print.mvgamstancode, print.mvgam, print.mvgam_formula, print_model_specification_simple, family.mvgam, formula.mvgam, nobs.mvgam, extract_mcmc_info (+ 1 more)
- **`R/as.data.frame.mvgam.R`** (10 functions) - Data documentation and loading
  - Key functions: as_draws.mvgam, as_draws_matrix.mvgam, as_draws_df.mvgam, as_draws_array.mvgam, as_draws_list.mvgam, as_draws_rvars.mvgam, as.data.frame.mvgam, as.matrix.mvgam, as.array.mvgam, validate_variables
- **`R/lfo_cv.mvgam.R`** (7 functions)
  - Key functions: lfo_cvlog_sum_exp log_mean_exp lfo_cv.mvgam plot.mvgam_lfo cv_split sum_rows (+ 6 more)
- **`R/loo.mvgam.R`** (6 functions)
  - Key functions: loo.mvgam, loo_compare.mvgam, split_mod_dots, named_list, clean_ll, samp_noinf
- **`R/index-mvgam.R`** (6 functions)
  - Key functions: categorize_mvgam_parameters, create_component, extract_trend_parameters, variables.mvgam, extract_parameters_by_type, extract_obs_parameters
- **`R/sim_mvgam.R`** (5 functions)
  - Key functions: sim_mvgamperiodic_gp lkj_corr sim_seasonal random_Sigma (+ 4 more)
- **`R/irf.mvgam.R`** (5 functions)
  - Key functions: irfirf.mvgam gen_irf var_phi var_psi (+ 4 more)
- **`R/ppc.mvgam.R`** (5 functions)
  - Key functions: ppc, pp_check.mvgam, ppc.mvgam, ecdf_plotdat, is_like_factor
- **`R/fevd.mvgam.R`** (4 functions)
  - Key functions: fevd, fevd.mvgam, gen_fevd, var_fecov
- **`R/plot.mvgam.R`** (2 functions) - Plotting and visualization
  - Key functions: plot.mvgam, plottable
- **`R/series_to_mvgam.R`** (2 functions)
  - Key functions: series_to_mvgamxts.to.ts (+ 1 more)
- **`R/residuals.mvgam.R`** (1 functions) - Residual analysis
  - Key functions: residuals.mvgam
- **`R/pairs.mvgam.R`** (1 functions)
  - Key functions: pairs.mvgam
- **`R/mcmc_plot.mvgam.R`** (1 functions) - Plotting and visualization
  - Key functions: mcmc_plot.mvgam

### Other Files
- **`R/trend_system.R`** (64 functions) - Trend modeling
  - Key functions: register_trend_type, list_trend_types, register_custom_trend, trend_param, is.trend_param, mvgam_trend_choices, custom_trend, create_mvgam_trend, RW, AR, CAR, VAR, GP, PW, ZMVN (+ 49 more)
- **`R/priors.R`** (28 functions) - Prior specification
  - Key functions: mvgam_formula, get_prior, extract_trend_priors, generate_trend_priors, generate_trend_priors_from_monitor_params, create_trend_parameter_prior, get_default_trend_parameter_prior, get_all_mvgam_trend_parameters, remove_trend_suffix_from_priors, build_ar_prior_spec, extract_prior_string, map_trend_priors, get_trend_parameter_prior, get_prior.default, get_prior.formula (+ 13 more)
- **`R/backends.R`** (26 functions)
  - Key functions: is.stanfit, is.brmsthreads, validate_threads, is.brmsopencl, validate_opencl, validate_silent, repair_stanfit, is.mvgam, nlist, parse_model (+ 16 more)
- **`R/glm_analysis.R`** (26 functions)
  - Key functions: analyze_stan, create_response_mapping, inject_trends_into_glm_calls, build_mu_with_trend_effects, to_injection, build_coefficient_addition_code, inject_trend_effects_linear, detect_glm_patterns, classify_mu_patterns, determine_glm_preservation (+ 16 more)
- **`R/mu_expression_analysis.R`** (25 functions)
  - Key functions: extract_mu_construction_with_classification, classify_mu_expressions_structurally, create_analysis_context, classify_single_mu_expression_structurally, analyze_expression_structure, analyze_indexing_patterns_comprehensive, analyze_mathematical_operations_comprehensive, analyze_variable_relationships_comprehensive, determine_expression_type, extract_expression_variables_comprehensive, extract_comprehensive_variable_references, create_empty_mu_result, check_loop_requirement, build_execution_dependency_plan (+ 11 more)
- **`R/monotonic.R`** (5 functions)
  - Key functions: smooth.construct.moi.smooth.spec, smooth.construct.mod.smooth.spec, Predict.matrix.moi.smooth, Predict.matrix.mod.smooth, add_mono_model_file
- **`R/ensemble.R`** (4 functions)
  - Key functions: ensembleensemble.mvgam_forecast allsame split_fc_dots (+ 3 more)
- **`R/mvgam_residcor-class.R`** (4 functions) - Class definitions and methods
  - Key functions: gather_matrix, cluster_cormat, reorder_clusters, plot.mvgam_residcor
- **`R/mvgam_fevd-class.R`** (3 functions) - Class definitions and methods
  - Key functions: summary.mvgam_fevd, plot.mvgam_fevd, fevd_df
- **`R/tidier_methods.R`** (3 functions) - Method implementations
  - Key functions: tidy.mvgam, split_hier_Sigma, augment.mvgam
- **`R/residual_cor.R`** (3 functions) - Residual analysis
  - Key functions: residual_corresidual_cor.mvgam residual_cor.jsdgam (+ 2 more)
- **`R/RcppExports.R`** (3 functions) - Exported functions
  - Key functions: ar3_recursC, var1_recursC, varma_recursC
- **`R/how_to_cite.R`** (3 functions)
  - Key functions: how_to_citeprint.how_to_cite how_to_cite.mvgam (+ 2 more)
- **`R/ordinate.jsdgam.R`** (2 functions)
  - Key functions: ordinateordinate.jsdgam (+ 1 more)
- **`R/mvgam_irf-class.R`** (2 functions) - Class definitions and methods
  - Key functions: summary.mvgam_irf, plot.mvgam_irf
- **`R/stability.R`** (2 functions)
  - Key functions: stabilitystability.mvgam (+ 1 more)
- **`R/zzz.R`** (2 functions) - Package startup and attachment
  - Key functions: core_unloaded, mvgam_attach
- **`R/mvgam_forecast-class.R`** (1 functions) - Class definitions and methods
  - Key functions: summary.mvgam_forecast
- **`R/dynamic.R`** (1 functions)
  - Key functions: dynamic
- **`R/lv_correlations.R`** (1 functions)
  - Key functions: lv_correlations
- **`R/cpp_funs.R`** (0 functions)
- **`R/mvgam-package.R`** (0 functions)
- **`R/mvgam-class.R`** (0 functions) - Class definitions and methods

### S3 Methods Files
- **`R/predictions.R`** (9 functions) - Prediction methods
  - Key functions: detect_gp_terms, compute_approx_gp, has_nlpars, extract_linpred_nonlinear, extract_smooth_coef, extract_linpred_from_prep, extract_random_effects_contribution, extract_linpred_univariate, extract_linpred_multivariate
- **`R/add_residuals.R`** (2 functions) - Residual analysis
  - Key functions: add_residualsadd_residuals.mvgam (+ 1 more)

### Stan/Modeling Files
- **`R/stan_assembly.R`** (81 functions) - Stan model integration
  - Key functions: apply_response_suffix_to_stanvars, generate_combined_stancode, generate_base_stancode_with_stanvars, extract_trend_stanvars_from_setup, detect_glm_usage, validate_mapping_arrays, find_stan_block, find_prior_only_insertion_point, inject_trend_into_linear_predictor, transform_glm_calls_post_processing, insert_after_mu_lines_in_model_block, handle_response_trend_injection, inject_multivariate_trends_into_linear_predictors, apply_suffix_to_stan_code, detect_shared_trends (+ 66 more)
- **`R/brms_integration.R`** (17 functions) - brms integration
  - Key functions: should_trend_formula_have_intercept, setup_brms_lightweight, extract_prior_from_setup, parse_multivariate_trends, is_multivariate_formula, extract_response_names, extract_mvbind_responses, extract_response_trends, handle_nonlinear_model, extract_nonlinear_components, parse_nonlinear_manually, extract_brmsterms_from_setup, create_trend_base_formula, determine_trend_injection_point (+ 3 more)
- **`R/stan_polish.R`** (11 functions) - Stan model integration
  - Key functions: polish_generated_stan_code, try_stanheaders_formatting, update_stan_header, reorganize_lprior_statements, clean_stan_comments, fix_blank_lines, add_targeted_comments, insert_comment_before_line, reorganize_target_statements, find_matching_closing_brace (+ 1 more)
- **`R/mock-stanfit.R`** (5 functions) - Model fitting
  - Key functions: get_brms_re_mapping, create_mock_stanfit, as_draws_matrix.mock_stanfit, prepare_predictions.mock_stanfit, get_safe_dummy_value
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
- **`R/plot_mvgam_pterms.R`** (1 functions) - Plotting and visualization
  - Key functions: plot_mvgam_pterms
- **`R/plot_mvgam_randomeffects.R`** (1 functions) - Plotting and visualization
  - Key functions: plot_mvgam_randomeffects
- **`R/plot_mvgam_factors.R`** (1 functions) - Plotting and visualization
  - Key functions: plot_mvgam_factors
- **`R/plot_mvgam_resids.R`** (1 functions) - Plotting and visualization
  - Key functions: plot_mvgam_resids
- **`R/plot_mvgam_trend.R`** (1 functions) - Trend modeling
  - Key functions: plot_mvgam_trend

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

## Potentially Unused Functions (Dead Code Candidates)
*These internal functions are never called directly or dynamically and may be safe to remove*
*Note: Functions called via get(), do.call(), match.fun() etc. are excluded from this list*

### `R/backends.R` (5 potentially unused):
- **algorithm_choices()** (internal, never called)
- **file_refit_options()** (internal, never called)
- **needs_recompilation()** (internal, never called)
- **repair_variable_names()** (internal, never called)
- **require_backend()** (internal, never called)

### `R/brms_integration.R` (1 potentially unused):
- **handle_nonlinear_model()** (internal, never called)

### `R/glm_analysis.R` (1 potentially unused):
- **get_operations_summary()** (internal, never called)

### `R/loo.mvgam.R` (1 potentially unused):
- **samp_noinf()** (internal, never called)

### `R/monotonic.R` (1 potentially unused):
- **add_mono_model_file()** (internal, never called)

### `R/plot.mvgam.R` (1 potentially unused):
- **plottable()** (internal, never called)

### `R/predictions.R` (1 potentially unused):
- **extract_linpred_from_prep()** (internal, never called)

### `R/print.mvgam.R` (1 potentially unused):
- **print_model_specification_simple()** (internal, never called)

### `R/priors.R` (2 potentially unused):
- **get_ar_parameter_prior()** (internal, never called)
- **get_car_parameter_prior()** (internal, never called)

### `R/RcppExports.R` (3 potentially unused):
- **ar3_recursC()** (internal, never called)
- **var1_recursC()** (internal, never called)
- **varma_recursC()** (internal, never called)

### `R/sim_mvgam.R` (1 potentially unused):
- **sim_seasonal()** (internal, never called)

### `R/stan_assembly.R` (1 potentially unused):
- **transform_glm_calls_post_processing()** (internal, never called)

### `R/summary.mvgam.R` (2 potentially unused):
- **check_mvgam_convergence()** (internal, never called)
- **match_trend_pars()** (internal, never called)

### `R/trend_system.R` (11 potentially unused):
- **ar_trend_properties()** (internal, never called)
- **car_trend_properties()** (internal, never called)
- **extract_regular_terms()** (internal, never called)
- **find_trend_terms()** (internal, never called)
- **get_factor_compatible_trends()** (internal, never called)
- **get_trend_info()** (internal, never called)
- **pw_trend_properties()** (internal, never called)
- **rw_trend_properties()** (internal, never called)
- **validate_trend_dispatch_consistency()** (internal, never called)
- **var_trend_properties()** (internal, never called)
- **zmvn_trend_properties()** (internal, never called)

### `R/validations.R` (5 potentially unused):
- **apply_validation_rules()** (internal, never called)
- **are_braces_balanced()** (internal, never called)
- **parse_data_declarations()** (internal, never called)
- **validate_autocor_separation()** (internal, never called)
- **validate_stan_code_structure()** (internal, never called)

### `R/zzz.R` (1 potentially unused):
- **mvgam_attach()** (internal, never called)


## Functions with Zero Internal Dependencies
*These functions don't call other internal functions (utilities and simple functions)*

### `R/add_residuals.R`:
- **add_residuals()** (exported)
- **add_residuals.mvgam()** (internal)

### `R/as.data.frame.mvgam.R`:
- **validate_variables()** (internal)

### `R/backends.R`:
- **algorithm_choices()** (internal)
- **backend_choices()** (internal)
- **compile_model()** (internal)
- **compiled_model()** (internal)
- **elapsed_time()** (internal)
- **file_refit_options()** (internal)
- **fit_model()** (internal)
- **is.brmsopencl()** (internal)
- **is.brmsthreads()** (internal)
- **is.mvgam()** (internal)
- **is.stanfit()** (internal)
- **is_equal()** (internal)
- **is_NA()** (internal)
- **needs_recompilation()** (internal)
- **nlist()** (internal)
- **parse_model()** (internal)
- **repair_stanfit()** (internal)
- **repair_variable_names()** (internal)
- **require_backend()** (internal)
- **require_package()** (internal)
- **ulapply()** (internal)
- **use_opencl()** (internal)
- **use_threading()** (internal)
- **validate_opencl()** (internal)
- **validate_silent()** (internal)
- **validate_threads()** (internal)

### `R/brms_integration.R`:
- **create_trend_base_formula()** (internal)
- **determine_trend_injection_point()** (internal)
- **extract_brmsterms_from_setup()** (internal)
- **extract_response_from_formula()** (internal)
- **extract_variable_name()** (internal)
- **has_mvbind_response()** (internal)

### `R/dynamic.R`:
- **dynamic()** (exported)

### `R/ensemble.R`:
- **allsame()** (internal)
- **ensemble()** (exported)
- **ensemble.mvgam_forecast()** (internal)
- **split_fc_dots()** (internal)

### `R/fevd.mvgam.R`:
- **fevd()** (exported)
- **gen_fevd()** (internal)
- **var_fecov()** (internal)

### `R/glm_analysis.R`:
- **analyze_stan()** (internal)
- **apply_glm_transformations()** (internal)
- **build_coefficient_addition_code()** (internal)
- **build_mu_with_trend_effects()** (internal)
- **classify_mu_patterns()** (internal)
- **create_response_mapping()** (internal)
- **detect_glm_patterns()** (internal)
- **determine_glm_preservation()** (internal)
- **determine_mu_construction_type()** (internal)
- **determine_optimization_reason()** (internal)
- **get_operations_summary()** (internal)
- **inject_trend_effects_linear()** (internal)
- **inject_trends_into_glm_calls()** (internal)
- **parse_glm_parameters_from_line()** (internal)
- **processing_state()** (internal)
- **to_analysis()** (internal)
- **to_assembly()** (internal)
- **to_conversion()** (internal)
- **to_injection()** (internal)
- **track_operations()** (internal)
- **transform_glm()** (internal)
- **transform_glm_call_to_mu_format()** (internal)
- **transform_glm_code()** (internal)
- **transform_single_glm_call()** (internal)
- **transition_with_tracking()** (internal)
- **update_model_block_positions()** (internal)

### `R/how_to_cite.R`:
- **how_to_cite()** (exported)
- **how_to_cite.mvgam()** (internal)
- **print.how_to_cite()** (internal)

### `R/index-mvgam.R`:
- **create_component()** (internal)
- **extract_obs_parameters()** (internal)
- **extract_parameters_by_type()** (internal)
- **extract_trend_parameters()** (internal)
- **variables.mvgam()** (internal)

### `R/irf.mvgam.R`:
- **gen_irf()** (internal)
- **irf()** (exported)
- **irf.mvgam()** (internal)
- **var_phi()** (internal)
- **var_psi()** (internal)

### `R/lfo_cv.mvgam.R`:
- **cv_split()** (internal)
- **lfo_cv()** (exported)
- **lfo_cv.mvgam()** (internal)
- **log_mean_exp()** (internal)
- **log_sum_exp()** (internal)
- **plot.mvgam_lfo()** (internal)
- **sum_rows()** (internal)

### `R/loo.mvgam.R`:
- **clean_ll()** (internal)
- **loo.mvgam()** (internal)
- **loo_compare.mvgam()** (internal)
- **named_list()** (internal)
- **samp_noinf()** (internal)
- **split_mod_dots()** (internal)

### `R/lv_correlations.R`:
- **lv_correlations()** (exported)

### `R/make_stan.R`:
- **generate_stan_components_mvgam_formula()** (internal)
- **stancode.mvgam_formula()** (internal)
- **standata.mvgam_formula()** (internal)

### `R/mcmc_plot.mvgam.R`:
- **mcmc_plot.mvgam()** (internal)

### `R/mock-stanfit.R`:
- **as_draws_matrix.mock_stanfit()** (internal)
- **create_mock_stanfit()** (internal)
- **get_brms_re_mapping()** (internal)
- **get_safe_dummy_value()** (internal)
- **prepare_predictions.mock_stanfit()** (internal)

### `R/monotonic.R`:
- **add_mono_model_file()** (internal)
- **Predict.matrix.mod.smooth()** (internal)
- **Predict.matrix.moi.smooth()** (internal)
- **smooth.construct.mod.smooth.spec()** (internal)
- **smooth.construct.moi.smooth.spec()** (internal)

### `R/mu_expression_analysis.R`:
- **analyze_assignment_type()** (internal)
- **analyze_function_usage()** (internal)
- **build_execution_dependency_plan()** (internal)
- **calculate_bracket_nesting_depth()** (internal)
- **calculate_variable_complexity()** (internal)
- **check_loop_requirement()** (internal)
- **count_mathematical_operators()** (internal)
- **create_empty_mu_result()** (internal)
- **determine_execution_order_from_type()** (internal)
- **extract_all_identifiers()** (internal)
- **extract_assignment_target()** (internal)
- **extract_declared_functions()** (internal)
- **extract_mu_assignment_lines()** (internal)
- **normalize_mu_expression()** (internal)

### `R/mvgam_core.R`:
- **extract_series_information()** (internal)
- **extract_time_information()** (internal)
- **extract_trend_component_info()** (internal)
- **pool_mvgam_fits()** (internal)
- **validate_missing_patterns()** (internal)

### `R/mvgam_fevd-class.R`:
- **fevd_df()** (internal)
- **plot.mvgam_fevd()** (internal)
- **summary.mvgam_fevd()** (internal)

### `R/mvgam_forecast-class.R`:
- **summary.mvgam_forecast()** (internal)

### `R/mvgam_irf-class.R`:
- **plot.mvgam_irf()** (internal)
- **summary.mvgam_irf()** (internal)

### `R/mvgam_residcor-class.R`:
- **cluster_cormat()** (internal)
- **gather_matrix()** (internal)
- **plot.mvgam_residcor()** (internal)
- **reorder_clusters()** (internal)

### `R/ordinate.jsdgam.R`:
- **ordinate()** (exported)
- **ordinate.jsdgam()** (internal)

### `R/pairs.mvgam.R`:
- **pairs.mvgam()** (internal)

### `R/plot.mvgam.R`:
- **plot.mvgam()** (internal)
- **plottable()** (internal)

### `R/plot_mvgam_factors.R`:
- **plot_mvgam_factors()** (exported)

### `R/plot_mvgam_fc.R`:
- **plot.mvgam_forecast()** (internal)
- **plot_mvgam_fc()** (exported)

### `R/plot_mvgam_pterms.R`:
- **plot_mvgam_pterms()** (exported)

### `R/plot_mvgam_randomeffects.R`:
- **plot_mvgam_randomeffects()** (exported)

### `R/plot_mvgam_resids.R`:
- **plot_mvgam_resids()** (exported)

### `R/plot_mvgam_series.R`:
- **plot_acf()** (internal)
- **plot_ecdf()** (internal)
- **plot_histogram()** (internal)
- **plot_mvgam_series()** (exported)
- **plot_time_series()** (internal)
- **validate_plot_data()** (internal)

### `R/plot_mvgam_smooth.R`:
- **plot_mvgam_smooth()** (exported)

### `R/plot_mvgam_trend.R`:
- **plot_mvgam_trend()** (exported)

### `R/plot_mvgam_uncertainty.R`:
- **intersect_hist()** (internal)
- **plot_mvgam_uncertainty()** (exported)

### `R/ppc.mvgam.R`:
- **ecdf_plotdat()** (internal)
- **is_like_factor()** (internal)
- **pp_check.mvgam()** (internal)
- **ppc()** (exported)
- **ppc.mvgam()** (internal)

### `R/predictions.R`:
- **compute_approx_gp()** (internal)
- **detect_gp_terms()** (internal)
- **extract_linpred_from_prep()** (internal)
- **extract_linpred_multivariate()** (internal)
- **extract_linpred_nonlinear()** (internal)
- **extract_linpred_univariate()** (internal)
- **extract_random_effects_contribution()** (internal)
- **extract_smooth_coef()** (internal)
- **has_nlpars()** (internal)

### `R/print.mvgam.R`:
- **extract_mcmc_info()** (internal)
- **family.mvgam()** (internal)
- **formula.mvgam()** (internal)
- **nobs.mvgam()** (internal)
- **print.mvgam()** (internal)
- **print.mvgam_formula()** (internal)
- **print.mvgam_prefit()** (internal)
- **print.mvgamstancode()** (internal)
- **print_model_specification_simple()** (internal)
- **stancode.mvgam()** (internal)
- **stancode.mvgam_prefit()** (internal)

### `R/priors.R`:
- **combine_obs_trend_priors()** (internal)
- **create_empty_brmsprior()** (internal)
- **filter_obs_priors()** (internal)
- **filter_trend_priors()** (internal)
- **get_ar_parameter_prior()** (internal)
- **get_best_prior_match()** (internal)
- **get_car_parameter_prior()** (internal)
- **get_parameter_type_default_prior()** (internal)
- **get_prior()** (exported)
- **get_prior.brmsformula()** (internal)
- **get_prior.mvgam_formula()** (internal)
- **get_trend_prior_spec()** (internal)
- **has_embedded_families()** (internal)
- **map_prior_to_stan_string()** (internal)

### `R/RcppExports.R`:
- **ar3_recursC()** (internal)
- **var1_recursC()** (internal)
- **varma_recursC()** (internal)

### `R/residual_cor.R`:
- **residual_cor()** (exported)
- **residual_cor.jsdgam()** (internal)
- **residual_cor.mvgam()** (internal)

### `R/residuals.mvgam.R`:
- **residuals.mvgam()** (internal)

### `R/series_to_mvgam.R`:
- **series_to_mvgam()** (exported)
- **xts.to.ts()** (internal)

### `R/sim_mvgam.R`:
- **lkj_corr()** (internal)
- **periodic_gp()** (internal)
- **random_Sigma()** (internal)
- **sim_mvgam()** (exported)
- **sim_seasonal()** (internal)

### `R/stability.R`:
- **stability()** (exported)
- **stability.mvgam()** (internal)

### `R/stan_assembly.R`:
- **add_hierarchical_support()** (internal)
- **append_if_not_null()** (internal)
- **apply_correct_transformation_order()** (internal)
- **apply_safe_parameter_replacement()** (internal)
- **apply_suffix_to_name()** (internal)
- **apply_suffix_to_stan_code()** (internal)
- **calculate_car_time_distances()** (internal)
- **combine_stanvars()** (internal)
- **convert_glm_to_standard_form()** (internal)
- **create_times_trend_matrix()** (internal)
- **deduplicate_stan_functions()** (internal)
- **detect_shared_trends()** (internal)
- **extract_and_rename_stan_blocks()** (internal)
- **extract_and_rename_standata_objects()** (internal)
- **extract_and_rename_trend_parameters()** (internal)
- **extract_computed_variables()** (internal)
- **extract_dependencies_from_declaration()** (internal)
- **extract_hierarchical_info()** (internal)
- **extract_mapping_arrays()** (internal)
- **extract_multivariate_standata()** (internal)
- **extract_non_likelihood_from_model_block()** (internal)
- **extract_response_names_from_brmsfit()** (internal)
- **extract_stan_block_content()** (internal)
- **extract_stan_functions_block()** (internal)
- **extract_stan_identifiers()** (internal)
- **extract_univariate_standata()** (internal)
- **filter_block_content()** (internal)
- **filter_renameable_identifiers()** (internal)
- **find_trend_computation_end()** (internal)
- **find_variable_declarations()** (internal)
- **generate_ar_trend_stanvars()** (internal)
- **generate_base_brms_standata()** (internal)
- **generate_car_trend_stanvars()** (internal)
- **generate_common_trend_data()** (internal)
- **generate_factor_model()** (internal)
- **generate_hierarchical_correlation_model()** (internal)
- **generate_hierarchical_correlation_parameters()** (internal)
- **generate_hierarchical_data_structures()** (internal)
- **generate_hierarchical_functions()** (internal)
- **generate_innovation_model()** (internal)
- **generate_matrix_z_multiblock_stanvars()** (internal)
- **generate_matrix_z_parameters()** (internal)
- **generate_matrix_z_tdata()** (internal)
- **generate_pw_trend_stanvars()** (internal)
- **generate_rw_trend_stanvars()** (internal)
- **generate_shared_innovation_stanvars()** (internal)
- **generate_times_trend_matrices()** (internal)
- **generate_trend_computation_tparameters()** (internal)
- **generate_trend_injection_code()** (internal)
- **generate_trend_priors_stanvar()** (internal)
- **generate_trend_specific_stanvars()** (internal)
- **generate_var_trend_stanvars()** (internal)
- **generate_zmvn_trend_stanvars()** (internal)
- **get_stan_reserved_words()** (internal)
- **handle_nonlinear_trend_injection()** (internal)
- **normalize_function_body()** (internal)
- **normalize_function_signature()** (internal)
- **parse_glm_parameters_single()** (internal)
- **parse_stan_functions()** (internal)
- **process_variable()** (internal)
- **reconstruct_functions_block()** (internal)
- **reconstruct_mu_trend_with_renamed_vars()** (internal)
- **remove_duplicate_functions()** (internal)
- **rename_parameters_in_block()** (internal)
- **replace_stan_functions_block()** (internal)
- **should_include_in_transformed_parameters()** (internal)
- **sort_stanvars()** (internal)
- **transform_glm_call()** (internal)

### `R/stan_polish.R`:
- **add_targeted_comments()** (internal)
- **clean_stan_comments()** (internal)
- **find_matching_closing_brace()** (internal)
- **fix_blank_lines()** (internal)
- **insert_comment_before_line()** (internal)
- **polish_generated_stan_code()** (internal)
- **reorganize_lprior_statements()** (internal)
- **reorganize_model_block_statements()** (internal)
- **reorganize_target_statements()** (internal)
- **try_stanheaders_formatting()** (internal)
- **update_stan_header()** (internal)

### `R/summary.mvgam.R`:
- **check_mvgam_convergence()** (internal)
- **compute_all_summaries()** (internal)
- **error()** (internal)
- **get_dpar_names()** (internal)
- **is_latent_state_param()** (internal)
- **match_dpar_fixed_pars()** (internal)
- **match_dpar_smooth_pars()** (internal)
- **match_family_pars()** (internal)
- **match_fixed_pars()** (internal)
- **match_random_pars()** (internal)
- **match_smooth_pars()** (internal)
- **match_trend_fixed_pars()** (internal)
- **match_trend_pars()** (internal)
- **match_trend_random_pars()** (internal)
- **match_trend_smooth_pars()** (internal)
- **match_trend_specific_pars()** (internal)
- **match_z_loadings()** (internal)
- **print.mvgam_pooled_summary()** (internal)
- **print.mvgam_summary()** (internal)
- **print_param_section()** (internal)
- **rename_summary_cols()** (internal)
- **round_numeric()** (internal)
- **summary.mvgam()** (internal)
- **summary.mvgam_pooled()** (internal)

### `R/tidier_methods.R`:
- **augment.mvgam()** (internal)
- **split_hier_Sigma()** (internal)
- **tidy.mvgam()** (internal)

### `R/trend_system.R`:
- **add_consistent_dispatch_metadata()** (internal)
- **apply_mvgam_trend_defaults()** (internal)
- **AR()** (exported)
- **ar_trend_properties()** (internal)
- **c.trend_param()** (internal)
- **CAR()** (exported)
- **car_trend_properties()** (internal)
- **create_mvgam_trend()** (exported)
- **custom_trend()** (exported)
- **eval_trend_constructor()** (internal)
- **extract_regular_terms()** (internal)
- **filter_ar_forecast_params()** (internal)
- **filter_car_forecast_params()** (internal)
- **filter_pw_forecast_params()** (internal)
- **filter_rw_forecast_params()** (internal)
- **filter_var_forecast_params()** (internal)
- **filter_zmvn_forecast_params()** (internal)
- **find_trend_terms()** (internal)
- **generate_car_monitor_params()** (internal)
- **generate_forecast_metadata()** (internal)
- **generate_forecast_required_params()** (internal)
- **generate_parameter_label()** (internal)
- **generate_pw_monitor_params()** (internal)
- **generate_rw_monitor_params()** (internal)
- **generate_summary_labels()** (internal)
- **generate_var_monitor_params()** (internal)
- **get_default_incompatibility_reason()** (internal)
- **get_default_validation_rules()** (internal)
- **get_factor_compatible_trends()** (internal)
- **get_mvgam_trend_defaults()** (internal)
- **get_trend_dispatch_function()** (internal)
- **get_trend_info()** (internal)
- **GP()** (exported)
- **is.trend_param()** (exported)
- **is_registry_initialized()** (internal)
- **list_trend_types()** (exported)
- **mvgam_trend_choices()** (exported)
- **mvgam_trend_pattern()** (internal)
- **mvgam_trend_registry()** (internal)
- **normalize_trend_type()** (internal)
- **parse_trend_formula()** (internal)
- **print.mvgam_trend()** (internal)
- **print.trend_param()** (internal)
- **process_arg()** (internal)
- **process_trend_params()** (internal)
- **PW()** (exported)
- **pw_trend_properties()** (internal)
- **RW()** (exported)
- **rw_trend_properties()** (internal)
- **trend_param()** (exported)
- **validate_trend_dispatch_consistency()** (internal)
- **validate_trend_properties()** (internal)
- **VAR()** (exported)
- **var_trend_properties()** (internal)
- **ZMVN()** (exported)
- **zmvn_trend_properties()** (internal)

### `R/validations.R`:
- **all_times_avail()** (internal)
- **are_braces_balanced()** (internal)
- **as_one_logical()** (internal)
- **cache_formula_latent_params()** (internal)
- **create_multivariate_series()** (internal)
- **deparse0()** (internal)
- **ensure_mvgam_variables()** (internal)
- **eval_silent()** (internal)
- **extract_all_bf_formulas()** (internal)
- **extract_and_validate_trend_components()** (internal)
- **extract_time_series_dimensions()** (internal)
- **extract_trend_data()** (internal)
- **filter_required_variables()** (internal)
- **format_pipeline_error()** (internal)
- **formula2str_mvgam()** (internal)
- **generate_obs_trend_mapping()** (internal)
- **get_series_for_grouping()** (internal)
- **get_time_for_grouping()** (internal)
- **get_trend_validation_patterns()** (internal)
- **get_validation_rule_dispatch_table()** (internal)
- **has_mvgam_variables()** (internal)
- **is.mvgam_trend()** (exported)
- **is_multivariate_brmsfit()** (internal)
- **is_multivariate_trend_specs()** (internal)
- **is_nonlinear_formula()** (internal)
- **is_trend_term()** (internal)
- **is_try_error()** (internal)
- **parse_base_formula_safe()** (internal)
- **parse_data_declarations()** (internal)
- **parse_model_cmdstanr()** (internal)
- **patterns()** (internal)
- **process_capacity_parameter()** (internal)
- **process_lag_parameters()** (internal)
- **remove_mvgam_variables()** (internal)
- **remove_trend_expressions()** (internal)
- **validate_and_process_trend_parameters()** (internal)
- **validate_autocor_separation()** (internal)
- **validate_factor_compatibility()** (internal)
- **validate_factor_levels()** (internal)
- **validate_grouping_arguments()** (internal)
- **validate_grouping_structure()** (internal)
- **validate_multivariate_series_time()** (internal)
- **validate_multivariate_trend_constraints()** (internal)
- **validate_mvgam_trend()** (internal)
- **validate_no_factor_hierarchical()** (internal)
- **validate_nonlinear_trend_compatibility()** (internal)
- **validate_pos_integer()** (internal)
- **validate_proportional()** (internal)
- **validate_required_variables()** (internal)
- **validate_series_time()** (internal)
- **validate_setup_components()** (internal)
- **validate_stan_code()** (internal)
- **validate_stan_code_structure()** (internal)
- **validate_time_series_for_trends()** (internal)
- **validate_trend_components()** (internal)
- **validate_trend_factor_compatibility()** (internal)
- **validate_trend_formula_restrictions()** (internal)
- **validate_univariate_series_time()** (internal)

### `R/zzz.R`:
- **core_unloaded()** (internal)
- **mvgam_attach()** (internal)


## System-Specific Function Breakdowns

### Prior System Functions
- **build_ar_prior_spec()** (`R/priors.R`) - calls: AR
- **combine_obs_trend_priors()** (`R/priors.R`)
- **create_empty_brmsprior()** (`R/priors.R`)
- **create_trend_parameter_prior()** (`R/priors.R`) - calls: get_default_trend_parameter_prior
- **extract_prior_string()** (`R/priors.R`) - calls: get_best_prior_match
- **extract_trend_priors()** (`R/priors.R`) - calls: generate_trend_priors, parse_trend_formula
- **filter_obs_priors()** (`R/priors.R`)
- **filter_trend_priors()** (`R/priors.R`)

### Trend System Functions
- **apply_mvgam_trend_defaults()** (`R/trend_system.R`)
- **ar_trend_properties()** (`R/trend_system.R`)
- **auto_register_trend_types()** (`R/trend_system.R`) - calls: register_trend_type, validate_trend_properties
- **c.trend_param()** (`R/trend_system.R`)
- **car_trend_properties()** (`R/trend_system.R`)
- **create_mvgam_trend()** (`R/trend_system.R`)
- **custom_trend()** (`R/trend_system.R`)
- **eval_trend_constructor()** (`R/trend_system.R`)

### Stan Assembly System Functions
- **apply_response_suffix_to_stanvars()** (`R/stan_assembly.R`) - calls: apply_suffix_to_name, apply_suffix_to_stan_code
- **apply_suffix_to_stan_code()** (`R/stan_assembly.R`)
- **calculate_car_time_distances()** (`R/stan_assembly.R`)
- **combine_stanvars()** (`R/stan_assembly.R`)
- **convert_glm_to_standard_form()** (`R/stan_assembly.R`)
- **deduplicate_stan_functions()** (`R/stan_assembly.R`)
- **extract_and_rename_stan_blocks()** (`R/stan_assembly.R`)
- **extract_and_rename_standata_objects()** (`R/stan_assembly.R`)

## Notes for LLM Agents
- **Primary workflow**: `mvgam()` -> prior/trend system setup -> Stan assembly -> model fitting -> S3 methods
- **Prior system**: `mvgam_formula()`, `get_prior()`, `set_prior()` for Bayesian prior specification
- **Trend system**: `trend_param()`, `register_trend_type()` for time series trend modeling
- **Stan assembly**: `stancode()`, `standata()` for Stan model compilation and data preparation
- **Stan integration**: Package heavily relies on Stan/brms for Bayesian computation
- **S3 system**: Extensive use of S3 classes (`mvgam`, `mvgam_forecast`, etc.) with method dispatch
- **File patterns**: `R/priors.R` (prior system), `R/trend_system.R` (trends), `R/stan_assembly.R` (Stan code)
- **Core functions often call**: validation functions, Stan code generation, data preprocessing utilities

