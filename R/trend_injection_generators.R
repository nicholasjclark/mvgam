#' Compute Data Info for Trend Injection
#'
#' Computes basic information needed for trend Stan code generation.
#'
#' @param data_specs List of data specifications
#' @return List with trend-relevant data information
#' @noRd
compute_trend_data_info <- function(data_specs) {
  list(
    n_obs = data_specs$n_obs %||% 100,
    n_series = data_specs$n_series %||% 1,
    series_var = data_specs$series_var %||% "series"
  )
}

# Shared Utility Functions for Factor Model Consistency
# These functions ensure all factor-compatible trends use identical patterns

#' Generate Data Block Injections for Matrix Z
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables  
#' @param n_series Number of observed series
#' @return List of data block stanvars
#' @noRd
generate_matrix_z_data_injectors <- function(is_factor_model, n_lv, n_series) {
  stanvars <- list()
  
  # Both factor and non-factor models need these data declarations
  stanvars$n_lv_data <- brms::stanvar(
    x = n_lv,
    name = "n_lv",
    scode = "int<lower=1> n_lv;",
    block = "data"
  )
  
  stanvars$n_series_data <- brms::stanvar(
    x = n_series,
    name = "n_series", 
    scode = "int<lower=1> n_series;",
    block = "data"
  )
  
  return(stanvars)
}

#' Generate Parameter Block Injections for Matrix Z
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables  
#' @param n_series Number of observed series
#' @return List of parameter block stanvars
#' @noRd
generate_matrix_z_parameter_injectors <- function(is_factor_model, n_lv, n_series) {
  stanvars <- list()
  
  if (is_factor_model) {
    # Factor model: estimate Z in parameters for dimensionality reduction
    stanvars$z_matrix_factor <- brms::stanvar(
      name = "Z",
      scode = glue::glue("matrix[n_series, n_lv] Z;"),
      block = "parameters"
    )
  }
  
  return(stanvars) 
}

#' Generate Transformed Data Block Injections for Matrix Z
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables  
#' @param n_series Number of observed series
#' @return List of transformed data block stanvars
#' @noRd
generate_matrix_z_transformed_data_injectors <- function(is_factor_model, n_lv, n_series) {
  stanvars <- list()
  
  if (!is_factor_model) {
    # Non-factor model: diagonal Z in transformed data
    stanvars$z_matrix_diagonal <- brms::stanvar(
      name = "Z",
      scode = glue::glue("matrix[n_series, n_lv] Z = diag_matrix(rep_vector(1.0, n_lv));"),
      block = "tdata"
    )
  }
  
  return(stanvars)
}

#' Generate Transformed Parameters Block Injections for Trend Computation
#' 
#' WHY: All trends must use the same computation pattern:
#' trend[i,s] = dot_product(Z[s,:], LV[i,:]) + mu_trend[ytimes[i,s]]
#' 
#' @param n_lv Number of latent variables
#' @param n_series Number of observed series  
#' @return List of transformed parameters block stanvars
#' @noRd
generate_trend_computation_transformed_parameters_injectors <- function(n_lv, n_series) {
  stanvars <- list()
  
  stanvars$trend_computation <- brms::stanvar(
    name = "trend",
    scode = glue::glue("
      // Derived latent trends using universal computation pattern
      matrix[n, n_series] trend;
      
      // Universal trend computation: state-space dynamics + linear predictors
      // dot_product captures dynamic component, mu_trend captures trend_formula
      for (i in 1:n) {{
        for (s in 1:n_series) {{
          trend[i, s] = dot_product(Z[s, :], LV[i, :]) + mu_trend[ytimes[i, s]];
        }}
      }}
    "),
    block = "tparameters"
  )
  
  return(stanvars)
}

#' Generate Model Block Injections for Factor Model Priors
#'
#' WHY: Factor models need fixed variance=1 constraints for identifiability
#' since the scale is captured by the loading matrix Z.
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param n_lv Number of latent variables
#' @return List of model block stanvars
#' @noRd  
generate_factor_model_model_injectors <- function(is_factor_model, n_lv) {
  stanvars <- list()
  
  if (is_factor_model) {
    # Factor model: fixed variance=1 for identifiability, priors for Z
    stanvars$factor_lv_priors <- brms::stanvar(
      name = "factor_lv_priors",
      scode = "to_vector(LV_raw) ~ std_normal();",
      block = "model"
    )
    
    stanvars$factor_z_priors <- brms::stanvar(
      name = "factor_z_priors", 
      scode = "to_vector(Z) ~ normal(0, 1);",
      block = "model"
    )
  }
  
  return(stanvars)
}

#' Generate Functions Block Injections for Hierarchical Correlations
#'
#' WHY: All trends (AR, VAR, CAR, ZMVN) need the same hierarchical correlation
#' machinery when groups are specified.
#'
#' @return List of functions block stanvars
#' @noRd
generate_hierarchical_functions_injectors <- function() {
  stanvars <- list()
  
  stanvars$combine_cholesky_function <- brms::stanvar(
    name = "combine_cholesky",
    scode = "
      /* Function to compute a partially pooled correlation matrix 
       * Combines global correlation structure with group-specific deviations
       * alpha controls mixing: 1 = pure global, 0 = pure local
       */
      matrix combine_cholesky(matrix global_chol_cor, matrix local_chol_cor,
                              real alpha) {
        int dim = rows(local_chol_cor);
        matrix[dim, dim] global_cor = multiply_lower_tri_self_transpose(global_chol_cor);
        matrix[dim, dim] local_cor = multiply_lower_tri_self_transpose(local_chol_cor);
        matrix[dim, dim] combined_chol_cor;
        combined_chol_cor = cholesky_decompose(alpha * global_cor
                                               + (1 - alpha) * local_cor);
        return combined_chol_cor;
      }
    ",
    block = "functions"
  )
  
  return(stanvars)
}

#' Generate Parameters Block Injections for Hierarchical Correlations
#'
#' @param n_groups Number of groups for hierarchical modeling
#' @param n_subgroups Number of subgroups (typically n_lv)
#' @return List of parameters block stanvars
#' @noRd
generate_hierarchical_correlation_parameter_injectors <- function(n_groups, n_subgroups) {
  stanvars <- list()
  
  stanvars$L_Omega_global <- brms::stanvar(
    name = "L_Omega_global",
    scode = glue::glue("cholesky_factor_corr[{n_subgroups}] L_Omega_global;"),
    block = "parameters"
  )
  
  stanvars$L_deviation_group <- brms::stanvar(
    name = "L_deviation_group",
    scode = glue::glue("array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_deviation_group;"),
    block = "parameters"
  )
  
  stanvars$alpha_cor <- brms::stanvar(
    name = "alpha_cor",
    scode = "real<lower=0, upper=1> alpha_cor;",
    block = "parameters"
  )
  
  return(stanvars)
}

#' Generate Model Block Injections for Hierarchical Correlation Priors
#'
#' @param n_groups Number of groups for hierarchical modeling
#' @return List of model block stanvars
#' @noRd
generate_hierarchical_correlation_model_injectors <- function(n_groups) {
  stanvars <- list()
  
  stanvars$alpha_cor_prior <- brms::stanvar(
    name = "alpha_cor_prior",
    scode = "alpha_cor ~ beta(3, 2);",
    block = "model"
  )
  
  stanvars$L_Omega_global_prior <- brms::stanvar(
    name = "L_Omega_global_prior",
    scode = "L_Omega_global ~ lkj_corr_cholesky(1);",
    block = "model"
  )
  
  stanvars$L_deviation_group_prior <- brms::stanvar(
    name = "L_deviation_group_prior",
    scode = glue::glue("for (g in 1:{n_groups}) {{ L_deviation_group[g] ~ lkj_corr_cholesky(6); }}"),
    block = "model"
  )
  
  return(stanvars)
}

#' Generate Trend Injection Stanvars
#'
#' Dispatches to appropriate trend generator based on trend_spec type.
#'
#' @param trend_spec Trend specification object
#' @param data_info Data information list
#' @return List of stanvars for trend injection
#' @noRd
generate_trend_injection_stanvars <- function(trend_spec, data_info) {
  # Validate inputs
  checkmate::assert_list(trend_spec)
  checkmate::assert_list(data_info)

  # Validate factor model compatibility if n_lv is specified
  validate_factor_compatibility(trend_spec)
  
  # Validate n_lv parameter constraints
  if (!is.null(trend_spec$n_lv)) {
    n_series <- data_info$n_series %||% 1
    if (trend_spec$n_lv > n_series) {
      stop(insight::format_error(
        "{.field n_lv} cannot exceed {.field n_series}.",
        paste0("Got n_lv = ", trend_spec$n_lv, " and n_series = ", n_series, "."),
        "Use fewer or equal latent variables than observed series."
      ))
    }
    # n_lv < n_series → Factor model (estimated matrix Z)
    # n_lv = n_series → Non-factor model (diagonal matrix Z)
    # Both cases are valid after factor compatibility check above
  }

  # Get trend info from registry
  trend_info <- get_trend_info(trend_spec$trend_type)
  if (is.null(trend_info)) {
    stop(insight::format_error(
      "Unknown trend type: {.field {trend_spec$trend_type}}"
    ))
  }

  # Generate stanvars using the appropriate generator
  trend_info$generator(trend_spec, data_info)
}

#' Random Walk Trend Generator
#'
#' Generates Stan code components for random walk trends.
#'
#' @param trend_spec Trend specification for RW model
#' @param data_info Data information including dimensions
#' @return List of stanvars for RW trend
#' @noRd
generate_rw_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% 1
  correlation <- trend_spec$correlation %||% FALSE
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  
  # Determine if this is a factor model
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Initialize stanvars list
  stanvars <- list()
  
  # Generate block-specific injectors for matrix Z
  stanvars <- c(stanvars, generate_matrix_z_data_injectors(is_factor_model, n_lv, n_series))
  stanvars <- c(stanvars, generate_matrix_z_parameter_injectors(is_factor_model, n_lv, n_series))
  stanvars <- c(stanvars, generate_matrix_z_transformed_data_injectors(is_factor_model, n_lv, n_series))

  if (correlation && n_lv > 1) {
    # Correlated case: RW(cor = TRUE) - multivariate approach
    if (is_factor_model) {
      # Factor model: fix variances to 1 - parameters block
      stanvars$L_Omega <- brms::stanvar(
        name = "L_Omega",
        scode = glue::glue("cholesky_factor_corr[{n_lv}] L_Omega;"),
        block = "parameters"
      )
      
      stanvars$LV_raw <- brms::stanvar(
        name = "LV_raw",
        scode = glue::glue("matrix[n, {n_lv}] LV_raw;"),
        block = "parameters"
      )
      
      # Transformed parameters block - same Stan code, proper stanvar
      stanvars$LV_transformed <- brms::stanvar(
        name = "LV",
        scode = glue::glue("
          // Latent states with correlated RW (variance = 1)
          matrix[n, {n_lv}] LV;
  
          // Apply correlated RW transformation with fixed variance
          for (i in 1:n) {{
            if (i == 1) {{
              LV[i, :] = (L_Omega * LV_raw[i, :]')';
            }} else {{
              LV[i, :] = LV[i-1, :] + (L_Omega * LV_raw[i, :]')';
            }}
          }}
        "),
        block = "tparameters"
      )
    } else {
      # Non-factor model: estimate variances, diagonal Z - parameters block
      stanvars$L_Omega <- brms::stanvar(
        name = "L_Omega",
        scode = glue::glue("cholesky_factor_corr[{n_lv}] L_Omega;"),
        block = "parameters"
      )
      
      stanvars$sigma_lv <- brms::stanvar(
        name = "sigma_lv",
        scode = glue::glue("vector<lower=0>[{n_lv}] sigma_lv;"),
        block = "parameters"
      )
      
      stanvars$LV_raw <- brms::stanvar(
        name = "LV_raw",
        scode = glue::glue("matrix[n, {n_lv}] LV_raw;"),
        block = "parameters"
      )

      # Transformed parameters block - same Stan code, proper stanvars
      stanvars$Sigma_transformed <- brms::stanvar(
        name = "Sigma",
        scode = glue::glue("matrix[{n_lv}, {n_lv}] Sigma = diag_pre_multiply(sigma_lv, L_Omega);"),
        block = "tparameters"
      )
      
      stanvars$LV_transformed <- brms::stanvar(
        name = "LV",
        scode = glue::glue("
          // Latent states with correlated RW
          matrix[n, {n_lv}] LV;
  
          // Apply correlated RW transformation
          for (i in 1:n) {{
            if (i == 1) {{
              LV[i, :] = (Sigma * LV_raw[i, :]')';
            }} else {{
              LV[i, :] = LV[i-1, :] + (Sigma * LV_raw[i, :]')';
            }}
          }}
        "),
        block = "tparameters"
      )
    }

  } else {
    # Simple uncorrelated case: RW() - non-centered parameterization
    if (is_factor_model) {
      # Factor model: fix variances to 1 - parameters block
      stanvars$LV_raw <- brms::stanvar(
        name = "LV_raw",
        scode = glue::glue("matrix[n, {n_lv}] LV_raw;"),
        block = "parameters"
      )
      
      # Transformed parameters block - same Stan code, proper stanvar
      stanvars$LV_transformed <- brms::stanvar(
        name = "LV",
        scode = glue::glue("
          // Latent states with non-centered parameterization (variance = 1)
          matrix[n, {n_lv}] LV;
  
          // Apply non-centered RW transformation with fixed variance
          LV = LV_raw;
          for (j in 1:{n_lv}) {{
            for (i in 2:n) {{
              LV[i, j] = LV[i-1, j] + LV[i, j];
            }}
          }}
        "),
        block = "tparameters"
      )
    } else {
      # Non-factor model: estimate variances, diagonal Z - parameters block
      stanvars$sigma <- brms::stanvar(
        name = "sigma",
        scode = glue::glue("vector<lower=0>[{n_lv}] sigma;"),
        block = "parameters"
      )
      
      stanvars$LV_raw <- brms::stanvar(
        name = "LV_raw",
        scode = glue::glue("matrix[n, {n_lv}] LV_raw;"),
        block = "parameters"
      )
  
      # Transformed parameters block - same Stan code, proper stanvar
      stanvars$LV_transformed <- brms::stanvar(
        name = "LV",
        scode = glue::glue("
          // Latent states with non-centered parameterization
          matrix[n, {n_lv}] LV;
  
          // Apply non-centered RW transformation
          LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));
          for (j in 1:{n_lv}) {{
            for (i in 2:n) {{
              LV[i, j] = LV[i-1, j] + LV[i, j];
            }}
          }}
        "),
        block = "tparameters"
      )

      # Model block - same Stan code, proper stanvars
      stanvars$LV_raw_prior <- brms::stanvar(
        name = "LV_raw_prior",
        scode = "to_vector(LV_raw) ~ std_normal();",
        block = "model"
      )
      
      stanvars$sigma_prior <- brms::stanvar(
        name = "sigma_prior",
        scode = "sigma ~ student_t(3, 0, 2.5);",
        block = "model"
      )
    }
  }
  
  # Use shared trend computation utility for universal pattern
  stanvars <- c(stanvars, generate_trend_computation_transformed_parameters_injectors(n_lv, n_series))
  
  # Use shared factor model priors if applicable
  if (is_factor_model) {
    stanvars <- c(stanvars, generate_factor_model_model_injectors(is_factor_model, n_lv))
  }

  return(stanvars)
}

#' VAR Trend Generator
#'
#' Generates Stan code components for vector autoregressive trends.
#' Supports factor models, hierarchical correlations, and consistent matrix Z patterns.
#'
#' @param trend_spec Trend specification for VAR model
#' @param data_info Data information including dimensions
#' @return List of stanvars for VAR trend
#' @noRd
generate_var_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% 1
  lags <- trend_spec$lags %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'
  unit_var <- trend_spec$unit %||% "time"
  
  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Initialize stanvars list
  stanvars <- list()
  
  # Use shared matrix Z utility for consistent factor model patterns
  stanvars <- c(stanvars, generate_matrix_z_stanvars(is_factor_model, n_lv, n_series))

  # VAR data block (needed for all cases)
  stanvars$var_data <- stanvar(
    x = list(
      n_lv = n_lv,
      n_lags = lags,
      lv_coefs = array(0, dim = c(n_lv, n_lv, lags))
    ),
    name = "var_data",
    scode = glue::glue("
    int<lower=1> n_lv;
    int<lower=1> n_lags;
    array[n_lv, n_lv, n_lags] real lv_coefs;
    ")
  )

  if (use_grouping) {
    # Hierarchical VAR case: VAR(p = 1, unit = site, gr = group, subgr = species)
    n_groups <- data_info$n_groups %||% 1
    n_subgroups <- data_info$n_subgroups %||% n_lv

    # Add shared hierarchical correlation utilities
    stanvars <- c(stanvars, generate_hierarchical_functions())
    stanvars <- c(stanvars, generate_hierarchical_correlation_params(n_groups, n_subgroups))

    # VAR-specific parameters for hierarchical case
    stanvars$var_hierarchical_params <- stanvar(
      x = NULL,
      name = "var_hierarchical_params",
      scode = glue::glue("
      parameters {{
        // VAR coefficient matrices for each lag
        array[{lags}] matrix[{n_lv}, {n_lv}] A;
        // Latent states
        matrix[n, {n_lv}] LV;
      }}
      ")
    )

    # Hierarchical VAR model implementation  
    stanvars$var_hierarchical_model <- stanvar(
      x = NULL,
      name = "var_hierarchical_model",
      scode = glue::glue("
      model {{
        // Derived hierarchical correlation matrices
        array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_Omega_group;
        for (g in 1 : {n_groups}) {{
          L_Omega_group[g] = combine_cholesky(L_Omega_global, L_deviation_group[g],
                                              alpha_cor);
        }}

        // Hierarchical VAR process with group-specific correlations
        for (i in 1:n_{unit_var}) {{
          for (g in 1:{n_groups}) {{
            // VAR dynamics with hierarchical residual correlation
            for (t in ({lags}+1):n) {{
              vector[{n_subgroups}] mu = rep_vector(0, {n_subgroups});
              for (lag in 1:{lags}) {{
                mu += A[lag] * LV[t-lag, :]';
              }}
              to_vector(LV[group_unit_indices[i, g], t]) ~ multi_normal_cholesky(mu, L_Omega_group[g]);
            }}
          }}
        }}

        // VAR coefficient priors
        for (lag in 1:{lags}) {{
          to_vector(A[lag]) ~ normal(0, 0.5);
        }}
      }}
      ")
    )

    # Use shared hierarchical priors
    stanvars <- c(stanvars, generate_hierarchical_correlation_priors(n_groups))

  } else {
    # Simple VAR case (no grouping)
    if (is_factor_model) {
      # Factor model: fixed variance in covariance, estimate Z
      stanvars$var_params <- stanvar(
        name = "var_params",
        scode = glue::glue("
        parameters {{
          // VAR coefficient matrices for each lag
          array[{lags}] matrix[{n_lv}, {n_lv}] A;
          // Innovation correlation matrix (variances fixed to 1)
          corr_matrix[{n_lv}] Omega;
          // Latent states
          matrix[n, {n_lv}] LV;
        }}
        "),
        block = "parameters"
      )

      # VAR model block for factor case
      stanvars$var_model <- stanvar(
        name = "var_model",
        scode = glue::glue("
        model {{
          // VAR process with fixed variance = 1
          for (t in ({lags}+1):n) {{
            vector[{n_lv}] mu = rep_vector(0, {n_lv});
            for (lag in 1:{lags}) {{
              mu += A[lag] * LV[t-lag, :]';
            }}
            LV[t, :] ~ multi_normal(mu', Omega);
          }}

          // Priors for factor model
          for (lag in 1:{lags}) {{
            to_vector(A[lag]) ~ normal(0, 0.5);
          }}
          Omega ~ lkj_corr(2);
        }}
        "),
        block = "model"
      )
    } else {
      # Non-factor model: estimate full covariance, diagonal Z
      stanvars$var_params <- stanvar(
        name = "var_params",
        scode = glue::glue("
        parameters {{
          // VAR coefficient matrices for each lag
          array[{lags}] matrix[{n_lv}, {n_lv}] A;
          // Innovation covariance matrix
          cov_matrix[{n_lv}] Sigma;
          // Latent states
          matrix[n, {n_lv}] LV;
        }}
        "),
        block = "parameters"
      )

      # VAR model block for non-factor case
      stanvars$var_model <- stanvar(
        name = "var_model",
        scode = glue::glue("
        model {{
          // VAR process
          for (t in ({lags}+1):n) {{
            vector[{n_lv}] mu = rep_vector(0, {n_lv});
            for (lag in 1:{lags}) {{
              mu += A[lag] * LV[t-lag, :]';
            }}
            LV[t, :] ~ multi_normal(mu', Sigma);
          }}

          // Priors for non-factor model
          for (lag in 1:{lags}) {{
            to_vector(A[lag]) ~ normal(0, 0.5);
          }}
          Sigma ~ inv_wishart({n_lv} + 1, diag_matrix(rep_vector(1, {n_lv})));
        }}
        "),
        block = "model"
      )
    }
    
    # Use shared factor model priors if applicable
    if (is_factor_model) {
      stanvars <- c(stanvars, generate_factor_model_priors(is_factor_model, n_lv))
    }
  }
  
  # Use shared trend computation utility for universal pattern
  stanvars <- c(stanvars, generate_trend_computation_code(n_lv, n_series))

  return(stanvars)
}

#' AR Trend Generator
#'
#' Generates Stan code components for autoregressive trends.
#' Supports factor models, hierarchical correlations, and consistent matrix Z patterns.
#'
#' @param trend_spec Trend specification for AR model
#' @param data_info Data information including dimensions
#' @return List of stanvars for AR trend
#' @noRd
generate_ar_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% 1
  lags <- trend_spec$lags %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'
  unit_var <- trend_spec$unit %||% "time"
  
  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Initialize stanvars list
  stanvars <- list()
  
  # Use shared matrix Z utility for consistent factor model patterns
  stanvars <- c(stanvars, generate_matrix_z_stanvars(is_factor_model, n_lv, n_series))

  if (use_grouping) {
    # Hierarchical AR case: AR(p = 1, unit = site, gr = group, subgr = species)
    n_groups <- data_info$n_groups %||% 1
    n_subgroups <- data_info$n_subgroups %||% n_lv

    # Add shared hierarchical correlation utilities
    stanvars <- c(stanvars, generate_hierarchical_functions())
    stanvars <- c(stanvars, generate_hierarchical_correlation_params(n_groups, n_subgroups))

    # AR-specific parameters for hierarchical case
    stanvars$ar_hierarchical_params <- stanvar(
      x = NULL,
      name = "ar_hierarchical_params",
      scode = glue::glue("
      parameters {{
        // AR coefficients for each latent variable
        matrix[{n_lv}, {lags}] phi;
        // Latent states
        matrix[n, {n_lv}] LV;
      }}
      ")
    )

    # Hierarchical AR model implementation
    stanvars$ar_hierarchical_model <- stanvar(
      x = NULL,
      name = "ar_hierarchical_model",
      scode = glue::glue("
      model {{
        // Derived hierarchical correlation matrices
        array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_Omega_group;
        for (g in 1 : {n_groups}) {{
          L_Omega_group[g] = combine_cholesky(L_Omega_global, L_deviation_group[g],
                                              alpha_cor);
        }}

        // Hierarchical AR process with group-specific correlations
        for (i in 1:n_{unit_var}) {{
          for (g in 1:{n_groups}) {{
            // AR dynamics with hierarchical residual correlation
            for (t in ({lags}+1):n) {{
              vector[{n_subgroups}] mu = rep_vector(0, {n_subgroups});
              for (lag in 1:{lags}) {{
                for (j in 1:{n_lv}) {{
                  mu[j] += phi[j, lag] * LV[t-lag, j];
                }}
              }}
              to_vector(LV[group_unit_indices[i, g], t]) ~ multi_normal_cholesky(mu, L_Omega_group[g]);
            }}
          }}
        }}

        // AR coefficient priors
        to_vector(phi) ~ normal(0, 0.5);
      }}
      ")
    )

    # Use shared hierarchical priors
    stanvars <- c(stanvars, generate_hierarchical_correlation_priors(n_groups))

  } else {
    # Simple AR case (no grouping)
    if (is_factor_model) {
      # Factor model: fixed variance = 1, estimate Z
      stanvars$ar_params <- stanvar(
        name = "ar_params",
        scode = glue::glue("
        parameters {{
          // AR coefficients for each latent variable
          matrix[{n_lv}, {lags}] phi;
          // Raw latent states for factor model (variance = 1)
          matrix[n, {n_lv}] LV_raw;
        }}
        "),
        block = "parameters"
      )
      
      stanvars$ar_transformed <- stanvar(
        name = "ar_transformed",
        scode = glue::glue("
        transformed parameters {{
          // Apply AR dynamics with fixed variance = 1
          matrix[n, {n_lv}] LV;
          LV = LV_raw;
          
          for (j in 1:{n_lv}) {{
            for (t in ({lags}+1):n) {{
              real mu = 0;
              for (lag in 1:{lags}) {{
                mu += phi[j, lag] * LV[t-lag, j];
              }}
              LV[t, j] = mu + LV_raw[t, j];
            }}
          }}
        }}
        "),
        block = "tparameters"
      )
    } else {
      # Non-factor model: estimate variances, diagonal Z  
      stanvars$ar_params <- stanvar(
        name = "ar_params",
        scode = glue::glue("
        parameters {{
          // AR coefficients for each latent variable
          matrix[{n_lv}, {lags}] phi;
          // Innovation standard deviations
          vector<lower=0>[{n_lv}] sigma;
          // Latent states
          matrix[n, {n_lv}] LV;
        }}
        "),
        block = "parameters"
      )

      # AR model block for non-factor case
      stanvars$ar_model <- stanvar(
        name = "ar_model",
        scode = glue::glue("
        model {{
          // AR process for each series independently
          for (j in 1:{n_lv}) {{
            for (t in ({lags}+1):n) {{
              real mu = 0;
              for (lag in 1:{lags}) {{
                mu += phi[j, lag] * LV[t-lag, j];
              }}
              LV[t, j] ~ normal(mu, sigma[j]);
            }}
          }}

          // Priors for non-factor model
          to_vector(phi) ~ normal(0, 0.5);
          sigma ~ student_t(3, 0, 2.5);
        }}
        "),
        block = "model"
      )
    }

    # Use shared factor model priors if applicable
    if (is_factor_model) {
      stanvars <- c(stanvars, generate_factor_model_priors(is_factor_model, n_lv))
      
      # Add AR-specific priors for factor model
      stanvars$ar_factor_priors <- stanvar(
        name = "ar_factor_priors", 
        scode = glue::glue("
        model {{
          // AR coefficient priors for factor model
          to_vector(phi) ~ normal(0, 0.5);
        }}
        "),
        block = "model"
      )
    }
  }
  
  # Use shared trend computation utility for universal pattern
  stanvars <- c(stanvars, generate_trend_computation_code(n_lv, n_series))

  return(stanvars)
}

#' CAR Trend Generator
#'
#' Generates Stan code components for conditional autoregressive trends.
#' Supports hierarchical correlations but NOT factor models (spatial structure incompatible).
#'
#' @param trend_spec Trend specification for CAR model
#' @param data_info Data information including dimensions
#' @return List of stanvars for CAR trend
#' @noRd
generate_car_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% 1
  car_order <- trend_spec$car_order %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'
  unit_var <- trend_spec$unit %||% "time"

  # CAR does not support factor models (spatial structure incompatible)
  is_factor_model <- FALSE

  # Initialize stanvars list
  stanvars <- list()
  
  # Use shared matrix Z utility (always diagonal for CAR)
  stanvars <- c(stanvars, generate_matrix_z_stanvars(is_factor_model, n_lv, n_series))

  # CAR data (adjacency structure - needed for all cases)
  stanvars$car_data <- stanvar(
    x = list(
      n_neighbors = 4,  # Simple 2D grid assumption
      adj_matrix = matrix(0, nrow = n_lv, ncol = n_lv)
    ),
    name = "car_data",
    scode = "
    int<lower=0> n_neighbors;
    matrix[n_lv, n_lv] adj_matrix;
    "
  )

  if (use_grouping) {
    # Hierarchical CAR case: CAR(unit = site, gr = group, subgr = species)
    n_groups <- data_info$n_groups %||% 1
    n_subgroups <- data_info$n_subgroups %||% n_lv

    # Add shared hierarchical correlation utilities
    stanvars <- c(stanvars, generate_hierarchical_functions())
    stanvars <- c(stanvars, generate_hierarchical_correlation_params(n_groups, n_subgroups))

    # CAR-specific parameters for hierarchical case
    stanvars$car_hierarchical_params <- stanvar(
      x = NULL,
      name = "car_hierarchical_params",
      scode = glue::glue("
      parameters {{
        // CAR precision parameter
        real<lower=0> tau;
        // Spatial autocorrelation
        real<lower=0, upper=1> rho;
        // Latent spatial states
        matrix[n, {n_lv}] LV;
      }}
      ")
    )

    # Hierarchical CAR model implementation
    stanvars$car_hierarchical_model <- stanvar(
      x = NULL,
      name = "car_hierarchical_model",
      scode = glue::glue("
      model {{
        // Derived hierarchical correlation matrices
        array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_Omega_group;
        for (g in 1 : {n_groups}) {{
          L_Omega_group[g] = combine_cholesky(L_Omega_global, L_deviation_group[g],
                                              alpha_cor);
        }}

        // Hierarchical CAR process with group-specific correlations
        for (i in 1:n_{unit_var}) {{
          for (g in 1:{n_groups}) {{
            // CAR spatial correlation with hierarchical residual correlation
            for (t in 1:n) {{
              vector[{n_subgroups}] spatial_mu = rep_vector(0, {n_subgroups});
              
              // Compute spatial means for CAR process
              for (j in 1:{n_lv}) {{
                real mu = 0;
                real var_adj = 0;
                for (k in 1:{n_lv}) {{
                  if (adj_matrix[j, k] > 0) {{
                    mu += adj_matrix[j, k] * LV[t, k];
                    var_adj += adj_matrix[j, k];
                  }}
                }}
                if (var_adj > 0) {{
                  spatial_mu[j] = rho * mu / var_adj;
                }}
              }}
              
              // Apply CAR with hierarchical correlation structure
              to_vector(LV[group_unit_indices[i, g], t]) ~ multi_normal_cholesky(spatial_mu, 
                                                                                 L_Omega_group[g] / sqrt(tau));
            }}
          }}
        }}

        // CAR parameter priors
        tau ~ gamma(2, 1);
        rho ~ beta(1, 1);
      }}
      ")
    )

    # Use shared hierarchical priors
    stanvars <- c(stanvars, generate_hierarchical_correlation_priors(n_groups))

  } else {
    # Simple CAR case (no grouping)
    stanvars$car_params <- stanvar(
      name = "car_params",
      scode = glue::glue("
      parameters {{
        // CAR precision parameter
        real<lower=0> tau;
        // Spatial autocorrelation
        real<lower=0, upper=1> rho;
        // Latent spatial states
        matrix[n, {n_lv}] LV;
      }}
      "),
      block = "parameters"
    )

    # CAR model block for simple case
    stanvars$car_model <- stanvar(
      name = "car_model",
      scode = glue::glue("
      model {{
        // CAR prior for spatial correlation
        for (t in 1:n) {{
          for (i in 1:{n_lv}) {{
            real mu = 0;
            real var_adj = 0;
            // Sum over neighbors
            for (j in 1:{n_lv}) {{
              if (adj_matrix[i, j] > 0) {{
                mu += adj_matrix[i, j] * LV[t, j];
                var_adj += adj_matrix[i, j];
              }}
            }}
            if (var_adj > 0) {{
              mu = rho * mu / var_adj;
              LV[t, i] ~ normal(mu, sqrt(1.0 / (tau * var_adj)));
            }} else {{
              LV[t, i] ~ normal(0, sqrt(1.0 / tau));
            }}
          }}
        }}

        // Priors
        tau ~ gamma(2, 1);
        rho ~ beta(1, 1);
      }}
      "),
      block = "model"
    )
  }
  
  # Use shared trend computation utility for universal pattern
  stanvars <- c(stanvars, generate_trend_computation_code(n_lv, n_series))

  return(stanvars)
}

#' GP Trend Generator
#'
#' Generates Stan code components for Gaussian process trends.
#'
#' @param trend_spec Trend specification for GP model
#' @param data_info Data information including dimensions
#' @return List of stanvars for GP trend
#' @noRd
# GP trends are handled via trend_formula, not as temporal dynamics
generate_gp_injection_stanvars <- function(trend_spec, data_info) {
  stop(insight::format_error(
    "GP trends are handled via trend_formula, not as temporal dynamics.",
    "Use: trend_formula = ~ gp(time, k = 10)..."
  ))
}

# Factor trend generator removed - factor models are now a capability of compatible trend types (AR, RW, VAR)
# Factor models are detected by n_lv < n_series on compatible trends, not as a separate trend type

#' ZMVN Trend Generator
#'
#' Generates Stan code components for zero-mean multivariate normal trends.
#'
#' @param trend_spec Trend specification for ZMVN model
#' @param data_info Data information including dimensions
#' @return List of stanvars for ZMVN trend
#' @noRd
generate_zmvn_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters following original ZMVN pattern
  n_lv <- trend_spec$n_lv %||% data_info$n_lv %||% data_info$n_series %||% 1
  n <- data_info$n_obs
  n_series <- data_info$n_series %||% 1
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'
  unit_var <- trend_spec$unit %||% "time"
  
  # Determine if this is a factor model (n_lv < n_series)
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series

  # Initialize stanvars list
  stanvars <- list()
  
  # Use shared matrix Z utility for consistent factor model patterns
  stanvars <- c(stanvars, generate_matrix_z_stanvars(is_factor_model, n_lv, n_series))

  if (use_grouping) {
    # Hierarchical ZMVN case: ZMVN(unit = site, gr = group, subgr = species)
    n_groups <- data_info$n_groups %||% 1
    n_subgroups <- data_info$n_subgroups %||% n_lv

    # Add shared hierarchical correlation utilities
    stanvars <- c(stanvars, generate_hierarchical_functions())
    stanvars <- c(stanvars, generate_hierarchical_correlation_params(n_groups, n_subgroups))

    # ZMVN-specific parameters for hierarchical case
    stanvars$zmvn_hierarchical_params <- stanvar(
      x = NULL,
      name = "zmvn_hierarchical_params",
      scode = glue::glue("
      parameters {{
        // Latent states for ZMVN
        matrix[n, {n_lv}] LV;
      }}
      ")
    )

    # Hierarchical ZMVN model implementation
    stanvars$zmvn_hierarchical_model <- stanvar(
      x = NULL,
      name = "zmvn_hierarchical_model",
      scode = glue::glue("
      model {{
        // Derived hierarchical correlation matrices
        array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_Omega_group;
        for (g in 1 : {n_groups}) {{
          L_Omega_group[g] = combine_cholesky(L_Omega_global, L_deviation_group[g],
                                              alpha_cor);
        }}

        // ZMVN residual correlation by {unit_var}
        for (i in 1:n_{unit_var}) {{
          for (g in 1:{n_groups}) {{
            to_vector(LV[group_unit_indices[i, g]]) ~ multi_normal_cholesky(rep_vector(0, {n_subgroups}),
                                                                             L_Omega_group[g]);
          }}
        }}
      }}
      ")
    )

    # Use shared hierarchical priors
    stanvars <- c(stanvars, generate_hierarchical_correlation_priors(n_groups))

  } else {
    # Simple ZMVN case: ZMVN(unit = site, subgr = species)
    stanvars$zmvn_simple_params <- stanvar(
      x = NULL,
      name = "zmvn_simple_params",
      scode = glue::glue("
      parameters {{
        // correlation matrix for ZMVN
        cholesky_factor_corr[{n_lv}] L_Omega;
        // Latent states for ZMVN
        matrix[n, {n_lv}] LV;
      }}
      ")
    )

    stanvars$zmvn_simple_model <- stanvar(
      x = NULL,
      name = "zmvn_simple_model",
      scode = glue::glue("
      model {{
        // Simple ZMVN residual correlation by {unit_var}
        L_Omega ~ lkj_corr_cholesky(2);
        for (i in 1:n_{unit_var}) {{
          to_vector(LV[{unit_var}_indices[i]]) ~ multi_normal_cholesky(rep_vector(0, {n_lv}), L_Omega);
        }}
      }}
      ")
    )
  }
  
  # Use shared trend computation utility for universal pattern
  stanvars <- c(stanvars, generate_trend_computation_code(n_lv, n_series))
  
  # Use shared factor model priors if applicable (only for factor models)
  if (is_factor_model) {
    stanvars <- c(stanvars, generate_factor_model_priors(is_factor_model, n_lv))
  }

  return(stanvars)
}

#' PW Trend Generator
#'
#' Generates Stan code components for piecewise trends.
#'
#' @param trend_spec Trend specification for PW model
#' @param data_info Data information including dimensions
#' @return List of stanvars for PW trend
#' @noRd
generate_pw_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% 1
  n_changepoints <- trend_spec$n_changepoints %||% 1
  n <- data_info$n_obs

  # Initialize stanvars list
  stanvars <- list()

  # PW parameters
  stanvars$pw_params <- stanvar(
    name = "pw_params",
    scode = glue::glue("
    parameters {{
      // Changepoint locations
      vector<lower=1, upper=n>[{n_changepoints}] changepoints;
      // Trend levels for each segment
      matrix[{n_changepoints}+1, {n_lv}] levels;
      // Innovation variance
      vector<lower=0>[{n_lv}] sigma;
    }}
    "),
    block = "parameters"
  )

  # PW model block
  stanvars$pw_model <- stanvar(
    name = "pw_model",
    scode = glue::glue("
    model {{
      // Priors for changepoints (uniform over time)
      changepoints ~ uniform(1, n);
      
      // Priors for levels
      to_vector(levels) ~ normal(0, 1);
      
      // Prior for innovation variance
      sigma ~ student_t(3, 0, 2.5);
    }}
    "),
    block = "model"
  )

  return(stanvars)
}

#' None Trend Generator (placeholder)
#'
#' Returns empty stanvars for no-trend models.
#'
#' @param trend_spec Trend specification (ignored)
#' @param data_info Data information (ignored)
#' @return Empty list
#' @noRd
generate_none_trend_stanvars <- function(trend_spec, data_info) {
  list()  # No trend components needed
}

# Note: Trend registry functionality moved to R/trend_registry.R
# This avoids duplicate registry definitions and centralizes all trend registration

#' Validate Trend Specification
#'
#' Checks if a trend specification is valid for code generation.
#'
#' @param trend_spec Trend specification object
#' @return Logical indicating validity
#' @noRd
validate_trend_spec <- function(trend_spec) {
  if (!is.list(trend_spec)) return(FALSE)
  if (is.null(trend_spec$trend_type)) return(FALSE)
  if (!trend_spec$trend_type %in% list_trend_types()) return(FALSE)

  TRUE
}

# Custom stanvar wrapper removed - using brms::stanvar directly with proper block specification
