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

  # Initialize stanvars list
  stanvars <- list()

  if (correlation && n_lv > 1) {
    # Correlated case: RW(cor = TRUE) - multivariate approach
    stanvars$rw_corr_data <- stanvar(
      x = list(
        n_lv = n_lv,
        lv_coefs = matrix(1, nrow = n_lv, ncol = n_lv)
      ),
      name = "rw_corr_data",
      scode = "
      int<lower=1> n_lv;
      matrix[n_lv, n_lv] lv_coefs;
      "
    )

    stanvars$rw_corr_params <- stanvar(
      name = "rw_corr_params",
      scode = glue::glue("
      parameters {{
        // Latent variable correlations
        cholesky_factor_corr[{n_lv}] L_Omega;
        // Latent variable SDs
        vector<lower=0>[{n_lv}] sigma_lv;
        // Raw latent states for non-centered parameterization
        matrix[n, {n_lv}] LV_raw;
      }}
      "),
      block = "parameters"
    )

    stanvars$rw_corr_transformed <- stanvar(
      name = "rw_corr_transformed",
      scode = glue::glue("
      transformed parameters {{
        // Combined covariance matrix
        matrix[{n_lv}, {n_lv}] Sigma = diag_pre_multiply(sigma_lv, L_Omega);
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
      }}
      "),
      block = "transformed parameters"
    )

  } else {
    # Simple uncorrelated case: RW() - non-centered parameterization
    stanvars$rw_simple_params <- stanvar(
      name = "rw_simple_params",
      scode = glue::glue("
      parameters {{
        // latent state SD parameters for RW
        vector<lower=0>[{n_lv}] sigma;
        // raw latent states for non-centered parameterization
        matrix[n, {n_lv}] LV_raw;
      }}
      "),
      block = "parameters"
    )

    stanvars$rw_simple_transformed <- stanvar(
      name = "rw_simple_transformed",
      scode = glue::glue("
      transformed parameters {{
        // latent states with non-centered parameterization
        matrix[n, {n_lv}] LV;

        // Apply non-centered RW transformation
        LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));
        for (j in 1:{n_lv}) {{
          for (i in 2:n) {{
            LV[i, j] = LV[i-1, j] + LV[i, j];
          }}
        }}
      }}
      "),
      block = "transformed parameters"
    )

    stanvars$rw_simple_model <- stanvar(
      name = "rw_simple_model",
      scode = glue::glue("
      model {{
        // priors for RW innovations
        to_vector(LV_raw) ~ std_normal();
        sigma ~ student_t(3, 0, 2.5);
      }}
      "),
      block = "model"
    )
  }

  return(stanvars)
}

#' VAR Trend Generator
#'
#' Generates Stan code components for vector autoregressive trends.
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

  # Initialize stanvars list
  stanvars <- list()

  # VAR data block
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

  # VAR parameters
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

  # VAR model block
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

      // Priors
      for (lag in 1:{lags}) {{
        to_vector(A[lag]) ~ normal(0, 0.5);
      }}
      Sigma ~ inv_wishart({n_lv} + 1, diag_matrix(rep_vector(1, {n_lv})));
    }}
    "),
    block = "model"
  )

  return(stanvars)
}

#' AR Trend Generator
#'
#' Generates Stan code components for autoregressive trends.
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

  # Initialize stanvars list
  stanvars <- list()

  # AR parameters
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

  # AR model block
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

      // Priors
      to_vector(phi) ~ normal(0, 0.5);
      sigma ~ student_t(3, 0, 2.5);
    }}
    "),
    block = "model"
  )

  return(stanvars)
}

#' CAR Trend Generator
#'
#' Generates Stan code components for conditional autoregressive trends.
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

  # Initialize stanvars list
  stanvars <- list()

  # CAR data (adjacency structure)
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

  # CAR parameters
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

  # CAR model block
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

#' Factor Trend Generator
#'
#' Generates Stan code components for dynamic factor trends.
#'
#' @param trend_spec Trend specification for factor model
#' @param data_info Data information including dimensions
#' @return List of stanvars for factor trend
#' @noRd
generate_factor_trend_stanvars <- function(trend_spec, data_info) {
  # Extract key parameters
  n_lv <- trend_spec$n_lv %||% 2
  n_factors <- trend_spec$n_factors %||% 1
  n <- data_info$n_obs

  # Initialize stanvars list
  stanvars <- list()

  # Factor parameters
  stanvars$factor_params <- stanvar(
    name = "factor_params",
    scode = glue::glue("
    parameters {{
      // Factor loadings
      matrix[{n_lv}, {n_factors}] lambda;
      // Common factors
      matrix[n, {n_factors}] factors;
      // Idiosyncratic errors
      matrix[n, {n_lv}] idio;
      // Factor innovation SDs
      vector<lower=0>[{n_factors}] sigma_factors;
      // Idiosyncratic SDs
      vector<lower=0>[{n_lv}] sigma_idio;
    }}
    "),
    block = "parameters"
  )

  # Factor transformed parameters
  stanvars$factor_transformed <- stanvar(
    name = "factor_transformed",
    scode = glue::glue("
    transformed parameters {{
      // Latent variables as factor model
      matrix[n, {n_lv}] LV = factors * lambda' + idio;
    }}
    "),
    block = "transformed parameters"
  )

  # Factor model block
  stanvars$factor_model <- stanvar(
    name = "factor_model",
    scode = glue::glue("
    model {{
      // Factor dynamics (RW)
      for (j in 1:{n_factors}) {{
        factors[1, j] ~ normal(0, sigma_factors[j]);
        for (t in 2:n) {{
          factors[t, j] ~ normal(factors[t-1, j], sigma_factors[j]);
        }}
      }}

      // Idiosyncratic errors
      for (j in 1:{n_lv}) {{
        for (t in 1:n) {{
          idio[t, j] ~ normal(0, sigma_idio[j]);
        }}
      }}

      // Priors
      to_vector(lambda) ~ normal(0, 1);
      sigma_factors ~ student_t(3, 0, 2.5);
      sigma_idio ~ student_t(3, 0, 2.5);
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

# Trend Registry
#' @noRd
.trend_registry <- list(
  RW = list(
    name = "Random Walk",
    generator = generate_rw_trend_stanvars,
    forecast_function = "forecast_rw"
  ),
  VAR = list(
    name = "Vector Autoregressive",
    generator = generate_var_trend_stanvars,
    forecast_function = "forecast_var"
  ),
  AR = list(
    name = "Autoregressive",
    generator = generate_ar_trend_stanvars,
    forecast_function = "forecast_ar"
  ),
  CAR = list(
    name = "Conditional Autoregressive",
    generator = generate_car_trend_stanvars,
    forecast_function = "forecast_car"
  ),
  Factor = list(
    name = "Dynamic Factor",
    generator = generate_factor_trend_stanvars,
    forecast_function = "forecast_factor"
  ),
  None = list(
    name = "No Trend",
    generator = generate_none_trend_stanvars,
    forecast_function = "forecast_none"
  )
)

#' Get Trend Information from Registry
#'
#' @param trend_type Character string identifying trend type
#' @return List with trend information or NULL if not found
#' @noRd
get_trend_info <- function(trend_type) {
  .trend_registry[[trend_type]]
}

#' List Available Trend Types
#'
#' @return Character vector of available trend type names
#' @noRd
list_trend_types <- function() {
  names(.trend_registry)
}

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

#' Wrapper for brms stanvar function
#'
#' Creates stanvar objects for Stan code injection.
#'
#' @param x Data object (can be NULL for code-only stanvars)
#' @param name Character string name
#' @param scode Character string Stan code
#' @param block Character string indicating Stan block (optional)
#' @return brms stanvar object
#' @noRd
stanvar <- function(x = NULL, name, scode, block = NULL) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop(insight::format_error("Package {.pkg brms} required for stanvar creation"))
  }

  # If x is NULL and no block specified, determine block from scode
  if (is.null(x) && is.null(block)) {
    if (grepl("parameters\\s*\\{", scode)) {
      block <- "parameters"
    } else if (grepl("transformed parameters\\s*\\{", scode)) {
      block <- "transformed parameters"
    } else if (grepl("model\\s*\\{", scode)) {
      block <- "model"
    } else if (grepl("generated quantities\\s*\\{", scode)) {
      block <- "generated quantities"
    } else {
      block <- "parameters"  # default fallback
    }
  }

  # Use brms stanvar function with appropriate parameters
  if (is.null(x)) {
    brms::stanvar(scode = scode, block = block, name = name)
  } else {
    brms::stanvar(x = x, name = name, scode = scode)
  }
}
