# Hierarchical Correlation Specification

## Expected Stan Code Structure

### Data Block
```stan
int<lower=1> N_groups_trend;                           // Number of groups
array[N_series_trend] int<lower=1> group_inds_trend;   // Maps series to groups
```

### Parameters Block
```stan
cholesky_factor_corr[N_subgroups_trend] L_Omega_global_trend;
array[N_groups_trend] cholesky_factor_corr[N_subgroups_trend] L_deviation_group_trend;
real<lower=0, upper=1> alpha_cor_trend;
```

### Functions Block
```stan
matrix combine_cholesky(matrix global_chol_cor, matrix local_chol_cor, real alpha) {
  int dim = rows(local_chol_cor);
  matrix[dim, dim] global_cor = multiply_lower_tri_self_transpose(global_chol_cor);
  matrix[dim, dim] local_cor = multiply_lower_tri_self_transpose(local_chol_cor);
  return cholesky_decompose(alpha * global_cor + (1 - alpha) * local_cor);
}
```

### Transformed Parameters Block
```stan
array[N_groups_trend] cholesky_factor_corr[N_subgroups_trend] L_Omega_group_trend;
array[N_groups_trend] cov_matrix[N_subgroups_trend] Sigma_group_trend;

for (g_idx in 1:N_groups_trend) {
  L_Omega_group_trend[g_idx] = combine_cholesky(
    L_Omega_global_trend, 
    L_deviation_group_trend[g_idx], 
    alpha_cor_trend
  );
  
  Sigma_group_trend[g_idx] = multiply_lower_tri_self_transpose(
    diag_pre_multiply(sigma_trend, L_Omega_group_trend[g_idx])
  );
}
```

### Model Block
```stan
alpha_cor_trend ~ beta(3, 2);
L_Omega_global_trend ~ lkj_corr_cholesky(1);
for (g_idx in 1:N_groups_trend) {
  L_deviation_group_trend[g_idx] ~ lkj_corr_cholesky(6);
}
```

## How Different Trend Types Use Sigma_group_trend

### AR/ZMVN Models (Simple Innovation Scaling)
```stan
// Direct application to innovations at each time point
for (t in 1:N_time_trend) {
  for (s in 1:N_series_trend) {
    int group_id = group_inds_trend[s];
    // Scale innovations using group-specific covariance
    scaled_innovations_trend[t, s] = 
      (Sigma_group_trend[group_id] * raw_innovations_trend[t, :]')[s];
  }
}
```

### VAR Models (Coefficient Matrix Transformation + Innovation Scaling)
```stan
// Step 1: Group-specific coefficient matrices with stationarity constraints
array[N_groups_trend, N_lags_trend] matrix[N_subgroups_trend, N_subgroups_trend] A_group_trend;

for (g_idx in 1:N_groups_trend) {
  for (lag in 1:N_lags_trend) {
    // Apply Heaps transformation using group-specific covariance
    array[1] matrix[N_subgroups_trend, N_subgroups_trend] P_group;
    P_group[1] = AtoP(A_raw_group_trend[g_idx, lag]);
    array[2, 1] matrix[N_subgroups_trend, N_subgroups_trend] result_group = 
      rev_mapping(P_group, Sigma_group_trend[g_idx]);
    A_group_trend[g_idx, lag] = result_group[1, 1];
  }
}

// Step 2: Block-diagonal assembly (groups don't interact)
cov_matrix[N_lv_trend] Sigma_trend = rep_matrix(0, N_lv_trend, N_lv_trend);
array[N_lags_trend] matrix[N_lv_trend, N_lv_trend] A_trend;

for (g_idx in 1:N_groups_trend) {
  // Insert group covariance into block-diagonal structure
  Sigma_trend[group_inds_trend[g_idx], group_inds_trend[g_idx]] = Sigma_group_trend[g_idx];
  
  for (lag in 1:N_lags_trend) {
    A_trend[lag][group_inds_trend[g_idx], group_inds_trend[g_idx]] = A_group_trend[g_idx, lag];
  }
}
```

## Critical Dimension Distinction

### N_lv_trend vs N_subgroups_trend
**CRITICAL**: Hierarchical models use two different dimensions that serve different purposes:

- **`N_lv_trend`**: Total number of latent variables/series across ALL groups (system-wide dimension)
  - Used for: System-wide matrices like `A_trend`, `Sigma_trend` in VAR models
  - Example: 3 groups × 2 series each = 6 total series → `N_lv_trend = 6`

- **`N_subgroups_trend`**: Number of series/members WITHIN each individual group (within-group dimension)  
  - Used for: Group-specific matrices like `L_Omega_group_trend[g]`, `Sigma_group_trend[g]`
  - Example: Each group has 2 series → `N_subgroups_trend = 2`

### Dimension Usage Rules
- **Group-specific matrices**: Always use `N_subgroups_trend`
  - `array[N_groups_trend] cholesky_factor_corr[N_subgroups_trend] L_Omega_group_trend`
  - `array[N_groups_trend] cov_matrix[N_subgroups_trend] Sigma_group_trend`

- **System-wide matrices**: Always use `N_lv_trend`  
  - `array[N_lags_trend] matrix[N_lv_trend, N_lv_trend] A_trend`
  - `cov_matrix[N_lv_trend] Sigma_trend`

## Key Requirements

### Shared Components (ALL trend types)
- `L_Omega_global_trend`, `L_deviation_group_trend`, `alpha_cor_trend`
- `combine_cholesky()` function  
- `N_groups_trend`, `group_inds_trend` data structures

### Hierarchical vs Factor Model Constraint
❌ **INCOMPATIBLE**: Hierarchical models (`gr` parameter) cannot use factor models (`n_lv` parameter)
- **Reason**: Both provide dimensionality reduction via different mechanisms
- **Hierarchical**: Groups reduce correlation complexity via partial pooling
- **Factor**: Latent variables reduce series dimensionality via factor loadings
- **Error message**: "Hierarchical {trend} models cannot use factor models. Use n_lv = n_series or remove gr/subgr parameters"

### Valid Combinations
✅ **Hierarchical only**: `AR(gr = habitat)`, `VAR(p = 1, gr = site)`, `ZMVN(gr = region)`
✅ **Factor only**: `AR(n_lv = 2)`, `VAR(p = 1, n_lv = 3)`, `ZMVN(n_lv = 2)`
❌ **Both together**: `AR(gr = habitat, n_lv = 2)` → **FORBIDDEN**

### Naming Conventions
- **ALL numbers**: Capital N prefix (`N_groups_trend`, `N_series_trend`, `N_lv_trend`, `N_subgroups_trend`)
- **ALL trend vars**: `_trend` suffix
- **Loop indices**: `g_idx` (consistent)
- **Dimension usage**: `N_subgroups_trend` for within-group, `N_lv_trend` for system-wide

### Critical Errors to Avoid
❌ Variable redefinition  
❌ Wrong function names (`cholesky_compose` doesn't exist)
❌ Mixed naming (`n_groups_trend` vs `N_groups_trend`)
❌ Missing `_trend` suffixes
❌ Combining hierarchical + factor models