# Stan Data Flow Pipeline

This document follows a model from the user's call to the Stan
program and data mvgam samples, in the order the code runs.
`architecture-decisions.md` gives the reasons for the design.

## Entry points

`mvgam()`, `stancode()` and `standata()` all reach
`generate_stan_components_mvgam_formula()` (`R/make_stan.R`), which
wraps `build_stan_components()`. brms validates the same data frame
several times while generating code, and the wrapper,
`warn_once_per_call()`, keeps each of its notices to one per call.

```
mvgam()                         R/mvgam_core.R
└─ mvgam_single()
   ├─ generate_stan_components_mvgam_formula()
   ├─ compile_model(), fit_model()          R/backends.R
   └─ create_mvgam_from_combined_fit()
stancode.mvgam_formula()        R/make_stan.R
standata.mvgam_formula()        R/make_stan.R
   └─ generate_stan_components_mvgam_formula()
```

## Steps in `build_stan_components()`

### 1. Families and formula
`resolve_observation_family()` (`R/families.R`) resolves one family
per response and refuses the families mvgam does not support.
`parse_multivariate_trends()` (`R/brms_integration.R`) parses the
observation formula and the trend formula and returns the
multivariate specification: the response names, the trend
specification per response, the trend formula's regular terms and
whether the model has a trend. `prepare_trend_specs()` then attaches
the `trend_map` and the loadings prior and refuses a factor request
on a trend without a factor form.

### 2. Threading
`suppress_brms_threading()` sets `threads = 1` for the brms calls
where brms threading would place the observation predictor out of
the trend injector's reach, or would wrap a family's own
`reduce_sum`. `warn_threads_trend_brms_native()` warns when that
leaves the user's request without effect.

### 3. The observation program
`setup_brms_lightweight()` (`R/brms_integration.R`) runs brms with
`backend = "mock"` on the observation formula, the user's data, the
observation priors and the family stanvars. It returns the brms
Stan code, the Stan data, the priors and the brms terms. brms drops
rows with a missing response and orders the data its own way, and
its Stan data follow brms's row order.

### 4. Trend data and axes
For a model with a trend, `extract_and_validate_trend_components()`
(`R/validations.R`) prepares the data's time and series attributes
with `ensure_mvgam_variables()`, then calls
`extract_time_series_dimensions()`. That call resolves the series,
time and factor axes once and builds, for every response, the two
mapping arrays from observations to the trend:

- `obs_trend_time[N]`: the time index of each observation brms
  scores, found by matching the observation's time against the
  ordered unique times;
- `obs_trend_series[N]`: the series index of each such observation,
  found the same way against the ordered series.

Only the rows with an observed response enter the arrays, and the
arrays align with brms's Stan data as a result. The function checks
the grid (`refuse_ragged_trend_grid()`,
`validate_regular_time_intervals()`, `validate_gr_balanced_groups()`),
builds the trend data with `trend_cell_frame()` and records the axes
on `trend_metadata`.

The axis record holds, under `axes$series`, the ordered levels, how
they were derived, their count, the group of each and the last time
each was observed; under `axes$time`, the user's ordered times, the
integer index and the step; under `axes$factor`, `n_lv`; under
`axes$grain`, what the second dimension of `times_trend` indexes;
and under `axes$vars`, the columns that place a row.

### 5. The trend program
`setup_brms_lightweight()` runs again on the trend formula and the
trend data, with a `gaussian()` placeholder family and the trend
priors with their suffix removed. Its Stan code supplies
`mu_trend`.

### 6. Assembly: `generate_combined_stancode()`
`R/stan_assembly.R` assembles the program in this order.

`extract_trend_stanvars_from_setup()` builds the trend stanvars.
`extract_and_rename_stan_blocks()` takes the trend program's blocks
and renames its variables with the `_trend` suffix, and
`extract_mu_construction_with_classification()`
(`R/mu_expression_analysis.R`) finds the statements that construct
its `mu`, which become `mu_trend`. `filter_block_content()` drops the
declarations the observation program already makes, and the trend
program's `Y`, `prior_only` and `N` never enter.
`create_times_trend_matrix()` builds `times_trend`, the
`[N_time_trend, N_series_trend]` array of rows of the trend data.
The mapping arrays from step 4 become data stanvars with a response
suffix. `detect_glm_usage()` inspects the observation program, and
a GLM likelihood gets a `mu_ones` data stanvar for the rewritten
call. `generate_trend_specific_stanvars()` adds the dimension
stanvars (`N_trend`, `N_time_trend`, `N_series_trend`,
`N_lv_trend`), the shared innovations and the trend's own Stan code
from its registered generator.

`sort_stanvars()` orders the stanvars by dependency: dimensions, then
the arrays those dimensions size, then the rest. Each stanvar is then
declared before its first use. `generate_base_stancode_with_stanvars()`
writes the observation program with the trend stanvars included.

`inject_trend_into_linear_predictors(base_stancode, resps)` then adds
the trend to each response's predictor. `resps` is `""` for a
univariate model and the brms response keys for a multivariate one,
and `<sfx>` below is `""` or `_<resp>`. For each response,
`inject_trend_for_response()` adds
`mu<sfx>[n] += trend[obs_trend_time<sfx>[n], obs_trend_series<sfx>[n]]`
in a loop:

- A GLM likelihood on `Y<sfx>`: `rewrite_glm_for_trend()` declares
  `mu<sfx> = design * coefs`, adds the intercept and the trend and
  rewrites the call to take `to_matrix(mu<sfx>)` and `mu_ones<sfx>`.
  brms writes an offset model's GLM call with the declared `mu<sfx>`
  as its intercept, and that call takes the next path unchanged.
- A built predictor: `add_trend_to_built_predictor()` places the loop
  before the first statement that transforms `mu<sfx>` (an inverse
  link, or the skew-normal mean shift), outside any loop enclosing
  it. Without a transform, the loop follows the last statement that
  builds `mu<sfx>`.

An unknown GLM family, or a model block with no single correct place
for the trend, raises `stop_mvgam_fault()`.

`deduplicate_stan_functions()` removes function definitions that the
two programs both brought, and `generate_base_brms_standata()`
builds the Stan data with the stanvar data merged in.

### 7. Polishing and checks
`polish_generated_stan_code()` formats the assembled program once,
for `mvgam()` and `stancode()` alike, and may move statements between
blocks. `validate_stan_code()` then parses the polished program with
the backend's own parser, which is the program that gets compiled.
`warn_confounded_design()` stacks the observation and trend designs
one likelihood sees together and warns when their coefficients are
not separately identified.

## GLM likelihoods

brms writes a GLM likelihood for several families, and mvgam keeps
it. `glm_call_layout` (`R/glm_analysis.R`) records the argument roles
of `normal_id_glm`, `poisson_log_glm`, `neg_binomial_2_log_glm`,
`bernoulli_logit_glm` and `ordered_logistic_glm`.
`categorical_logit_glm` has no entry because mvgam refuses
`brms::categorical()`. `parse_glm_parameters_from_line()` maps each
argument of a call to its role. The rewritten call computes the
predictor as `Xc * b` in one matrix product, adds the intercept and
the trend in one loop and passes the result as a design matrix of
one column with `mu_ones` as its coefficient, which keeps Stan's GLM
implementation.
