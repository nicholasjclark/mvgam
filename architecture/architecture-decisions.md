# mvgam Architecture Decisions

Each section records one design decision, the reason for it and where
the code implements it. `stan-data-flow-pipeline.md` follows a model
through Stan code generation step by step, and `quick-reference.md`
holds formula patterns and function names for day-to-day work.

## 1. The fitted object and the combined Stan program

brms writes two linear predictors from two formulae: `mu` from the
observation formula and `mu_trend` from the trend formula. mvgam
builds each as a brms program with `backend = "mock"`, splices the
trend machinery into the observation program and samples the
combined program once. The fit has class `c("mvgam", "brmsfit")`
and stores the complete stanfit in its `fit` slot. Inheriting from
`brmsfit` lets the brms post-processing methods work on an mvgam fit
and delegate to that slot.

A model's working variables stay out of the posterior, and one
projection names and orders what remains.
`mvgam_excluded_pars()` (`R/par_exclusion.R`) computes before
sampling the names a brms fit leaves out: the standardised deviates
a group-level block is scaled from, the unscaled coefficients a
shrinkage prior scales, the ordered intercepts a mixture is
identified by. The model reports each of these under another name.
The list reaches both backends through `exclude`, and `save_pars()`
states which of them a user keeps, with brms's own meaning.

`mvgam_user_pars()` (`R/as.data.frame.mvgam.R`) maps every Stan name
to the name a user sees and orders the result by class the way brms
orders a fitted object. It leaves out the placeholder column an
empty observation formula is given and the factor block whose
rotation is indeterminate. `variables()`, `extract_mvgam_draws()` and
`tidy()` all take their names from it. `mvgam_par_kind()` and
`mvgam_par_side()` (`R/par_taxonomy.R`) classify a name by kind and
by the side of the model it belongs to. The `variable =` keyword
resolver, the `summary()` blocks and the parameter buckets are
defined in terms of them.

brms renames a fit's parameters in `rename_pars()`, which mvgam does
not run. The `mvgam_*_aliases()` builders rebuild the same names for
every parameter block brms renames, from exported brms functions and
the stored Stan data. `summary()` prints the blocks `brms:::summary.brmsfit()`
reports, as `summary_blocks()` lists them. `MVGAM_FAMILY_DPARS` lists
each family's distributional parameters for the prediction paths and
for the kind `family`.

## 2. The formula interface

mvgam extends the brms formula with a `trend_formula` argument:

```r
mvgam(formula, trend_formula, data, family, ...)
```

A missing or `NULL` `trend_formula` fits a brms model with no
trend. A `trend_formula` without a trend constructor takes the
default `ZMVN()`, as `parse_trend_formula()` resolves it:

```r
mvgam(y ~ x1 + x2, data = data)                      # brms model
mvgam(y ~ x1 + x2, trend_formula = ~ 1, data = data) # ZMVN() trend
mvgam(y ~ x1 + x2, trend_formula = ~ AR(), data = data)
```

The trend formula takes fixed effects, interactions, random effects,
smooths and `gp()` terms. `validate_trend_formula()` refuses the brms
addition terms (`offset()`, `weights()`, `cens()` and the rest) and
the brms autocorrelation terms (`ar()`, `ma()` and the rest), which
it identifies by call head against the `brms_addition_terms` and
`brms_autocor_terms` constants. `mvgam_formula()` refuses a
distributional formula such as `bf(..., sigma ~ z)` in the trend
formula. Distributional formulas belong in the observation formula,
where they take the full brms syntax. The trend enters `mu` alone,
and auxiliary parameters such as `sigma`, `zi` and `hu` follow
brms.

Observation-level residual correlation and state-space dynamics model
different sources of dependence, and one model may include both:

```r
mvgam(count ~ Trt + unstr(visit, patient), trend_formula = ~ AR(p = 1),
      data = data)
```

A brms autocorrelation term inside the trend formula would specify
a second temporal model for the same latent state, and it is
refused:

```r
mvgam(y ~ s(x), trend_formula = ~ s(time) + ar(p = 1), data = data)
```

A multivariate model applies one trend constructor to every
response. The model stores the latent states in one `lv_trend`
matrix with one set of dynamics parameters. A trend type per response
would need parallel dynamics machinery, separate parameter blocks and
per-response extraction in every post-fit method, and mixing AR and
RW dynamics across responses is better expressed as separate models.
Every entry point asserts that `trend_formula` is one one-sided
formula, refusing `bf()` and named lists. `validate_trend_formula()`
refuses a second trend constructor. Responses differ in their
observation families and observation formulas, and a hierarchical
trend groups series through `gr`.

`get_prior()`, `stancode()` and `standata()` are mvgam-owned S3
generics. The `formula` and `brmsformula` methods delegate to brms,
and the `mvgam_formula` method adds the trend. A model without a
trend gives the same result as the brms function.

## 3. Trend registry and constructors

The registry (`R/trend_system.R`) holds the six built-in trends: AR,
RW, VAR, ZMVN, CAR and PW. Users cannot register their own. For trend
`FOO` the package defines `generate_foo_trend_stanvars()`, which
emits its Stan code, and `foo_trend_properties()`, which declares its
entry. `register_core_trends()` finds both by name on the first use
of the registry. Each entry records:

- `supports_factors`, and the `incompatibility_reason` a factor
  request is refused with;
- `covariance_pattern`: `"none"` for PW, `"cholesky_scaled"` for RW,
  AR, ZMVN and CAR, `"full_covariance"` for VAR;
- `stationary_source`: how a marginal prediction obtains the
  covariance it integrates over (section 10);
- `requires_regular_intervals`: `TRUE` for a trend that indexes its
  lags by position, `FALSE` for CAR, whose kernel takes the elapsed
  gap, and for ZMVN, whose likelihood is exchangeable in time;
- `generator`, the Stan generator.

Because no property has a default, every trend states each one.
`get_trend_info()` returns an entry, and `trend_property()` returns
one field, with `"none"` for a fit without a trend. Functions obtain
these facts from the registry, and no function compares trend names
to decide them.

Each constructor validates its own arguments and returns an
`mvgam_trend` object through `create_mvgam_trend()`, which fills the
shared defaults, checks the arguments every trend shares and takes
`validation_rules` from the registered `requires_regular_intervals`.
The `trend` field always holds the base type: `AR(p = c(1, 12))` has
`trend = "AR"` and `PW(growth = "logistic")` has `trend = "PW"`.

`AR(p = k)` means the consecutive lags `1:k` and declares
`ar1_trend` to `ark_trend`. `AR(p = c(1, 12))` selects a sparse lag
set and declares only `ar1_trend` and `ar12_trend`.
`resolve_active_lags()` (`R/trend_propagation.R`) makes this
resolution once for the Stan generators and the post-fit methods. A
consecutive lag set with `p >= 2` samples partial autocorrelations on
`(-1, 1)`, and the Levinson-Durbin recursion derives the coefficients
from them, which keeps every draw stationary. `VAR(p = k)` takes a
scalar order. Its stationary parameterisation (Heaps 2023) is defined
on the companion form of consecutive lags. Because a sparse lag set
has no companion form with the same identified stationary covariance,
`VAR()` refuses a vector `p`.

Every trend parameter name ends in `_trend`, which keeps it distinct
from an observation parameter of the same name. The innovation scale
is `sigma_trend`, the correlation factor `L_Omega_trend`, a sampled
covariance `Sigma_trend`, the autoregressive coefficients `ar1_trend`
to `ark_trend`, the moving-average coefficient `theta1_trend` and the
VAR coefficients `Phi_trend[lag]`. The loadings matrix is `Z`.

`generate_monitor_params()` lists the parameters a trend samples. A
`trend_map` and a loadings prior both join the specification after
the constructor runs, and each changes the list. A list stored on the
trend object would miss those changes, and the function computes the
list from the prepared specification on every call. A fully fixed
`trend_map` samples no `Z`, and a partial one samples its `NA`
entries (`samples_any_loading()`). With every loading sampled, the
factor scale is fixed at 1 and `sigma_trend` and `L_Omega_trend`
leave the list (`samples_factor_loadings()`). Multiplicative gamma
process shrinkage derives `sigma_trend` (`samples_innovation_scale()`).
Each trend adds its own parameters through
`generate_<type>_monitor_params()`. The prior table and the set of
classes mvgam withholds from brms (`get_all_mvgam_trend_parameters()`)
both call it.

## 4. Stan assembly

Assembly has two stages. `generate_trend_specific_stanvars()`
(`R/stan_assembly.R`) builds the trend stanvars: it takes the
generator from the trend's registry entry and adds the dimension
stanvars from `generate_common_trend_data()`, the shared innovations
from `generate_shared_innovation_stanvars()` and the check
`validate_no_factor_hierarchical()`. `generate_common_trend_data()` is
the only function that creates dimension stanvars, which keeps any
dimension from being declared twice. The observation program is then
generated with those stanvars (`generate_base_stancode_with_stanvars()`)
and `inject_trend_into_linear_predictors()` adds the trend to `mu`.
`stan-data-flow-pipeline.md` gives the order of every step.

The trend program comes from brms with a `gaussian()` placeholder
family. `extract_non_likelihood_from_model_block()` drops its
likelihood, and what remains supplies `mu_trend`.
`extract_and_rename_stan_blocks()` renames its parameters and data
with the `_trend` suffix, skipping the Stan reserved words
`get_stan_reserved_words()` lists, and
`extract_mu_construction_with_classification()`
(`R/mu_expression_analysis.R`) finds the statements that build its
`mu`. Fixed effects, smooths, Gaussian process predictions and
random effects all reach `mu_trend` this way. The latent innovations
are Gaussian, or multivariate t when a constructor is given finite
or estimated `df`.

Every trend generator returns a brms `stanvars` object. Generators
create each stanvar as its own object and combine them with
`combine_stanvars()`. `sort_stanvars()` orders dimensions before the
arrays sized by them. Stanvars name their block with the brms
abbreviations: `"data"`, `"tdata"`, `"parameters"`, `"tparameters"`,
`"model"` and `"genquant"`. Validation functions accept the
abbreviated and the full names.

The time axis and the trend design axis are separate dimensions.
`N_time_trend` counts the unique time points and sizes the latent
dynamics, the innovations, the `trend` matrix and `times_trend`.
`N_trend = nrow(trend_data)`, which is `N_time_trend * N_series_trend`
in the standard layout, sizes `mu_trend` and `X_trend`. The trend
formula's effects can then vary by time and series. The recurrence
steps from one time point to the previous one, and only a time axis
gives that step a meaning. The indexing contract in
`create_times_trend_matrix()` is
`times_trend[i, s] = (i - 1) * N_series_trend + s`, the row of
`trend_data` for time `i` and series `s`. It relies on
`trend_cell_frame()` (`R/validations.R`) arranging `trend_data` by
time and then series.

```stan
transformed parameters {
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += X_trend * b_trend;
  matrix[N_time_trend, N_lv_trend] lv_trend;   // the latent dynamics
  matrix[N_time_trend, N_series_trend] trend;
  for (i in 1:N_time_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :])
                    + mu_trend[times_trend[i, s]];
    }
  }
}
```

The observation program then adds the trend to each observation it
scores, through two index arrays that map an observation onto the
trend matrix:

```stan
for (n in 1:N) {
  mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
}
```

A multivariate model follows the brms naming: each response has its
own `mu_<resp>` and its own mapping arrays, `obs_trend_time_<resp>`
and `obs_trend_series_<resp>`. The dynamics parameters are shared
across responses.

mvgam keeps the brms optimisations of the observation model. A GLM
likelihood is rewritten to take `to_matrix(mu)`, which keeps the GLM
call. Threading has two paths. The closure-unit families (`nmix()`
variants and `occ()`) and the multi-response families (`diri()`,
`mvn()`, `mvt()`, `multi()` and `categ()`) emit their own
`partial_sum_<family>_lpmf` and `reduce_sum` call. brms places the
`mu` declaration and every linear predictor assignment of a native
family inside `partial_log_lik_lpmf` when it threads, where the trend
injector cannot reach them. For a native family with a trend, and for
the families that emit their own `reduce_sum`,
`suppress_brms_threading()` (`R/make_stan.R`) hands brms
`threads = 1`. The user's `threads` still reaches
`cpp_options$stan_threads` through `mvgam_single()`, and the mvgam
`reduce_sum` parallelises where the family has one. A native family
with a trend has none, and it warns on every call that the threads
request has no effect. Lifting that case needs the injector to splice
the trend into the `partial_log_lik_lpmf` body and to pass the
mapping arrays through its signature.

## 5. Data axes and validation

mvgam never modifies the user's data. `ensure_mvgam_variables()`
(`R/validations.R`) records each row's time and series as attributes,
`get_time_for_grouping()` and `get_series_for_grouping()` return
them and `remove_mvgam_variables()` clears them. `mvgam_time` holds a
sequential index over the unique times, and `mvgam_original_time`
keeps the user's values for the CAR distances.

A trend model's data names each row's series in one of three ways,
even for a single series. `assert_axis_column()` refuses an absent
column with its one-line fix and requires the series column to be a
factor, whose levels fix the order of the series.

- Explicit: the series column itself.
- Hierarchical: `hierarchical_series_values()`, which is
  `interaction(gr, subgr, sep = "_", lex.order = TRUE)`, keeping each
  group's subgroups adjacent on the axis.
- Multivariate: a wide frame holds one row per time and one column
  per response. The series of an observation belongs to the pair of
  row and response, which no per-row vector can express. Cutting the
  rows into a block per response would make the frame look stacked
  and give the first stretch of the timeline to one response. The
  axis is recorded as the level set, `attr(data,
  "mvgam_series_levels") <- response_vars`, with the per-row values
  held at one level. Series `k` is the `k`th response and the `k`th
  row of the loadings.

A model without a trend needs no series column. `mvgam_data()`
resolves its axes through the same `ensure_mvgam_variables()`.

`extract_and_validate_trend_components()` is the entry point for a
trend model's data. It prepares the attributes, calls
`extract_time_series_dimensions()`, which resolves the series, time
and factor axes once and builds the observation-to-trend mapping
arrays for every response in the same pass, and runs the checks:
`refuse_ragged_trend_grid()` requires every series on one time grid,
`validate_regular_time_intervals()` enforces the regular-interval
rule of a trend that declares it, and `validate_gr_balanced_groups()`
requires the same number of series in every group. It returns the
trend data from `trend_cell_frame()`, the specification with its
dimensions attached and the fit's `trend_metadata`.

The axes are recorded once. `axis_record()` stores them on
`trend_metadata$axes` with the trend formula's covariates and the
`by = lv_axis()` grain, and `enrich_trend_metadata()`
(`R/trend_propagation.R`) adds the trend's own fields: `trend_type`,
`ar_lags`, `ma_lags`, `max_lag`, `has_cor`, `n_lv`, `df`, `fixed_Z`
and, for PW, the growth form and changepoint range. Every post-fit
method takes the axes from `mvgam_axes()`. A printed label, a
forecast horizon and a Stan index then all come from the same record.
A prediction frame goes through `ensure_mvgam_variables()` with the
fit's metadata (`prepare_mvgam_frame()` in `R/sample_innovations.R`),
which prepares it the way the training frame was prepared.

A multivariate model holds one copy of the trend specification per
response, keyed by the response name brms gives it. Every copy is
the same specification. Code calls three helpers in `R/axes.R` in
place of testing the shape: `trend_spec_head()` returns the one
specification whatever shape arrives, `map_trend_specs()` applies a
change to every copy and keeps the shape, and `is_trend_spec_list()`
tells a per-response list from a single specification.

## 6. Factor models

A factor model is a capability of a trend type. AR, RW, VAR and ZMVN
take `n_lv`, and CAR and PW refuse it with their registered reason.
`validate_n_lv_ceiling()` (`R/validations.R`), shared by `mvgam()`
and `jsdgam()`, requires `n_lv < n_series` under the default iid
prior on `Z`. Because the multiplicative gamma process prior of
Bhattacharya & Dunson (2011) treats `n_lv` as a truncation ceiling
and shrinks redundant columns toward zero, `loadings_prior = "mgp"`
allows `n_lv <= n_series`.

`generate_matrix_z_multiblock_stanvars()` emits one of five forms of
`Z`:

- Sampled, the default factor model: `Z` is a parameter with the
  prior `to_vector(Z) ~ student_t(3, 0, 0.5)`, identified after
  sampling by the QR step below.
- Sampled under a structured prior: a `loadings_prior` replaces the
  iid prior with the per-column matrix-normal
  `Z[, i] ~ multi_normal_cholesky(0, L_Phi * sqrt(Psi_diag[i]))`.
- Partially fixed, from a `trend_map` with `NA` entries: the fixed
  entries come from a data-block template, and the free entries are
  sampled as a vector and assembled in transformed parameters.
- Fully fixed, from a `trend_map` without `NA` entries: `Z` is data,
  with no parameters and no prior.
- Not a factor model: an identity `Z` in transformed data.

With every loading sampled, the factor innovations are fixed at unit
scale and zero correlation. The likelihood sees the factors only
through `Z %*% lv`, where any scale or rotation of the factors passes
into the loadings, and identification needs the scale fixed.
`samples_factor_loadings()` decides this once for the prior table and
both Stan generators. The program keeps `sigma_trend` and
`L_Omega_trend` as transformed parameters equal to 1 and the
identity, forecasting and the covariance extractors use them
unchanged, and a prior on either is refused. Fixed loadings pin the
factors' scale, and those fits keep a sampled scale and correlation.
Multiplicative gamma process shrinkage derives the scale as
`sqrt(Psi_diag)`.

A `trend_map` takes three input forms: a numeric matrix, a
`data.frame(series, trend)` and the character codes `"identity"` and
`"shared"`. All three normalise to a numeric `n_series x n_lv`
matrix stored on `trend_metadata$fixed_Z`. A constructor takes it,
and `mvgam(trend_map = ...)` passes it to the constructor. An `NA`
entry marks a loading to sample, and a finite entry stays on `Z`
exactly.

`factor_identification()` (`R/factor_alignment.R`) chooses how
sampled loadings are identified, for the Stan generators and the
post-fit step alike. The choice follows from the transforms of the
factors that leave the model unchanged.

Factors without their own coefficients (RW, VAR, ZMVN and AR with
`coef_sharing = "shared"`) are unchanged by any rotation. Following
Heaps & Jermyn (2024), generated quantities compute
`Z_tilde = qr_thin_R(Z')'` and rotate the factor paths to
`lv_trend_tilde` with the matching orthogonal matrix, which the
program keeps local. A VAR factor model also rotates its
coefficients to `Phi_trend_tilde`. `qr_thin_R()` returns a
non-negative diagonal, which fixes each factor's sign. The
factorisation is exact in every draw and the likelihood is unchanged.

AR factors each take their own coefficients, and are unchanged only
by reordering and sign flips. A rotation would mix factors with
different dynamics and leave `ar1_trend` without a consistent label.
`relabel_factors()` runs once where the fit is assembled. It matches
each draw's loading columns to a reference by the largest summed
absolute inner product, iterates the reference to the mean of the
aligned draws, then orders the factors by the variance they
contribute and signs each so its largest loading is positive. The
stored draws of every parameter in `factor_indexed_pars` are
rewritten together, and every post-fit method uses the one labelling.
The Stan program emits no rotation for these models.

A partial `trend_map` limits the relabelling to columns that share a
template, and a column with a non-zero fixed entry keeps its sign.
Under the multiplicative gamma process the shrinkage scales
`Psi_diag` move with their factors and `varrho_inv` is recomputed
from them. A partial map with a sampled
correlation matrix, and a `by = lv_axis()` model, keep the labelling
they were sampled in. `resolve_factor_loadings()`
(`R/plot_helpers.R`) returns the per-draw loadings for every
consumer: `Z_tilde` where it exists, `Z` otherwise and the fixed
matrix for a fully fixed map.

`mvgam(loadings_prior = ...)` replaces the iid prior on `Z` with the
structured matrix-normal of Heaps & Jermyn (2024). A per-series
feature matrix contributes an ARD exponential kernel through the Stan
function `gp_exponential_cov()`, and each pairwise distance matrix
contributes an `exp(-d / theta)` factor. The factors multiply into
the among-row scale matrix `Phi`. The prior matters through
`E(Delta) = tr(Psi^2) * Phi` (Heaps & Jermyn, Sect. 3), where
`Delta = Z * Z'` is the shared variation among series. `Phi` is then
proportional to the prior expectation of an observable quantity, the
cross-series covariance the factors induce, and domain knowledge
encoded in `Phi` shapes the prior on that observable. Length-scales
take a `lognormal(0, 1)` prior on distances rescaled to `max(d) = 1`,
following the simulation (Sect. 6.1.2) and gas-demand (Sect. 6.3.1)
applications of Heaps & Jermyn. `column_shrinkage = "mgp"` adds the
multiplicative gamma process on `Psi_diag`. A fixed or partially
fixed `Z` leaves no free loadings for a structured prior, and
`assert_loadings_prior_compatible()` refuses a loadings prior with a
`trend_map`.

The closure-unit families group by `(series, time)` by default.
`occ(multi_season = TRUE)` and `nmix(multi_season = TRUE)` group by
`(series, site, time)` through `attr(family, "mvgam_unit_grouping")`,
which `prepare_closure_unit_family()` passes to the validator and the
array builder. The factor model is unchanged: `Z` is
`[N_species, N_lv_trend]`, `lv_trend` is `[N_time_trend, N_lv_trend]`
and the trend runs on the season axis. Site effects enter through the
observation formula.

## 7. Hierarchical correlations

RW, AR, VAR and ZMVN take `gr` and `subgr`. A named `gr` sets up a
global correlation matrix and a deviation matrix per group, and
`alpha_cor_trend` weights the two in each group's correlation. The
model applies with or without a factor structure. Two dimensions size
it: `N_lv_trend` counts the series across all groups, and
`N_subgroups_trend` counts the series within each group. Group
matrices take `N_subgroups_trend` and system matrices take
`N_lv_trend`.

Groups must be balanced, because `N_subgroups_trend` is one scalar
and every group's Cholesky and scale blocks share its size.
`validate_gr_balanced_groups()` refuses an unbalanced design. Ragged
arrays, available from Stan 2.31, with a per-group size array would
lift the restriction and admit observational designs with any number
of series per group.

## 8. Priors

mvgam uses the brms `brmsprior` class throughout. A trend class name
ends in `_trend`, and that suffix assigns a prior row to the trend.
`brms::prior()`, `brms::prior_string()` and `brms::set_prior()` all
create trend priors.

`get_prior.mvgam_formula()` builds the trend rows from the
specification Stan generation uses: `prepare_trend_specs()` and
`extract_and_validate_trend_components()` prepare it,
`generate_trend_priors()` lists `generate_monitor_params()` of it and
`combine_obs_trend_priors()` joins the rows to the observation
priors. A prior the table offers is then one the program declares.

A default resolves through `get_default_trend_parameter_prior()`
(`R/priors.R`): an optional `get_<trend>_parameter_prior()` in the
mvgam namespace, then the shared table `common_trend_priors`, then
defaults by name pattern. The Stan generators take a prior through
`get_trend_parameter_prior()`, which returns a user's prior for the
class where one was given and this default otherwise.

## 9. Observation families

brms's multi-category families and `mixture()` are refused.
`categorical`, `multinomial`, `dirichlet` and `logistic_normal` each
need one linear predictor per category, while a trend adds to one,
and the refusal names mvgam's long-format equivalent: `categ()`,
`multi()`, `diri()` or `mvn()`. No post-fit method has a kernel for a
mixture of families. `validate_supported_family()` makes the check,
called for every response by `resolve_observation_family()`
(`R/families.R`), which `mvgam()`, `stancode()`, `standata()` and
`mvgam_data()` all reach.

The ordinal families `cumulative`, `sratio`, `cratio` and `acat` use
one linear predictor with thresholds, and their expected values are
category probabilities: a matrix of draws by observations becomes an
array of draws by observations by categories.

The post-fit density, distribution and quantile functions of a
family come from `family_dist_spec()` (`R/log_lik_addition_terms.R`).
`log_lik()`, censoring and truncation, truncated posterior
prediction and the randomised quantile residual all use that one
parameterisation. `dispatch_log_lik()` uses a family's own
`log_lik_<family>()` kernel where one exists and the spec's density
otherwise.

## 10. Prediction surfaces and forecasting

mvgam has two prediction surfaces with different semantics for the
latent state. The split is intentional, and the wrong choice misleads
without an error.

The marginal surface (`posterior_predict()`, `posterior_epred()`,
`posterior_linpred()`) never uses the fitted `trend[t, s]`. The trend
contributes its deterministic submodel plus a state drawn per
posterior draw from the distribution it settles into, sampled afresh
on every call under `process_error = TRUE`. The prediction is then
the same at every time point. Under the default `FALSE` the submodel
contributes alone. That suits a question about a covariate effect. It
misleads for a fit whose covariates explain little of the signal,
where `hindcast()` and `forecast()` apply.

The distribution a state settles into is the stationary one: an
AR(1) at `sigma^2 / (1 - ar^2)`. Sampling the innovation covariance
instead left the marginal mean 12% low against a simulation with
known truth. `AR()` takes `ar_stationary_factor()`
(`R/sample_innovations.R`): the moving-average weights of each series
give `Gamma[a, b] = Sigma[a, b] * sum_j psi_a,j psi_b,j`, exact for
contiguous and sparse lag sets with or without a moving-average term.
A draw close to a unit root takes an exact companion solve. `VAR()`
takes `Omega_trend`, the stationary joint variance its Stan model
computes. The registry's `stationary_source` records which of these
applies: `"lift"` for AR and CAR, `"omega"` for VAR and `"none"` for
the rest.

The Stan program starts every contiguous `AR()` at the same law: the
scalar closed forms at one lag, `ar_stationary_init()` for
independent series above one lag and `joint_init_stanblock()` for
correlated or grouped innovations at any order and for a
moving-average term above one lag. A random walk has no stationary
distribution and `ZMVN()` has no dynamics to settle into. Both keep
their innovation covariance.

`CAR()` is the exact discretisation of a continuous-time AR(1). Across
a gap `d` the state decays by `ar^d` and the innovations have
covariance `Gamma[a, b] (1 - (ar_a ar_b)^d)`, where
`Gamma[a, b] = Sigma_trend[a, b] / (1 - ar_a ar_b)` is the covariance
the states hold at every occasion and the first state is drawn from.
Adding an occasion between two others leaves the law of the remaining
states unchanged. Gaps are measured in units of the median gap between
two consecutive observations of one series, which the axis record
holds as `time$observation_gap`. The Stan data and `forecast()` both
take it through `car_time_scale()`. The model is then the same in any
time unit, and the unit stays put when another series adds times to
the grid. Under `cor = TRUE`, `L_Omega_trend` is the correlation of
the shocks over an instant and `Sigma_trend`, the innovation
covariance over a gap of one, follows from it through
`car_unit_coherence()`. A correlation placed on the unit-gap
innovations directly gives a covariance that is not positive definite
at gaps below one when the series damp at different rates. Every
post-fit path takes the innovation correlation from the stored
`Sigma_trend` (`stores_innovation_cov()`). A CAR forecast steps every
series from the last occasion of the fitted grid, where each has a
state, including a series whose last responses are missing.

The registry's `completes_time_grid` is `TRUE` for CAR alone. Such a
trend takes series observed at their own times:
`trend_cell_frame()` adds a cell for each time a series has no row
at, and the frame is the model its `NA`-padded form gives, with
identical Stan data. Every other trend is refused by
`refuse_ragged_trend_grid()`. `forecast()` steps all series over the
union of the forecast times and reports each at its own.
`score()` sums series at each forecast time, and a joint score needs
shared times. `lfo_cv()` scores the series observed in each fold and
records the count as `n_obs`. `df` is refused on such a frame
(`refuse_heavy_tails_on_ragged_grid()`). A sparse lag set bounds its
coefficients one at a time, and a draw can be explosive. It keeps the
raw start in Stan, keeps its innovation covariance on the R side, and
`warn_explosive_draws()` counts such draws.

The conditional surface (`forecast()`, `hindcast()`, `residuals()`,
`pp_check()`) uses the fitted latent state from the posterior.
`hindcast()` returns it at the training grid and `forecast()`
extrapolates it. A time outside the grid has no fitted state and
takes the per-series marginal. The result is exact and reproducible
across calls.

Counterfactuals and covariate-level reasoning go through the marginal
surface. Forecasting, hindcasting and model comparison by ELPD or
scoring rules go through the conditional one, and `@seealso` blocks
cross-link the two. The likelihood is conditional, and so is every
method that scores it: `loo()`, `waic()`, `lfo_cv()`, `kfold()`,
`loo_R2()` and `bayes_R2()`. Scoring the marginal would leave the
importance weights describing a series the model never saw, and the
effective parameter count would exceed the number of observations.

Three invariants hold this together. A second innovation draw
downstream would double the process variance, and
`get_combined_linpred()` is the one place innovations are sampled.
Wherever importance weights meet draws, both halves come from the
same surface, including the `loo_*` prediction methods and
`pp_check()`'s `loo_pit*` types. And one implementation computes the
conditional state, `get_combined_linpred(trend_state =
"conditional")`, reached through the `posterior_*` methods under
`incl_autocor = TRUE`. A second copy of it once drifted from the
first.

Diagnostics name their surface through `diagnostic_surface_args()`:
the conditional state in sample, the marginal out of sample and the
conditional state wherever importance weights are involved.
`residuals()`, `pp_check()` and `predictive_error()` all route through
it, and a fit reports one account of its in-sample uncertainty.

Two arguments choose the behaviour, each with one spelling.
`incl_autocor` picks the surface on every method that offers a
choice, defaulting to `FALSE` on the prediction methods and `TRUE` on
`log_lik()`, `loo()` and `waic()`. `process_error` decides whether
the marginal surface samples innovations and defaults to `FALSE`
everywhere, which keeps `predict()` and the method it forwards to in
agreement. `trend_state` is the internal name `get_combined_linpred()`
takes, and `autocor_to_trend_state()` is the one translation. The
ELPD methods still accept the 1.x spelling `incl_dynamics`, which
yields to `incl_autocor` when both are given.

Leaving out `y[t]` does not remove its influence on the state
inferred at `t`, and PSIS-LOO is optimistic for a state-space fit on
either surface. Model comparison should use `lfo_cv()`.

`forecast.mvgam()` (`R/forecast.mvgam.R`) propagates the fitted
latent state one posterior draw at a time. `extract_last_state()`
returns the draw's trend parameters and the state at the end of the
training data, and `trend_linpred_grid()` evaluates the trend
formula's predictor over the training tail and the horizon.
`propagate_trend()` (`R/trend_propagation.R`) steps the zero-mean
state forward, with a branch per trend type: `propagate_arma()` for
RW, AR and VAR, then `propagate_car()`, `propagate_zmvn()` and
`propagate_pw()`. The observation predictor for the same draw is
added, the inverse link applied and, for `type = "response"`,
observation noise drawn, in one pass over all draws after the loop.
`coef_uncertainty`, `trend_uncertainty` and `obs_uncertainty` each
fix one source of variation, as the header of `R/forecast.mvgam.R`
describes.

## 11. User-facing conditions

Each kind of condition has one spelling, and the spelling decides
when it reaches the user.

| Kind | Spelling | Reaches the user |
|---|---|---|
| Error | `stop(insight::format_error(...))` | always |
| Warning, every call | `insight::format_warning(...)` | always, tests included |
| Warning, once per session | `warn_once(message, id)` | first time per session; quiet under testthat |
| Message, once per session | `inform_once(message, id)` | first time per session; quiet under testthat and at `silent = 2` |
| Message, every call | `rlang::inform(...)` inside `if (silent < 2)` | when `silent < 2` |

`R/utils-conditions.R` defines `warn_once()` and `inform_once()`. The
`id` names the counter and becomes the condition class. Package code
outside that file never calls `Sys.getenv("TESTTHAT")` or passes
`.frequency = "once"`, and `tests/local/debt_scan.R idioms` checks
both.

`silent` turns off messages alone, as in brms: `?brms::brm` says
`silent = 2` suppresses informational messages, and no brms warning
checks `silent`. A warning reports a problem and reaches the user at
every verbosity. `mvgam()` and `jsdgam()` store the call's `silent`
in the `mvgam.silent` option until the call returns, and
`inform_once()` takes it from there.

A once-per-session warning spent in one test would be missing from
the later test that asserts it, and the suite allows no warnings.
Under testthat the session warnings are silent, and a test asserting
one clears the variable with `withr::local_envvar(TESTTHAT = "")`. A
warning raised on every call reaches every test, and each test that
triggers one asserts it.

brms's "Rows containing NAs were excluded from the model" and
`pp_check()`'s "Observations with a missing response are omitted from
the plot" stay warnings on every call. A user who did not mean to
leave a response missing learns it from them, and those rows would
otherwise leave the likelihood unannounced. `refit_on_held_out()`
muffles brms's notice for the fold it masked itself.

The main line of a message states the problem. Details go in `x =`
bullets and the fix in `i =` bullets, each bullet one plain sentence.
`tests/local/debt_scan.R messages` extracts every message for the
prose linter.

A state only a fault in mvgam can reach is refused through
`stop_mvgam_fault()`, which points the user at the issue tracker.

## 12. S3 result objects

The default `summary.default` list dump is no use for a result a user
inspects, and every user-facing result class has a `summary()` method
returning a structured object.

A fit object uses two layers. `summary.mvgam()` returns an
`mvgam_summary` with its own print method, and `print.mvgam()` gives
a one-screen overview. The two-layer classes are `mvgam` and
`mvgam_pooled` (`R/summary.mvgam.R`).

A lighter result uses one layer: `summary()` returns a tibble with
one row per logical observation (per time, series or fold), which the
tibble print method renders. The one-layer classes are
`mvgam_forecast`, `mvgam_irf`, `mvgam_fevd` and `mvgam_lfo`.
Quantile columns follow the `predQ<p>` convention, and
`probs = c(0.025, 0.975)` is the standard band argument. A
`print.<class>` method is added only for a headline view that differs
from the summary tibble, such as totals across its rows. It writes
plain text with `cat()` and calls `print()` only on nested objects
with their own print methods.
