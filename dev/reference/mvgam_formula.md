# Create an mvgam formula object

Constructs a lightweight mvgam formula object that combines observation
and trend model formulas. The inspection functions
[`get_prior()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md),
[`stancode()`](https://paulbuerkner.com/brms/reference/stancode.html)
and
[`standata()`](https://paulbuerkner.com/brms/reference/standata.html)
take it.

## Usage

``` r
mvgam_formula(formula, trend_formula = NULL)
```

## Arguments

- formula:

  An object of class `formula`, `brmsformula`, or `mvbrmsformula`
  describing the observation model.

- trend_formula:

  An optional formula describing trend dynamics. Default NULL results in
  pure brms equivalent model. See Details for trend syntax.

## Value

An object of class `c("mvgam_formula", base_class)` where base_class is
the class of the input formula, containing:

- `formula`: The observation model formula

- `trend_formula`: The trend model formula (or NULL)

## Details

The mvgam_formula object is a minimal container that pairs an
observation formula with an optional trend formula. Data, family, and
other model specifications are provided when calling inspection or
fitting functions:

`get_prior(mvgam_formula(y ~ x, ~ AR()), data = dat, family = poisson())`

When `trend_formula = NULL`, the model reduces to a pure brms
observation model. When specified, the trend_formula can include:

- Trend constructors:
  [`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
  [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md),
  [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
  [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md),
  [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)

- Covariates that affect trend dynamics

- Smooth terms using mgcv syntax: `s()`, `t2()`

- Random effects: `(1|group)`, `(slope|group)`

- Gaussian processes: `gp()`

## Trend formula restrictions

The trend_formula has specific restrictions to maintain compatibility
with State-Space dynamics and prevent conflicts between
observation-level and trend-level modeling:

**Forbidden Terms:**

- brms addition-terms:
  [`weights()`](https://rdrr.io/r/stats/weights.html), `se()`, `cens()`,
  [`trunc()`](https://rdrr.io/r/base/Round.html), `trials()`, `rate()`,
  `vreal()`, `vint()`, [`subset()`](https://rdrr.io/r/base/subset.html),
  `index()`, `dec()`, [`cat()`](https://rdrr.io/r/base/cat.html),
  `thres()`

- Missing-value imputation: `mi()`

- brms autocorrelation: [`ar()`](https://rdrr.io/r/stats/ar.html),
  `ma()`, `arma()`, `cosy()`, `unstr()`, `autocor()`

- Offset terms: [`offset()`](https://rdrr.io/r/stats/offset.html)

**Rationale:** Addition terms change the observation model, through
weights, censoring or exposure. They belong in the observation formula.
Autocorrelation terms conflict with the trend's state-space dynamics.
brms imputes a `mi()` predictor as a response of the model. A trend
formula holds no response.

## Multivariate models, single shared trend type

For multivariate observation models (`mvbind()` or `bf() + bf()`), **a
single trend constructor applies to all responses**. Different trend
types per response (e.g.
[`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
for one response and `AR(p = 3)` for another) are **not supported and
will not be added**. mvgam stores latent trend states in a single
`lv_trend` matrix with shared dynamics parameters; mixing trend types
per response would require parallel dynamics machinery throughout the
codegen, sampling, and prediction pipelines.

What multivariate models support:

- Different observation families per response (e.g.
  `bf(count ~ x, family = poisson()) + bf(biomass ~ x, family = Gamma())`).

- Different fixed-effect, smooth, or GP terms per response in the
  observation formulas.

- Hierarchical / grouped trends via the `gr` argument on the (single)
  trend constructor.

If you need different dynamics per response, fit independent univariate
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
models for each response.

**Examples:**


    # Correct usage
    mvgam_formula(y ~ x + weights(w), trend_formula = ~ AR(p = 1))
    mvgam_formula(count ~ offset(log_exposure), trend_formula = ~ RW())

    # Incorrect usage (will error)
    mvgam_formula(y ~ x, trend_formula = ~ AR() + weights(w))
    mvgam_formula(y ~ x, trend_formula = ~ RW() + offset(z))

## See also

[`get_prior.mvgam_formula`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md),
[`stancode`](https://paulbuerkner.com/brms/reference/stancode.html),
[`standata`](https://paulbuerkner.com/brms/reference/standata.html),
[`mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)

Other model fitting:
[`check_newdata()`](https://nicholasjclark.github.io/mvgam/dev/reference/check_newdata.md),
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md),
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md),
[`mvgam_multiple()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_multiple.md),
[`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md),
[`pivot_species_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_species_matrix.md),
[`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/series_to_mvgam.md),
[`stancode.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md),
[`update.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/update.mvgam.md)
