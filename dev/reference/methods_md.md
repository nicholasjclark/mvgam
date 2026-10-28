# The equations of a model, as Markdown

Writes the model in mathematical notation for a methods section. The
Data section names the response, the dimensions and the predictors. The
Model section holds the likelihood, the linear predictors and the latent
dynamics in one display equation, with a glossary of every symbol. The
Priors section states every prior the Stan program applies. The
Implementation section gives the call that fits the model and the
software versions the fit recorded. The equations are LaTeX inside `$$`
fences, which Quarto, R Markdown and MathJax render.

## Usage

``` r
methods_md(object, file = NULL, implementation = TRUE, ...)

# S3 method for class 'mvgam'
methods_md(object, file = NULL, implementation = TRUE, heading_level = 2L, ...)
```

## Arguments

- object:

  A fitted `mvgam` or `jsdgam` object, or a prefit from
  `mvgam(..., run_model = FALSE)`.

- file:

  Optional path. The Markdown is written there and returned invisibly.

- implementation:

  Logical. `TRUE` (the default) adds the Implementation section.

- ...:

  Unused. Anything passed here is refused.

- heading_level:

  Integer Markdown level of the section headings. The default `2` writes
  `## Model`. A higher level nests the sections under a heading of the
  host document.

## Value

A character string of class `mvgam_methods_md`, which prints as its
Markdown.

## See also

[`how_to_cite()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md)
writes the matching methods paragraph and its references.
[`prior_summary()`](https://mc-stan.org/rstantools/reference/prior_summary.html)
lists the priors as a table.

Other posterior summaries:
[`VarCorr.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/VarCorr.mvgam.md),
[`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md),
[`how_to_cite.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md),
[`hypothesis.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hypothesis.mvgam.md),
[`index-mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md),
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md),
[`mvgam_draws`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md),
[`pairs.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pairs.mvgam.md),
[`posterior_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_interval.mvgam.md),
[`ranef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/ranef.mvgam.md),
[`smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/smooths.mvgam.md),
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                     n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
              data    = simdat$data_train,
              family  = poisson(),
              chains  = 2, silent = 2)
#> Warning: 5 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# The equations, glossary and priors as Markdown
md <- methods_md(mod)
md
#> ## Data
#> 
#> $y$: non-negative integer counts
#> 
#> $N = 90$ observations, $S = 1$ series, $T = 90$ time points
#> 
#> Predictors:
#> 
#> - $x$: continuous covariate
#> 
#> ## Model
#> 
#> $$
#> \begin{aligned}
#> t &\in \{1, \ldots, 90\} \\
#> y_{i,t} &\sim \text{Poisson}(\mu_{i,t}) \\
#> \log \mu_{i,t} &= \alpha + f_{x}(x_{i,t}) + \eta_{i,t} \\
#> f_{x}(x) &= \sum_{m=1}^{K_{x}} \beta^{(x)}_m B_m(x) \\
#> \eta_{i,t} &= \phi_{1} \eta_{i,t-1} + \epsilon^{(\eta)}_{i,t} \\
#> \epsilon^{(\eta)}_{i,t} &\sim \text{Normal}(0, \sigma_\eta)
#> \end{aligned}
#> $$
#> 
#> where:
#> 
#> - $i$ indexes series, $t$ indexes time
#> - $\mu_{i,t}$: conditional mean of $y_{i,t}$
#> - $\alpha$: population intercept
#> - $f_{x}$: thin plate regression spline in $x$, basis size $K_{x} = 5$, smoothing SD $\sigma^{(s)}_{x}$
#> - $\eta_{i,t}$: latent state at series $i$ time $t$
#> - $\epsilon^{(\eta)}_{i,t}$: process innovation, SD $\sigma_\eta$
#> - $\phi_{l}$: autoregressive coefficient at lag $l$
#> 
#> ## Priors
#> 
#> $$
#> \begin{aligned}
#> \alpha &\sim \text{StudentT}(3, 1.6, 2.5) \\
#> \sigma^{(s)}_{x} &\sim \text{StudentT}(3, 0, 2.5) \\
#> \sigma_\eta &\sim \text{Exponential}(2) \\
#> \phi_{1} &\sim \text{Normal}(0, 0.5)
#> \end{aligned}
#> $$
#> 
#> Every other parameter takes an improper flat prior on its support.
#> 
#> ## Implementation
#> 
#> Fitted with mvgam 2.0.0 (brms 2.23.2; Stan 2.40.0 via cmdstanr). Hamiltonian Monte Carlo: 2 chains, 1000 warmup + 1000 sampling iterations per chain.
#> 
#> ```
#> mvgam(
#>   formula       = y ~ s(x, k = 5),
#>   trend_formula = ~AR(p = 1),
#>   data          = simdat$data_train,
#>   family        = poisson(),
#>   backend       = "cmdstanr",
#>   chains        = 2,
#>   iter          = 2000,
#>   warmup        = 1000
#> )
#> ```

# Written to a file, nested under a level-two heading
methods_md(mod, file = tempfile(fileext = ".md"), heading_level = 3L)

# Next, forecast(mod) predicts the times after the training data
# }
```
