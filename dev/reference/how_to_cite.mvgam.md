# A methods paragraph and references for an mvgam model

Drafts a methods paragraph for a fitted model, with the references it
cites and their BibTeX entries. The text and references follow the
model's structure: the family and link, the trend, the factor
identification and loadings prior, Gaussian process smooths, piecewise
trends, the Stan backend and the sampling algorithm.

## Usage

``` r
how_to_cite(object, ...)

# S3 method for class 'mvgam'
how_to_cite(object, ...)
```

## Arguments

- object:

  A fitted `mvgam` or `jsdgam` object.

- ...:

  Unused. Anything passed here is refused.

## Value

A `how_to_cite` object holding the `methods_text`, the `citations` it
makes, an `other_citations` list of related references and a `bibtex`
list keyed by reference.
[`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md)
extracts the BibTeX entries.
[`print()`](https://rdrr.io/r/base/print.html) shows every section.

## See also

[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md)
writes the model's equations to accompany the paragraph.
[`utils::citation()`](https://rdrr.io/r/utils/citation.html) cites the
package itself.

Other posterior summaries:
[`VarCorr.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/VarCorr.mvgam.md),
[`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md),
[`hypothesis.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hypothesis.mvgam.md),
[`index-mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md),
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md),
[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md),
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
mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
              data    = simdat$data_train,
              family  = poisson(),
              chains  = 2, silent = 2)
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# A methods paragraph naming the Poisson family, the smooth and
# the AR(1) trend, with the references it cites
cite <- how_to_cite(mod)
cite
#> Methods text skeleton
#> We used the R package mvgam (version 2.0.0; Clark & Wells, 2023) to
#>   construct, fit and interrogate the model. mvgam fits Bayesian
#>   state-space models that combine flexible predictor effects in both the
#>   process and observation components, using functionality from the brms
#>   (Burkner 2017) and mgcv (Wood 2017) packages. Observations followed the
#>   poisson family with a log link. The latent process followed AR(1)
#>   dynamics. The mvgam-constructed model and data were passed to Stan
#>   (Carpenter et al. 2017) via the cmdstanr interface (Gabry et al. 2024).
#>   We ran 2 Hamiltonian Monte Carlo chains for 1000 warmup iterations and
#>   1000 sampling iterations. Rank-normalised split Rhat and effective
#>   sample sizes (Vehtari et al. 2021) were used to monitor convergence.
#> 
#> Primary references
#> Burkner PC (2017). brms: An R Package for Bayesian Multilevel Models
#>   Using Stan. Journal of Statistical Software, 80(1), 1-28.
#>   https://doi.org/10.18637/jss.v080.i01
#> Carpenter B, Gelman A, Hoffman MD, Lee D, Goodrich B, Betancourt M,
#>   Brubaker M, Guo J, Li P and Riddell A (2017). Stan: A probabilistic
#>   programming language. Journal of Statistical Software, 76(1), 1-32.
#>   https://doi.org/10.18637/jss.v076.i01
#> Clark NJ and Wells K (2023). Dynamic Generalized Additive Models (DGAMs)
#>   for forecasting discrete ecological time series. Methods in Ecology and
#>   Evolution, 14(3), 771-784. https://doi.org/10.1111/2041-210X.13974
#> Gabry J, Cesnovar R, Johnson A and Bronder S (2024). cmdstanr: R
#>   Interface to 'CmdStan'. https://mc-stan.org/cmdstanr/
#> Vehtari A, Gelman A, Simpson D, Carpenter B and Burkner P (2021).
#>   Rank-normalization, folding, and localization: An improved Rhat for
#>   assessing convergence of MCMC. Bayesian Analysis, 16(2), 667-718.
#>   https://doi.org/10.1214/20-BA1221
#> Wood SN (2017). Generalized Additive Models: An Introduction with R (2nd
#>   edition). Chapman and Hall/CRC.
#> 
#> Other useful references
#> Arel-Bundock V, Greifer N and Heiss A (2024). How to interpret
#>   statistical models using marginaleffects for R and Python. Journal of
#>   Statistical Software, 111(9), 1-32.
#>   https://doi.org/10.18637/jss.v111.i09
#> Burkner PC, Gabry J and Vehtari A (2020). Approximate leave-future-out
#>   cross-validation for Bayesian time series models. Journal of Statistical
#>   Computation and Simulation, 90(14), 2499-2523.
#>   https://doi.org/10.1080/00949655.2020.1783262
#> Gabry J, Simpson D, Vehtari A, Betancourt M and Gelman A (2019).
#>   Visualization in Bayesian workflow. Journal of the Royal Statistical
#>   Society: Series A (Statistics in Society), 182(2), 389-402.
#>   https://doi.org/10.1111/rssa.12378
#> Vehtari A, Gelman A and Gabry J (2017). Practical Bayesian model
#>   evaluation using leave-one-out cross-validation and WAIC. Statistics and
#>   Computing, 27(5), 1413-1432. https://doi.org/10.1007/s11222-016-9696-4
#> 
#> Use `bibtex(x)` for a BibTeX block ready for your .bib file.

# The BibTeX entries, for a reference manager
bibtex(cite)
#> @article{clark2023dgam,
#>   title = {Dynamic Generalized Additive Models {(DGAMs)} for forecasting discrete ecological time series},
#>   author = {Clark, Nicholas J. and Wells, Konstans},
#>   journal = {Methods in Ecology and Evolution},
#>   volume = {14},
#>   number = {3},
#>   pages = {771--784},
#>   year = {2023},
#>   doi = {10.1111/2041-210X.13974}
#> }
#> 
#> @article{burkner2017brms,
#>   title = {{brms}: An {R} Package for {B}ayesian Multilevel Models Using {S}tan},
#>   author = {B{\"u}rkner, Paul-Christian},
#>   journal = {Journal of Statistical Software},
#>   volume = {80},
#>   number = {1},
#>   pages = {1--28},
#>   year = {2017},
#>   doi = {10.18637/jss.v080.i01}
#> }
#> 
#> @book{wood2017gam,
#>   title = {Generalized Additive Models: An Introduction with {R}},
#>   author = {Wood, Simon N.},
#>   publisher = {Chapman and Hall/CRC},
#>   edition = {2},
#>   year = {2017}
#> }
#> 
#> @article{carpenter2017stan,
#>   title = {{Stan}: A Probabilistic Programming Language},
#>   author = {Carpenter, Bob and Gelman, Andrew and Hoffman, Matthew D. and Lee, Daniel and Goodrich, Ben and Betancourt, Michael and Brubaker, Marcus and Guo, Jiqiang and Li, Peter and Riddell, Allen},
#>   journal = {Journal of Statistical Software},
#>   volume = {76},
#>   number = {1},
#>   pages = {1--32},
#>   year = {2017},
#>   doi = {10.18637/jss.v076.i01}
#> }
#> 
#> @misc{gabry2024cmdstanr,
#>   title = {{cmdstanr}: {R} Interface to {'CmdStan'}},
#>   author = {Gabry, Jonah and {\v C}e{\v s}novar, Rok and Johnson, Andrew and Bronder, Steve},
#>   year = {2024},
#>   url = {https://mc-stan.org/cmdstanr/}
#> }
#> 
#> @article{vehtari2021rhat,
#>   title = {Rank-Normalization, Folding, and Localization: {A}n Improved {R-hat} for Assessing Convergence of {MCMC}},
#>   author = {Vehtari, Aki and Gelman, Andrew and Simpson, Daniel and Carpenter, Bob and B{\"u}rkner, Paul-Christian},
#>   journal = {Bayesian Analysis},
#>   volume = {16},
#>   number = {2},
#>   pages = {667--718},
#>   year = {2021},
#>   doi = {10.1214/20-BA1221}
#> }
#> 
#> @article{arelbundock2024marginal,
#>   title = {How to interpret statistical models using {marginaleffects} for {R} and {Python}},
#>   author = {Arel-Bundock, Vincent and Greifer, Noah and Heiss, Andrew},
#>   journal = {Journal of Statistical Software},
#>   volume = {111},
#>   number = {9},
#>   pages = {1--32},
#>   year = {2024},
#>   doi = {10.18637/jss.v111.i09}
#> }
#> 
#> @article{gabry2019workflow,
#>   title = {Visualization in {B}ayesian workflow},
#>   author = {Gabry, Jonah and Simpson, Daniel and Vehtari, Aki and Betancourt, Michael and Gelman, Andrew},
#>   journal = {Journal of the Royal Statistical Society: Series A (Statistics in Society)},
#>   volume = {182},
#>   number = {2},
#>   pages = {389--402},
#>   year = {2019},
#>   doi = {10.1111/rssa.12378}
#> }
#> 
#> @article{vehtari2017loo,
#>   title = {Practical {B}ayesian model evaluation using leave-one-out cross-validation and {WAIC}},
#>   author = {Vehtari, Aki and Gelman, Andrew and Gabry, Jonah},
#>   journal = {Statistics and Computing},
#>   volume = {27},
#>   number = {5},
#>   pages = {1413--1432},
#>   year = {2017},
#>   doi = {10.1007/s11222-016-9696-4}
#> }
#> 
#> @article{burkner2020lfo,
#>   title = {Approximate leave-future-out cross-validation for {B}ayesian time series models},
#>   author = {B{\"u}rkner, Paul-Christian and Gabry, Jonah and Vehtari, Aki},
#>   journal = {Journal of Statistical Computation and Simulation},
#>   volume = {90},
#>   number = {14},
#>   pages = {2499--2523},
#>   year = {2020},
#>   doi = {10.1080/00949655.2020.1783262}
#> }

# Next, methods_md() writes the model's equations
# }
```
