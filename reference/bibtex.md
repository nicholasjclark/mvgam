# The BibTeX entries of a `how_to_cite` object

Joins the BibTeX entries of
[`how_to_cite()`](https://nicholasjclark.github.io/mvgam/reference/how_to_cite.mvgam.md)
into one string. It is printed, or written to `file`.

## Usage

``` r
bibtex(x, ...)

# S3 method for class 'how_to_cite'
bibtex(x, file = NULL, other = TRUE, ...)
```

## Arguments

- x:

  A `how_to_cite` object.

- ...:

  Unused. Anything passed here is refused.

- file:

  Optional path of a `.bib` file to write the entries to in place of
  printing them.

- other:

  Logical. `TRUE` (the default) includes the related references. `FALSE`
  keeps the references the methods text cites.

## Value

The string of entries separated by blank lines, invisibly.

## See also

[`how_to_cite()`](https://nicholasjclark.github.io/mvgam/reference/how_to_cite.mvgam.md)
builds the object, with an example of both.
[`methods_md()`](https://nicholasjclark.github.io/mvgam/reference/methods_md.md)
writes the model's equations.

Other posterior summaries:
[`VarCorr.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/VarCorr.mvgam.md),
[`how_to_cite.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/how_to_cite.mvgam.md),
[`hypothesis.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/hypothesis.mvgam.md),
[`index-mvgam`](https://nicholasjclark.github.io/mvgam/reference/index-mvgam.md),
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/mcmc_plot.mvgam.md),
[`methods_md()`](https://nicholasjclark.github.io/mvgam/reference/methods_md.md),
[`mvgam_draws`](https://nicholasjclark.github.io/mvgam/reference/mvgam_draws.md),
[`pairs.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/pairs.mvgam.md),
[`posterior_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/posterior_interval.mvgam.md),
[`ranef.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/ranef.mvgam.md),
[`smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/smooths.mvgam.md),
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/summary.mvgam.md)
