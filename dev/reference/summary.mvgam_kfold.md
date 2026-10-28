# Tibble summary of an `mvgam_kfold` object

Tibble summary of an `mvgam_kfold` object

## Usage

``` r
# S3 method for class 'mvgam_kfold'
summary(object, ...)
```

## Arguments

- object:

  An `mvgam_kfold` object.

- ...:

  Unused. Anything passed here is refused.

## Value

A tibble with one row per held-out group.

- `group`:

  The group label.

- `elpd`, `elpd_share`:

  The group's ELPD and its share of the total.

- `elpd_psis`, `lift`:

  For a refitted group, the PSIS estimate before the refit and the
  refit's change from it.

- `pareto_k`, `refit`:

  The Pareto k of the PSIS estimate and whether the group's fold was
  refitted.
