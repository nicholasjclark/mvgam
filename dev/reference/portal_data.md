# Portal Project rodent capture survey data

A dataset containing time series of total captures (across all control
plots) for select rodent species from the Portal Project

## Usage

``` r
portal_data
```

## Format

A `data.frame` containing the following fields:

- time:

  time of sampling, in lunar monthly cycles

- series:

  factor indicator of the time series, i.e. the species

- captures:

  total captures across all control plots at each time point

- ndvi_ma12:

  12-month moving average of the mean Normalised Difference Vegetation
  Index

- mintemp:

  monthly mean of minimum temperature

## Source

<https://github.com/weecology/PortalData/blob/main/SiteandMethods/Methods.md>
