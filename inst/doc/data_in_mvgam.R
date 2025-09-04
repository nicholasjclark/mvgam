params <-
list(EVAL = TRUE)

## ----echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)


## ----setup, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  dpi = 100,
  fig.asp = 0.8,
  fig.width = 6,
  out.width = "60%",
  fig.align = "center"
)
library(mvgam)
library(ggplot2)
theme_set(theme_bw(base_size = 12, base_family = "serif"))


## -----------------------------------------------------------------------
simdat <- sim_mvgam(
  n_series = 4, 
  T = 24, 
  prop_missing = 0.2
)
head(simdat$data_train, 16)


## -----------------------------------------------------------------------
class(simdat$data_train$series)
levels(simdat$data_train$series)


## -----------------------------------------------------------------------
all(levels(simdat$data_train$series) %in% 
      unique(simdat$data_train$series))


## -----------------------------------------------------------------------
summary(glm(
  y ~ series + time,
  data = simdat$data_train,
  family = poisson()
))


## -----------------------------------------------------------------------
summary(mgcv::gam(
  y ~ series + s(time, by = series),
  data = simdat$data_train,
  family = poisson()
))


## -----------------------------------------------------------------------
gauss_dat <- data.frame(
  outcome = rnorm(10),
  series = factor("series1",
    levels = "series1"
  ),
  time = 1:10
)
gauss_dat


## -----------------------------------------------------------------------
mgcv::gam(outcome ~ time,
  family = betar(),
  data = gauss_dat
)


## ----error=TRUE---------------------------------------------------------
try({
mvgam(outcome ~ time,
  family = betar(),
  data = gauss_dat
)
})


## -----------------------------------------------------------------------
# A function to ensure all timepoints within a sequence are identical
all_times_avail <- function(time, min_time, max_time) {
  identical(
    as.numeric(sort(time)),
    as.numeric(seq.int(from = min_time, to = max_time))
  )
}

# Get min and max times from the data
min_time <- min(simdat$data_train$time)
max_time <- max(simdat$data_train$time)

# Check that all times are recorded for each series
data.frame(
  series = simdat$data_train$series,
  time = simdat$data_train$time
) %>%
  dplyr::group_by(series) %>%
  dplyr::summarise(all_there = all_times_avail(
    time,
    min_time,
    max_time
  )) -> checked_times
if (any(checked_times$all_there == FALSE)) {
  warning("One or more series in is missing observations for one or more timepoints")
} else {
  cat("All series have observations at all timepoints :)")
}


## -----------------------------------------------------------------------
bad_times <- data.frame(
  time = seq(1, 16, by = 2),
  series = factor("series_1"),
  outcome = rnorm(8)
)
bad_times


## ----error = TRUE-------------------------------------------------------
try({
get_mvgam_priors(outcome ~ 1,
  data = bad_times,
  family = gaussian()
)
})


## -----------------------------------------------------------------------
bad_times %>%
  dplyr::right_join(expand.grid(
    time = seq(
      min(bad_times$time),
      max(bad_times$time)
    ),
    series = factor(unique(bad_times$series),
      levels = levels(bad_times$series)
    )
  )) %>%
  dplyr::arrange(time) -> good_times
good_times


## ----error = TRUE-------------------------------------------------------
try({
get_mvgam_priors(outcome ~ 1,
  data = good_times,
  family = gaussian()
)
})


## -----------------------------------------------------------------------
bad_levels <- data.frame(
  time = 1:8,
  series = factor("series_1",
    levels = c(
      "series_1",
      "series_2"
    )
  ),
  outcome = rnorm(8)
)

levels(bad_levels$series)


## ----error = TRUE-------------------------------------------------------
try({
get_mvgam_priors(outcome ~ 1,
  data = bad_levels,
  family = gaussian()
)
})


## -----------------------------------------------------------------------
setdiff(levels(bad_levels$series), 
        unique(bad_levels$series))


## -----------------------------------------------------------------------
bad_levels %>%
  dplyr::mutate(series = droplevels(series)) -> good_levels
levels(good_levels$series)


## ----error = TRUE-------------------------------------------------------
try({
get_mvgam_priors(
  outcome ~ 1,
  data = good_levels,
  family = gaussian()
)
})


## -----------------------------------------------------------------------
miss_dat <- data.frame(
  outcome = rnorm(10),
  cov = c(NA, rnorm(9)),
  series = factor("series1",
    levels = "series1"
  ),
  time = 1:10
)
miss_dat


## ----error = TRUE-------------------------------------------------------
try({
get_mvgam_priors(
  outcome ~ cov,
  data = miss_dat,
  family = gaussian()
)
})


## -----------------------------------------------------------------------
miss_dat <- list(
  outcome = rnorm(10),
  series = factor("series1",
    levels = "series1"
  ),
  time = 1:10
)
miss_dat$cov <- matrix(rnorm(50), ncol = 5, nrow = 10)
miss_dat$cov[2, 3] <- NA


## ----error=TRUE---------------------------------------------------------
try({
get_mvgam_priors(
  outcome ~ cov,
  data = miss_dat,
  family = gaussian()
)
})


## ----fig.alt = "Plotting time series features for GAM models in mvgam"----
plot_mvgam_series(
  data = simdat$data_train,
  y = "y",
  series = "all"
)


## ----fig.alt = "Plotting time series features for GAM models in mvgam"----
plot_mvgam_series(
  data = simdat$data_train,
  y = "y",
  series = 1
)


## ----fig.alt = "Plotting time series features for GAM models in mvgam"----
plot_mvgam_series(
  data = simdat$data_train,
  newdata = simdat$data_test,
  y = "y",
  series = 1
)


## -----------------------------------------------------------------------
data("all_neon_tick_data")
str(dplyr::ungroup(all_neon_tick_data))


## -----------------------------------------------------------------------
plotIDs <- c(
  "SCBI_013", "SCBI_002",
  "SERC_001", "SERC_005",
  "SERC_006", "SERC_012",
  "BLAN_012", "BLAN_005"
)


## -----------------------------------------------------------------------
model_dat <- all_neon_tick_data %>%
  dplyr::ungroup() %>%
  dplyr::mutate(target = ixodes_scapularis) %>%
  dplyr::filter(plotID %in% plotIDs) %>%
  dplyr::select(Year, epiWeek, plotID, target) %>%
  dplyr::mutate(epiWeek = as.numeric(epiWeek))


## -----------------------------------------------------------------------
model_dat %>%
  # Create all possible combos of plotID, Year and epiWeek;
  # missing outcomes will be filled in as NA
  dplyr::full_join(expand.grid(
    plotID = unique(model_dat$plotID),
    Year = unique(model_dat$Year),
    epiWeek = seq(1, 52)
  )) %>%
  # left_join back to original data so plotID and siteID will
  # match up, in case you need the siteID for anything else later on
  dplyr::left_join(all_neon_tick_data %>%
    dplyr::select(siteID, plotID) %>%
    dplyr::distinct()) -> model_dat


## -----------------------------------------------------------------------
model_dat %>%
  dplyr::mutate(
    series = plotID,
    y = target
  ) %>%
  dplyr::mutate(
    siteID = factor(siteID),
    series = factor(series)
  ) %>%
  dplyr::select(-target, -plotID) %>%
  dplyr::arrange(Year, epiWeek, series) -> model_dat


## -----------------------------------------------------------------------
model_dat %>%
  dplyr::ungroup() %>%
  dplyr::group_by(series) %>%
  dplyr::arrange(Year, epiWeek) %>%
  dplyr::mutate(time = seq(1, dplyr::n())) %>%
  dplyr::ungroup() -> model_dat


## -----------------------------------------------------------------------
levels(model_dat$series)


## ----error=TRUE---------------------------------------------------------
try({
get_mvgam_priors(
  y ~ 1,
  data = model_dat,
  family = poisson()
)
})


## -----------------------------------------------------------------------
testmod <- mvgam(
  y ~ s(epiWeek, by = series, bs = "cc") +
    s(series, bs = "re"),
  trend_model = AR(),
  data = model_dat,
  backend = "cmdstanr",
  run_model = FALSE
)


## -----------------------------------------------------------------------
str(testmod$model_data)


## -----------------------------------------------------------------------
stancode(testmod)

