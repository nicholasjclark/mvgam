params <-
list(EVAL = TRUE)

## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)


## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  dpi = 100,
  fig.asp = 0.7,
  fig.width = 6,
  out.width = "70%",
  fig.align = "center"
)
library(mvgam)
library(ggplot2)
theme_set(theme_bw(base_size = 12, base_family = "serif"))


## ----birdsong-head------------------------------------------------------------
data(birdsong)
str(birdsong)
head(birdsong, 4)


## ----sim-mvgam-head-----------------------------------------------------------
set.seed(1)
simdat <- sim_mvgam(n_series = 3L, n_timepoints = 24L,
                     prop_missing = 0.2)
head(simdat$data_train, 6)


## ----series-class-------------------------------------------------------------
class(birdsong$series)
levels(birdsong$series)


## ----series-character-fail, error = TRUE--------------------------------------
try({
char_series <- birdsong
char_series$series <- as.character(char_series$series)
check_mvgam_data(char_series, family = poisson(), plot = FALSE)
})


## ----dropping-levels----------------------------------------------------------
extra_level <- birdsong
extra_level$series <- factor(
  extra_level$series,
  levels = c(levels(extra_level$series), "phantom_species")
)
setdiff(levels(extra_level$series), unique(extra_level$series))

# Tidy by dropping unused levels.
extra_level$series <- droplevels(extra_level$series)
levels(extra_level$series)


## ----time-range---------------------------------------------------------------
range(birdsong$time)
tapply(birdsong$time, birdsong$series, function(t) diff(range(t)) + 1L)


## ----expand-grid-fix----------------------------------------------------------
sparse <- birdsong[!(birdsong$series == "warbler" &
                       birdsong$time %in% c(5L, 6L, 7L)), ]
nrow(sparse)
filled <- merge(
  expand.grid(series = levels(sparse$series),
              time   = sort(unique(sparse$time))),
  sparse, by = c("series", "time"), all.x = TRUE
)
nrow(filled)
sum(is.na(filled$y))


## ----bad-family, error = TRUE-------------------------------------------------
try({
bad_family <- data.frame(
  y      = rnorm(20),
  time   = 1:20,
  series = factor("series_1")
)
mvgam(y ~ 1, data = bad_family, family = Beta(),
      chains = 1L, samples = 100L, silent = 2L)
})


## ----check-good, fig.asp = 0.85, out.width = "85%"----------------------------
check_mvgam_data(
  data    = birdsong,
  family  = poisson(),
  series  = 1L
)


## ----check-bad-family, error = TRUE-------------------------------------------
try({
bad_y <- birdsong
bad_y$y <- bad_y$y - 0.5  # introduce negative non-integers
check_mvgam_data(bad_y, family = poisson(), plot = FALSE)
})


## ----check-missing-times, error = TRUE----------------------------------------
try({
gap <- birdsong[birdsong$time != 5L, ]
check_mvgam_data(gap, family = poisson(), trend_model = AR(),
                  plot = FALSE)
})


## ----check-covariate-na, error = TRUE-----------------------------------------
try({
miss_cov <- birdsong
miss_cov$temp <- rnorm(nrow(miss_cov))
miss_cov$temp[5L] <- NA
check_mvgam_data(miss_cov, formula = y ~ temp,
                  family = poisson(), plot = FALSE)
})


## ----validate-newdata-ok------------------------------------------------------
future_df <- data.frame(
  series = factor(
    rep(levels(birdsong$series), each = 4L),
    levels = levels(birdsong$series)
  ),
  time   = rep(81L:84L, times = 4L)
)
validate_newdata(future_df, birdsong)


## ----validate-newdata-bad, error = TRUE---------------------------------------
try({
bad_future <- data.frame(
  series = factor("phantom_species"),
  time   = 81L
)
validate_newdata(bad_future, birdsong)
})


## ----closure-shape------------------------------------------------------------
occ_dat <- sim_closure_unit_data(
  family    = occ(),
  type      = 1L,
  n_species = 2L,
  n_sites   = 12L,
  n_visits  = 3L,
  seed      = 1L
)
str(occ_dat$data_train)
head(occ_dat$data_train, 6)


## ----check-closure------------------------------------------------------------
check_mvgam_data(occ_dat$data_train, family = occ(),
                  plot = FALSE)


## ----custom-names, results = "hide", eval = FALSE-----------------------------
# # Equivalent to a `time` / `series` setup, no renaming required:
# mvgam(
#   y ~ s(week_in_year, bs = "cc", k = 10),
#   trend_formula = ~ AR(time = week, series = species),
#   data    = birdsong,
#   family  = poisson(),
#   chains  = 2L, silent = 2L
# )

