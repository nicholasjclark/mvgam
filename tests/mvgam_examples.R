# Small mvgam examples for testing post-fitting functions such as
# predict, forecast, hindcast etc...
testthat::skip_on_cran()
library(mvgam)
mvgam_examp_dat <- list(
  data_train = structure(
    list(
      y = c(
        -1.6435760529886,
        0.0576506632876403,
        -0.398982741359959,
        0.166263635072232,
        NA,
        -0.178792865387502,
        0.0378992006898741,
        -0.46704324582468,
        -0.20005752901963,
        NA,
        -0.7648331324566,
        -1.95818875683478,
        -0.489141832766607,
        NA,
        -0.781926449502298,
        -0.173065622618926,
        NA,
        -0.431888938737423,
        -1.33563987611521,
        -0.30668079493666,
        -1.59343527302515,
        -2.08089938293457
      ),
      season = c(
        1L,
        1L,
        2L,
        2L,
        3L,
        3L,
        4L,
        4L,
        5L,
        5L,
        6L,
        6L,
        7L,
        7L,
        8L,
        8L,
        9L,
        9L,
        10L,
        10L,
        11L,
        11L
      ),
      year = c(
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L,
        1L
      ),
      series = structure(
        c(
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L,
          1L,
          2L
        ),
        levels = c("series_1", "series_2"),
        class = "factor"
      ),
      time = c(
        1L,
        1L,
        2L,
        2L,
        3L,
        3L,
        4L,
        4L,
        5L,
        5L,
        6L,
        6L,
        7L,
        7L,
        8L,
        8L,
        9L,
        9L,
        10L,
        10L,
        11L,
        11L
      )
    ),
    class = "data.frame",
    row.names = c(NA, -22L)
  ),
  data_test = structure(
    list(
      y = c(
        -0.825903793273796,
        NA,
        -0.409364591883054,
        -0.801934825421605,
        NA,
        0.993612304219531,
        0.465708559827663,
        -0.268653159692507
      ),
      season = c(12L, 12L, 1L, 1L, 2L, 2L, 3L, 3L),
      year = c(1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L),
      series = structure(
        c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
        levels = c("series_1", "series_2"),
        class = "factor"
      ),
      time = c(12L, 12L, 13L, 13L, 14L, 14L, 15L, 15L)
    ),
    class = "data.frame",
    row.names = c(NA, -8L)
  )
)

# Univariate process without trend_formula
mvgam_example1 <- mvgam(
  y ~ s(season, k = 4),
  trend_model = RW(),
  family = gaussian(),
  data = mvgam_examp_dat$data_train,
  burnin = 50,
  samples = 5,
  chains = 1,
  backend = 'rstan'
)

# Univariate process with trend_formula, trend_map and correlated process errors
trend_map <- data.frame(
  series = unique(mvgam_examp_dat$data_train$series),
  trend = c(1, 1)
)
mvgam_example2 <- mvgam(
  y ~ 1,
  trend_formula = ~ s(season, k = 4),
  trend_model = RW(cor = TRUE),
  trend_map = trend_map,
  family = gaussian(),
  data = mvgam_examp_dat$data_train,
  burnin = 50,
  samples = 5,
  chains = 1,
  backend = 'rstan'
)

# Multivariate process without trend_formula
mvgam_example3 <- mvgam(
  y ~ s(season, k = 4),
  trend_model = VAR(cor = TRUE),
  family = gaussian(),
  data = mvgam_examp_dat$data_train,
  burnin = 50,
  samples = 5,
  chains = 1,
  backend = 'rstan',
  lfo = TRUE
)

# GP dynamic factors (use list format to ensure it works in tests)
list_data <- list()
for (i in 1:NCOL(mvgam_examp_dat$data_train)) {
  list_data[[i]] <- mvgam_examp_dat$data_train[, i]
}
names(list_data) <- colnames(mvgam_examp_dat$data_train)
mvgam_example4 <- mvgam(
  y ~ series + s(season, k = 4),
  trend_model = GP(),
  family = gaussian(),
  use_lv = TRUE,
  n_lv = 2,
  data = list_data,
  burnin = 50,
  samples = 5,
  chains = 1,
  backend = 'rstan',
  lfo = TRUE
)

# Save examples as internal data
usethis::use_data(
  mvgam_examp_dat,
  mvgam_example1,
  mvgam_example2,
  mvgam_example3,
  mvgam_example4,
  internal = TRUE,
  overwrite = TRUE,
  compress = 'xz'
)
