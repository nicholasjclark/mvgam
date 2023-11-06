# Small mvgam examples for testing post-fitting functions such as
# predict, forecast, hindcast etc...
library(mvgam)
set.seed(1234)
mvgam_examp_dat <- sim_mvgam(family = gaussian(),
                             T = 40)

# Univariate process without trend_formula
mvgam_example1 <- mvgam(y ~ s(season, k = 5),
                        trend_model = 'RW',
                        family = gaussian(),
                        data = mvgam_examp_dat$data_train,
                        burnin = 300,
                        samples = 30,
                        chains = 1)

# Univariate process with trend_formula and correlated process errors
mvgam_example2 <- mvgam(y ~ 1,
                        trend_formula = ~ s(season, k = 5),
                        trend_model = RW(cor = TRUE),
                        family = gaussian(),
                        data = mvgam_examp_dat$data_train,
                        burnin = 300,
                        samples = 30,
                        chains = 1)

# Multivariate process without trend_formula
mvgam_example3 <- mvgam(y ~ s(season, k = 5),
                        trend_model = 'VAR1cor',
                        family = gaussian(),
                        data = mvgam_examp_dat$data_train,
                        burnin = 300,
                        samples = 30,
                        chains = 1)

# Multivariate process with trend_formula and moving averages
mvgam_example4 <- mvgam(y ~ 1,
                        trend_formula = ~ s(season, k = 5),
                        trend_model = VAR(ma = TRUE, cor = TRUE),
                        family = gaussian(),
                        data = mvgam_examp_dat$data_train,
                        burnin = 300,
                        samples = 30,
                        chains = 1)

# Save examples as internal data
usethis::use_data(
  mvgam_examp_dat,
  mvgam_example1,
  mvgam_example2,
  mvgam_example3,
  mvgam_example4,
  internal = TRUE,
  overwrite = TRUE
)
