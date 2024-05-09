devtools::load_all()
library(testthat)
capture_output(sim_mvgam(family = gaussian(),
                         trend_model = RW()))
capture_output(sim_mvgam(family = gaussian(),
                         trend_model = AR(p = 1)))
capture_output(sim_mvgam(family = gaussian(),
                         trend_model = AR(p = 2)))
capture_output(sim_mvgam(family = gaussian(),
                         trend_model = AR(p = 3)))
capture_output(sim_mvgam(family = gaussian(),
                         trend_model = VAR()))
capture_output(sim_mvgam(family = gaussian(),
                         trend_model = VAR(cor = TRUE)))
