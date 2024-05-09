devtools::load_all()
library(testthat)

# Test the c++ functions, which can all be done using sim_mvgam()
# and forecast()
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
fc <- forecast(mvgam:::mvgam_example1,
               newdata = mvgam:::mvgam_examp_dat$data_test)
