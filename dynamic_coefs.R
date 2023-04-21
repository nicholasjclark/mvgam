
N = 200
beta_temp <- mvgam:::sim_gp(rnorm(1),
                            alpha_gp = 0.75,
                            rho_gp = 10,
                            h = N) + 0.5


set.seed(80)
beta_temp <- vector(length = N)
beta_temp[1] <- 0.2
for(i in 2:N){
  beta_temp[i] <- rnorm(n = 1,
                        mean = 0.0125 + beta_temp[i - 1],
                        sd = 0.25)
}
plot(beta_temp, type = 'l')

temp <- rnorm(N, sd = 1)
out <- rnorm(N, mean = 4 + beta_temp * temp,
             sd = 0.25)
time <- seq_along(temp)
plot(out, type = 'l')

data = data.frame(out, temp, time)
data_train <- data[1:190,]
data_test <- data[191:200,]

mvgam:::interpret_mvgam(out ~ dynamic(temp, rho = 10, stationary = FALSE), N = 100)
formula = out ~ dynamic(temp, rho = 10, stationary = TRUE)
mod <- mvgam(formula = out ~ dynamic(temp, rho = 10, stationary = TRUE),
             family = gaussian(),
             data = data_train)
summary(mod)
code(mod)

plot(mod, type = 'forecast', newdata = data_test)
plot(mod, type = 'smooths')
plot_mvgam_smooth(mod, smooth = 1, newdata = data)
abline(v = 180, lty = 'dashed', lwd = 2)
lines(beta_temp)
