#### Experimental code for adding dynamic coefficients ####
rw = function(...){
  vars <- as.list(substitute(list(...)))[-1]
  if(length(vars) > 1) stop("rw() can only handle one term at a time.")
  term <- deparse(vars[[1]])
  if (term[1]==".") stop("rw(.) not supported.")

  term <- attr(terms(reformulate(term)),"term.labels")
  class(term) <- "rw.spec"
  term
}
rw(handle)

formula <- formula(y ~ rw(handle) + s(season))
form_terms <- terms(formula(formula))
attr(terms, form_terms, 'term.labels')
attr(terms(formula(formula)), 'term.labels')


set.seed(123456)
N = 200
beta_temp <- vector(length = N)
beta_temp[1] <- 0.4
for(i in 2:N){
  beta_temp[i] <- rnorm(1, mean = beta_temp[i - 1],
                        sd = 0.025)
}
plot(beta_temp, type = 'l')


temp <- rnorm(N, sd = 1)

out <- rnorm(N, mean = 4 + beta_temp * temp,
             sd = 0.05)
time <- seq_along(temp)
plot(out, type = 'l')

# User simply supplies rho (default is to use ceiling(N / 2) but this can
# smooth over important variation and will often
# lead to very computationally demanding predictions)
rho <- ceiling(N / 2)
rho <- 8
k <- min(50, min(N, max(8, ceiling(N / (rho - (rho / 10))))))
k

mod <- gam(out ~ s(time, by = temp,
                   # Make sure k is quite large
                   k = k,
                   bs = 'gp',
                   # m[1] = -2 sets exponential kernel; the negative ensures
                   # the process is stationary
                   # m[2] = 2 sets rho (needs to be supplied by user)
                   # m[3] = 2 is used for squared exponential;
                   # alpha is fixed at 1 in this case
                   m = c(-2, rho, 2)))
plot(mod)
plot(beta_temp, type = 'l')

plot(gam(out ~ s(time, by = temp, k = 20)))
