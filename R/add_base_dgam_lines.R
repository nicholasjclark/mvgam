#' Dynamic GAM model file additions
#'
#'
#' @noRd
#' @param use_lv Logical (use latent variables or not?)
#' @param stan Logical (convert existing model to a Stan model?)
#' @param offset Logical (include an offset in the linear predictor?)
#' @return A character string to add to the mgcv jagam model file
add_base_dgam_lines = function(use_lv, stan = FALSE, offset = FALSE){

  if(stan){
    if(use_lv){
      add <- "
    ##insert data
    transformed data {
    // Number of non-zero lower triangular factor loadings
    // Ensures identifiability of the model - no rotation of factors
    int<lower=1> M;
    M = n_lv * (n_series - n_lv) + n_lv * (n_lv - 1) / 2 + n_lv;
    }

    parameters {
    // raw basis coefficients
    row_vector[num_basis] b_raw;

    // dynamic factors
    matrix[n, n_lv] LV_raw;

    // dynamic factor lower triangle loading coefficients
    vector[M] L;

    // smoothing parameters
    vector<lower=0>[n_sp] lambda;
    }

    transformed parameters {
    // GAM contribution to expectations (log scale)
    vector[total_obs] eta;

    // trends and dynamic factor loading matrix
    matrix[n, n_series] trend;
    matrix[n_series, n_lv] lv_coefs_raw;

    // basis coefficients
    row_vector[num_basis] b;

    // constraints allow identifiability of loadings
    for (i in 1:(n_lv - 1)) {
    for (j in (i + 1):(n_lv)){
    lv_coefs_raw[i, j] = 0;
    }
    }
    {
    int index;
    index = 0;
    for (j in 1:n_lv) {
      for (i in j:n_series) {
        index = index + 1;
        lv_coefs_raw[i, j] = L[index];
      }
    }
    }

    // derived latent trends
    for (i in 1:n){
    for (s in 1:n_series){
    trend[i, s] = dot_product(lv_coefs_raw[s,], LV_raw[i,]);
    }
    }

    eta = to_vector(b * X);
    }

    model {
    ##insert smooths

    // priors for smoothing parameters
    lambda ~ normal(5, 30);

    // priors for dynamic factor loading coefficients
    L ~ student_t(5, 0, 1);

    // dynamic factor estimates
    for (j in 1:n_lv) {
    LV_raw[1, j] ~ normal(0, 0.1);
    }

    for (j in 1:n_lv) {
    LV_raw[2:n, j] ~ normal(LV_raw[1:(n - 1), j], 0.1);
    }

    // likelihood functions
    for (i in 1:n) {
    for (s in 1:n_series) {
    if (y_observed[i, s])
    y[i, s] ~ poisson_log(eta[ytimes[i, s]] + trend[i, s]);
    }
    }
    }

    generated quantities {
    matrix[n, n_lv] LV;
    matrix[n_series, n_lv] lv_coefs;
    vector[n_sp] rho;
    vector[n_lv] penalty;
    matrix[n, n_series] ypred;
    rho = log(lambda);
    penalty = rep_vector(100.0, n_lv);

    // Sign correct factor loadings and factors
    for(j in 1:n_lv){
    if(lv_coefs_raw[j, j] < 0){
      lv_coefs[,j] = -1 * lv_coefs_raw[,j];
      LV[,j] = -1 * LV_raw[,j];
    } else {
      lv_coefs[,j] = lv_coefs_raw[,j];
      LV[,j] = LV_raw[,j];
    }
    }

    // posterior predictions
    for(i in 1:n){
    for(s in 1:n_series){
    ypred[i, s] = poisson_log_rng(eta[ytimes[i, s]] + trend[i, s]);
    }
    }
    }
    "

    } else {
      add <- "
    ##insert data
    parameters {
    // raw basis coefficients
    row_vector[num_basis] b_raw;

    // latent trend variance parameters
    vector<lower=0>[n_series] sigma;

    // latent trends
    matrix[n, n_series] trend;

    // smoothing parameters
    vector<lower=0>[n_sp] lambda;
    }

    transformed parameters {
    // GAM contribution to expectations (log scale)
    vector[total_obs] eta;

    // basis coefficients
    row_vector[num_basis] b;

    eta = to_vector(b * X);
    }

    model {
    ##insert smooths

    // priors for smoothing parameters
    lambda ~ normal(5, 30);

    // priors for latent trend variance parameters
    sigma ~ exponential(2);

    // trend estimates
    for (s in 1:n_series) {
    trend[1, s] ~ normal(0, sigma[s]);
    }

    for (s in 1:n_series) {
    trend[2:n, s] ~ normal(trend[1:(n - 1), s], sigma[s]);
    }

    // likelihood functions
    for (i in 1:n) {
    for (s in 1:n_series) {
    if (y_observed[i, s])
    y[i, s] ~ poisson_log(eta[ytimes[i, s]] + trend[i, s]);
    }
    }
    }

    generated quantities {
    vector[n_sp] rho;
    vector[n_series] tau;
    matrix[n, n_series] ypred;
    rho = log(lambda);
    for (s in 1:n_series) {
    tau[s] = pow(sigma[s], -2.0);
    }

    // posterior predictions
    for(i in 1:n){
    for(s in 1:n_series){
    ypred[i, s] = poisson_log_rng(eta[ytimes[i, s]] + trend[i, s]);
    }
    }
    }
    "
    }

  } else {
    if(use_lv){
      add <- c("
               #### Begin model ####
               model {

               ## GAM linear predictor
               eta <- X %*% b

               ## mean expectations
               for (i in 1:n) {
               for (s in 1:n_series) {
               mus[i, s] <- exp(eta[ytimes[i, s]] + trend[i, s])
               }
               }

               ## latent factors evolve as time series with penalised precisions;
               ## the penalty terms force any un-needed factors to evolve as flat lines
               for (j in 1:n_lv) {
               LV_raw[1, j] ~ dnorm(0, penalty[j])
               }

               for (j in 1:n_lv) {
               LV_raw[2, j] ~ dnorm(drift[j] + ar1[j]*LV_raw[1, j], penalty[j])
               }

               for (j in 1:n_lv) {
               LV_raw[3, j] ~ dnorm(drift[j]*2 + ar1[j]*LV_raw[2, j] + ar2[j]*LV_raw[1, j], penalty[j])
               }

               for (i in 4:n) {
               for (j in 1:n_lv) {
               LV_raw[i, j] ~ dnorm(drift[j]*(i - 1) + ar1[j]*LV_raw[i - 1, j] +
               ar2[j]*LV_raw[i - 2, j] + ar3[j]*LV_raw[i - 3, j], penalty[j])
               }
               }

               ## AR components
               for (s in 1:n_lv) {
               drift[s] ~ dnorm(0, 10)
               ar1[s] ~ dnorm(0, 10)
               ar2[s] ~ dnorm(0, 10)
               ar3[s] ~ dnorm(0, 10)
               }

               ## shrinkage penalties for each factor's precision parameter act to squeeze
               ## the entire factor toward a flat white noise process if supported by
               ## the data. The prior for individual factor penalties allows each factor to possibly
               ## have a relatively large penalty, which shrinks the prior for that factor's variance
               ## substantially. Penalties increase exponentially with the number of factors following
               ## Welty, Leah J., et al. Bayesian distributed lag models: estimating effects of particulate
               ## matter air pollution on daily mortality Biometrics 65.1 (2009): 282-291.
               pi ~ dunif(0, n_lv)
               X2 ~ dnorm(0, 1)T(0, )

               # eta1 controls the baseline penalty
               eta1 ~ dunif(-1, 1)

               # eta2 controls how quickly the penalties exponentially increase
               eta2 ~ dunif(-1, 1)

               for (t in 1:n_lv) {
               X1[t] ~ dnorm(0, 1)T(0, )
               l.dist[t] <- max(t, pi[])
               l.weight[t] <- exp(eta2[] * l.dist[t])
               l.var[t] <- exp(eta1[] * l.dist[t] / 2) * 1
               theta.prime[t] <- l.weight[t] * X1[t] + (1 - l.weight[t]) * X2[]
               penalty[t] <- max(0.0001, theta.prime[t] * l.var[t])
               }

               ## latent factor loadings: standard normal with identifiability constraints
               ## upper triangle of loading matrix set to zero
               for (j in 1:(n_lv - 1)) {
               for (j2 in (j + 1):n_lv) {
               lv_coefs_raw[j, j2] <- 0
               }
               }

               ## positive constraints on loading diagonals
               for (j in 1:n_lv) {
               lv_coefs_raw[j, j] ~ dnorm(0, 1)T(0, 1);
               }

               ## lower diagonal free
               for (j in 2:n_lv) {
               for (j2 in 1:(j - 1)) {
               lv_coefs_raw[j, j2] ~ dnorm(0, 1)T(-1, 1);
               }
               }

               ## other elements also free
               for (j in (n_lv + 1):n_series) {
               for (j2 in 1:n_lv) {
               lv_coefs_raw[j, j2] ~ dnorm(0, 1)T(-1, 1);
               }
               }

               ## trend evolution depends on latent factors
               for (i in 1:n) {
               for (s in 1:n_series) {
               trend[i, s] <- inprod(lv_coefs_raw[s,], LV_raw[i,])
               }
               }

               # sign-correct factor loadings and coefficients
               for (j in 1:n_lv){
                if(lv_coefs[j,j] < 0){
                 lv_coefs[,j] <- -1 * lv_coefs_raw[,j]
                 LV[,j] <- -1 * LV_raw[,j]
                } else {
                 lv_coefs[,j] <- lv_coefs_raw[,j]
                 LV[,j] <- LV_raw[,j]
                }
               }

               ## likelihood functions
               for (i in 1:n) {
               for (s in 1:n_series) {
               y[i, s] ~ dnegbin(rate[i, s], phi[s])T(, upper_bound[s]);
               rate[i, s] <- ifelse((phi[s] / (phi[s] + mus[i, s])) < min_eps, min_eps,
               (phi[s] / (phi[s] + mus[i, s])))
               }
               }

               ## complexity penalising prior for the overdispersion parameter;
               ## where the likelihood reduces to a 'base' model (Poisson) unless
               ## the data support overdispersion
               for (s in 1:n_series) {
               phi[s] <- 1 / phi_inv[s]
               phi_inv[s] ~ dexp(5)
               }

               ## posterior predictions
               for (i in 1:n) {
               for (s in 1:n_series) {
               ypred[i, s] ~ dnegbin(rate[i, s], phi[s])T(, upper_bound[s])
               }
               }

               ## GAM-specific priors")
    } else {
      add <- c("
                          #### Begin model ####
                          model {

                          ## GAM linear predictor
                          eta <- X %*% b

                          ## mean expectations
                          for (i in 1:n) {
                          for (s in 1:n_series) {
                          mus[i, s] <- exp(eta[ytimes[i, s]] + trend[i, s])
                          }
                          }

                          ## trend estimates
                          for (s in 1:n_series) {
                          trend[1, s] ~ dnorm(0, tau[s])
                          }

                          for (s in 1:n_series) {
                          trend[2, s] ~ dnorm(drift[s] + ar1[s]*trend[1, s], tau[s])
                          }

                          for (s in 1:n_series) {
                          trend[3, s] ~ dnorm(drift[s]*2 + ar1[s]*trend[2, s] + ar2[s]*trend[1, s], tau[s])
                          }

                          for (i in 4:n) {
                          for (s in 1:n_series){
                          trend[i, s] ~ dnorm(drift[s]*(i - 1) + ar1[s]*trend[i - 1, s] + ar2[s]*trend[i - 2, s] + ar3[s]*trend[i - 3, s], tau[s])
                          }
                          }

                          ## AR components
                          for (s in 1:n_series){
                          drift[s] ~ dnorm(0, 10)
                          ar1[s] ~ dnorm(0, 10)
                          ar2[s] ~ dnorm(0, 10)
                          ar3[s] ~ dnorm(0, 10)
                          tau[s] <- pow(sigma[s], -2)
                          sigma[s] ~ dexp(2)T(0.075, 5)
                          }

                          ## likelihood functions
                          for (i in 1:n) {
                          for (s in 1:n_series) {
                          y[i, s] ~ dnegbin(rate[i, s], phi[s])T(, upper_bound[s]);
                          rate[i, s] <- ifelse((phi[s] / (phi[s] + mus[i, s])) < min_eps, min_eps,
                          (phi[s] / (phi[s] + mus[i, s])))
                          }
                          }

                          ## complexity penalising prior for the overdispersion parameter;
                          ## where the likelihood reduces to a 'base' model (Poisson) unless
                          ## the data support overdispersion
                          for (s in 1:n_series) {
                          phi[s] <- 1 / phi_inv[s]
                          phi_inv[s] ~ dexp(5)
                          }

                          ## posterior predictions
                          for (i in 1:n) {
                          for (s in 1:n_series) {
                          ypred[i, s] ~ dnegbin(rate[i, s], phi[s])T(, upper_bound[s])
                          }
                          }

                          ## GAM-specific priors")

    }
  }

  return(add)
    }
