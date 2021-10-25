#### Testing the mvjagam function ####

#### NEON mv_gam ####
library(mvgam)
library(dplyr)
data("all_neon_tick_data")


# Hypothesis testing
# NULL. There is no seasonal pattern to be estimated, and we simply let the latent
# factors and site-level effects of growing days influence the series dynamics
null_hyp = y ~ siteID + s(cum_gdd, by = siteID, k = 3) - 1

# 1. Do all series share same seasonal pattern, with any remaining variation due to
# non-seasonal local variation captured by the trends?
hyp1 = y ~
  siteID +
  s(cum_gdd, by = siteID, k = 3) +
  # Global cyclic seasonality term (smooth)
  s(season, k = 12, m = 2, bs = 'cc') - 1

# 2. Do all series share same seasonal pattern but with different magnitudes
# (i.e. random intercepts per series)?
hyp2 = y ~
  siteID +
  s(cum_gdd, by = siteID, k = 3) +
  s(season, k = 12, m = 2, bs = 'cc') +
  # Hierarchical variable intercepts
  s(series, bs = 're') - 1

# 3. Is there evidence for global seasonality but each site's seasonal pattern deviates
# based on more local conditions?
hyp3 = y ~
  siteID +
  s(cum_gdd, by = siteID, k = 3) +
  s(season, k = 4, m = 2, bs = 'cc') +
  s(series, bs = 're') +
  # Site-level deviations from global pattern, which can be wiggly (m=1 to reduce concurvity);
  # If these dominate, they will have smaller smoothing parameters and the global seasonality
  # will become less important (larger smoothing parameter). Sites with the smallest smooth
  # parameters are those that deviate the most from the global seasonality
  s(season, by = siteID, m = 1, k = 8) - 1

# 4. Is there evidence for global seasonality but each plot's seasonal pattern deviates
# based on even more local conditions than above (i.e. plot-level is not as informative)?
# If evidence of gdd effects, can also let use a global smoother and then site-level
# deviations for a 'hierarchical' setup
hyp4 = y ~
  siteID +
  s(cum_gdd, by = siteID, k = 3) +
  s(season, k = 4, m = 2, bs = 'cc') +
  s(series, bs = 're') +
  # Series-level deviations from global pattern
  s(season, by = series, m = 1, k = 8) - 1

# Additional parameters for the model
use_mv <- T

# First simulate from the model's prior distributions
out_gam_sim <- mvjagam(formula = hyp1,
                       data_train = data_train,
                       data_test = data_test,
                       prior_simulation = TRUE,
                       n.adapt = 1000,
                       n.burnin = 1000,
                       n.iter = 1000,
                       thin = 2,
                       use_mv = use_mv,
                       use_nb = FALSE,
                       # Laplace distribution emphasizes our prior that smooths should not be overly wiggly
                       # unless the data supports this
                       rho_prior = 'ddexp(5, 0.2)T(-12, 12)',
                       # Prior is that latent trends should have positive autocorrelation
                       phi_prior = 'dbeta(2,2)',
                       tau_prior = 'dunif(0.1, 100)')

# Now condition the model on the observed data
out_gam_mod <- mvjagam(formula = hyp1,
                        data_train = data_train,
                        data_test = data_test,
                        n.adapt = 1000,
                        n.burnin = 5000,
                        n.iter = 5000,
                        thin = 10,
                        # auto_update attempts to update until some reasonable convergence, but can
                        # be slow!!
                        auto_update = FALSE,
                        use_mv = use_mv,
                        use_nb = FALSE,
                        # Laplace distribution emphasizes prior that smooths should not be overly wiggly
                        # unless the data supports this
                        rho_prior = 'ddexp(5, 0.2)T(-12, 12)',
                        # Prior is that latent trends should have positive autocorrelation
                        phi_prior = 'dbeta(2, 2)',
                        tau_prior = 'dunif(0.1, 100)')

# View the modified JAGS model file
writeLines(out_gam_mod$model_file)

# Summary of key parameters
library(MCMCvis)
MCMCvis::MCMCsummary(out_gam_mod$jags_output, c('phi', 'rho'))

# Traces of key parameters, with prior distributions overlain to investigate
# how informative the data are for each parameter of interest
# Negative binomial size parameter (set to 10,000 if use_nb = FALSE)
MCMCtrace(out_gam_mod$jags_output, params = c('r'),
          pdf = F,
          n.eff = TRUE,
          Rhat = TRUE,
          priors = MCMCvis::MCMCchains(out_gam_sim$jags_output, 'r'),
          post_zm = FALSE)

# Penalties of smooth components (smaller means an effect is more nonlinear)
out_gam_mod$smooth_param_details # rhos are the logged versions of the lambdas
MCMCtrace(out_gam_mod$jags_output, c('rho'),
          pdf = F,
          n.eff = TRUE,
          Rhat = TRUE,
          priors = MCMCvis::MCMCchains(out_gam_sim$jags_output, 'rho'),
          post_zm = FALSE)

# AR1 persistence coefficients for latent dynamic factors (or for each
# individual series if use_mv = FALSE)
MCMCtrace(out_gam_mod$jags_output, c('phi'),
          pdf = F,
          n.eff = TRUE,
          Rhat = TRUE,
          priors = MCMCvis::MCMCchains(out_gam_sim$jags_output, 'phi'),
          post_zm = FALSE)

# Precision for latent dynamic factors (if using latent factors)
MCMCtrace(out_gam_mod$jags_output, c('tau_fac'),
          pdf = F,
          n.eff = TRUE,
          Rhat = TRUE,
          priors = MCMCvis::MCMCchains(out_gam_sim$jags_output, 'tau_fac'),
          post_zm = T)

# Precision for latent trends (if using independent Gaussian trends)
MCMCtrace(out_gam_mod$jags_output, c('tau'),
          pdf = F,
          n.eff = TRUE,
          Rhat = TRUE,
          priors = MCMCvis::MCMCchains(out_gam_sim$jags_output, 'tau'),
          post_zm = FALSE)
dev.off()

if(use_mv){
  if(length(unique(data_train$series)) > 5){
    # Calculate the correlation matrix from the latent variables
    samps <- jags.samples(out_gam_mod$jags_model,
                          variable.names = 'lv_coefs',
                          n.iter = 1000, thin = 1)
    lv_coefs <- samps$lv_coefs
    n_series <- dim(lv_coefs)[1]
    n_lv <- dim(lv_coefs)[2]
    n_samples <- prod(dim(lv_coefs)[3:4])

    # Get arrat of latend variable loadings
    coef_array <- array(NA, dim = c(n_series, n_lv, n_samples))
    for(i in 1:n_series){
      for(j in 1:n_lv){
        coef_array[i, j, ] <- c(lv_coefs[i, j, , 1],
                                lv_coefs[i, j, , 2])
      }
    }

    # Variances of each series' latent trend are fixed at (1/100)^2
    eps_res <- rep((1/100)^2, n_series)

    # Posterior correlations based on latent variable loadings
    correlations <- array(NA, dim = c(n_series, n_series, n_samples))
    for(i in 1:n_samples){
      correlations[,,i] <- cov2cor(tcrossprod(coef_array[,,i]) + diag(eps_res))
    }
    mean_correlations <- apply(correlations, c(1,2), function(x) quantile(x, 0.5))

    # Plot the mean posterior correlations
    mean_correlations[upper.tri(mean_correlations)] <- NA
    mean_correlations <- data.frame(mean_correlations)
    rownames(mean_correlations) <- colnames(mean_correlations) <- levels(data_train$series)

    library(ggplot2)
    ggplot(mean_correlations %>%
             tibble::rownames_to_column("series1") %>%
             tidyr::pivot_longer(-c(series1), names_to = "series2", values_to = "Correlation"),
           aes(x = series1, y = series2)) + geom_tile(aes(fill = Correlation)) +
      scale_fill_gradient2(low="darkred", mid="white", high="darkblue",
                           midpoint = 0,
                           breaks = seq(-1,1,length.out = 5),
                           limits = c(-1, 1),
                           name = 'Residual\ncorrelation') + labs(x = '', y = '') + theme_dark() +
      theme(axis.text.x = element_text(angle = 45, hjust=1))

  }
}

#### If GAM component is LESS supported, we should see evidence in the form of: ####
# 1. Poorer convergence of smoothing parameter estimates, suggesting the model
# is more 'mis-specified' and harder to fit

# 2. Stronger residual correlations, suggesting we are missing some site-level structure
summary(as.vector(as.matrix(mean_correlations)))

# 3. Smaller precisions for tau_fac (i.e. larger variance for latent dynamic factors)
MCMCsummary(out_gam_mod$jags_output, 'tau_fac')

# 4. Visual evidence of seasonality in latent trends
# Total number of series in the set
length(unique(data_train$series))
series = 4
opar <- par()
par(mfrow = c(3, 1))
# Plot the estimated seasonality function
plot_mvgam_season(out_gam_mod, series = series, data_test = data_test,
                  data_train = data_train, xlab = 'Epidemiological week')
# Plot the posterior predictions for the training and testing sets
plot_mvgam_fc(out_gam_mod, series = series, data_test = data_test,
              data_train = data_train)
# Plot the estimated latent trend component
plot_mvgam_trend(out_gam_mod, series = series, data_test = data_test,
                 data_train = data_train)
par(opar)

#### Other aspects to investigate ####
# Plot the estimated cumulative growing degree days function, with a rug at the bottom
# to show the observed values of the covariate for this particular series
plot_mvgam_gdd(out_gam_mod, series = series, data_test = data_test,
               mean_gdd = mean_gdd, sd_gdd = sd_gdd,
               data_train = data_train, xlab = 'Epidemiological week')

# Calculate Discrete Rank Probability Score and nominal coverage of 90% credible interval for
# each unique series
all_drps <- do.call(rbind, lapply(seq_len(length(unique(data_train$series))), function(series){
  calculate_drps(out_gam_mod = out_gam_mod, data_test = data_test,
                 data_train = data_train, series = series, interval_width = 0.9)
}))


# Consider adding an AR(frequency) term so that current trend also depends on the trend's value from one year
# ago (i.e. here we have frequency 26, so we could have an AR(1, 26) model for the trend)
