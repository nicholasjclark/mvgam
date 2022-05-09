# Testing Rstan for modelling discrete time series with missing values
library(mvgam)
sim_data <- sim_mvgam(T = 65,
                      n_series = 3,
                      prop_missing = 0.1,
                      n_trends = 2,
                      train_prop = 0.85,
                      trend_rel = 0.55,
                      seasonality = 'shared',
                      mu_obs = c(6, 11, 4),
                      family = 'poisson')

# Fit the model using JAGS
hier_mod <- mvjagam(data_train = sim_data$data_train,
                    data_test = sim_data$data_test,
                    formula = y ~ s(series, bs = 're') +
                      s(season, k = 12, m = 2, bs = 'cc') - 1,
                    knots = list(season = c(0.5, 12.5)),
                    trend_model = 'AR3',
                    family = 'poisson',
                    burnin = 5000,
                    chains = 4,
                    return_jags_data = TRUE,
                    run_model = T)
hier_mod$model_file

# Extract data and modify for fitting in Stan; requires the full JAGS model be made first,
# but not actually fitted
jags_data <- hier_mod$jags_data
model_file <- hier_mod$model_file
trend <- 'AR3'


# The skeleton Stan file
  stan_skeleton <- "
##insert data
parameters {
// raw basis coefficients
row_vector[num_basis] b_raw;

// latent trend variance parameters
vector<lower=0.15, upper=5>[n_series] sigma;

// latent trends
matrix[n, n_series] trend;

// smoothing parameters
vector<lower=0>[n_sp] lambda;
}

transformed parameters {
// basis coefficients
row_vector[num_basis] b;

// GAM contribution to expectations (log scale)
vector[total_obs] eta;
eta = to_vector(b * X);
}

model {
##insert smooths

// priors for smoothing parameters
lambda ~ exponential(0.25);

// priors for latent trend variance parameters
sigma ~ exponential(1);

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
rho = log(lambda);

// posterior predictions
int<lower=0> ypred[n, n_series];
for (i in 1:n) {
for (s in 1:n_series) {
ypred[i, s] = poisson_log_rng(eta[ytimes[i, s]] + trend[i, s]);
}
}
}
"

# Modify the Stan file
unlink('base_gam_stan.txt')
cat(stan_skeleton, file = 'base_gam_stan.txt', sep = '\n', append = T)
base_stan_model <- suppressWarnings(readLines('base_gam_stan.txt'))

# Add in necessary trend structure
if(trend == 'AR1'){
  base_stan_model[grep('// raw basis', base_stan_model) + 1] <-
    paste0('row_vector[num_basis] b_raw;\n\n// latent trend AR1 terms\nvector<lower=-1, upper=1>[n_series] ar1;')

  base_stan_model[grep('// trend estimates', base_stan_model) + 6] <-
    paste0('trend[2:n, s] ~ normal(ar1[s] * trend[1:(n - 1), s], sigma[s]);')

  base_stan_model[grep('model \\{', base_stan_model) + 2] <-
  paste0('\n// priors for AR parameters\nar1 ~ normal(0, 0.5);\n')
  base_stan_model <- readLines(textConnection(base_stan_model), n = -1)
}

if(trend == 'AR2'){
  base_stan_model[grep('// raw basis', base_stan_model) + 1] <-
    paste0('row_vector[num_basis] b_raw;\n\n// latent trend AR1 terms\nvector<lower=-1, upper=1>[n_series] ar1;\n\n',
           '// latent trend AR2 terms\nvector<lower=-1, upper=1>[n_series] ar2;')
  base_stan_model[grep('// trend estimates', base_stan_model) + 2] <-
    paste0('trend[1, s] ~ normal(0, sigma[s]);')

  base_stan_model <- base_stan_model[-(grep('// trend estimates', base_stan_model) + 5:7)]
  base_stan_model[grep('// trend estimates', base_stan_model) + 5] <-
    paste0('for (s in 1:n_series) {\n',
           'trend[2, s] ~ normal(trend[1, s] * ar1[s], sigma[s]);\n',
           '}\n\n',
           'for (i in 3:n) {\n',
           'for (s in 1:n_series) {\n',
           'trend[i, s] ~ normal(ar1[s] * trend[i - 1, s] + ar2[s] * trend[i - 2, s], sigma[s]);\n',
           '}\n}\n')

  base_stan_model[grep('model \\{', base_stan_model) + 2] <-
    paste0('\n// priors for AR parameters\nar1 ~ normal(0, 0.5);\nar2 ~ normal(0, 0.5);\n')
  base_stan_model <- readLines(textConnection(base_stan_model), n = -1)
}

if(trend == 'AR3'){
  base_stan_model[grep('// raw basis', base_stan_model) + 1] <-
    paste0('row_vector[num_basis] b_raw;\n\n// latent trend AR1 terms\nvector<lower=-1, upper=1>[n_series] ar1;\n\n',
           '// latent trend AR2 terms\nvector<lower=-1, upper=1>[n_series] ar2;\n\n',
           '// latent trend AR3 terms\nvector<lower=-1, upper=1>[n_series] ar3;')
  base_stan_model[grep('// trend estimates', base_stan_model) + 2] <-
    paste0('trend[1, s] ~ normal(0, sigma[s]);')

  base_stan_model <- base_stan_model[-(grep('// trend estimates', base_stan_model) + 5:7)]
  base_stan_model[grep('// trend estimates', base_stan_model) + 5] <-
    paste0('for (s in 1:n_series) {\n',
           'trend[2, s] ~ normal(trend[1, s] * ar1[s], sigma[s]);\n',
           '}\n\n',

           'for (s in 1:n_series) {\n',
           'trend[3, s] ~ normal(trend[2, s] * ar1[s] + trend[1, s] * ar2[s], sigma[s]);\n',
           '}\n\n',

           'for (i in 4:n) {\n',
           'for (s in 1:n_series) {\n',
           'trend[i, s] ~ normal(ar1[s] * trend[i - 1, s] + ar2[s] * trend[i - 2, s] +ar3[s] * trend[i - 3, s], sigma[s]);\n',
           '}\n}\n')

  base_stan_model[grep('model \\{', base_stan_model) + 2] <-
    paste0('\n// priors for AR parameters\nar1 ~ normal(0, 0.5);\nar2 ~ normal(0, 0.5);\nar3 ~ normal(0, 0.5);\n')
  base_stan_model <- readLines(textConnection(base_stan_model), n = -1)
}

# Get dimensions and numbers of smooth terms
snames <- names(jags_data)[grep('S.*', names(jags_data))]
smooth_dims <- matrix(NA, ncol = 2, nrow = length(snames))
for(i in 1:length(snames)){
  smooth_dims[i,] <- dim(jags_data[[snames[i]]])
}

# Insert the data block for the model
smooth_penalty_data <- vector()
for(i in 1:length(snames)){
  smooth_penalty_data[i] <- paste0('matrix[', smooth_dims[i, 1],
                                   ',',
                                   smooth_dims[i, 2], '] ',
                                   snames[i],
                                   '; // mgcv smooth penalty matrix ', snames[i])
}

# Search for any non-contiguous indices that sometimes are used by mgcv
if(any(grep('in c\\(', model_file))){
  add_idxs <- TRUE
  seq_character = function(x){
    all_nums <- as.numeric(unlist(strsplit(x, ':')))
    if(length(all_nums) > 1){
      out <- seq(all_nums[1], all_nums[2])
    } else {
      out <- all_nums
    }
    out
  }

  idx_locations <- grep('in c\\(', model_file)
  idx_vals <- list()
  idx_data <- vector()
  for(i in 1:length(idx_locations)){
    list_vals <- unlist(strsplit(gsub('^.*c\\(*|\\s*).*$', '', model_file[idx_locations[i]]), ','))
    idx_vals[[i]] <- unlist(lapply(list_vals, seq_character))
    idx_data[i] <- paste0('int idx', i, '[', length(idx_vals[[i]]), ']; // discontiguous index values')
    model_file[idx_locations][i] <- sub("in.*\\)\\)", paste0("in idx", i, ')'), model_file[idx_locations][i])
  }

  base_stan_model[grep('##insert data',
                       base_stan_model)] <- paste0('//Stan code generated by package mvgam\n',
                                                   'data {',
                                                   '\n',
                                                   paste0(idx_data, collapse = '\n'), '\n',
                                                   'int<lower=0> total_obs; // total number of observations\n',
                                                   'int<lower=0> n; // number of timepoints per series\n',
                                                   'int<lower=0> n_sp; // number of smoothing parameters\n',
                                                   'int<lower=0> n_series; // number of series\n',
                                                   'int<lower=0> num_basis; // total number of basis coefficients\n',
                                                   'vector[num_basis] zero; // priro locations for basis coefficients\n',
                                                   'matrix[num_basis, total_obs] X; // transposed mgcv GAM design matrix\n',
                                                   'int<lower=0> ytimes[n, n_series]; // time-ordered matrix (which col in X belongs to each [time, series] observation?)\n',
                                                   paste0(smooth_penalty_data, collapse = '\n'), '\n',
                                                   'int<lower=0, upper=1> y_observed[n, n_series]; // indices of missing vs observed\n',
                                                   'int<lower=-1> y[n, n_series]; // time-ordered observations, with -1 indicating missing\n',
                                                   '}\n')
} else {
  add_idxs <- FALSE
  base_stan_model[grep('##insert data',
                       base_stan_model)] <- paste0('//Stan code generated by package mvgam\n',
                                                   'data {',
                                                   '\n',
                                                   'int<lower=0> total_obs; // total number of observations\n',
                                                   'int<lower=0> n; // number of timepoints per series\n',
                                                   'int<lower=0> n_sp; // number of smoothing parameters\n',
                                                   'int<lower=0> n_series; // number of series\n',
                                                   'int<lower=0> num_basis; // total number of basis coefficients\n',
                                                   'vector[num_basis] zero; // prior locations for basis coefficients\n',
                                                   'matrix[num_basis, total_obs] X; // transposed mgcv GAM design matrix\n',
                                                   'int<lower=0> ytimes[n, n_series]; // time-ordered matrix (which col in X belongs to each [time, series] observation?)\n',
                                                   paste0(smooth_penalty_data, collapse = '\n'), '\n',
                                                   'int<lower=0, upper=1> y_observed[n, n_series]; // indices of missing vs observed\n',
                                                   'int<lower=-1> y[n, n_series]; // time-ordered observations, with -1 indicating missing\n',
                                                   '}\n')
}

base_stan_model <- readLines(textConnection(base_stan_model), n = -1)

# Modify the model block to include each smooth term
smooths_start <- grep('## GAM-specific priors', model_file) + 1
smooths_end <- grep('## smoothing parameter priors...', model_file) - 1
jags_smooth_text <- model_file[smooths_start:smooths_end]
jags_smooth_text <- gsub('##', '//', jags_smooth_text)
jags_smooth_text <- gsub('dexp', 'exponential', jags_smooth_text)

K_starts <- grep('K.* <- ', jags_smooth_text)
for(i in 1:length(K_starts)){
  jags_smooth_text[K_starts[i]+1] <- gsub('\\bb\\b', 'b_raw', gsub('dmnorm', 'multi_normal_prec',
                                          paste0(gsub('K.*',
                                                      trimws(gsub('K.* <- ', '',
                                                                  jags_smooth_text[K_starts[i]])),
       jags_smooth_text[K_starts[i]+1]), ')')))
}
jags_smooth_text <- jags_smooth_text[-K_starts]
if(any(grep('b\\[i\\] <- b_raw', jags_smooth_text))){
  jags_smooth_text <- jags_smooth_text[-grep('b\\[i\\] <- b_raw', jags_smooth_text)]
}
jags_smooth_text <- gsub('dnorm', 'normal', jags_smooth_text)
jags_smooth_text <- gsub('  ', ' ', jags_smooth_text)
jags_smooth_text[-grep('//|\\}|\\{', jags_smooth_text)] <-
  paste0(jags_smooth_text[-grep('//|\\}|\\{', jags_smooth_text)], ';')
jags_smooth_text <- gsub(') }', '); }', jags_smooth_text)
jags_smooth_text <- gsub('}', '}\n', jags_smooth_text)
jags_smooth_text[(grep('//',
                       jags_smooth_text) - 1)[-1]] <-
  paste0(jags_smooth_text[(grep('//',
                                jags_smooth_text) - 1)[-1]], '\n')
base_stan_model[grep('##insert smooths',
                     base_stan_model)] <- paste0(jags_smooth_text, collapse = '\n')
base_stan_model <- readLines(textConnection(base_stan_model), n = -1)

# Deal with random effect priors
if(any(grep('b_raw\\[i\\] ~', base_stan_model))){
  b_raw_string <- paste0(base_stan_model[grep('b_raw\\[i\\] ~', base_stan_model)-1], collapse = ',')
  n_b_raw <- max(as.numeric(unlist(regmatches(b_raw_string,
             gregexpr("[[:digit:]]+",
                      b_raw_string)))))

  n_sigma_raw <- max(as.numeric(unlist(regmatches(grep('sigma_raw', base_stan_model, value = T),
                                                    gregexpr("[[:digit:]]+",
                                                             grep('sigma_raw',
                                                                  base_stan_model, value = T))))))


  base_stan_model <- base_stan_model[-grep('mu_raw.* ~ ', base_stan_model)]
  base_stan_model <- base_stan_model[-grep('<- mu_raw', base_stan_model)]
  base_stan_model <- base_stan_model[-grep('sigma_raw.* ~ ', base_stan_model)]
  base_stan_model[grep('model \\{', base_stan_model)] <-
    paste0('model {\n// prior for random effect population variances\nsigma_raw ~ exponential(0.5);\n\n',
             '// prior for random effect population means\nmu_raw ~ normal(0, 1);\n')

  base_stan_model[grep('parameters \\{', base_stan_model)[1] + 2] <-
    paste0(base_stan_model[grep('parameters \\{', base_stan_model)[1] + 2],
           '\n',
           '\n// random effect variances\n',
           paste0('vector<lower=0>[',n_sigma_raw,'] sigma_raw', ';\n', collapse = ''),
           '\n',
           paste0('vector<lower=0>[',n_sigma_raw,'] mu_raw', ';\n', collapse = ''))

  b_raw_text <- vector()
  b_raw_indices <- grep('b_raw\\[i\\] ~', base_stan_model)
  for(i in 1:length(b_raw_indices)){

    b_raw_text[i] <- paste0('for (i in ', as.numeric(sub('.*(?=.$)', '',
                                       sub("\\:.*", "",
                                           base_stan_model[b_raw_indices[i] - 1]), perl=T)),
           ':', as.numeric(substr(sub(".*\\:", "",
                                     base_stan_model[b_raw_indices[i]-1]),
                                 1, 1)),') {\nb[i] <- mu_raw[', i, '] + b_raw[i] * sigma_raw[',i,
           '];\n}')
  }

  # If parametric coefficients are included, they'll come before random effects
  min_re_betas <- as.numeric(sub('.*(?=.$)', '',
                                 sub("\\:.*", "",
                                     base_stan_model[b_raw_indices[1]-1]), perl=T))
  if(min_re_betas > 1){
    b_raw_text <- c(paste0('\nfor (i in 1:',
                    min_re_betas - 1, ') {\nb[i] <- b_raw[i];\n}'),
                    b_raw_text,
                    paste0('\nfor (i in ',  n_b_raw+1,':num_basis) {\nb[i] <- b_raw[i];\n}\n'))
  } else {
    b_raw_text <- c(b_raw_text,
                    paste0('\nfor (i in ',  n_b_raw+1,':num_basis) {\nb[i] <- b_raw[i];\n}\n'))
  }

  base_stan_model[grep('// basis coefficients', base_stan_model) + 2] <- paste0(b_raw_text,
                                                                                collapse = '\n')
  base_stan_model <- readLines(textConnection(base_stan_model), n = -1)

  # If no random effects, betas are equal to beta_raws
} else {
  base_stan_model[grep('// basis coefficients', base_stan_model) + 2] <-
    paste0('\nfor (i in ','1:num_basis) {\nb[i] <- b_raw[i];\n}')
  base_stan_model <- readLines(textConnection(base_stan_model), n = -1)
}

# Update parametric effect priors
if(any(grep('// parametric effect', base_stan_model))){
  base_stan_model[grep('// parametric effect', base_stan_model) + 1] <-
    paste0('for (i in ',

  as.numeric(sub('.*(?=.$)', '',
                 sub("\\:.*", "",
                     base_stan_model[grep('// parametric effect', base_stan_model) + 1]), perl=T)),
  ':', as.numeric(substr(sub(".*\\:", "",
                             base_stan_model[grep('// parametric effect', base_stan_model) + 1]),
                         1, 1)),
  ') {\nb[i] ~ normal(0, 1);\n}')
  base_stan_model <- readLines(textConnection(base_stan_model), n = -1)
}
unlink('base_gam_stan.txt')
base_stan_model <- readLines(textConnection(base_stan_model), n = -1)

# Final tidying of the Stan model for readability
clean_up <- vector()
for(x in 1:length(base_stan_model)){
  clean_up[x] <- base_stan_model[x-1] == "" & base_stan_model[x] == ""
}
clean_up[is.na(clean_up)] <- FALSE
base_stan_model <- base_stan_model[!clean_up]


# Modify the Stan data list
# Create matrix representing whether an observation was missing or not
y_observed <- matrix(NA, ncol = NCOL(jags_data$y),
                     nrow = NROW(jags_data$y))
for (i in 1:dim(jags_data$y)[1]) {
  for (s in 1:dim(jags_data$y)[2]) {
    if (is.na(jags_data$y[i, s])) {
      y_observed[i, s] = 0
    } else {
      y_observed[i, s] = 1
    }
  }
}

# Use -1 for any missing observations so Stan doesn't throw errors due to NAs
y <- jags_data$y
y[is.na(y)] <- -1

# The data list for Stan
stan_data <- jags_data
stan_data$y <- y
stan_data$y_observed <- y_observed
stan_data$X <- t(stan_data$X)
stan_data$total_obs <- NCOL(stan_data$X)
stan_data$num_basis <- NROW(stan_data$X)
stan_data$n_sp <- as.numeric(sub('\\) \\{', '',
                                 sub('for \\(i in 1\\:', '',
                                     model_file[grep('lambda\\[i\\] ~ ',
                                                     trimws(model_file)) - 1])))

# Add discontiguous index values if required
if(add_idxs){
  names(idx_vals) <- paste0('idx', seq_len(length(idx_vals)))
  stan_data <- append(stan_data, idx_vals)
}

library(rstan)
options(mc.cores = parallel::detectCores())

fit1 <- stan(model_code = base_stan_model, iter = 5000, chains = 4,
             data = stan_data, cores = 4)
pairs(fit1, pars = c('sigma_raw', 'b[1]', 'b[2]', 'b[3]'))

# Convert stanfit object to samples
library(coda)
model_output <- mcmc.list(lapply(1:NCOL(fit1),
                                 function(x) mcmc(as.array(fit1)[,x,])))

MCMCvis::MCMCtrace(model_output, 'sigma_raw', pdf = F)
MCMCvis::MCMCtrace(model_output, 'rho', pdf = F)
MCMCvis::MCMCtrace(hier_mod$jags_output, 'rho', pdf = F)
MCMCvis::MCMCtrace(model_output, 'ar1', pdf = F)
MCMCvis::MCMCtrace(hier_mod$jags_output, 'ar1', pdf = F)
MCMCvis::MCMCtrace(model_output, c('sigma'), pdf = F)
MCMCvis::MCMCtrace(hier_mod$jags_output, c('sigma'), pdf = F)

object <- hier_mod
object$jags_output <- model_output
class(object) = 'mvgam'

par(mfrow = c(2,1))
plot(object, type = 'forecast', series = 1, data_test = sim_data$data_test)
plot(hier_mod, type = 'forecast', series = 1, data_test = sim_data$data_test)
plot(object, type = 'forecast', series = 2, data_test = sim_data$data_test)
plot(hier_mod, type = 'forecast', series = 2, data_test = sim_data$data_test)
plot(object, type = 'forecast', series = 3, data_test = sim_data$data_test)
plot(hier_mod, type = 'forecast', series = 3, data_test = sim_data$data_test)

plot(object, type = 'trend', series = 1, data_test = sim_data$data_test)
plot(hier_mod, type = 'trend', series = 1, data_test = sim_data$data_test)
plot(object, type = 'trend', series = 2, data_test = sim_data$data_test)
plot(hier_mod, type = 'trend', series = 2, data_test = sim_data$data_test)
plot(object, type = 'trend', series = 3, data_test = sim_data$data_test)
plot(hier_mod, type = 'trend', series = 3, data_test = sim_data$data_test)


plot(object, type = 'smooths')
plot(hier_mod, type = 'smooths')

ppc(object, type = 'rootogram', series = 2)
ppc(hier_mod, type = 'rootogram', series = 2)

plot(object, type = 're')
plot(hier_mod, type = 're')
