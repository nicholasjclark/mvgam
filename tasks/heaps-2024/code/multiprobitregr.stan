/* Stan model for fitting the multivariate probit regression model with 
   structured matrix-t prior to the Finnish bird data. Parameters are 
   named as per the manuscipt. */
functions {
  // calculates the sum of all integers in a two-dimensional array a
  int sum2d(int[,] a) {
    int s = 0;
    for (i in 1:size(a)) s += sum(a[i]);
    return s;
  }
  /* constructs the among-row scale matrix Phi for the Finnish birds example:
     x is an array of vectors whose ith element contains the values of the ith
     metacovariate for all p species, d_P is the matrix of phylogenetic distances
     s_sq is the overall scale of Phi, set equal to 1 in the paper, and r is an
     array containing the length-scale parameters */
  matrix projExpCov_variance_matrix(vector[] x, matrix d_P, real s_sq, real[] r) {
    int n = size(x);
    int C = size(r);
    matrix[n, n] S = rep_matrix(0.0, n, n);
    for(i in 1:n) {
      for(j in 1:n) {
        if(i!=j) {
          real s = d_P[i, j] / r[C];
          S[i, j] = -s;
        }
      }
    }
    return s_sq * exp(S + log(gp_exponential_cov(x, 1.0, r[1:(C-1)])));
  }
}
data {
  int<lower=1> n; // number of sampling areas
  int<lower=1> p; // number of species
  int<lower=1> c; // number of environmental covariates
  int<lower=1> q; // number of meta-covariates used in the mean for B
  int<lower=1> ledermann_bound; // varphi(p) = ceiling((2p+1-sqrt(8p+1))/2) - 1
  int<lower=1, upper=ledermann_bound> H; // fixed truncation point
  int<lower=1> C; // number of meta-covariates used in the mean for Delta
  int<lower=0,upper=1> Y[n, p]; // occurrence data
  matrix[n, c] W; // environmental covariates
  matrix[p, q] X; // meta-covariates used in the mean for B
  vector[C] m_logvartheta; // hyperparameters in priors for log(vartheta_i)
  vector<lower=0>[C] s_logvartheta; // hyperparameters in prior for log(vartheta_i)
  matrix<lower=0>[p, p] dist_P; // matrix of phylogenetic distances 
  vector[C-1] x_forcov[p]; // array of meta-covariates used in the mean for Delta
  real<lower=0> a1; // hyperparameter in prior for Psi
  real<lower=0> a2; // hyperparameter in prior for Psi
  real<lower=0> a_varsigmacheck; // hyperparameter in gamma prior for check{varsigma}
  real<lower=0> b_varsigmacheck; // hyperparameter in gamma prior for check{varsigma}
  real<lower=0> s_beta; // hyperparameter in prior for B
  real<lower=0> s_kappa; // hyperparameter in prior for Kappa
}
transformed data {
  int<lower=0> N_pos = sum2d(Y);
  int<lower=0> N_neg = (n * p) - N_pos;
  int<lower=1, upper=n> n_pos[N_pos];
  int<lower=1, upper=p> m_pos[N_pos];
  int<lower=1, upper=n> n_neg[N_neg];
  int<lower=1, upper=p> m_neg[N_neg];
  vector[p] zero_vec = rep_vector(0.0, p);
  matrix[p, p] identity = diag_matrix(rep_vector(1.0, p));
  {
    int i = 1;
    int j = 1;
    for (ii in 1:n) {
      for (d in 1:p) {
        if (Y[ii, d] == 1) {
          n_pos[i] = ii; m_pos[i] = d; i += 1;
        } else {
          n_neg[j] = ii; m_neg[j] = d; j += 1;
        }
      }
    }
  }
}
parameters {
  matrix[c, p] B;
  matrix[q, c] Kappa;
  matrix[p, H] Lambda;
  vector<lower=0>[N_pos] z_pos;
  vector<upper=0>[N_neg] z_neg;
  matrix[n, H] Eta;
  real logvartheta[C];
  vector<lower=0>[H] varrho_inv;
  real<lower=0> varsigmacheck;
  cov_matrix[p] S;
}
transformed parameters {
  vector[p] Z[n];
  real<lower=0> vartheta[C];
  cov_matrix[p] S_Lambda;
  real<lower=4> varsigma;
  vector<lower=0>[H] Psi_diag = exp(cumulative_sum(log(varrho_inv)));
  for (i in 1:N_pos) {
    Z[n_pos[i], m_pos[i]] = z_pos[i];
  }
  for (i in 1:N_neg) {
    Z[n_neg[i], m_neg[i]] = z_neg[i];
  }
  varsigma = 1.0 / varsigmacheck + 4.0;
  for(i in 1:C) vartheta[i] = exp(logvartheta[i]);
  S_Lambda = inverse_spd(projExpCov_variance_matrix(x_forcov, dist_P, varsigma - 2.0, vartheta));
}
model {
  // Likelihood
  for(i in 1:H) Eta[,i] ~ normal(0.0, 1.0);
  {
    matrix[n, p] z_mu = W * B + Eta * Lambda';
    for(i in 1:p) {
      Z[,i] ~ normal(z_mu[,i], 1);
    }
  }
  // Prior
  for(j in 1:p) B[,j] ~ normal(Kappa' * X[j,]', s_beta);
  for(j in 1:q) Kappa[j,] ~ normal(0, s_kappa);
  S ~ wishart(varsigma+p-1, S_Lambda);
  for(i in 1:H) {
    Lambda[,i] ~ multi_normal_prec(zero_vec, S / Psi_diag[i]);
  }
  logvartheta ~ normal(m_logvartheta, s_logvartheta);
  varrho_inv[1] ~ inv_gamma(a1, 1);
  varrho_inv[2:H] ~ inv_gamma(a2, 1);
  varsigmacheck ~ gamma(a_varsigmacheck, b_varsigmacheck);
}
generated quantities {
  // Calculate identified parameters for diagnostic checking
  matrix[n, H] Etatilde;
  vector[H * p - (H * (H + 1)) / 2] Lambdatilde_offdiag;
  vector<lower=0>[H] Lambdatilde_diag;
  {
    int k = 1;
    matrix[p, H] Lambdatilde = qr_R(Lambda')';
    matrix[H, H] Q = qr_Q(Lambda')';
    Etatilde = Eta * Q';
    for(i in 1:H) {
      Lambdatilde_diag[i] = Lambdatilde[i, i];
      if((i+1)<=p) {
        for(j in (i+1):p) {
          Lambdatilde_offdiag[k] = Lambdatilde[j, i];
          k = k + 1;
        }
      }
    }
  }
}
