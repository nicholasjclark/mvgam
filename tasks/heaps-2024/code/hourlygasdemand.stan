/* Stan model for fitting the stationary dynamic factor model with 
   structured matrix-normal prior to the hourly gas demand data. Parameters 
   are named as per the manuscipt. */
functions {
  // calculates the symmetric square root of a square matrix A
  matrix sqrtm(matrix A) {
    int m = rows(A);
    vector[m] root_root_evals = sqrt(sqrt(eigenvalues_sym(A)));
    matrix[m, m] evecs = eigenvectors_sym(A);
    matrix[m, m] eprod = diag_post_multiply(evecs, root_root_evals);
    return tcrossprod(eprod);
  }
  /* converts transformed (and unconstrained) partial autocorrelation
     matrix A to partial autocorrelation matrix P */
  matrix AtoP(matrix A) {
    int m = rows(A);
    matrix[m, m] B = tcrossprod(A);
    for(i in 1:m) B[i, i] += 1.0;
    return mdivide_left_spd(sqrtm(B), A);
  }
  /* constructs a n x n tridiagonal Toeplitz matrix with corners:
     diag goes on the diagonal and offdiag on the subdiagonal and 
     supradiagonal and in the top-left and bottom-right corners */
  matrix triDiagCorners(int n, real diag, real offdiag) {
    matrix[n, n] value = diag_matrix(rep_vector(diag, n));
    if(n > 1) {
      for(i in 1:(n-1)) value[i, i+1] = offdiag;
      for(i in 2:n) value[i, i-1] = offdiag;
    }
    if(n > 2) {
      value[1, n] = offdiag;
      value[n, 1] = offdiag;
    }
    return value;
  }
  /* constructs precision matrix Xi of a stationary circular 
     autoregressive process using parameterization in paper */
  matrix circAR1pos_precision_matrix(int n, real s_sq, real r) {
    matrix[n, n] P = triDiagCorners(n, 2.0, -r) / (2.0 * s_sq);
    return P;
  }
}
data {
  int<lower=1> n; // number of days
  int<lower=1> p; // number of hours in day
  int<lower=1> c; // number of daily covariates
  int<lower=1> ledermann_bound; // varphi(p) = ceiling((2p+1-sqrt(8p+1))/2) - 1
  int<lower=1, upper=ledermann_bound> H; // fixed truncation point
  int<lower=0> num_missing; // number of missing observations
  matrix[n, p] Y; // log gas demand data
  matrix[n, c] W; // daily covariates
  int indices_missing[num_missing, 2]; /* array whose ith row contains the
      row (column 1) and column (column 2) of Y of the ith missing observation*/
  real<lower=0> a_sigma; // hyperparameter in prior for Sigma
  real<lower=0> b_sigma; // hyperparameter in prior for Sigma
  real m_logitvartheta; // hyperparameter in prior for logit(vartheta)
  real<lower=0> s_logitvartheta; // hyperparameter in prior for logit(vartheta)
  real<lower=0> a1; // hyperparameter in prior for Psi
  real<lower=0> a2; // hyperparameter in prior for Psi
  vector[c] m_mu_beta; // hyperparameter in hierarchical prior for B
  vector<lower=0>[c] s_mu_beta; // hyperparameter in hierarchical prior for B
  vector<lower=0>[c] a_tau_beta; // hyperparameter in hierarchical prior for B
  vector<lower=0>[c] b_tau_beta; // hyperparameter in hierarchical prior for B
}
transformed data {
  matrix[H, H] identity = diag_matrix(rep_vector(1.0, H));
  vector[p] zero_vec = rep_vector(0.0, p);
}
parameters {
  vector<lower=0>[p] Sigma_diag;
  matrix[p, H] Lambda;
  matrix[H, H] Amat;
  matrix[n+1, H] Eta;
  real logitvartheta;
  vector<lower=0>[H] varrho_inv;
  matrix[p, c] B;
  vector[c] mu_beta;
  vector<lower=0>[c] tau_beta;
  vector[num_missing] y_missing;
}
transformed parameters {
  real vartheta;
  vector<lower=0>[H] Psi_diag = exp(cumulative_sum(log(varrho_inv)));
  cov_matrix[p] Xi;
  matrix[H, H] Gamma = AtoP(Amat);
  cov_matrix[H] Pi = identity - Gamma * Gamma';
  matrix[n, p] Y_complete;
  Y_complete = Y;
  if(num_missing > 0) {
    for(i in 1:num_missing) {
      Y_complete[indices_missing[i, 1], indices_missing[i, 2]] = y_missing[i];
    }
  }
  vartheta = inv_logit(logitvartheta);
  Xi = circAR1pos_precision_matrix(p, 1.0, vartheta);
}
model {
  // Likelihood
  Eta[1,] ~ normal(0.0, 1.0);
  for(t in 1:n) {
    Eta[t+1,] ~ multi_normal(Gamma * Eta[t,]', Pi);
  }
  {
    matrix[n, p] y_mu = W * B' + Eta[2:(n+1),] * Lambda';
    for(i in 1:p) {
      Y_complete[,i] ~ normal(y_mu[,i], sqrt(Sigma_diag[i]));
    }
  }
  // Prior
  Sigma_diag ~ inv_gamma(a_sigma, b_sigma);
  to_vector(Amat) ~ normal(0.0, 1.0);
  for(i in 1:c) {
    B[,i] ~ normal(mu_beta[i], 1 / sqrt(tau_beta[i]));
  }
  mu_beta ~ normal(m_mu_beta, s_mu_beta);
  tau_beta ~ gamma(a_tau_beta, b_tau_beta);
  for(i in 1:H) {
    Lambda[,i] ~ multi_normal_prec(zero_vec, Xi / Psi_diag[i]);
  }
  logitvartheta ~ normal(m_logitvartheta, s_logitvartheta);
  varrho_inv[1] ~ inv_gamma(a1, 1);
  varrho_inv[2:H] ~ inv_gamma(a2, 1);
}
generated quantities {
  // Calculate identified parameters for diagnostic checking
  matrix[n+1, H] Etatilde;
  matrix[H, H] Gammatilde;
  matrix[H, H] Amattilde;
  vector[H * p - (H * (H + 1)) / 2] Lambdatilde_offdiag;
  vector<lower=0>[H] Lambdatilde_diag;
  {
    int k = 1;
    matrix[p, H] Lambdatilde = qr_R(Lambda')';
    matrix[H, H] Q = qr_Q(Lambda')';
    Etatilde = Eta * Q';
    Gammatilde = Q * Gamma * Q';
    Amattilde = Q * Amat * Q';
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
