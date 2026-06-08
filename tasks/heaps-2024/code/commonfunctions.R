library(bayesSurv) # Provides random multivariate normal simulation based on precision parameterisation
library(expm)
library(dlm)
library(matrixcalc)

####################
# LQ decomposition #
####################

householder = function(x) {
  require(Matrix)
  e = c(1, rep(0, length(x)-1))
  vk = sign(x[1]) * sqrt(sum(x^2)) * e + x
  # Compute tau and the H matrix
  tau = 2 / sum(vk^2)
  hk = diag(length(x)) - tau * (vk %*% t(vk))
  w = vk / sqrt(sum(vk^2))
  w[1] = (hk %*% x)[1]
  return(list(tau=tau, H=hk, w=w))
}

my_LQ_decomp = function(A, posDiag=FALSE) {
  dims = dim(A)
  N = dims[1]
  M = dims[2]
  tau = numeric(min(dims))
  Qlist = list()
  Q = diag(M)
  for(i in 1:min(dims)) {
    ## Compute the Householder transformation to reduce the j-th
    ## column of the matrix to a multiple of the j-th unit vector
    x = A[i, i:M]
    tau_i = householder(x)
    A[i, i:M] = tau_i$w
    Qlist[[i]] = as.matrix(Matrix::bdiag(diag(i-1), tau_i$H))
    Q = Qlist[[i]] %*% Q
    tau[i] = tau_i$tau
    ## Apply the transformation to the remaining columns and
    ## update the norms
    if((i+1) <= N) {
      m = A[(i+1):N, i:M]
      m = m %*% tau_i$H
      A[(i+1):N, i:M] = m
    }
  }
  L = A
  for(i in 1:min(dims)) {
    if((i+1) <= M) L[i,(i+1):M] = 0
  }
  if(posDiag) {
    correct = which(diag(L)<0)
    L[,correct] = -L[,correct]
    Q[correct,] = -Q[correct,]
  }
  return(list(L=L, Q=Q))
}

#####################
# General functions #
#####################

tracem = function(x) sum(diag(x))

rmatrixt = function(nu, Sigma, Psi) {
  n = ncol(Sigma); m = ncol(Psi)
  X = matrix(rMVNorm(1, rep(0, n*m), Sigma=Psi %x% diag(n)), n, m)
  S = stats::rWishart(1, nu+n-1, solve(Sigma))[,,1]
  Srt = chol(S)
  Tmat = solve(Srt, X)
  return(Tmat)
}

##########################################
# FCDs and priors for general parameters #
##########################################

sample_fcd_Sigma_diag = function(a, b, Y, Eta, Lambda, prior_temp=1, like_temp=1) {
  shape = prior_temp * a + like_temp * nrow(Y) / 2 + (prior_temp - 1)
  rate = prior_temp * b + like_temp * apply(Y - Eta[-1,] %*% t(Lambda), 2, function(x) sum(x^2)) / 2
  return(1 / rgamma(ncol(Y), shape, rate))
}

sample_prior_Sigma_diag = function(m, a, b) {
  return(1 / rgamma(m, a, b))
}

sample_fcd_mu = function(Y, Eta, Lambda, Sigma_diag, mn, prec, prior_temp=1, like_temp=1) {
  N = nrow(Y)
  pprec = prior_temp * prec + like_temp * N * diag(1/Sigma_diag)
  pmn = solve(pprec, prior_temp * prec %*% mn + like_temp * N * colMeans(Y - Eta[-1,] %*% t(Lambda)) / Sigma_diag)
  return(rMVNorm(1, pmn, Q=pprec))
}

sample_prior_mu_or_beta = function(mn, prec) {
  return(rMVNorm(1, mn, Q=prec))
}

sample_fcd_beta = function(Y, X, Eta, Lambda, Sigma_diag, mn, prec, prior_temp=1, like_temp=1) {
  Y = Y - Eta[-1,] %*% t(Lambda)
  pprec = crossprod(X) %x% diag(like_temp / Sigma_diag) + prior_temp * diag(prec)
  pmn = solve(pprec, prior_temp * mn * prec + (t(X) %x% diag(like_temp / Sigma_diag)) %*% as.numeric(t(Y)))
  return(matrix(rMVNorm(1, pmn, Q=pprec), ncol(X), ncol(Y), byrow=TRUE))
}

sample_fcd_mu_beta = function(m_beta, s_beta, beta, tau_beta, prior_temp=1) {
  denom = (length(beta) * s_beta^2 + 1 / tau_beta)
  mean = (sum(beta) * s_beta^2 + m_beta / tau_beta) / denom
  var = (s_beta^2 / tau_beta) / denom
  return(rnorm(1, mean, sqrt(var/prior_temp)))
}

sample_prior_mu_beta = function(m_beta, s_beta) {
  return(rnorm(1, m_beta, s_beta))
}

sample_fcd_tau_beta = function(a_beta, b_beta, beta, mu_beta, prior_temp=1) {
  shape = prior_temp * (a_beta + length(beta) / 2) - (prior_temp - 1)
  rate = prior_temp * (b_beta + sum((beta-mu_beta)^2) / 2)
  return(rgamma(1, shape, rate))
}

sample_prior_tau_beta = function(a_beta, b_beta) {
  return(rgamma(1, a_beta, b_beta))
}

sample_fcd_missing_data = function(mean_Y, Eta, Lambda, Sigma_diag, indices_missing, like_temp=1) {
  n = nrow(indices_missing)
  pmn = (mean_Y[indices_missing[,1],] + Eta[indices_missing[,1]+1,] %*% t(Lambda))[cbind(1:n, indices_missing[,2])]
  pvar = Sigma_diag[indices_missing[,2]]
  return(rnorm(n, pmn, sqrt(pvar/like_temp)))
}

prior_Lambda = function(a1, a2, P_Lambda, H=NA, delta=NULL, nu) {
  # if delta is non-null, set H equal to 1 to sample new delta and set equal to something
  # else otherwise
  if(is.na(H) & is.null(delta)) stop("Need to specify either H or delta.")
  p = nrow(P_Lambda)
  if(!is.null(delta)) {
    # This is an adaptation step
    # Add a column
    if(length(delta)==0) tmp = rgamma(1, a1, 1)
    else tmp = rgamma(1, a2, 1)
    # Recompute theta
    theta = 1 / exp(sum(log(c(delta, tmp))))
    delta = tmp
    # Sample last column of Lambda
    Lambda = rMVNorm(1, rep(0, p), Q=P_Lambda/theta)
  }
  else {
    # This is the initialisation step
    # Sample delta
    delta = c(rgamma(1, a1, 1), rgamma(H-1, a2, 1))
    # Compute theta
    theta = 1 / exp(cumsum(log(delta)))
    # Sample Lambda
    Lambda = matrix(0, p, H)
    if(is.na(nu)) {
      for(j in 1:H) {
        Lambda[,j] = rMVNorm(1, rep(0, p), Q=P_Lambda/theta[j])
      }
    } else {
      Lambda = rmatrixt(nu, P_Lambda, diag(theta, H)) # P_Lambda is actually S_Lambda here
    }
  }
  return(list(delta=delta, theta=theta, Lambda=Lambda))
}

sample_fcd_Lambda_MGP = function(Y, Eta, Sigma_diag, P_Lambda, theta, prior_temp=1, like_temp=1) {
  N = nrow(Y)
  m = ncol(Y)
  num_fac = ncol(Eta)
  # Construct prior mean and precision
  mn = rep(0, num_fac * m)
  P = matrix(0, num_fac * m, num_fac * m)
  for(i in 1:num_fac) {
    P[((i-1)*m+1):(i*m), ((i-1)*m+1):(i*m)] = P_Lambda / theta[i]
  }
  P = P * prior_temp
  # Compute posterior mean and precision
  Sigma_diag = Sigma_diag / like_temp
  Fstar = Eta[-1,] %x% diag(m)
  tmp = sweep(Fstar, 1, rep(Sigma_diag, N), "/")
  pP = P + crossprod(tmp, Fstar)
  pmn = P %*% mn + crossprod(tmp, as.numeric(t(Y)))
  pmn = as.numeric(solve(pP, pmn))
  Lambda = matrix(rMVNorm(1, pmn, Q=pP), m, num_fac)
  return(Lambda)
}

fcd_theta = function(a1, a2, delta, Lambda, P_Lambda, prior_temp=1) {
  m = nrow(P_Lambda)
  num_fac = length(delta)
  a = c(a1, rep(a2, num_fac-1))
  for(h in 1:num_fac) {
    rate = 0
    for(l in h:num_fac) {
      rate = rate + exp(sum(log(delta[1:l][-h]))) * sum(crossprod(Lambda[,l], P_Lambda) * Lambda[,l])
    }
    rate = (rate / 2 + 1) * prior_temp
    shape = prior_temp * (a[h] + m * (num_fac-h+1) / 2 - 1) + 1
    delta[h] = rgamma(1, shape, rate)
  }
  theta = 1 / exp(cumsum(log(delta)))
  return(list(delta=delta, theta=theta))
}

########################################################
# FCDs and priors for (static) factor model parameters #
########################################################

sample_fcd_Eta = function(Y, Lambda, Sigma_diag, prior_temp=1, like_temp=1) {
  num_fac = ncol(Lambda)
  Sigma_diag = Sigma_diag / like_temp
  tmp = sweep(Lambda, 1, Sigma_diag, "/")
  prec = diag(num_fac) * prior_temp + crossprod(Lambda, tmp)
  mn = solve(prec, tcrossprod(t(tmp), Y))
  if(num_fac > 1) Eta = t(apply(mn, 2, function(col) rMVNorm(1, mean=col, Q=prec)))
  else Eta = matrix(apply(mn, 2, function(col) rMVNorm(1, mean=col, Q=prec)), nrow(Y), 1)
  Eta = rbind(rnorm(num_fac, 0, sqrt(1/prior_temp)), Eta)
  return(Eta)
}

sample_prior_Eta = function(N, num_fac) {
  return(matrix(rnorm((N+1)*num_fac), N+1, num_fac))
}

#######################################################
# FCDs and priors for dynamic factor model parameters #
#######################################################

sample_fcd_dynEta = function(Y, Lambda, Sigma_diag, Gamma, Pi, prior_temp=1, like_temp=1) {
  num_fac = ncol(Gamma)
  dlmFilt = dlmFilter(Y, list(m0=rep(0, num_fac), C0=diag(num_fac)/prior_temp, 
                              FF=Lambda, V=diag(Sigma_diag)/like_temp, GG=Gamma, W=Pi/prior_temp))
  Eta = dlmBSample(dlmFilt)
  return(Eta)
}

sample_prior_dynEta = function(N, Gamma, Pi) {
  num_fac = ncol(Gamma)
  Eta = matrix(NA, N+1, num_fac)
  Eta[1,] = rnorm(num_fac)
  for(t in 2:(N+1)) {
    Eta[t,] = rMVNorm(1, Gamma %*% Eta[t-1,], Sigma=Pi)
  }
  return(Eta)
}

##################################################################
# FCDs and priors for stationary dynamic factor model parameters #
##################################################################

Amat_update = function(Amean, Aprec, Eta, curr_Pi, curr_Gamma, curr_Amat, prop_Var, prior_temp=1) {
  # Compute summary statistics
  num_fac = nrow(curr_Amat)
  N = nrow(Eta) - 1
  # Generate proposal
  prop_Amat = matrix(rMVNorm(1, as.numeric(curr_Amat), Sigma=prop_Var), num_fac, num_fac)
  prop_Gamma = lemma2_2a(prop_Amat)
  prop_Pi = diag(num_fac) - prop_Gamma %*% t(prop_Gamma)
  # Compute log-likelihoods
  curr_tmp = Eta[-1,] - Eta[-(N+1),] %*% t(curr_Gamma)
  prop_tmp = Eta[-1,] - Eta[-(N+1),] %*% t(prop_Gamma)
  curr_ll = -N / 2 * determinant(curr_Pi, logarithm = TRUE)$modulus - 
    tracem(solve(curr_Pi, t(curr_tmp) %*% curr_tmp)) / 2
  prop_ll = -N / 2 * determinant(prop_Pi, logarithm = TRUE)$modulus - 
    tracem(solve(prop_Pi, t(prop_tmp) %*% prop_tmp)) / 2
  # Compute log prior densities
  curr_lprior = -crossprod(as.numeric(curr_Amat) - Amean, Aprec) %*% (as.numeric(curr_Amat) - Amean) / 2
  prop_lprior = -crossprod(as.numeric(prop_Amat) - Amean, Aprec) %*% (as.numeric(prop_Amat) - Amean) / 2
  # Accept / reject
  logA = prior_temp * (prop_ll - curr_ll + prop_lprior - curr_lprior)
  if(log(runif(1))<logA) {
    curr_Pi = prop_Pi
    curr_Gamma = prop_Gamma
    curr_Amat = prop_Amat
    accept = TRUE
  } else accept=FALSE
  return(list(Pi=curr_Pi, Gamma=curr_Gamma, Amat=curr_Amat, accept=accept))
}

Amat_block_update = function(Amean, Aprec_vec, Eta, curr_Pi, curr_Gamma, curr_Amat, prop_Var, 
                             block_membership, prior_temp=1) {
  # Compute summary statistics
  num_fac = nrow(curr_Amat)
  N = nrow(Eta) - 1
  num_blocks = max(unique(block_membership))
  curr_tmp = Eta[-1,] - Eta[-(N+1),] %*% t(curr_Gamma)
  curr_ll = -N / 2 * determinant(curr_Pi, logarithm = TRUE)$modulus - 
    tracem(solve(curr_Pi, t(curr_tmp) %*% curr_tmp)) / 2
  accept =rep(FALSE, num_blocks)
  for(block in 1:num_blocks) {
    # Generate proposal
    prop_Amat = as.numeric(curr_Amat)
    block_curr_Amat = prop_Amat[block_membership==block]
    block_prop_Var = prop_Var[block_membership==block, block_membership==block, drop=FALSE]
    block_prop_Amat = rMVNorm(1, block_curr_Amat, Sigma=block_prop_Var)
    prop_Amat[block_membership==block] = block_prop_Amat
    prop_Amat = matrix(prop_Amat, num_fac, num_fac)
    prop_Gamma = lemma2_2a(prop_Amat)
    prop_Pi = diag(num_fac) - prop_Gamma %*% t(prop_Gamma)
    # Compute log-likelihoods
    prop_tmp = Eta[-1,] - Eta[-(N+1),] %*% t(prop_Gamma)
    prop_ll = -N / 2 * determinant(prop_Pi, logarithm = TRUE)$modulus - 
      tracem(solve(prop_Pi, t(prop_tmp) %*% prop_tmp)) / 2
    # Compute log prior densities
    block_Amean = Amean[block_membership==block]
    block_Asd = 1/sqrt(Aprec_vec[block_membership==block])
    curr_lprior = sum(dnorm(block_curr_Amat, block_Amean, block_Asd, log=TRUE))
    prop_lprior = sum(dnorm(block_prop_Amat, block_Amean, block_Asd, log=TRUE))
    # Accept / reject
    logA = prior_temp * (prop_ll - curr_ll + prop_lprior - curr_lprior)
    if(log(runif(1))<logA) {
      curr_Pi = prop_Pi
      curr_Gamma = prop_Gamma
      curr_Amat = prop_Amat
      curr_ll = prop_ll
      accept[block] = TRUE
    }
  }
  return(list(Pi=curr_Pi, Gamma=curr_Gamma, Amat=curr_Amat, accept=accept))
}

calculate_gradient = function(Eta, Amat, Amean, Aprec) {
  num_fac = ncol(Eta)
  N = nrow(Eta) - 1
  Pi = diag(num_fac) + Amat %*% t(Amat)
  AxI = Amat %x% diag(num_fac)
  root_Pi = sqrtm(Pi)
  if(num_fac>1) IpI = diag(num_fac^2) + commutation.matrix(num_fac)
  else IpI = diag(2, 1)
  IpI_AxI = IpI %*% AxI
  IpI_IxAt = IpI %*% (diag(num_fac) %x% t(Amat))
  kronSum = root_Pi %x% diag(num_fac) + diag(num_fac) %x% root_Pi
  tmp = t(AxI) %*% solve(kronSum, IpI_AxI) + diag(num_fac) %x% root_Pi
  tmp1 = matrix(1, 1, num_fac) %x% Eta
  tmp2 = Eta %x% matrix(1, 1, num_fac)
  grad = -matrix(crossprod(as.numeric(Amat)-Amean, Aprec), 1, num_fac^2) + N / 2 * matrix(as.numeric(solve(Pi)), 1, num_fac^2) %*% IpI_AxI -
    colSums((tmp2[-1,] * tmp1[-1,]) %*% IpI_AxI + (tmp2[-(N+1),] * tmp1[-(N+1),]) %*% IpI_IxAt - 
              2 * (tmp2[-(N+1),] * tmp1[-1,]) %*% tmp) / 2
  return(grad[1,])
}

Amat_update_MALA = function(Amean, Aprec, Eta, curr_Pi, curr_Gamma, curr_Amat, prop_Var, prior_temp=1) {
  # Compute summary statistics
  num_fac = ncol(Eta)
  N = nrow(Eta) - 1
  # Generate proposal
  curr_grad = calculate_gradient(Eta, curr_Amat, Amean, Aprec)
  prop_Amat = matrix(rMVNorm(1, as.numeric(curr_Amat) + 0.5 * prop_Var %*% curr_grad, Sigma = prop_Var), num_fac, num_fac)
  prop_Gamma = lemma2_2a(prop_Amat)
  prop_Pi = diag(num_fac) - prop_Gamma %*% t(prop_Gamma)
  prop_grad = calculate_gradient(Eta, prop_Amat, Amean, Aprec)
  # Compute log proposal densities
  curr_tmp =  as.numeric(curr_Amat) - as.numeric(prop_Amat) - 0.5 * prop_Var %*% prop_grad
  curr_lprop = -sum(curr_tmp * solve(prop_Var, curr_tmp)) / 2
  prop_tmp =  as.numeric(prop_Amat) - as.numeric(curr_Amat) - 0.5 * prop_Var %*% curr_grad
  prop_lprop = -sum(prop_tmp * solve(prop_Var, prop_tmp)) / 2
  # Compute log-likelihoods
  curr_tmp = Eta[-1,] - Eta[-(N+1),] %*% t(curr_Gamma)
  prop_tmp = Eta[-1,] - Eta[-(N+1),] %*% t(prop_Gamma)
  curr_ll = -N / 2 * determinant(curr_Pi, logarithm = TRUE)$modulus - 
    tracem(solve(curr_Pi, t(curr_tmp) %*% curr_tmp)) / 2
  prop_ll = -N / 2 * determinant(prop_Pi, logarithm = TRUE)$modulus - 
    tracem(solve(prop_Pi, t(prop_tmp) %*% prop_tmp)) / 2
  # Compute log prior densities
  curr_lprior = -crossprod(as.numeric(curr_Amat) - Amean, Aprec) %*% (as.numeric(curr_Amat) - Amean) / 2
  prop_lprior = -crossprod(as.numeric(prop_Amat) - Amean, Aprec) %*% (as.numeric(prop_Amat) - Amean) / 2
  # Accept / reject
  logA = as.numeric(prior_temp * (prop_ll + prop_lprior - curr_ll - curr_lprior) + curr_lprop - prop_lprop)
  if(log(runif(1))<logA) {
    curr_Pi = prop_Pi
    curr_Gamma = prop_Gamma
    curr_Amat = prop_Amat
    accept = TRUE
  } else accept=FALSE
  return(list(Pi=curr_Pi, Gamma=curr_Gamma, Amat=curr_Amat, accept=accept))
}

Amat_block_update_MALA = function(Amean, Aprec_vec, Eta, curr_Pi, curr_Gamma, curr_Amat, prop_Var, 
                                  block_membership, prior_temp=1) {
  # Compute summary statistics
  num_fac = ncol(Eta)
  N = nrow(Eta) - 1
  num_blocks = max(unique(block_membership))
  curr_tmp = Eta[-1,] - Eta[-(N+1),] %*% t(curr_Gamma)
  curr_ll = -N / 2 * determinant(curr_Pi, logarithm = TRUE)$modulus - 
    tracem(solve(curr_Pi, t(curr_tmp) %*% curr_tmp)) / 2
  accept =rep(FALSE, num_blocks)
  for(block in 1:num_blocks) {
    # Generate proposal
    curr_grad = calculate_gradient(Eta, curr_Amat, Amean, diag(Aprec_vec, num_fac^2))
    block_curr_grad = curr_grad[block_membership==block]
    prop_Amat = as.numeric(curr_Amat)
    block_curr_Amat = prop_Amat[block_membership==block]
    block_prop_Var = prop_Var[block_membership==block, block_membership==block, drop=FALSE]
    block_prop_Amat = rMVNorm(1, block_curr_Amat + 0.5 * block_prop_Var %*% block_curr_grad, Sigma=block_prop_Var)
    prop_Amat[block_membership==block] = block_prop_Amat
    prop_Amat = matrix(prop_Amat, num_fac, num_fac)
    prop_Gamma = lemma2_2a(prop_Amat)
    prop_Pi = diag(num_fac) - prop_Gamma %*% t(prop_Gamma)
    prop_grad = calculate_gradient(Eta, prop_Amat, Amean, diag(Aprec_vec, num_fac^2))
    block_prop_grad = prop_grad[block_membership==block]
    # Compute log proposal densities
    curr_tmp =  block_curr_Amat - block_prop_Amat - 0.5 * block_prop_Var %*% block_prop_grad
    curr_lprop = -sum(curr_tmp * solve(block_prop_Var, curr_tmp)) / 2
    prop_tmp =  block_prop_Amat - block_curr_Amat - 0.5 * block_prop_Var %*% block_curr_grad
    prop_lprop = -sum(prop_tmp * solve(block_prop_Var, prop_tmp)) / 2
    # Compute log-likelihoods
    prop_tmp = Eta[-1,] - Eta[-(N+1),] %*% t(prop_Gamma)
    prop_ll = -N / 2 * determinant(prop_Pi, logarithm = TRUE)$modulus - 
      tracem(solve(prop_Pi, t(prop_tmp) %*% prop_tmp)) / 2
    # Compute log prior densities
    block_Amean = Amean[block_membership==block]
    block_Asd = 1/sqrt(Aprec_vec[block_membership==block])
    curr_lprior = sum(dnorm(block_curr_Amat, block_Amean, block_Asd, log=TRUE))
    prop_lprior = sum(dnorm(block_prop_Amat, block_Amean, block_Asd, log=TRUE))
    # Accept / reject
    logA = as.numeric(prior_temp * (prop_ll + prop_lprior - curr_ll - curr_lprior) + curr_lprop - prop_lprop)
    if(log(runif(1))<logA) {
      curr_Pi = prop_Pi
      curr_Gamma = prop_Gamma
      curr_Amat = prop_Amat
      curr_ll = prop_ll
      accept[block] = TRUE
    }
  }
  return(list(Pi=curr_Pi, Gamma=curr_Gamma, Amat=curr_Amat, accept=accept))
}

sample_prior_A = function(num_fac, s_Amat) {
  Amat = matrix(rnorm(num_fac^2, 0, s_Amat), num_fac, num_fac)
  Gamma = lemma2_2a(Amat)
  Pi = diag(num_fac) - Gamma %*% t(Gamma)
  return(list(Amat=Amat, Gamma=Gamma, Pi=Pi))
}

############################################################################################
# Conversion between partial autocorrelations P and transformed partial autocorrelations A #
############################################################################################

mysqrtm = function(x) {
  if(is.matrix(x)) sqrtm(x)
  else if(is.numeric(x) && length(x)==1) sqrt(x)
  else stop("x must be matrix or scalar.")
}

lemma2_2a = function(A) {
  A = as.matrix(A)
  m = nrow(A)
  if(ncol(A)!=m) stop("A must be square.")
  B = mysqrtm(diag(m) + A %*% t(A))
  P = solve(B, A)
  return(as.matrix(P))
}

lemma2_2b = function(P) {
  P = as.matrix(P)
  m = nrow(P)
  if(ncol(P)!=m) stop("P must be square.")
  Binv = mysqrtm(diag(m) - P %*% t(P))
  A = solve(Binv, P)
  return(as.matrix(A))
}