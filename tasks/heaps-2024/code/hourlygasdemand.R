source("commonfunctions.R")

#################
# Main function #
#################

my_gibbs = function(iters, burnin, thin, Y, W, 
                    a_sigma, b_sigma, 
                    m_mu_beta, s_mu_beta, a_tau_beta, b_tau_beta, 
                    Lambdavar_funcs, indices_missing=matrix(nrow=0, ncol=2), 
                    tune_Amat, MALA=TRUE, num_blocks=1,
                    a1, a2, threshold,
                    alpha0=NA, alpha1=NA, ibar=Inf,
                    seed=NA, fix_H=NA, init=NULL) {
  
  # - iters is the number of iterations required after burn-in
  # - burnin is the number of burn-in iterations (after which output is stored)
  # - thin is the thinning rate for the post burn-in samples
  # - Y and W are self-explanatory
  # - a_sigma and b_sigma are the hyperparameters in the prior for Sigma_diag
  # - m_mu_beta, s_mu_beta, a_tau_beta and b_tau_beta are the hyperparameters in the hierarchical prior
  #   for B
  # - Lambdavar_funcs is created by a call to circAR1pos_function_factory (below) and accounts for the matrix 
  #   normal prior for Lambda | Psi; circAR1pos_function_factory could be changed to create a matrix-t 
  #   prior with minimal changes to my_gibbs
  # - indices_missing is a matrix indicating the positions in Y of any missing data, with column 1 indicating
  #   the row and column 2 indicating the column; if there are no missing data it should be left equal to its
  #   default value of matrix(nrow=0, ncol=2)
  # - tune_Amat is the Hmax x Hmax tuning covariance matrix for the Atilde update
  # - MALA is a boolean; if true, Atilde is updated using MALA, otherwise it is updated using a Gaussian random
  #   walk
  # - num_blocks is the number of blocks to divide Atilde into for the purposes of updating
  # - a1 and a2 are the hyperparameters in the multivariate gamma process prior
  # - threshold is the proportion T in the truncation criterion of Schiavon and Canale (2020)
  # - alpha0 and alpha1 are the parameters in the diminishing adaptation condition if adaptation is used;
  #   otherwise leave at default of NA
  # - ibar is the number of iterations after which adaptation can start if adaptation is used; otherwise
  #   leave at default of Inf
  # - seed is the seed for random number generation
  # - fix_H should be left at its default value of NA if adaptation is required; otherwise set equal to the 
  #   fixed truncation point
  # - init is a list of initial values for the model parameters, with components mu_beta, tau_beta, 
  #   vec_B (i.e. vec(B)), Sigma_diag, vec_Amat (i.e. vec(Amat)), Lambdavar, varrho, 
  #   Psi_diag, vec_Lambda (i.e. vec(Lambda)); if left equal to its default value of NULL, parameters are
  #   initialised through a sample from the prior
  
  # Set seed
  if(is.na(seed)) seed = sample(.Machine$integer.max, 1)
  set.seed(seed)
  # Record hyperparameters in prior (and threshold)
  hyperparameters = list(a_sigma=a_sigma, b_sigma=b_sigma, m_mu_beta=m_mu_beta, s_mu_beta=s_mu_beta, 
                         a_tau_beta=a_tau_beta, a_tau_beta=a_tau_beta, a1=a1, a2=a2, threshold=threshold)
  # Compute summary statistics
  n = nrow(Y)
  p = ncol(Y)
  q = ncol(W)
  num_missing = nrow(indices_missing)
  adapt = is.finite(ibar)
  ind = as.matrix(expand.grid(1:n, 1:p))
  # Initialise
  if(adapt) {
    Hmax = ceiling((2*p+1-sqrt(8*p+1))/2) - 1
    if(is.null(init)) H = min(Hmax, ceiling(5 * log(p)))
    else H = init$H
  } else {
    if(is.na(fix_H)) stop("Need to provide upper limit for H if not adapting.")
    if(fix_H>(ceiling((2*p+1-sqrt(8*p+1))/2) - 1)) stop("Truncation point can be at most phi(p).")
    H = Hmax = fix_H
  }
  num_lambda = p * min(c(Hmax, p)) - min(c(Hmax, p)) * (min(c(Hmax, p))-1) / 2
  # Set up blocks if needed
  if(num_blocks<1) stop("num_blocks must be at least 1.")
  if(num_blocks>1) {
    block_size = ceiling(Hmax^2 / num_blocks)
    block_membership = rep(num_blocks, Hmax^2)
    block_membership[1:(block_size*(num_blocks-1))] = rep(1:(num_blocks-1), each=block_size)
    num_blocks = max(unique(block_membership)) # might be num_blocks - 1
  }
  # Initialise from prior or at init
  if(is.null(init)) {
    mu_beta = numeric(q); tau_beta = numeric(q)
    BT = matrix(NA, q, p)
    for(i in 1:q) {
      mu_beta[i] = sample_prior_mu_beta(m_mu_beta[i], s_mu_beta[i])
      tau_beta[i] = sample_prior_tau_beta(a_tau_beta[i], b_tau_beta[i])
      BT[i,] = sample_prior_mu_or_beta(rep(mu_beta[i], p), diag(tau_beta[i], p))
    }
    Sigma_diag = sample_prior_Sigma_diag(p, a_sigma, b_sigma)
    Amat = matrix(rMVNorm(1, rep(0, H^2), Q=diag(1, H^2)), H, H)
    # Sample Phi
    Lambdavar = Lambdavar_funcs$initialise()
    # Sample (Psi, Lambda) | Phi
    tmp = prior_Lambda(a1, a2, Lambdavar$P_Lambda, H=H, nu=Lambdavar_funcs$get_varsigma())
    varrho = tmp$delta; Psi_diag = tmp$theta; Lambda = tmp$Lambda
  } else {
    mu_beta = init$mu_beta
    tau_beta = init$tau_beta
    BT = t(matrix(init$vec_B, p, q))
    Sigma_diag = init$Sigma_diag
    Amat = matrix(init$vec_Amat, H, H)
    Lambdavar = Lambdavar_funcs$initialise(x=init$Lambdavar)
    varrho = init$varrho; Psi_diag = init$Psi_diag
    Lambda = matrix(init$vec_Lambda, p, H)
  }
  mu_mat = W %*% BT
  Gamma = lemma2_2a(Amat)
  Pi = diag(H) - Gamma %*% t(Gamma)
  # Create storage
  B_mcmc = matrix(NA, trunc(iters / thin), p*q)
  colnames(B_mcmc) = paste("B[", rep(1:p, times=q), ",", rep(1:q, each=p), "]", sep="")
  mu_beta_mcmc = matrix(NA, trunc(iters / thin), q)
  colnames(mu_beta_mcmc) = paste("mu_beta[", 1:q, "]", sep="")
  tau_beta_mcmc = matrix(NA, trunc(iters / thin), q)
  colnames(tau_beta_mcmc) = paste("tau_beta[", 1:q, "]", sep="")
  Sigma_diag_mcmc = matrix(NA, trunc(iters / thin), p)
  colnames(Sigma_diag_mcmc) = paste("Sigma_diag[", 1:p, "]", sep="")
  Lambdatilde_diag_mcmc = matrix(0, trunc(iters / thin), min(c(Hmax, p)))
  colnames(Lambdatilde_diag_mcmc) = paste("Lambdatilde_diag[", 1:min(c(Hmax, p)), "]", sep="")
  Lambdatilde_offdiag_mcmc = matrix(0, trunc(iters / thin), num_lambda-min(c(Hmax, p)))
  colnames(Lambdatilde_offdiag_mcmc) = paste("Lambdatilde_offdiag[", 1:(num_lambda-min(c(Hmax, p))), "]", sep="")
  Lambda_mcmc = matrix(0, trunc(iters / thin), p*Hmax)
  colnames(Lambda_mcmc) = paste("Lambda[", rep(1:p, Hmax), ",", rep(1:Hmax, each=p), "]", sep="")
  Amattilde_mcmc = matrix(NA, trunc(iters / thin), Hmax^2)
  colnames(Amattilde_mcmc) = paste("Amattilde[", rep(1:Hmax, Hmax), ",", rep(1:Hmax, each=Hmax), "]", sep="")
  count_Amat = rep(0, num_blocks)
  count_Amat_denom = rep(0, num_blocks)
  Gammatilde_mcmc = matrix(NA, trunc(iters / thin), Hmax^2)
  colnames(Gammatilde_mcmc) = paste("Gammatilde[", rep(1:Hmax, Hmax), ",", rep(1:Hmax, each=Hmax), "]", sep="")
  Etatilde_mcmc = matrix(0, trunc(iters / thin), (n+1) * Hmax)
  colnames(Etatilde_mcmc) = paste("Etatilde[", rep(1:(n+1), Hmax), ",", rep(1:Hmax, each=(n+1)), "]", sep="")
  tmp = Lambdavar_funcs$names()
  Lambdavar_mcmc = matrix(NA, trunc(iters / thin), length(tmp))
  colnames(Lambdavar_mcmc) = tmp
  count_Lambdavar = rep(0, length(tmp))
  Hstar_mcmc = numeric(trunc(iters / thin))
  varrho_mcmc = matrix(NA, trunc(iters / thin), Hmax)
  colnames(varrho_mcmc) = paste("varrho[", 1:Hmax, "]", sep="")
  Psi_diag_mcmc = matrix(0, trunc(iters / thin), Hmax)
  colnames(Psi_diag_mcmc) = paste("Psi_diag[", 1:Hmax, "]", sep="")
  if(num_missing>0) {
    Y_mcmc = matrix(NA, trunc(iters / thin), num_missing)
    colnames(Y_mcmc) = paste("Y[", indices_missing[,1], ",", indices_missing[,2], "]", sep="")
  }
  if(adapt) H_mcmc = numeric(trunc(iters / thin))
  # Main loop
  for(i in 1:(iters+burnin)) {
    if(i%%1000==0) cat(paste(i, "", sep=" "))
    # Eta
    Eta = sample_fcd_dynEta(Y - mu_mat, Lambda, Sigma_diag, Gamma, Pi)
    # Missing data
    if(num_missing>0) Y[indices_missing] = sample_fcd_missing_data(mu_mat, Eta, Lambda, Sigma_diag, indices_missing)
    # Lambda
    Lambda = sample_fcd_Lambda_MGP(Y - mu_mat, Eta, Sigma_diag, Lambdavar$P_Lambda, Psi_diag)
    # Gamma
    # (Note: prior for Atilde same as prior for A)
    if(num_blocks==1) {
      if(!MALA) tmp = Amat_update(rep(0, H^2), diag(1, H^2), Eta, Pi, Gamma, Amat, tune_Amat[1:H^2, 1:H^2, drop=FALSE])
      else tmp = Amat_update_MALA(rep(0, H^2), diag(1, H^2), Eta, Pi, Gamma, Amat, tune_Amat[1:H^2, 1:H^2, drop=FALSE])
    } else {
      if(!MALA) tmp = Amat_block_update(rep(0, H^2), rep(1, H^2), Eta, Pi, Gamma, Amat, tune_Amat[1:H^2, 1:H^2, drop=FALSE], block_membership[1:(H^2)])
      else tmp = Amat_block_update_MALA(rep(0, H^2), rep(1, H^2), Eta, Pi, Gamma, Amat, tune_Amat[1:H^2, 1:H^2, drop=FALSE], block_membership[1:(H^2)])
    }
    Amat = tmp$Amat; Pi = tmp$Pi; Gamma = tmp$Gamma
    count_Amat[1:length(tmp$accept)] = count_Amat[1:length(tmp$accept)] + tmp$accept
    count_Amat_denom[1:length(tmp$accept)] = count_Amat_denom[1:length(tmp$accept)] + rep(1, length(tmp$accept))
    # Sigma_diag
    Sigma_diag = sample_fcd_Sigma_diag(a_sigma, b_sigma, Y - mu_mat, Eta, Lambda)
    # B^T
    BT = sample_fcd_beta(Y, W, Eta, Lambda, Sigma_diag, rep(mu_beta, each=p), rep(tau_beta, each=p))
    mu_mat = W %*% BT
    # Hyperparameters
    for(k in 1:q) mu_beta[k] = sample_fcd_mu_beta(m_mu_beta[k], s_mu_beta[k], BT[k,], tau_beta[k])
    for(k in 1:q) tau_beta[k] = sample_fcd_tau_beta(a_tau_beta[k], b_tau_beta[k], BT[k,], mu_beta[k])
    tmp = Lambdavar_funcs$update(Lambda, Lambdavar, Psi_diag=Psi_diag)
    Lambdavar = tmp$Lambdavar
    count_Lambdavar = count_Lambdavar + tmp$accept
    tmp = fcd_theta(a1, a2, varrho, Lambda, Lambdavar$P_Lambda)
    Psi_diag = tmp$theta; varrho = tmp$delta
    # Check for redundant columns
    centeredY = Y - mu_mat
    denom = sum(centeredY^2)
    es = centeredY - tcrossprod(Eta[-1,], Lambda)
    sh = numeric(H)
    for(h in 1:H) sh[h] = sum(Eta[ind[,1]+1, h] * Lambda[ind[,2], h] * (centeredY[ind] + es[ind]))
    active = rep(TRUE, H)
    for(h in order(sh)) {
      copy = active
      copy[h] = FALSE
      if((sum(es^2)+sum(sh[copy]))/denom > threshold) active = copy
      else break
    }
    Hstar = sum(active)
    if(i>=ibar) {
      # Adapt
      if(runif(1)<exp(alpha0 + alpha1 * i)) {
        if(Hstar==H & Hstar<Hmax) {
          # Add a column
          Hstar = Hstar + 1
          tmp = prior_Lambda(a1, a2, Lambdavar$P_Lambda, H=1, delta=varrho, nu=Lambdavar_funcs$get_varsigma())
          varrho = c(varrho, tmp$delta)
          Psi_diag = c(Psi_diag, tmp$theta)
          Lambda = cbind(Lambda, tmp$Lambda)
          Gamma = rbind(cbind(Gamma, rep(0, Hstar-1)), c(rep(0, Hstar-1), runif(1, 0, min(svd(Gamma)$d))))
          Pi = diag(Hstar) - Gamma %*% t(Gamma)
          Amat = lemma2_2b(Gamma)
          tmp = numeric(n+1)
          tmp[1] = rnorm(1)
          for(t in 1:n) tmp[t+1] = rnorm(1, Gamma[Hstar, Hstar] * tmp[t], sqrt(Pi[Hstar, Hstar]))
          Eta = cbind(Eta, tmp)
        } else if(Hstar<H) {
          # Delete inactive column(s)
          if(Hstar==0) {
            Hstar = 1
            active[1] = TRUE # retain one column
          }
          Psi_diag = Psi_diag[active]
          varrho = 1 / exp(c(log(Psi_diag[1]), diff(log(Psi_diag))))
          Lambda = Lambda[,active, drop=FALSE]
          Gamma = Gamma[active, active, drop=FALSE]
          Pi = diag(Hstar) - Gamma %*% t(Gamma)
          Amat = lemma2_2b(Gamma)
          Eta = Eta[,active, drop=FALSE]
        }
        H = Hstar
      }
    }
    # Store
    if(i>burnin && (i-burnin)%%thin==0) {
      B_mcmc[(i-burnin)/thin,] = as.numeric(t(BT))
      mu_beta_mcmc[(i-burnin)/thin,] = mu_beta
      tau_beta_mcmc[(i-burnin)/thin,] = tau_beta
      Sigma_diag_mcmc[(i-burnin)/thin,] = Sigma_diag
      Lambda_mcmc[(i-burnin)/thin, 1:(H*p)] = as.numeric(Lambda)
      Lambda_decomp = my_LQ_decomp(Lambda, posDiag=TRUE)
      Lambdatilde_diag_mcmc[(i-burnin)/thin, 1:min(c(H,p))] = diag(Lambda_decomp$L)
      tmp = Lambda_decomp$L[lower.tri(Lambda_decomp$L)]
      Lambdatilde_offdiag_mcmc[(i-burnin)/thin, 1:length(tmp)] = tmp
      Amattilde = Lambda_decomp$Q %*% Amat %*% t(Lambda_decomp$Q)
      Gammatilde = Lambda_decomp$Q %*% Gamma %*% t(Lambda_decomp$Q)
      Etatilde = tcrossprod(Eta, Lambda_decomp$Q)
      if(adapt) {
        tmp = matrix(0, Hmax, Hmax)
        tmp[1:H, 1:H] = Amattilde
        Amattilde_mcmc[(i-burnin)/thin,] = as.numeric(tmp)
        tmp[1:H, 1:H] = Gammatilde
        Gammatilde_mcmc[(i-burnin)/thin,] = as.numeric(tmp)
      } else {
        Amattilde_mcmc[(i-burnin)/thin,] = as.numeric(Amattilde)
        Gammatilde_mcmc[(i-burnin)/thin,] = as.numeric(Gammatilde)
      }
      Etatilde_mcmc[(i-burnin)/thin, 1:(H*(n+1))] = as.numeric(Etatilde)
      Lambdavar_mcmc[(i-burnin)/thin,] = Lambdavar_funcs$print(Lambdavar)
      varrho_mcmc[(i-burnin)/thin, 1:H] = varrho
      Psi_diag_mcmc[(i-burnin)/thin, 1:H] = Psi_diag
      Hstar_mcmc[(i-burnin)/thin] = Hstar
      if(adapt) H_mcmc[(i-burnin)/thin] = H
      if(num_missing>0) Y_mcmc[(i-burnin)/thin,] = Y[indices_missing]
    }
  }
  cat("\n")
  # Return object
  mcmc = cbind(B_mcmc, Sigma_diag_mcmc, Lambdatilde_diag_mcmc, Lambdatilde_offdiag_mcmc,
               Amattilde_mcmc, Gammatilde_mcmc, Lambdavar_mcmc, mu_beta_mcmc, tau_beta_mcmc,
               varrho_mcmc, Psi_diag_mcmc, Hstar=Hstar_mcmc)
  if(adapt) mcmc = cbind(mcmc, H=H_mcmc)
  if(num_missing==0) Y_mcmc = NULL
  return(list(mcmc=mcmc, fac_mcmc=Etatilde_mcmc,  missing_data=Y_mcmc, Lambda_mcmc=Lambda_mcmc,
              acc_Lambdavar=count_Lambdavar/(iters+burnin), acc_Amat=count_Amat/count_Amat_denom, 
              hyperparameters=hyperparameters, seed=seed))
  
}

##############################
# Prior for Lambda given Psi #
##############################

circAR1pos_function_factory = function(prior_Lambdavar, tune_Lambdavar, nrows_Lambda) {
  # - prior_Lambdavar is a list with 2 components: m_logitvartheta, s_logitvartheta
  # - tune_Lambdavar is a list with 1 component: sd
  # NOTE: in a Lambdavar object for this matrix normal prior, P_Lambda is Phi^{-1}, i.e. Xi
  update = function(Lambda, curr_Lambdavar, prior_temp=1, Psi_diag=NULL) {
    num_fac = ncol(Lambda)
    if(!is.null(Psi_diag)) Lambda = sweep(Lambda, 2, sqrt(Psi_diag), "/")
    prop_Lambdavar = curr_Lambdavar
    prop_Lambdavar$logitvartheta = rnorm(1, curr_Lambdavar$logitvartheta, tune_Lambdavar$sd)
    prop_Lambdavar$vartheta = expit(prop_Lambdavar$logitvartheta)
    prop_Lambdavar$P_Lambda = compute_P_Lambda(prop_Lambdavar)
    logA = dnorm(prop_Lambdavar$logitvartheta, prior_Lambdavar$m_logitvartheta, prior_Lambdavar$s_logitvartheta, log=TRUE) -
      dnorm(curr_Lambdavar$logitvartheta, prior_Lambdavar$m_logitvartheta, prior_Lambdavar$s_logitvartheta, log=TRUE) + 
      num_fac * (determinant(prop_Lambdavar$P_Lambda, logarithm = TRUE)$modulus - 
                   determinant(curr_Lambdavar$P_Lambda, logarithm = TRUE)$modulus) / 2
    for(i in 1:num_fac) {
      logA = logA - (sum(Lambda[,i] * (prop_Lambdavar$P_Lambda %*% Lambda[,i])) - 
                       sum(Lambda[,i] * (curr_Lambdavar$P_Lambda %*% Lambda[,i]))) / 2
    }
    logA = logA * prior_temp
    if(log(runif(1))<logA) {
      curr_Lambdavar = prop_Lambdavar
      accept = c(TRUE, TRUE)
    } else accept = c(TRUE, FALSE)
    return(list(Lambdavar=curr_Lambdavar, accept=accept))
  }
  compute_P_Lambda = function(Lambdavar) {
    P_Lambda = triDiagCorners(2, -Lambdavar$vartheta, nrows_Lambda) / (2 * Lambdavar$vartheta0)
    return(P_Lambda)
  }
  get_varsigma = function() {
    return(NA)
  }
  initialise = function(x=NULL) {
    Lambdavar = list()
    Lambdavar$vartheta0 = 1
    if(is.null(x)) {
      Lambdavar$logitvartheta = rnorm(1, prior_Lambdavar$m_logitvartheta, prior_Lambdavar$s_logitvartheta)
      Lambdavar$vartheta = expit(Lambdavar$logitvartheta)
    } else {
      Lambdavar$vartheta = x[2]
      Lambdavar$logitvartheta = logit(x[2])
    }
    Lambdavar$P_Lambda = compute_P_Lambda(Lambdavar)
    return(Lambdavar)
  }
  names = function() {
    ret = c("vartheta0", "vartheta")
    return(ret)
  }
  print = function(Lambdavar) {
    ret = c(Lambdavar$vartheta0, Lambdavar$vartheta)
    return(ret)
  }
  triDiag = function(diag, offdiag, n) {
    value = diag(diag, n)
    R = row(value)
    C = col(value)
    value[C == R + 1] = offdiag
    value[C == R - 1] = offdiag
    return(value)
  }
  triDiagCorners = function(diag, offdiag, n) {
    value = triDiag(diag, offdiag, n)
    if(n > 1) {
      value[1, n] = value[n, 1] = offdiag
    }
    return(value)
  }
  expit = function(x) {
    return(1 / (1 + exp(-x)))
  }
  logit = function(p) {
    return(log(p) - log(1 - p))
  }
  return(list(update=update, initialise=initialise, names=names, print=print,
              get_varsigma=get_varsigma))
}
