source("commonfunctions.R")
library(truncnorm)
library(CholWishart)

#################
# Main function #
#################

my_gibbs = function(iters, burnin, thin, Y, X, W,
                    s_beta, s_kappa, Lambdavar_funcs,
                    a1, a2, threshold,
                    alpha0=NA, alpha1=NA, ibar=Inf,
                    seed=NA, fix_H=NA) {
  
  # - iters is the number of iterations required after burn-in
  # - burnin is the number of burn-in iterations (after which output is stored)
  # - thin is the thinning rate for the post burn-in samples
  # - Y, X and W are self-explanatory
  # - s_beta and s_kappa are the standard deviations in the priors for B and Kappa
  # - Lambdavar_funcs is created by a call to projExpCov_function_factory (below) and accounts for the matrix-t
  #   prior for Lambda | Psi; projExpCov_function_factory could be changed to create a matrix normal 
  #   prior with minimal changes to my_gibbs
  # - a1 and a2 are the hyperparameters in the multivariate gamma process prior
  # - threshold is the proportion T in the truncation criterion of Schiavon and Canale (2020)
  # - alpha0 and alpha1 are the parameters in the diminishing adaptation condition if adaptation is used;
  #   otherwise leave at default of NA
  # - ibar is the number of iterations after which adaptation can start if adaptation is used; otherwise
  #   leave at default of Inf
  # - seed is the seed for random number generation
  # - fix_H should be left at its default value of NA if adaptation is required; otherwise set equal to the 
  #   fixed truncation point
  
  # Set seed
  if(is.na(seed)) seed = sample(.Machine$integer.max, 1)
  set.seed(seed)
  # Record hyperparameters in prior (and threshold)
  hyperparameters = list(s_beta=s_beta, s_kappa=s_kappa, a1=a1, a2=a2, threshold=threshold)
  # Compute summary statistics
  n = nrow(Y)
  p = ncol(Y)
  c = ncol(W)
  q = ncol(X)
  adapt = is.finite(ibar)
  ind = as.matrix(expand.grid(1:n, 1:p))
  lower = ifelse(Y==1, 0, -Inf)
  upper = ifelse(Y==1, Inf, 0)
  Sigma_diag = rep(1, p)
  # Initialise
  if(adapt) {
    Hmax = ceiling((2*p+1-sqrt(8*p+1))/2) - 1
    H = min(Hmax, ceiling(5 * log(p)))
  } else {
    if(is.na(fix_H)) stop("Need to provide upper limit for H if not adapting.")
    if(fix_H>(ceiling((2*p+1-sqrt(8*p+1))/2) - 1)) stop("Truncation point can be at most phi(p).")
    H = Hmax = fix_H
  }
  num_lambda = p * min(c(Hmax, p)) - min(c(Hmax, p)) * (min(c(Hmax, p))-1) / 2
  # Initialise from prior
  KappaT = sample_prior_KappaT_mvp(c, q, 0, s_kappa)
  BT = sample_prior_BT_mvp(KappaT, X, s_beta)
  mu_mat = tcrossprod(W, BT)
  Eta = sample_prior_Eta(n, H)
  # Sample (Phi, varsigma)
  Lambdavar = Lambdavar_funcs$initialise()
  # Sample (Psi, Lambda) | Phi, varsigma
  tmp = prior_Lambda(a1, a2, Lambdavar$S_Lambda, H=H, nu=Lambdavar_funcs$get_varsigma(Lambdavar))
  varrho = tmp$delta; Psi_diag = tmp$theta; Lambda = tmp$Lambda
  # Sample S | Lambda, Phi, Psi, varsigma
  Lambdavar = Lambdavar_funcs$sample_S(Lambdavar, Psi_diag, Lambda)
  # Create storage
  B_mcmc = matrix(NA, trunc(iters / thin), p*c)
  colnames(B_mcmc) = paste("B[", rep(1:c, times=p), ",", rep(1:p, each=c), "]", sep="")
  Kappa_mcmc = matrix(NA, trunc(iters / thin), q*c)
  colnames(Kappa_mcmc) = paste("Kappa[", rep(1:q, times=c), ",", rep(1:c, each=q), "]", sep="")
  Lambdatilde_diag_mcmc = matrix(0, trunc(iters / thin), min(c(Hmax, p)))
  colnames(Lambdatilde_diag_mcmc) = paste("Lambdatilde_diag[", 1:min(c(Hmax, p)), "]", sep="")
  Lambdatilde_offdiag_mcmc = matrix(0, trunc(iters / thin), num_lambda-min(c(Hmax, p)))
  colnames(Lambdatilde_offdiag_mcmc) = paste("Lambdatilde_offdiag[", 1:(num_lambda-min(c(Hmax, p))), "]", sep="")
  Lambda_mcmc = matrix(0, trunc(iters / thin), p*Hmax)
  colnames(Lambda_mcmc) = paste("Lambda[", rep(1:p, Hmax), ",", rep(1:Hmax, each=p), "]", sep="")
  Etatilde_mcmc = matrix(0, trunc(iters / thin), (n+1) * Hmax)
  colnames(Etatilde_mcmc) = paste("Etatilde[", rep(1:(n+1), Hmax), ",", rep(1:Hmax, each=(n+1)), "]", sep="")
  Z_mcmc = matrix(NA, trunc(iters / thin), n * p)
  colnames(Z_mcmc) = paste("Z[", rep(1:n, p), ",", rep(1:p, each=n), "]", sep="")
  tmp = Lambdavar_funcs$names()
  Lambdavar_mcmc = matrix(NA, trunc(iters / thin), length(tmp))
  colnames(Lambdavar_mcmc) = tmp
  count_Lambdavar = rep(0, length(tmp))
  Hstar_mcmc = numeric(trunc(iters / thin))
  varrho_mcmc = matrix(NA, trunc(iters / thin), Hmax)
  colnames(varrho_mcmc) = paste("varrho[", 1:Hmax, "]", sep="")
  Psi_diag_mcmc = matrix(0, trunc(iters / thin), Hmax)
  colnames(Psi_diag_mcmc) = paste("Psi_diag[", 1:Hmax, "]", sep="")
  if(adapt) H_mcmc = numeric(trunc(iters / thin))
  # Main loop
  for(i in 1:(iters+burnin)) {
    if(i%%1000==0) cat(paste(i, "", sep=" "))
    # Z
    Z = sample_FCD_Z_mvp(mu_mat, Eta, Lambda, lower, upper)
    # Eta
    Eta = tryCatch(
      sample_fcd_Eta(Z - mu_mat, Lambda, Sigma_diag),
      error=function(cond) {
        message(paste("Error caused by sample_fcd_Eta for iteration", i))
        message("Here's the original error message:")
        message(paste(cond, "\n"))
        return(Eta)
      }
    )
    # Lambda
    Lambda = tryCatch(
      sample_fcd_Lambda_MGP(Z - mu_mat, Eta, Sigma_diag, Lambdavar$P_Lambda, Psi_diag),
      error=function(cond) {
        message(paste("Error caused by sample_fcd_Lambda_MGP for iteration", i))
        message("Here's the original error message:")
        message(paste(cond, "\n"))
        return(Lambda)
      }
    )
    # B^T
    BT = tryCatch(
      sample_fcd_BT_mvp(Z, W, Eta, Lambda, X, KappaT, s_beta),
      error=function(cond) {
        message(paste("Error caused by sample_fcd_BT_mvp for iteration", i))
        message("Here's the original error message:")
        message(paste(cond, "\n"))
        return(BT)
      }
    )
    mu_mat = tcrossprod(W, BT)
    # Kappa^T
    KappaT = tryCatch(
      sample_fcd_KappaT_mvp(BT, X, s_beta, s_kappa),
      error=function(cond) {
        message(paste("Error caused by sample_fcd_KappaT_mvp for iteration", i))
        message("Here's the original error message:")
        message(paste(cond, "\n"))
        return(KappaT)
      }
    )
    # Hyperparameters
    tmp = Lambdavar_funcs$update(Lambda, Lambdavar, Psi_diag=Psi_diag)
    Lambdavar = tmp$Lambdavar
    count_Lambdavar = count_Lambdavar + tmp$accept
    tmp = fcd_theta(a1, a2, varrho, Lambda, Lambdavar$P_Lambda)
    Psi_diag = tmp$theta; varrho = tmp$delta
    # Check for redundant columns
    centeredZ = Z - mu_mat
    denom = sum(centeredZ^2)
    es = centeredZ - tcrossprod(Eta[-1,], Lambda)
    sh = numeric(H)
    for(h in 1:H) sh[h] = sum(Eta[ind[,1]+1, h] * Lambda[ind[,2], h] * (centeredZ[ind] + es[ind]))
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
          tmp = prior_Lambda(a1, a2, Lambdavar$P_Lambda, H=1, delta=varrho, nu=Lambdavar_funcs$get_varsigma(Lambdavar))
          varrho = c(varrho, tmp$delta)
          Psi_diag = c(Psi_diag, tmp$theta)
          Lambda = cbind(Lambda, tmp$Lambda)
          Eta = cbind(Eta, rnorm(n+1))
        } else if(Hstar<H) {
          # Delete inactive column(s)
          if(Hstar==0) {
            Hstar = 1
            active[1] = TRUE # retain one column
          }
          Psi_diag = Psi_diag[active]
          varrho = 1 / exp(c(log(Psi_diag[1]), diff(log(Psi_diag))))
          Lambda = Lambda[,active, drop=FALSE]
          Eta = Eta[,active, drop=FALSE]
        }
        H = Hstar
      }
    }
    # Store
    if(i>burnin && (i-burnin)%%thin==0) {
      B_mcmc[(i-burnin)/thin,] = as.numeric(t(BT))
      Kappa_mcmc[(i-burnin)/thin,] = as.numeric(t(KappaT))
      Lambda_mcmc[(i-burnin)/thin, 1:(H*p)] = as.numeric(Lambda)
      Lambda_decomp = my_LQ_decomp(Lambda, posDiag=TRUE)
      Lambdatilde_diag_mcmc[(i-burnin)/thin, 1:min(c(H,p))] = diag(Lambda_decomp$L)
      tmp = Lambda_decomp$L[lower.tri(Lambda_decomp$L)]
      Lambdatilde_offdiag_mcmc[(i-burnin)/thin, 1:length(tmp)] = tmp
      Etatilde = tcrossprod(Eta, Lambda_decomp$Q)
      Etatilde_mcmc[(i-burnin)/thin, 1:(H*(n+1))] = as.numeric(Etatilde)
      Z_mcmc[(i-burnin)/thin, 1:(n*p)] = as.numeric(Z)
      Lambdavar_mcmc[(i-burnin)/thin,] = Lambdavar_funcs$print(Lambdavar)
      varrho_mcmc[(i-burnin)/thin, 1:H] = varrho
      Psi_diag_mcmc[(i-burnin)/thin, 1:H] = Psi_diag
      Hstar_mcmc[(i-burnin)/thin] = Hstar
      if(adapt) H_mcmc[(i-burnin)/thin] = H
    }
  }
  cat("\n")
  # Return object
  mcmc = cbind(B_mcmc, Kappa_mcmc, Lambdatilde_diag_mcmc, Lambdatilde_offdiag_mcmc, 
               Lambdavar_mcmc, varrho_mcmc, Psi_diag_mcmc, Hstar=Hstar_mcmc)
  if(adapt) mcmc = cbind(mcmc, H=H_mcmc)
  return(list(mcmc=mcmc, fac_mcmc=Etatilde_mcmc, Z_mcmc=Z_mcmc, Lambda_mcmc=Lambda_mcmc,
              acc_Lambdavar=count_Lambdavar/(iters+burnin), hyperparameters=hyperparameters, seed=seed))
  
}

###################################
# Sample from prior distributions #
###################################

sample_prior_KappaT_mvp = function(c, q, m_kappa, s_kappa) {
  return(matrix(rnorm(q*c, m_kappa, s_kappa), c, q))
}

sample_prior_BT_mvp = function(KappaT, X, s_beta) {
  p = nrow(X)
  c = nrow(KappaT)
  BT = matrix(rnorm(c*p, t(tcrossprod(KappaT, X)), s_beta), p, c)
  return(BT)
}

##############################################
# Sample from full conditional distributions #
##############################################

sample_FCD_Z_mvp = function(mu_mat, Eta, Lambda, lower, upper, prior_temp=1) {
  n = nrow(Eta) - 1
  p = ncol(mu_mat)
  tmp = mu_mat + tcrossprod(Eta[-1,], Lambda)
  Z = matrix(NA, n, p)
  for(i in 1:ncol(mu_mat)) Z[,i] = rtruncnorm(n, lower[,i], upper[,i], tmp[,i], 1/sqrt(prior_temp))
  return(Z)
}

# This could be simplified by exploiting conditional independence structure in posterior for Kappa
sample_fcd_KappaT_mvp = function(BT, X, s_beta, s_kappa, prior_temp=1) {
  p = nrow(BT)
  c = ncol(BT)
  q = ncol(X)
  return(t(sample_fcd_beta(BT, X, matrix(0, p+1, 1), matrix(0, c, 1), rep(s_beta^2, c), rep(0, c*q), 
                           rep(1/s_kappa^2, c*q), prior_temp, prior_temp)))
}

# This could be simplified by exploiting conditional independence structure in posterior for B
sample_fcd_BT_mvp = function(Z, W, Eta, Lambda, X, KappaT, s_beta, prior_temp=1) {
  c = ncol(W)
  p = nrow(X)
  return(t(sample_fcd_beta(Z, W, Eta, Lambda, rep(1, p), as.numeric(tcrossprod(X, KappaT)), 
                           rep(1/s_beta^2, c*p), prior_temp, prior_temp)))
}

##############################
# Prior for Lambda given Psi #
##############################

projExpCov_function_factory = function(prior_Lambdavar, tune_Lambdavar, distance_mat) {
  # - prior_Lambdavar is a list with 4 components: m_logvartheta (C-vector), s_logvartheta (C-vector), 
  #   a_varsigmacheck, b_varsigmacheck
  # - tune_Lambdavar is a list with 2 components: sd (C-vector), sd_varsigmacheck
  # - distance_mat is an array of dimensions (C, p, p) in which distance_mat[i,,] for i=1,..,C-1
  #   is a Manhattan distance matrix for covariate i and distance_mat[C,,] is the ultrametric
  #   distance matrix of phylogenetic distances
  # NOTE: in a Lambdavar object for this matrix-t prior, P_Lambda refers to S and S_Lambda refers to 
  # breve{Phi}
  C = dim(distance_mat)[1]
  nrows_Lambda = dim(distance_mat)[2]
  update = function(Lambda, curr_Lambdavar, prior_temp=1, Psi_diag=NULL) {
    num_fac = ncol(Lambda)
    prop_Lambdavar = curr_Lambdavar
    prop_Lambdavar$logvartheta = rnorm(C, curr_Lambdavar$logvartheta, tune_Lambdavar$sd)
    prop_Lambdavar$vartheta = exp(prop_Lambdavar$logvartheta)
    prop_Lambdavar$S_Lambda = compute_S_Lambda(prop_Lambdavar)
    logA = sum(dnorm(prop_Lambdavar$logvartheta, prior_Lambdavar$m_logvartheta, prior_Lambdavar$s_logvartheta, log=TRUE)) -
      sum(dnorm(curr_Lambdavar$logvartheta, prior_Lambdavar$m_logvartheta, prior_Lambdavar$s_logvartheta, log=TRUE)) + 
      (curr_Lambdavar$varsigma + nrows_Lambda - 1) * (determinant(prop_Lambdavar$S_Lambda, logarithm = TRUE)$modulus - 
                                                        determinant(curr_Lambdavar$S_Lambda, logarithm = TRUE)$modulus) / 2 -
      (sum(diag(prop_Lambdavar$S_Lambda %*% prop_Lambdavar$P_Lambda)) - sum(diag(curr_Lambdavar$S_Lambda %*% curr_Lambdavar$P_Lambda))) / 2
    logA = logA * prior_temp
    if(log(runif(1))<logA) {
      curr_Lambdavar = prop_Lambdavar
      accept = c(TRUE, rep(TRUE, C))
    } else accept = c(TRUE, rep(FALSE, C))
    tmp = sample_varsigma_and_S(Lambda, curr_Lambdavar, Psi_diag, prior_temp)
    curr_Lambdavar = tmp$Lambdavar
    accept = c(accept, tmp$accept, rep(tmp$accept, nrows_Lambda*(nrows_Lambda+1)/2))
    return(list(Lambdavar=curr_Lambdavar, accept=accept))
  }
  sample_S = function(Lambdavar, Psi_diag, Tmat, prior_temp=1) {
    Psi_inv = diag(1 / Psi_diag, length(Psi_diag))
    Lambdavar$P_Lambda = stats::rWishart(1, prior_temp*(Lambdavar$varsigma+ncol(Psi_inv)-2)+nrows_Lambda+1, 
                                         solve(Lambdavar$S_Lambda + Tmat %*% Psi_inv %*% t(Tmat)) / prior_temp)[,,1]
    return(Lambdavar)
  }
  sample_varsigma_and_S = function(Lambda, curr_Lambdavar, Psi_diag, prior_temp=1) {
    num_fac = ncol(Lambda)
    Phi_tilde = curr_Lambdavar$S_Lambda / (curr_Lambdavar$varsigma - 2)
    Psi_inv = diag(1 / Psi_diag, length(Psi_diag))
    varsigmacheck_star = rlnorm(1, log(curr_Lambdavar$varsigmacheck), tune_Lambdavar$sd_varsigmacheck)
    varsigma_star = 1 / varsigmacheck_star + 4
    logA = dgamma(varsigmacheck_star, prior_Lambdavar$a_varsigmacheck, prior_Lambdavar$b_varsigmacheck, log=TRUE) -
      dgamma(curr_Lambdavar$varsigmacheck, prior_Lambdavar$a_varsigmacheck, prior_Lambdavar$b_varsigmacheck, log=TRUE)
    tmp = solve(Phi_tilde, Lambda) %*% sweep(t(Lambda), 1, Psi_diag, "/")
    logA = logA + lmvgamma((varsigma_star+num_fac+nrows_Lambda-1)/2, nrows_Lambda) - 
      lmvgamma((varsigma_star+nrows_Lambda-1)/2, nrows_Lambda) - (lmvgamma((curr_Lambdavar$varsigma+num_fac+nrows_Lambda-1)/2, nrows_Lambda) - 
                                                                    lmvgamma((curr_Lambdavar$varsigma+nrows_Lambda-1)/2, nrows_Lambda)) - 
      num_fac*nrows_Lambda*(log(varsigma_star-2)-log(curr_Lambdavar$varsigma-2))/2 -
      ((varsigma_star+num_fac+nrows_Lambda-1)*determinant(diag(1, nrows_Lambda) + tmp / (varsigma_star-2), logarithm=TRUE)$modulus/2 - 
         (curr_Lambdavar$varsigma+num_fac+nrows_Lambda-1)*determinant(diag(1, nrows_Lambda) + 
                                                                        tmp / (curr_Lambdavar$varsigma-2), logarithm=TRUE)$modulus/2)
    logA = prior_temp * logA + log(varsigmacheck_star) - log(curr_Lambdavar$varsigmacheck)
    if(log(runif(1))<logA) {
      accept = TRUE
      curr_Lambdavar$varsigma = varsigma_star
      curr_Lambdavar$varsigmacheck = varsigmacheck_star
      curr_Lambdavar$S_Lambda = (curr_Lambdavar$varsigma - 2) * Phi_tilde
      curr_Lambdavar = sample_S(curr_Lambdavar, Psi_diag, Lambda, prior_temp)
    } else accept=FALSE
    return(list(Lambdavar=curr_Lambdavar, accept=accept))
  }
  compute_P_Lambda = function(Lambdavar) {
    return(chol2inv(chol(compute_S_Lambda(Lambdavar))))
  }
  compute_S_Lambda = function(Lambdavar) {
    S_Lambda = expCov(distance_mat, Lambdavar$vartheta0, Lambdavar$vartheta)
    S_Lambda = (Lambdavar$varsigma - 2) * S_Lambda
    return(S_Lambda)
  }
  get_varsigma = function(Lambdavar) {
    return(Lambdavar$varsigma)
  }
  initialise = function(x=NULL) {
    Lambdavar = list()
    Lambdavar$vartheta0 = 1
    if(is.null(x)) {
      Lambdavar$logvartheta = rnorm(C, prior_Lambdavar$m_logvartheta, prior_Lambdavar$s_logvartheta)
      Lambdavar$vartheta = exp(Lambdavar$logvartheta)
      Lambdavar$varsigmacheck = rgamma(1, prior_Lambdavar$a_varsigmacheck, prior_Lambdavar$b_varsigmacheck)
      Lambdavar$varsigma = 1 / Lambdavar$varsigmacheck + 4
    } else {
      Lambdavar$vartheta = x[2:(C+1)]
      Lambdavar$logvartheta = log(x[2:(C+1)])
      Lambdavar$varsigma = x[C+2]
      Lambdavar$varsigmacheck = 1 / (x[C+2] - 4)
    }
    Lambdavar$P_Lambda = NULL
    Lambdavar$S_Lambda = compute_S_Lambda(Lambdavar)
    if(!is.null(x)) Lambdavar$P_Lambda = matrix(x[-(1:(C+1))], nrows_Lambda, nrows_Lambda)
    return(Lambdavar)
  }
  names = function() {
    ret = c("vartheta0", paste("vartheta[", 1:C, "]", sep=""), "varsigma")
    return(c(ret, paste("S[", unlist(sapply(1:nrows_Lambda, function(i) i:nrows_Lambda)), ",", 
                        rep(1:nrows_Lambda, nrows_Lambda:1), "]", sep="")))
  }
  print = function(Lambdavar) {
    ret = c(Lambdavar$vartheta0, Lambdavar$vartheta, Lambdavar$varsigma)
    return(c(ret, Lambdavar$P_Lambda[lower.tri(Lambdavar$P_Lambda, diag=TRUE)]))
  }
  expCov = function(d, s_sq, r) {
    p = dim(d)[2]
    C = dim(d)[1]
    S = matrix(1, p, p)
    for(i in 1:p) {
      for(j in 1:p) {
        if(i!=j) {
          s = 0
          for(k in 1:(C-1)) {
            s = s + d[k, i, j]^2 / r[k]^2
          }
          s = sqrt(s) + d[C, i, j] / r[C]
          S[i, j] = exp(-s)
        }
      }
    }
    return(S * s_sq)
  }
  return(list(update=update, initialise=initialise, names=names, print=print, sample_S=sample_S,
              get_varsigma=get_varsigma))
}

