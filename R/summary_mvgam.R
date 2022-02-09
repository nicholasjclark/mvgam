#'Summary for a fitted mvjagam object
#'
#'This function takes a fitted \code{mvjagam} object and prints various useful summaries from it
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@author Nicholas J Clark
#'@details A brief summary of the model's call is printed, along with posterior intervals for
#'some of the key parameters in the model. Note that some smooths have extra penalties on the null space,
#'so summaries for the \code{rho} parameters may include more penalty terms than the number of smooths in
#'the original model formula. The function also returns an approximate smooth term
#'significance table, but note that this signficance comes with the usual caveats associated with calculations
#'of p-values for smooth terms in \code{\link[mcgv]{summary.gam}} (i.e. all p-values are computed without
#'considering uncertainty in the smoothing parameter estimates). The significances are calculated following
#'Wood (2013) Biometrika 100(1), 221-228
#'@return A \code{list} is printed on-screen showing the summaries for the model
#'@export
summary_mvgam = function(object){
  # Grab the sim2jam object, which is needed to
  # calculate effective degrees of freedom and approximate p-values for smooth terms
  jam = object$jam_model

  #### Functions used directly from Simon Wood's mgcv summary functions R script:
  # https://github.com/cran/mgcv/blob/master/R/mgcv.r
  testStat <- function(p,X,V,rank=NULL,type=0,res.df= -1) {
    ## Implements Wood (2013) Biometrika 100(1), 221-228
    ## The type argument specifies the type of truncation to use.
    ## on entry `rank' should be an edf estimate
    ## 0. Default using the fractionally truncated pinv.
    ## 1. Round down to k if k<= rank < k+0.05, otherwise up.
    ## res.df is residual dof used to estimate scale. <=0 implies
    ## fixed scale.

    qrx <- qr(X,tol=0)
    R <- qr.R(qrx)
    V <- R%*%V[qrx$pivot,qrx$pivot,drop=FALSE]%*%t(R)
    V <- (V + t(V))/2
    ed <- eigen(V,symmetric=TRUE)
    ## remove possible ambiguity from statistic...
    siv <- sign(ed$vectors[1,]);siv[siv==0] <- 1
    ed$vectors <- sweep(ed$vectors,2,siv,"*")

    k <- max(0,floor(rank))
    nu <- abs(rank - k)     ## fractional part of supplied edf
    if (type==1) { ## round up is more than .05 above lower
      if (rank > k + .05||k==0) k <- k + 1
      nu <- 0;rank <- k
    }

    if (nu>0) k1 <- k+1 else k1 <- k

    ## check that actual rank is not below supplied rank+1
    r.est <- sum(ed$values > max(ed$values)*.Machine$double.eps^.9)
    if (r.est<k1) {k1 <- k <- r.est;nu <- 0;rank <- r.est}

    ## Get the eigenvectors...
    # vec <- qr.qy(qrx,rbind(ed$vectors,matrix(0,nrow(X)-ncol(X),ncol(X))))
    vec <- ed$vectors
    if (k1<ncol(vec)) vec <- vec[,1:k1,drop=FALSE]

    ## deal with the fractional part of the pinv...
    if (nu>0&&k>0) {
      if (k>1) vec[,1:(k-1)] <- t(t(vec[,1:(k-1)])/sqrt(ed$val[1:(k-1)]))
      b12 <- .5*nu*(1-nu)
      if (b12<0) b12 <- 0
      b12 <- sqrt(b12)
      B <- matrix(c(1,b12,b12,nu),2,2)
      ev <- diag(ed$values[k:k1]^-.5,nrow=k1-k+1)
      B <- ev%*%B%*%ev
      eb <- eigen(B,symmetric=TRUE)
      rB <- eb$vectors%*%diag(sqrt(eb$values))%*%t(eb$vectors)
      vec1 <- vec
      vec1[,k:k1] <- t(rB%*%diag(c(-1,1))%*%t(vec[,k:k1]))
      vec[,k:k1] <- t(rB%*%t(vec[,k:k1]))
    } else {
      vec1 <- vec <- if (k==0) t(t(vec)*sqrt(1/ed$val[1])) else
        t(t(vec)/sqrt(ed$val[1:k]))
      if (k==1) rank <- 1
    }
    ## there is an ambiguity in the choise of test statistic, leading to slight
    ## differences in the p-value computation depending on which of 2 alternatives
    ## is arbitrarily selected. Following allows both to be computed and p-values
    ## averaged (can't average test stat as dist then unknown)
    d <- t(vec)%*%(R%*%p)
    d <- sum(d^2)
    d1 <- t(vec1)%*%(R%*%p)
    d1 <- sum(d1^2)
    ##d <- d1 ## uncomment to avoid averaging

    rank1 <- rank ## rank for lower tail pval computation below

    ## note that for <1 edf then d is not weighted by EDF, and instead is
    ## simply refered to a chi-squared 1

    if (nu>0) { ## mixture of chi^2 ref dist
      if (k1==1) rank1 <- val <- 1 else {
        val <- rep(1,k1) ##ed$val[1:k1]
        rp <- nu+1
        val[k] <- (rp + sqrt(rp*(2-rp)))/2
        val[k1] <- (rp - val[k])
      }

      if (res.df <= 0) pval <- (psum.chisq(d,val)+psum.chisq(d1,val))/2 else {  ## (liu2(d,val) + liu2(d1,val))/2 else
        k0 <- max(1,round(res.df))
        pval <- (psum.chisq(0,c(val,-d/k0),df=c(rep(1,length(val)),k0)) + psum.chisq(0,c(val,-d1/k0),df=c(rep(1,length(val)),k0)) )/2
        #pval <- (simf(d,val,res.df) + simf(d1,val,res.df))/2
      }
    } else { pval <- 2 }
    ## integer case still needs computing,
    ## OLD: also liu/pearson approx only good in
    ## upper tail. In lower tail, 2 moment approximation is better (Can check this
    ## by simply plotting the whole interesting range as a contour plot!)
    ##if (pval > .5)
    if (pval > 1) {
      if (res.df <= 0) pval <- (pchisq(d,df=rank1,lower.tail=FALSE)+pchisq(d1,df=rank1,lower.tail=FALSE))/2 else
        pval <- (pf(d/rank1,rank1,res.df,lower.tail=FALSE)+pf(d1/rank1,rank1,res.df,lower.tail=FALSE))/2
    }
    list(stat=d,pval=min(1,pval),rank=rank)
  } ## end of testStat

  reTest <- function(b,m) {
    ## Test the mth smooth for equality to zero
    ## and accounting for all random effects in model

    ## check that smooth penalty matrices are full size.
    ## e.g. "fs" type smooths estimated by gamm do not
    ## have full sized S matrices, and we can't compute
    ## p-values here....
    if (ncol(b$smooth[[m]]$S[[1]]) != b$smooth[[m]]$last.para-b$smooth[[m]]$first.para+1) {
      return(list(stat=NA,pval=NA,rank=NA))
    }

    ## find indices of random effects other than m
    rind <- rep(0,0)
    for (i in 1:length(b$smooth)) if (!is.null(b$smooth[[i]]$random)&&b$smooth[[i]]$random&&i!=m) rind <- c(rind,i)
    ## get frequentist cov matrix of effects treating smooth terms in rind as random
    rc <- recov(b,rind,m)
    Ve <- rc$Ve
    ind <- b$smooth[[m]]$first.para:b$smooth[[m]]$last.para
    B <- mroot(Ve[ind,ind,drop=FALSE]) ## BB'=Ve

    Rm <- rc$Rm

    b.hat <- coef(b)[ind]
    d <- Rm%*%b.hat
    stat <- sum(d^2)/b$sig2
    ev <- eigen(crossprod(Rm%*%B)/b$sig2,symmetric=TRUE,only.values=TRUE)$values
    ev[ev<0] <- 0
    rank <- sum(ev>max(ev)*.Machine$double.eps^.8)

    if (b$scale.estimated) {
      #pval <- simf(stat,ev,b$df.residual)
      k <- max(1,round(b$df.residual))
      pval <- psum.chisq(0,c(ev,-stat/k),df=c(rep(1,length(ev)),k))
    } else {
      #pval <- liu2(stat,ev)
      pval <- psum.chisq(stat,ev)
    }
    list(stat=stat,pval=pval,rank=rank)
  } ## end reTest

  recov <- function(b,re=rep(0,0),m=0) {
    ## b is a fitted gam object. re is an array of indices of
    ## smooth terms to be treated as fully random....
    ## Returns frequentist Cov matrix based on the given
    ## mapping from data to params, but with dist of data
    ## corresponding to that implied by treating terms indexed
    ## by re as random effects... (would be usual frequentist
    ## if nothing treated as random)
    ## if m>0, then this indexes a term, not in re, whose
    ## unpenalized cov matrix is required, with the elements of re
    ## dropped.
    if (!inherits(b,"gam")) stop("recov works with fitted gam objects only")
    if (is.null(b$full.sp)) sp <- b$sp else sp <- b$full.sp
    if (length(re)<1) {
      if (m>0) {
        ## annoyingly, need total penalty
        np <- length(coef(b))
        k <- 1;S1 <- matrix(0,np,np)
        for (i in 1:length(b$smooth)) {
          ns <- length(b$smooth[[i]]$S)
          ind <- b$smooth[[i]]$first.para:b$smooth[[i]]$last.para
          if (ns>0) for (j in 1:ns) {
            S1[ind,ind] <- S1[ind,ind] + sp[k]*b$smooth[[i]]$S[[j]]
            k <- k + 1
          }
        }
        LRB <- rbind(b$R,t(mroot(S1)))
        ii <- b$smooth[[m]]$first.para:b$smooth[[m]]$last.para
        ## ii is cols of LRB related to smooth m, which need
        ## to be moved to the end...
        LRB <- cbind(LRB[,-ii],LRB[,ii])
        ii <- (ncol(LRB)-length(ii)+1):ncol(LRB)
        Rm <- qr.R(qr(LRB,tol=0,LAPACK=FALSE))[ii,ii] ## unpivoted QR
      } else Rm <- NULL
      return(list(Ve=(t(b$Ve)+b$Ve)*.5,Rm=Rm))
    }

    if (m%in%re) stop("m can't be in re")
    ## partition R into R1 ("fixed") and R2 ("random"), with S1 and S2
    p <- length(b$coefficients)
    rind <- rep(FALSE,p) ## random coefficient index
    for (i in 1:length(re)) {
      rind[b$smooth[[re[i]]]$first.para:b$smooth[[re[i]]]$last.para] <- TRUE
    }
    p2 <- sum(rind) ## number random
    p1 <- p - p2 ## number fixed
    map <- rep(0,p) ## remaps param indices to indices in split version
    map[rind] <- 1:p2 ## random
    map[!rind] <- 1:p1 ## fixed

    ## split R...
    R1 <- b$R[,!rind]  ## fixed effect columns
    R2 <- b$R[,rind]   ## random effect columns
    ## seitdem ich dich kennen, hab ich ein probleme,

    ## assemble S1 and S2
    S1 <- matrix(0,p1,p1);S2 <- matrix(0,p2,p2)

    k <- 1
    for (i in 1:length(b$smooth)) {
      ns <- length(b$smooth[[i]]$S)
      ind <- map[b$smooth[[i]]$first.para:b$smooth[[i]]$last.para]
      is.random <- i%in%re
      if (ns>0) for (j in 1:ns) {
        if (is.random) S2[ind,ind] <- S2[ind,ind] +  sp[k]*b$smooth[[i]]$S[[j]] else
          S1[ind,ind] <- S1[ind,ind] + sp[k]*b$smooth[[i]]$S[[j]]
        k <- k + 1
      }
    }
    ## pseudoinvert S2
    if (nrow(S2)==1) {
      S2[1,1] <- 1/sqrt(S2[1,1])
    } else if (max(abs(diag(diag(S2))-S2))==0) {
      ds2 <- diag(S2)
      ind <- ds2 > max(ds2)*.Machine$double.eps^.8
      ds2[ind] <- 1/ds2[ind];ds2[!ind] <- 0
      diag(S2) <- sqrt(ds2)
    } else {
      ev <- eigen((S2+t(S2))/2,symmetric=TRUE)
      ind <- ev$values > max(ev$values)*.Machine$double.eps^.8
      ev$values[ind] <- 1/ev$values[ind];ev$values[!ind] <- 0
      ## S2 <- ev$vectors%*%(ev$values*t(ev$vectors))
      S2 <- sqrt(ev$values)*t(ev$vectors)
    }
    ## choleski of cov matrix....
    ## L <- chol(diag(p)+R2%*%S2%*%t(R2)) ## L'L = I + R2 S2^- R2'
    L <- chol(diag(p) + crossprod(S2%*%t(R2)))

    ## now we need the square root of the unpenalized
    ## cov matrix for m
    if (m>0) {
      ## llr version
      LRB <- rbind(L%*%R1,t(mroot(S1)))
      ii <- map[b$smooth[[m]]$first.para:b$smooth[[m]]$last.para]
      ## ii is cols of LRB related to smooth m, which need
      ## to be moved to the end...
      LRB <- cbind(LRB[,-ii],LRB[,ii])
      ii <- (ncol(LRB)-length(ii)+1):ncol(LRB) ## need to pick up final block
      Rm <- qr.R(qr(LRB,tol=0,LAPACK=FALSE))[ii,ii,drop=FALSE] ## unpivoted QR
    } else Rm <- NULL

    list(Ve= crossprod(L%*%b$R%*%b$Vp)/b$sig2, ## Frequentist cov matrix
         Rm=Rm)
    # mapi <- (1:p)[!rind] ## indexes mapi[j] is index of total coef vector to which jth row/col of Vb/e relates

  } ## end of recov

#### Standard summary of formula and model argumements ####
message("GAM formula:")
print(object$call)
message()

message("Family:")
cat(paste0(object$family, '\n'))
message()

if(object$use_lv){
  message("N latent factors:")
  cat(object$n_lv, '\n')
  message()
}

message('N series:')
cat(NCOL(object$ytimes), '\n')
message()

message('N observations per series:')
if(class(object$obs_data) == 'list'){
  cat(length(object$obs_data$y) / NCOL(object$ytimes), '\n')
} else {
  cat(NROW(object$obs_data) / NCOL(object$ytimes), '\n')
}
message()

if(object$family == 'Negative Binomial'){
  message("Dispersion parameter estimates:")
  print(MCMCvis::MCMCsummary(object$jags_output, 'r')[,c(3:7)])
  message()
}

#### Summary table and approximate tests for non-flat smooth functions ####
coef_names <- names(object$mgcv_model$coefficients)
m <- length(object$mgcv_model$smooth)
useR <- TRUE
residual.df <- length(object$y) - sum(object$edf)
est.disp <- object$mgcv_model$scale.estimated

df <- edf1 <- edf <- s.pv <- chi.sq <- array(0, m)
  ## Bayesian p-values required
  if (useR)  X <- object$mgcv_model$R else {
    sub.samp <- max(1000,2*length(object$coefficients))
    if (nrow(object$model)>sub.samp) { ## subsample to get X for p-values calc.
      kind <- temp.seed(11)
      ind <- sample(1:nrow(object$model),sub.samp,replace=FALSE)  ## sample these rows from X
      X <- predict(object,object$model[ind,],type="lpmatrix")
      #RNGkind(kind[1],kind[2])
      #assign(".Random.seed",seed,envir=.GlobalEnv) ## RNG behaves as if it had not been used
      temp.seed(kind)
    } else { ## don't need to subsample
      X <- model.matrix(object)
    }
    X <- X[!is.na(rowSums(X)),] ## exclude NA's (possible under na.exclude)
  } ## end if (m>0)
  ii <- 0

  # Median beta params for smooths and their covariances
  V <- cov(MCMCvis::MCMCchains(object$jags_output, 'b')[1:1000,])
  object$mgcv_model$Ve <- V
  object$mgcv_model$Vp <- V
  object$mgcv_model$Vc <- V
  p <- MCMCvis::MCMCsummary(object$jags_output, 'b')[,c(4)]
  names(p) <- coef_names
  object$mgcv_model$coefficients <- p

  # Smoothing parameters and EDF
  object$mgcv_model$sp <- jam$sp
  object$mgcv_model$edf <- jam$edf

  for (i in 1:m) { ## loop through smooths
    start <- object$mgcv_model$smooth[[i]]$first.para;stop <- object$mgcv_model$smooth[[i]]$last.para

    edf1i <- edfi <- sum(object$mgcv_model$edf[start:stop])# edf for this smooth
    Xt <- X[start:stop,start:stop,drop=FALSE]
    # Complicated to test the parametric hypothesis given by the null space of the component’s
    # smoothing penalty, which is the default in mgcv for fully penalized smooths when the
    # estimate of the frequentist covariance matrix is present. In our case, we do not have this
    # estimate so we rely on the method from Wood 2013 (Biometrika) to test for approximate
    # significance (i.e. testing for equality to zero), which uses the Bayesian covariance
    # matrix and our penalty-informed estimated degrees of freedom
     rdf <- -1
     res <- testStat(p[start:stop],Xt,V,min(ncol(Xt),edf1i),type=0,res.df = residual.df)

    if (!is.null(res)) {
      ii <- ii + 1
      df[ii] <- reTest(object$mgcv_model,i)$rank
      chi.sq[ii] <- res$stat
      s.pv[ii] <- res$pval
      edf1[ii] <- edf1i
      edf[ii] <- edfi
      names(chi.sq)[ii]<- object$mgcv_model$smooth[[i]]$label
    }
  }
  if (ii==0) df <- edf1 <- edf <- s.pv <- chi.sq <- array(0, 0) else {
    df <- df[1:ii];chi.sq <- chi.sq[1:ii];edf1 <- edf1[1:ii]
    edf <- edf[1:ii];s.pv <- s.pv[1:ii]
  }
  if (!est.disp) {
    s.table <- cbind(edf, df, chi.sq, s.pv)
    dimnames(s.table) <- list(names(chi.sq), c("edf", "Ref.df", "Chi.sq", "p-value"))
  } else {
    s.table <- cbind(edf, df, chi.sq/df, s.pv)
    dimnames(s.table) <- list(names(chi.sq), c("edf", "Ref.df", "F", "p-value"))
  }

message('GAM smooth term approximate significances:')
printCoefmat(s.table, digits = 4, signif.stars = T)
message()

message("GAM coefficient (beta) estimates:")
mvgam_coefs <- MCMCvis::MCMCsummary(object$jags_output, 'b')[,c(3:7)]
rownames(mvgam_coefs) <- coef_names
print(mvgam_coefs)
message()

message("GAM smoothing parameter (rho) estimates:")
rho_coefs <- MCMCvis::MCMCsummary(object$jags_output, 'rho')[,c(3:7)]

name_starts <- unlist(purrr:::map(jam$smooth, 'first.sp'))
name_ends <- unlist(purrr:::map(jam$smooth, 'last.sp'))

rho_names <- unlist(lapply(seq(1:length(object$mgcv_model$smooth)), function(i){

  number_seq <- seq(1:(1 + name_ends[i] - name_starts[i]))
  number_seq[1] <- ''

  paste0(rep(object$mgcv_model$smooth[[i]]$label,
      length(number_seq)),
      number_seq)
}))
rownames(rho_coefs) <- rho_names
print(rho_coefs)
message()

message("Latent trend drift (phi) and AR parameter estimates:")
print(MCMCvis::MCMCsummary(object$jags_output, c('phi', 'ar1',
                                                 'ar2', 'ar3'))[,c(3:7)])
message()
}



