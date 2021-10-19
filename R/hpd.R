# Calculate the highest posterior density interval
#'
#'This function uses estimated densities to calculate HPD intevals. Code originally supplied by Martyn Plummer
#'
#'@param x \code{vector} of values representing the distribution to be summarised
#'@param coverage \code{numeric} value specifying the width of the HPD interval. Default is 0.95
#'@return A \code{list} containing the reconciled forecast distributions for each series in \code{y}. Each element in
#'the \code{vector} with three values: lower estimate, median estimate and upper estimate of the HPD interval
#'
#'@export
#'
hpd <- function(x, coverage = 0.95)
{
  x <- as.matrix(x)
  out <- matrix(NA, nrow = ncol(x), ncol = 3)
  rownames(out) <- dimnames(x)[[2]]
  colnames(out) <- c("mode", "lower", "upper")

  f <- function(p) {
    if (p == density.range[2]) {
      set.coverage <- 0
    }
    else {
      p.upper <- min(y.density$y[y.density$y > p])
      p.lower <- max(y.density$y[y.density$y <= p])
      cov.upper <- sum(y.counts[y.density$y >= p.upper]) / sum(y.counts)
      cov.lower <- sum(y.counts[y.density$y >= p.lower]) / sum(y.counts)
      c <- (p.upper - p)/(p.upper - p.lower)
      set.coverage <- c * cov.upper + (1 - c) * cov.lower
    }
    return(set.coverage - coverage)
  }

  for (i in 1:ncol(x)) {
    y <- unclass(x[,i])
    y.density <- stats::density(y, n=1024)
    m <- length(y.density$x)

    ## Find the midpoint
    out[i,1] <- y.density$x[which.max(y.density$y)]
    dx <- diff(range(y.density$x)) / m
    breaks <- c(y.density$x[1] - dx / 2, y.density$x + dx /2)
    y.counts <- hist(y, breaks = breaks, plot = FALSE)$counts
    density.range <- range(y.density$y)
    uniroot.out <- stats::uniroot(f, density.range)

    ## Assuming that we have a single interval, find the limits
    out[i,2:3] <- range(y.density$x[y.density$y > uniroot.out$root])

    ## Check!
    if (sum(abs(diff(y.density$y > uniroot.out$root))) != 2) {
      warning("HPD set is not a closed interval")
    }
  }
  out <- c(out[2], out[1], out[3])
  names(out) <- c('lower', 'median', 'upper')
  return(out)
}
