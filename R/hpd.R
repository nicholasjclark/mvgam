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
  x <- as.mcmc(x)
  interval <- coda::HPDinterval(x, prob = coverage)
  interval_med <- as.vector(coda::HPDinterval(x, prob = 0.5))
  interval_med <- mean(as.vector(interval_med))
  out <- matrix(NA, ncol = 3, nrow = 1)
  colnames(out) <- c("lower", "mode", "upper")
  out[1,2] <- interval_med
  out[1,1] <- as.vector(interval)[1]
  out[1,3] <- as.vector(interval)[2]

  return(out)
}
