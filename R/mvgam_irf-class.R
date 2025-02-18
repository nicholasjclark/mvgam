#' `mvgam_irf` object description
#'
#' A \code{mvgam_irf} object returned by function \code{\link{irf}}.
#' Run `methods(class = "mvgam_irf")` to see an overview of available methods.
#' @details A `mvgam_irf` object contains a list of posterior IRFs, each stored as
#' its own list
#' @seealso [mvgam], [VAR]
#' @author Nicholas J Clark
#' @name mvgam_irf-class
NULL

#'Plot impulse responses from an `mvgam_irf` object
#'
#'This function takes an \code{mvgam_irf} object and produces plots of Impulse Response Functions
#'
#'@param x \code{list} object of class \code{mvgam_irf}. See [irf()]
#'@param series \code{integer} specifying which process series should be given the shock
#'@param ... ignored
#'@return A base R plot or set of plots
#'@author Nicholas J Clark
#'@export
plot.mvgam_irf = function(x, series = 1, ...){

  all_irfs <- x
  n_processes <- dim(all_irfs[[1]][[1]])[2]
  h <- dim(all_irfs[[1]][[1]])[1]

  # Extract IRFs for the specific series
  impulse_responses <- lapply(seq_along(all_irfs), function(j){
    all_irfs[[j]][series]
  })

  # Plot imp responses for all series apart from the impacted one
  c_light <- c("#DCBCBC")
  c_light_highlight <- c("#C79999")
  c_mid <- c("#B97C7C")
  c_mid_highlight <- c("#A25050")
  c_dark <- c("#8F2727")
  c_dark_highlight <- c("#7C0000")


  n_plots <- n_processes
  pages <- 1

  if (n_plots > 4) pages <- 2
  if (n_plots > 8) pages <- 3
  if (n_plots > 12) pages <- 4
  if (pages != 0)  {
    ppp <- n_plots %/% pages

    if (n_plots %% pages != 0) {
      ppp <- ppp + 1
      while (ppp * (pages - 1) >= n_plots) pages <- pages - 1
    }

    # Configure layout matrix
    c <- r <- trunc(sqrt(ppp))
    if (c<1) r <- c <- 1
    if (c*r < ppp) c <- c + 1
    if (c*r < ppp) r <- r + 1

    .pardefault <- par(no.readonly=T)
    on.exit(par(.pardefault))
    oldpar <- par(mfrow = c(r,c))

  } else { ppp <- 1; oldpar <- par()}

  for(x in 1:n_plots){
      responses <- do.call(rbind, lapply(seq_along(impulse_responses), function(j){
        impulse_responses[[j]][[1]][,x]
      }))

      probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
      cred <- sapply(1:NCOL(responses),
                     function(n) quantile(responses[,n],
                                          probs = probs, na.rm = TRUE))
      pred_vals <- 1:h
      plot(1, type = "n", bty = 'L',
           xlab = '',
           xaxt = 'n',
           ylab = '',
           xlim = c(1, h),
           ylim = c(-1.1 * max(abs(cred)), 1.1 * max(abs(cred))))
      polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[9,])),
              col = c_light, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[2,], rev(cred[8,])),
              col = c_light_highlight, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[3,], rev(cred[7,])),
              col = c_mid, border = NA)
      polygon(c(pred_vals, rev(pred_vals)), c(cred[4,], rev(cred[6,])),
              col = c_mid_highlight, border = NA)
      lines(pred_vals, cred[5,], col = c_dark, lwd = 2.5)
      abline(h = 0, lwd = 3, col = 'white')
      abline(h = 0, lwd = 2.5)
      box(bty = 'L', lwd = 2)

    axis(1, cex.axis = 1, tck = -0.05)
    title(paste0('process_', series, ' -> process_', x), line = 0)

    # if(x != series){
    #   axis(1, cex.axis = 1, tck = -0.05)
    #   title(paste0('process_', series, ' -> process_', x), line = 0)
    # }

  }
  layout(1)
}
