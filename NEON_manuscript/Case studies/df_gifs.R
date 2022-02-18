# Script to visualise a dynamic factor
library(xts)
library(forecast)
data("AirPassengers")
set.seed(200)
dat <- floor(AirPassengers + cumsum(rnorm(length(AirPassengers),
                                          sd = 10)))
cols <- rev(viridis::plasma(5)[1:4])
dat <- dat + abs(min(dat))
series <- as.vector(scale(dat))
pred_vals <- seq(0, length(series), length.out = length(series))

ran_points <- rnorm(length(series), mean = series, sd = runif(length(series), 0.1, 0.25))
med <- predict(loess(ran_points ~ pred_vals,
              span = 0.25))
upper <- med + 0.25
lower <- med - 0.25

series1_pt <- 0.85 + med * 0.55 + rnorm(length(series), 0, 0.15)
series1_med <- predict(loess(series1_pt ~ pred_vals,
                             span = 0.1))
series1_pt <- jitter(series1_pt, amount = 0.2)
series1_upper <- series1_med + 0.3
series1_lower <- series1_med - 0.3

series2_pt <- -1.5 + med * 0.25 + rnorm(length(series), 0, 0.17)
series2_med <- predict(loess(series2_pt ~ pred_vals,
                             span = 0.1))
series2_pt <- jitter(series2_pt, amount = 0.2)
series2_upper <- series2_med + 0.34
series2_lower <- series2_med - 0.34

series3_pt <- 0.85 + med * -0.75 + rnorm(length(series), 0, 0.13)
series3_med <- predict(loess(series3_pt ~ pred_vals,
                             span = 0.1))
series3_pt <- jitter(series3_pt, amount = 0.2)
series3_upper <- series3_med + 0.26
series3_lower <- series3_med - 0.25

series4_pt <- 0.1 + med * -0.35 + rnorm(length(series), 0, 0.19)
series4_med <- predict(loess(series4_pt ~ pred_vals,
                             span = 0.1))
series4_pt <- jitter(series4_pt, amount = 0.2)
series4_upper <- series4_med + 0.26
series4_lower <- series4_med - 0.25

meds <- cbind(series1_med, series2_med, series3_med, series4_med)
uppers <- cbind(series1_upper, series2_upper, series3_upper, series4_upper)
lowers <- cbind(series1_lower, series2_lower, series3_lower, series4_lower)

n_cuts <- 35
starts = floor(seq.int(1, length(series1_pt), length.out = n_cuts))
starts = starts[-length(starts)]
ends = c(starts - 1, length(series1_pt))
ends = ends[-1]


library(animation)
saveGIF({
plot(1, type = "n",
     xlab = '',
     xaxt = 'n',
     yaxt = 'n',
     ylab = '',
     xlim = c(-35, length(series)),
     ylim = c(-2.2, 2.2))
abline(v = -2)
mtext('                                   Time', side = 1)

for(i in 1:35){
      plot(1, type = "n",
           xlab = '',
           xaxt = 'n',
           yaxt = 'n',
           ylab = '',
           xlim = c(-35, length(series)),
           ylim = c(-2.2, 2.2))
      abline(v = -2)
      mtext('                                   Time', side = 1)
      mtext('            Time series that may not look terribly correlated')
      lines(series1_pt,
            lwd = 2,
            col = scales::alpha(cols[1], 0.4))
      lines(series2_pt,
            lwd = 2,
            col = scales::alpha(cols[2], 0.4))
      lines(series3_pt,
            lwd = 2,
            col = scales::alpha(cols[3], 0.4))
      lines(x = pred_vals,
            y = series4_pt,
            lwd = 2,
            col = scales::alpha(cols[4], 0.4))
}

for(j in 1:(n_cuts-1)){
plot(1, type = "n",
     xlab = '',
     xaxt = 'n',
     yaxt = 'n',
     ylab = '',
     xlim = c(-35, length(series)),
     ylim = c(-2.2, 2.2))
abline(v = -2)
mtext('                                   Time', side = 1)
lines(series1_pt,
      lwd = 2,
      col = scales::alpha(cols[1], 0.3))
lines(series2_pt,
      lwd = 2,
      col = scales::alpha(cols[2], 0.3))
lines(series3_pt,
      lwd = 2,
      col = scales::alpha(cols[3], 0.3))
lines(series4_pt,
      lwd = 2,
      col = scales::alpha(cols[4], 0.3))
polygon(c(pred_vals[1:ends[j]], rev(pred_vals[1:ends[j]])),
        c(upper[1:ends[j]], rev(lower[1:ends[j]])),
        col = scales::alpha('black', 0.4),
        border = scales::alpha('black', 0.5))
lines(med[1:ends[j]],
      col = scales::alpha('black', 0.6),
      lwd = 3)
text(x = -20, y = med[1], "Dynamic factor")
mtext('            Dynamic factor captures trend dependencies')

for(i in 1:4){
  polygon(c(pred_vals[1:ends[j]], rev(pred_vals[1:ends[j]])),
          c(uppers[,i][1:ends[j]], rev(lowers[,i][1:ends[j]])),
          col = scales::alpha(cols[i], 0.2),
          border = scales::alpha(cols[i], 0.2))
  lines(meds[,i][1:ends[j]],
        col = scales::alpha(cols[i], 0.3),
        lwd = 3)
}

text(x = -20, y = meds[1,1], expression(beta == 0.55),
     col = cols[1])
text(x = -20, y = meds[1,2], expression(beta == 0.25),
     col = cols[2])
text(x = -20, y = meds[1,3], expression(beta == -0.75),
     col = cols[3])
text(x = -20, y = meds[1,4], expression(beta == -0.35),
     col = cols[4])

lines(series1_pt[1:ends[j]],
      lwd = 2,
      col = scales::alpha(cols[1], 0.1))
lines(series2_pt[1:ends[j]],
      lwd = 2,
      col = scales::alpha(cols[2], 0.1))
lines(series3_pt[1:ends[j]],
      lwd = 2,
      col = scales::alpha(cols[3], 0.1))
lines(series4_pt[1:ends[j]],
      lwd = 2,
      col = scales::alpha(cols[4], 0.1))

}


for(x in 1:35){
  plot(1, type = "n",
       xlab = '',
       xaxt = 'n',
       yaxt = 'n',
       ylab = '',
       xlim = c(-35, length(series)),
       ylim = c(-2.2, 2.2))
  abline(v = -2)
  mtext('                                   Time', side = 1)
  lines(series1_pt,
        lwd = 2,
        col = scales::alpha(cols[1], 0.3))
  lines(series2_pt,
        lwd = 2,
        col = scales::alpha(cols[2], 0.3))
  lines(series3_pt,
        lwd = 2,
        col = scales::alpha(cols[3], 0.3))
  lines(series4_pt,
        lwd = 2,
        col = scales::alpha(cols[4], 0.3))
  polygon(c(pred_vals, rev(pred_vals)), c(upper, rev(lower)),
          col = scales::alpha('black', 0.4),
          border = scales::alpha('black', 0.5))
  lines(med,
        col = scales::alpha('black', 0.6),
        lwd = 3)
  text(x = -20, y = med[1], "Dynamic factor")
  mtext('            Dynamic factor captures trend dependencies')
  for(j in 1:4){
    polygon(c(pred_vals, rev(pred_vals)),
            c(uppers[,j], rev(lowers[,j])),
            col = scales::alpha(cols[j], 0.2),
            border = scales::alpha(cols[j], 0.2))
    lines(meds[,j],
          col = scales::alpha(cols[j], 0.3),
          lwd = 3)
  }

  text(x = -20, y = meds[1,1], expression(beta == 0.55),
       col = cols[1])
  text(x = -20, y = meds[1,2], expression(beta == 0.25),
       col = cols[2])
  text(x = -20, y = meds[1,3], expression(beta == -0.75),
       col = cols[3])
  text(x = -20, y = meds[1,4], expression(beta == -0.35),
       col = cols[4])

  lines(series1_pt,
        lwd = 2,
        col = scales::alpha(cols[1], 0.1))
  lines(series2_pt,
        lwd = 2,
        col = scales::alpha(cols[2], 0.1))
  lines(series3_pt,
        lwd = 2,
        col = scales::alpha(cols[3], 0.1))
  lines(series4_pt,
        lwd = 2,
        col = scales::alpha(cols[4], 0.1))
}

}, movie.name = 'df.gif', interval = 0.1, ani.width = 1220, outdir = getwd(),
ani.height = 680, ani.res = 160)


gif_compress <- function(ingif, outgif, show=TRUE, extra.opts=""){
  command <-  sprintf("gifsicle -O3 %s < %s > %s", extra.opts, ingif, outgif)
  system.fun <- if (.Platform$OS.type == "windows") shell else system
  if(show) message("Executing: ", strwrap(command, exdent = 2, prefix = "\n"))
  system.fun(ifelse(.Platform$OS.type == "windows", sprintf("\"%s\"", shQuote(command)), command))
}

gif_compress("df.gif","df.gif",extra.opts="--colors 256")

png('df_final.png', width = 1220, height = 680, res = 160)
plot(1, type = "n",
     xlab = '',
     xaxt = 'n',
     yaxt = 'n',
     ylab = '',
     xlim = c(-35, length(series)),
     ylim = c(-2.2, 2.2))
abline(v = -2)
mtext('                                   Time', side = 1)
lines(series1_pt,
      lwd = 2,
      col = scales::alpha(cols[1], 0.3))
lines(series2_pt,
      lwd = 2,
      col = scales::alpha(cols[2], 0.3))
lines(series3_pt,
      lwd = 2,
      col = scales::alpha(cols[3], 0.3))
lines(series4_pt,
      lwd = 2,
      col = scales::alpha(cols[4], 0.3))
polygon(c(pred_vals, rev(pred_vals)), c(upper, rev(lower)),
        col = scales::alpha('black', 0.4),
        border = scales::alpha('black', 0.5))
lines(med,
      col = scales::alpha('black', 0.6),
      lwd = 3)
text(x = -20, y = med[1], "Dynamic factor")
mtext('            Dynamic factor captures trend dependencies')
for(j in 1:4){
  polygon(c(pred_vals, rev(pred_vals)),
          c(uppers[,j], rev(lowers[,j])),
          col = scales::alpha(cols[j], 0.2),
          border = scales::alpha(cols[j], 0.2))
  lines(meds[,j],
        col = scales::alpha(cols[j], 0.3),
        lwd = 3)
}

text(x = -20, y = meds[1,1], expression(beta == 0.55),
     col = cols[1])
text(x = -20, y = meds[1,2], expression(beta == 0.25),
     col = cols[2])
text(x = -20, y = meds[1,3], expression(beta == -0.75),
     col = cols[3])
text(x = -20, y = meds[1,4], expression(beta == -0.35),
     col = cols[4])

lines(series1_pt,
      lwd = 2,
      col = scales::alpha(cols[1], 0.1))
lines(series2_pt,
      lwd = 2,
      col = scales::alpha(cols[2], 0.1))
lines(series3_pt,
      lwd = 2,
      col = scales::alpha(cols[3], 0.1))
lines(series4_pt,
      lwd = 2,
      col = scales::alpha(cols[4], 0.1))
dev.off()
