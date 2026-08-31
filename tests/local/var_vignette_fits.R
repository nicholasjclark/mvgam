# Build the cached fit consumed by `vignettes/articles/var.Rmd`.
#
# Run once locally with:
#   Rscript tests/local/var_vignette_fits.R
#
# The article reads the RDS file unconditionally when present;
# regenerate this cache whenever a relevant API or default shifts.
# Install the package from HEAD first: the article renders against
# the installed build, not the working tree.

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
  library(mvgam)
})

# Cache lives under pkgdown/, which is already in .Rbuildignore
# (and so excluded from the CRAN tarball). pkgdown::build_articles()
# runs from the project root, so the article can read this path
# relative to the working directory at render time.
cache_dir <- file.path("pkgdown", "var_cache")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Same source and reshaping the article shows, so the cached fit is
# the fit its visible code describes.
load(url(paste0(
  "https://github.com/atsa-es/MARSS/raw/master/data/kestrel.rda"
)))
regions <- c("BC", "Alb", "Sask")
model_data <- do.call(rbind, lapply(seq_along(regions), function(x) {
  data.frame(
    year = kestrel[, 1],
    adj_count = exp(kestrel[, 1 + x]),
    region = regions[x]
  )
}))
model_data$series <- as.factor(model_data$region)
model_data$time <- model_data$year
model_data <- as.data.frame(model_data)

train <- model_data[model_data$year < 2004, ]
test <- model_data[model_data$year >= 2004, ]

cat("Fitting varmod ...\n")
varmod <- mvgam(
  formula       = adj_count ~ -1,
  trend_formula = ~ region + VAR(cor = TRUE),
  priors        = c(
    prior(std_normal(), class = b_trend),
    prior(exponential(2.5), class = sigma_trend)
  ),
  data          = train,
  newdata       = test,
  family        = Gamma(),
  adapt_delta   = 0.99,
  chains        = 4,
  iter          = 3000,
  warmup        = 1500,
  silent        = 2
)

saveRDS(
  list(varmod = varmod, train = train, test = test),
  file.path(cache_dir, "kestrel_fit.rds")
)

# Report the diagnostics the article has to speak to.
draws <- as_draws_df(varmod)
pars <- setdiff(names(draws), c(".chain", ".iteration", ".draw"))
rhats <- vapply(pars, function(p) posterior::rhat(draws[[p]]), numeric(1))
esss <- vapply(pars, function(p) posterior::ess_bulk(draws[[p]]), numeric(1))
cat(sprintf(
  "\nmax rhat = %.3f, min bulk ESS = %.0f\n",
  max(rhats, na.rm = TRUE), min(esss, na.rm = TRUE)
))
cat("Cached to ", cache_dir, "\n", sep = "")
