params <-
  list(EVAL = TRUE)

## ----echo = FALSE----------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)


## ----setup, include=FALSE--------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  dpi = 100,
  fig.asp = 0.8,
  fig.width = 6,
  out.width = "60%",
  fig.align = "center"
)
library(mvgam)
library(ggplot2)
library(dplyr)
# A custom ggplot2 theme
theme_set(
  theme_classic(base_size = 12, base_family = "serif") +
    theme(
      axis.line.x.bottom = element_line(
        colour = "black",
        size = 1
      ),
      axis.line.y.left = element_line(
        colour = "black",
        size = 1
      )
    )
)
options(
  ggplot2.discrete.colour = c(
    "#A25050",
    "#00008b",
    "darkred",
    "#010048"
  ),
  ggplot2.discrete.fill = c(
    "#A25050",
    "#00008b",
    "darkred",
    "#010048"
  )
)


## --------------------------------------------------------------------------------
set.seed(999)
# Simulate observations for species 1, which shows a declining trend and 0.7 detection probability
data.frame(
  site = 1,
  # five replicates per year; six years
  replicate = rep(1:5, 6),
  time = sort(rep(1:6, 5)),
  species = "sp_1",
  # true abundance declines nonlinearly
  truth = c(
    rep(28, 5),
    rep(26, 5),
    rep(23, 5),
    rep(16, 5),
    rep(14, 5),
    rep(14, 5)
  ),
  # observations are taken with detection prob = 0.7
  obs = c(
    rbinom(5, 28, 0.7),
    rbinom(5, 26, 0.7),
    rbinom(5, 23, 0.7),
    rbinom(5, 15, 0.7),
    rbinom(5, 14, 0.7),
    rbinom(5, 14, 0.7)
  )
) %>%
  # add 'series' information, which is an identifier of site, replicate and species
  dplyr::mutate(
    series = paste0(
      "site_",
      site,
      "_",
      species,
      "_rep_",
      replicate
    ),
    time = as.numeric(time),
    # add a 'cap' variable that defines the maximum latent N to
    # marginalize over when estimating latent abundance; in other words
    # how large do we realistically think the true abundance could be?
    cap = 100
  ) %>%
  dplyr::select(-replicate) -> testdat

# Now add another species that has a different temporal trend and a smaller
# detection probability (0.45 for this species)
testdat <- testdat %>%
  dplyr::bind_rows(
    data.frame(
      site = 1,
      replicate = rep(1:5, 6),
      time = sort(rep(1:6, 5)),
      species = "sp_2",
      truth = c(
        rep(4, 5),
        rep(7, 5),
        rep(15, 5),
        rep(16, 5),
        rep(19, 5),
        rep(18, 5)
      ),
      obs = c(
        rbinom(5, 4, 0.45),
        rbinom(5, 7, 0.45),
        rbinom(5, 15, 0.45),
        rbinom(5, 16, 0.45),
        rbinom(5, 19, 0.45),
        rbinom(5, 18, 0.45)
      )
    ) %>%
      dplyr::mutate(
        series = paste0(
          "site_",
          site,
          "_",
          species,
          "_rep_",
          replicate
        ),
        time = as.numeric(time),
        cap = 50
      ) %>%
      dplyr::select(-replicate)
  )


## --------------------------------------------------------------------------------
testdat$species <- factor(testdat$species, levels = unique(testdat$species))
testdat$series <- factor(testdat$series, levels = unique(testdat$series))


## --------------------------------------------------------------------------------
dplyr::glimpse(testdat)
head(testdat, 12)


## --------------------------------------------------------------------------------
testdat %>%
  # each unique combination of site*species is a separate process
  dplyr::mutate(trend = as.numeric(factor(paste0(site, species)))) %>%
  dplyr::select(trend, series) %>%
  dplyr::distinct() -> trend_map
trend_map


## ----include = FALSE, results='hide'---------------------------------------------
mod <- mvgam(
  # the observation formula sets up linear predictors for
  # detection probability on the logit scale
  formula = obs ~ species - 1,

  # the trend_formula sets up the linear predictors for
  # the latent abundance processes on the log scale
  trend_formula = ~ s(time, by = trend, k = 4) + species,

  # the trend_map takes care of the mapping
  trend_map = trend_map,

  # nmix() family and data
  family = nmix(),
  data = testdat,

  # priors can be set in the usual way
  priors = c(
    prior(std_normal(), class = b),
    prior(normal(1, 1.5), class = Intercept_trend)
  ),
  samples = 1000
)


## ----eval = FALSE----------------------------------------------------------------
# mod <- mvgam(
#   # the observation formula sets up linear predictors for
#   # detection probability on the logit scale
#   formula = obs ~ species - 1,
#
#   # the trend_formula sets up the linear predictors for
#   # the latent abundance processes on the log scale
#   trend_formula = ~ s(time, by = trend, k = 4) + species,
#
#   # the trend_map takes care of the mapping
#   trend_map = trend_map,
#
#   # nmix() family and data
#   family = nmix(),
#   data = testdat,
#
#   # priors can be set in the usual way
#   priors = c(
#     prior(std_normal(), class = b),
#     prior(normal(1, 1.5), class = Intercept_trend)
#   ),
#   samples = 1000
# )

## --------------------------------------------------------------------------------
code(mod)


## --------------------------------------------------------------------------------
summary(mod)


## --------------------------------------------------------------------------------
loo(mod)


## --------------------------------------------------------------------------------
plot(mod, type = "smooths", trend_effects = TRUE)


## --------------------------------------------------------------------------------
marginaleffects::plot_predictions(
  mod,
  condition = "species",
  type = "detection"
) +
  ylab("Pr(detection)") +
  ylim(c(0, 1)) +
  theme_classic() +
  theme(legend.position = "none")


## --------------------------------------------------------------------------------
hc <- hindcast(mod, type = "latent_N")

# Function to plot latent abundance estimates vs truth
plot_latentN <- function(hindcasts, data, species = "sp_1") {
  all_series <- unique(
    data %>%
      dplyr::filter(species == !!species) %>%
      dplyr::pull(series)
  )

  # Grab the first replicate that represents this series
  # so we can get the true simulated values
  series <- as.numeric(all_series[1])
  truths <- data %>%
    dplyr::arrange(time, series) %>%
    dplyr::filter(series == !!levels(data$series)[series]) %>%
    dplyr::pull(truth)

  # In case some replicates have missing observations,
  # pull out predictions for ALL replicates and average over them
  hcs <- do.call(
    rbind,
    lapply(all_series, function(x) {
      ind <- which(names(hindcasts$hindcasts) %in% as.character(x))
      hindcasts$hindcasts[[ind]]
    })
  )

  # Calculate posterior empirical quantiles of predictions
  pred_quantiles <- data.frame(t(apply(hcs, 2, function(x) {
    quantile(
      x,
      probs = c(
        0.05,
        0.2,
        0.3,
        0.4,
        0.5,
        0.6,
        0.7,
        0.8,
        0.95
      )
    )
  })))
  pred_quantiles$time <- 1:NROW(pred_quantiles)
  pred_quantiles$truth <- truths

  # Grab observations
  data %>%
    dplyr::filter(series %in% all_series) %>%
    dplyr::select(time, obs) -> observations

  # Plot
  ggplot(pred_quantiles, aes(x = time, group = 1)) +
    geom_ribbon(aes(ymin = X5., ymax = X95.), fill = "#DCBCBC") +
    geom_ribbon(aes(ymin = X30., ymax = X70.), fill = "#B97C7C") +
    geom_line(aes(x = time, y = truth), colour = "black", linewidth = 1) +
    geom_point(
      aes(x = time, y = truth),
      shape = 21,
      colour = "white",
      fill = "black",
      size = 2.5
    ) +
    geom_jitter(
      data = observations,
      aes(x = time, y = obs),
      width = 0.06,
      shape = 21,
      fill = "darkred",
      colour = "white",
      size = 2.5
    ) +
    labs(
      y = "Latent abundance (N)",
      x = "Time",
      title = species
    )
}


## --------------------------------------------------------------------------------
plot_latentN(hc, testdat, species = "sp_1")
plot_latentN(hc, testdat, species = "sp_2")


## --------------------------------------------------------------------------------
# Date link
load(url(
  "https://github.com/doserjef/spAbundance/raw/main/data/dataNMixSim.rda"
))
data.one.sp <- dataNMixSim

# Pull out observations for one species
data.one.sp$y <- data.one.sp$y[1, , ]

# Abundance covariates that don't change across repeat sampling observations
abund.cov <- dataNMixSim$abund.covs[, 1]
abund.factor <- as.factor(dataNMixSim$abund.covs[, 2])

# Detection covariates that can change across repeat sampling observations
# Note that `NA`s are not allowed for covariates in mvgam, so we randomly
# impute them here
det.cov <- dataNMixSim$det.covs$det.cov.1[,]
det.cov[is.na(det.cov)] <- rnorm(length(which(is.na(det.cov))))
det.cov2 <- dataNMixSim$det.covs$det.cov.2
det.cov2[is.na(det.cov2)] <- rnorm(length(which(is.na(det.cov2))))


## --------------------------------------------------------------------------------
mod_data <- do.call(
  rbind,
  lapply(1:NROW(data.one.sp$y), function(x) {
    data.frame(
      y = data.one.sp$y[x, ],
      abund_cov = abund.cov[x],
      abund_fac = abund.factor[x],
      det_cov = det.cov[x, ],
      det_cov2 = det.cov2[x, ],
      replicate = 1:NCOL(data.one.sp$y),
      site = paste0("site", x)
    )
  })
) %>%
  dplyr::mutate(
    species = "sp_1",
    series = as.factor(paste0(site, "_", species, "_", replicate))
  ) %>%
  dplyr::mutate(
    site = factor(site, levels = unique(site)),
    species = factor(species, levels = unique(species)),
    time = 1,
    cap = max(data.one.sp$y, na.rm = TRUE) + 20
  )


## --------------------------------------------------------------------------------
NROW(mod_data)
dplyr::glimpse(mod_data)
head(mod_data)


## --------------------------------------------------------------------------------
mod_data %>%
  # each unique combination of site*species is a separate process
  dplyr::mutate(trend = as.numeric(factor(paste0(site, species)))) %>%
  dplyr::select(trend, series) %>%
  dplyr::distinct() -> trend_map

trend_map %>%
  dplyr::arrange(trend) %>%
  head(12)


## ----include = FALSE, results='hide'---------------------------------------------
mod <- mvgam(
  # effects of covariates on detection probability;
  # here we use penalized splines for both continuous covariates
  formula = y ~ s(det_cov, k = 3) + s(det_cov2, k = 3),

  # effects of the covariates on latent abundance;
  # here we use a penalized spline for the continuous covariate and
  # hierarchical intercepts for the factor covariate
  trend_formula = ~ s(abund_cov, k = 3) +
    s(abund_fac, bs = "re"),

  # link multiple observations to each site
  trend_map = trend_map,

  # nmix() family and supplied data
  family = nmix(),
  data = mod_data,

  # standard normal priors on key regression parameters
  priors = c(
    prior(std_normal(), class = "b"),
    prior(std_normal(), class = "Intercept"),
    prior(std_normal(), class = "Intercept_trend"),
    prior(std_normal(), class = "sigma_raw_trend")
  ),

  # use Stan's variational inference for quicker results
  algorithm = "meanfield",

  # no need to compute "series-level" residuals
  residuals = FALSE,
  samples = 1000
)


## ----eval=FALSE------------------------------------------------------------------
# mod <- mvgam(
#   # effects of covariates on detection probability;
#   # here we use penalized splines for both continuous covariates
#   formula = y ~ s(det_cov, k = 4) + s(det_cov2, k = 4),
#
#   # effects of the covariates on latent abundance;
#   # here we use a penalized spline for the continuous covariate and
#   # hierarchical intercepts for the factor covariate
#   trend_formula = ~ s(abund_cov, k = 4) +
#     s(abund_fac, bs = "re"),
#
#   # link multiple observations to each site
#   trend_map = trend_map,
#
#   # nmix() family and supplied data
#   family = nmix(),
#   data = mod_data,
#
#   # standard normal priors on key regression parameters
#   priors = c(
#     prior(std_normal(), class = "b"),
#     prior(std_normal(), class = "Intercept"),
#     prior(std_normal(), class = "Intercept_trend"),
#     prior(std_normal(), class = "sigma_raw_trend")
#   ),
#
#   # use Stan's variational inference for quicker results
#   algorithm = "meanfield",
#
#   # no need to compute "series-level" residuals
#   residuals = FALSE,
#   samples = 1000
# )

## --------------------------------------------------------------------------------
summary(mod, include_betas = FALSE)


## --------------------------------------------------------------------------------
marginaleffects::avg_predictions(mod, type = "detection")


## --------------------------------------------------------------------------------
abund_plots <- plot(
  conditional_effects(
    mod,
    type = "link",
    effects = c(
      "abund_cov",
      "abund_fac"
    )
  ),
  plot = FALSE
)


## --------------------------------------------------------------------------------
abund_plots[[1]] +
  ylab("Expected latent abundance")


## --------------------------------------------------------------------------------
abund_plots[[2]] +
  ylab("Expected latent abundance")


## --------------------------------------------------------------------------------
det_plots <- plot(
  conditional_effects(
    mod,
    type = "detection",
    effects = c(
      "det_cov",
      "det_cov2"
    )
  ),
  plot = FALSE
)


## --------------------------------------------------------------------------------
det_plots[[1]] +
  ylab("Pr(detection)")
det_plots[[2]] +
  ylab("Pr(detection)")


## --------------------------------------------------------------------------------
fivenum_round <- function(x) round(fivenum(x, na.rm = TRUE), 2)

marginaleffects::plot_predictions(
  mod,
  newdata = marginaleffects::datagrid(
    det_cov = unique,
    det_cov2 = fivenum_round
  ),
  by = c("det_cov", "det_cov2"),
  type = "detection"
) +
  theme_classic() +
  ylab("Pr(detection)")
