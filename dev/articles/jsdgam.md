# Joint species distribution models with mvgam

Joint species distribution models (JSDMs) assign the species
co-occurrence that remains once the measured environment is accounted
for to a few latent factors (Warton et al.
[2015](https://doi.org/10.1016/j.tree.2015.09.007); Ovaskainen et al.
[2017](https://doi.org/10.1111/ele.12757)). The loadings imply a
residual covariance among species. The factor scores ordinate the sites.

[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
fits this model with the loadings identified by a QR rotation (Heaps and
Jermyn [2024](https://doi.org/10.1007/s11222-024-10454-0)). This article
fits it to counts of epigaeic ants at 30 sites in south-eastern
Australia (Gibb et al.
[2015](https://doi.org/10.1007/s00442-014-3101-9)) and adds three
extensions in turn: environmental effects that act through the factors,
a loadings prior informed by species traits and a prior under which the
data decide how many factors are active. The [JSDM
cheatsheet](https://nicholasjclark.github.io/mvgam/dev/cheatsheets/jsdgam_cheatsheet.pdf)
summarises the functions on one page.

## The data

`antTraits` from the mvabund package holds counts of 41 ant species at
30 sites, five environmental covariates and five species traits. Two
traits are ordinal scores, which the code below converts to numbers.
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
takes one row per site and species:

``` r

data(antTraits, package = "mvabund")
abund <- antTraits$abund
# Restore the species names that make.names() mangled, such as
# "Pheidole.sp..A" for "Pheidole sp. A"
species_names <- gsub("\\.(?=\\S)", " ",
                      gsub("\\.\\.", ". ", colnames(abund)), perl = TRUE)
colnames(abund) <- species_names
env <- as.data.frame(scale(antTraits$env))
traits <- antTraits$traits
rownames(traits) <- species_names
# Pilosity and polymorphism are scores stored as unordered factors
traits$Pilosity <- as.numeric(as.character(traits$Pilosity))
traits$Polymorphism <- as.numeric(as.character(traits$Polymorphism))

ant_data <- data.frame(
  site = rep(seq_len(nrow(abund)), times = ncol(abund)),
  species = factor(rep(colnames(abund), each = nrow(abund)),
                   levels = colnames(abund)),
  counts = as.vector(as.matrix(abund))
)
ant_data <- cbind(ant_data, env[ant_data$site, ])
```

Because the variance of each species’ counts rises well above its mean,
the counts take a negative binomial family:

``` r

moments <- data.frame(
  mean = tapply(ant_data$counts, ant_data$species, mean),
  var = tapply(ant_data$counts, ant_data$species, var)
)
ggplot(moments, aes(mean, var)) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  geom_point(colour = "#8F2727") +
  scale_x_continuous(trans = "log1p") +
  scale_y_continuous(trans = "log1p") +
  labs(x = "Mean count", y = "Variance of counts")
```

![Mean and variance of each species' counts on log1p axes. The dashed
line marks the equality a Poisson family would
imply.](jsdgam_files/figure-html/mean-variance-1.png)

Mean and variance of each species’ counts on log1p axes. The dashed line
marks the equality a Poisson family would imply.

## Latent factors alone

The first model leaves all structure among species to two latent
factors. `factor_formula = ~ -1` puts no covariates on the factors:

``` r

mod_base <- jsdgam(
  counts ~ 1,
  factor_formula = ~ -1,
  data = ant_data,
  family = negbinomial(),
  n_lv = 2,
  unit = site,
  species = species,
  chains = 4, seed = 2025, silent = 2
)
```

[`ordinate()`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md)
draws the posterior medians of the site scores and the species loadings
as a biplot:

``` r

ordinate(mod_base)
```

![Biplot of the site scores (numbered points) and species loadings
(arrows) from the model with latent factors
alone.](jsdgam_files/figure-html/ordinate-base-1.png)

Biplot of the site scores (numbered points) and species loadings
(arrows) from the model with latent factors alone.

The loadings of Aphaenogaster longiceps and Crematogaster sp. A lie
furthest from the origin. Their abundances vary most across sites beyond
what the intercepts predict.

## Environmental effects on the factors

A constrained ordination lets the environment move the sites along the
factors.
[`lv_axis()`](https://nicholasjclark.github.io/mvgam/dev/reference/lv_axis.md)
in `factor_formula` gives each factor its own slope on a covariate. Each
species then responds to the covariate through its loadings, with two
slopes per covariate in place of 41:

``` r

mod_env <- jsdgam(
  counts ~ 1,
  factor_formula = ~ -1 + Bare.ground:lv_axis() +
    Canopy.cover:lv_axis() + Shrub.cover:lv_axis(),
  data = ant_data,
  family = negbinomial(),
  n_lv = 2,
  unit = site,
  species = species,
  chains = 4, seed = 2025, silent = 2
)
fixef(mod_env, trend_effects = TRUE)
#>                               Estimate Est.Error       Q2.5     Q97.5
#> Bare.ground:.trend1_trend   0.14757957 0.2299146 -0.3253114 0.5851231
#> Bare.ground:.trend2_trend   0.14904403 0.2375116 -0.3283565 0.6094964
#> .trend1:Canopy.cover_trend  0.38902847 0.3817168 -0.4469189 1.0555936
#> .trend2:Canopy.cover_trend  0.55781086 0.4310188 -0.5233924 1.2117815
#> .trend1:Shrub.cover_trend  -0.02135222 0.2207813 -0.4441711 0.4091492
#> .trend2:Shrub.cover_trend   0.09645195 0.2345308 -0.3851571 0.5364400
```

[`fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html) reports the
slopes in the same rotated basis as the identified loadings `Z_tilde`. A
species’ response is the product of its loadings and these slopes.
[`species_effects()`](https://nicholasjclark.github.io/mvgam/dev/reference/species_effects.md)
returns each species’ response to each covariate, draw by draw, on the
log scale of expected counts. A black outline marks a response whose
sign has a posterior probability above 0.95:

``` r

se <- species_effects(mod_env, variables = c("Bare.ground", "Canopy.cover",
                                             "Shrub.cover"))
plot(se)
```

![Posterior mean response of each species to one standard deviation of
each covariate, on the log scale of expected counts. Outlined cells have
a posterior probability of their sign above
0.95.](jsdgam_files/figure-html/species-effects-1.png)

Posterior mean response of each species to one standard deviation of
each covariate, on the log scale of expected counts. Outlined cells have
a posterior probability of their sign above 0.95.

Under denser canopy 8 of the 41 species become more abundant and 10 less
abundant, each with a posterior probability above 0.95. The mean
responses run from -0.99 for Monomorium rothsteini to 1.89 for
Aphaenogaster longiceps. `summary(se)` lists every species’ estimate and
interval.

## Trait-informed loadings

The default prior treats every species’ loadings as independent. The
`traits` argument replaces it with the structured prior of Heaps and
Jermyn ([2024](https://doi.org/10.1007/s11222-024-10454-0)). Each column
of loadings is drawn from a multivariate normal whose covariance among
species comes from a kernel on the standardised traits, with one length
scale per trait. A short length scale lets that trait separate species’
loadings. A long one leaves them exchangeable.

``` r

mod_traits <- jsdgam(
  counts ~ 1,
  factor_formula = ~ -1,
  data = ant_data,
  family = negbinomial(),
  n_lv = 2,
  traits = traits,
  unit = site,
  species = species,
  control = list(adapt_delta = 0.95),
  chains = 4, seed = 2025, silent = 2
)
```

Each length scale is named for its trait:

``` r

mcmc_plot(mod_traits, variable = "theta_features", regex = TRUE,
          type = "intervals")
```

![Posterior medians with 50% and 90% credible intervals of the length
scale of each trait.](jsdgam_files/figure-html/theta-1.png)

Posterior medians with 50% and 90% credible intervals of the length
scale of each trait.

The median length scale of Femur length, 0.28, is the shortest of the
traits. It falls below the 5% quantile of every other trait’s length
scale. A shorter length scale gives a trait’s differences between
species more weight in the loadings prior.
[`compare_loadings()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_loadings.md)
aligns the two fits’ loadings by a Procrustes rotation and draws how far
each species moved:

``` r

compare_loadings(mod_base, mod_traits,
                 labels = c("uninformed", "trait-informed"),
                 label_species = "shift")
```

![Posterior median loadings of each species without and with the trait
prior, aligned to a shared basis. Labels mark the ten species that moved
furthest.](jsdgam_files/figure-html/compare-loadings-1.png)

Posterior median loadings of each species without and with the trait
prior, aligned to a shared basis. Labels mark the ten species that moved
furthest.

Leave-one-out cross-validation compares the two priors on the same
likelihood:

``` r

loo_cmp <- loo_compare(list(uninformed = loo(mod_base),
                            `trait-informed` = loo(mod_traits)))
loo_cmp
#>           model elpd_diff se_diff p_worse diag_diff       diag_elpd
#>  trait-informed       0.0     0.0      NA           21 k_psis > 0.7
#>      uninformed     -30.0     7.3    1.00           18 k_psis > 0.7
```

Leave-one-out ranks the trait-informed prior first. The other trails it
by 30.0 (SE 7.3) in expected log predictive density.

## Residual correlations

[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
summarises the posterior residual correlation between every pair of
species. `plot(cluster = TRUE)` orders the species to bring blocks of
co-occurring species onto the diagonal:

``` r

rcor <- residual_cor(mod_traits)
plot(rcor, cluster = TRUE)
```

![Posterior mean residual correlations from the trait-informed model.
Outlined cells have a posterior probability of their sign above
0.95.](jsdgam_files/figure-html/resid-cor-1.png)

Posterior mean residual correlations from the trait-informed model.
Outlined cells have a posterior probability of their sign above 0.95.

Of the 820 species pairs, 346 correlate positively and 62 negatively,
each with a posterior probability of its sign above 0.95. A residual
correlation may reflect a shared response to an unmeasured driver or an
interaction between species. The model alone cannot tell the two apart.

## The community along a gradient

Ecologists report a community by its richness and the traits of its
species. To see how canopy cover changes both,
[`community()`](https://nicholasjclark.github.io/mvgam/dev/reference/community.md)
predicts every species along a gradient of canopy cover and combines the
predictions draw by draw. It averages over the latent factors, which
gives the expected community of a site with that cover:

``` r

canopy <- data.frame(Canopy.cover = seq(-1.5, 1.5, length.out = 25),
                     Bare.ground = 0, Shrub.cover = 0, site = 1)
cm <- community(mod_env, newdata = canopy, traits = traits)
plot(cm)
```

![Posterior median expected richness along canopy cover, with nested
20%, 40%, 60% and 80% credible
intervals.](jsdgam_files/figure-html/community-1.png)

Posterior median expected richness along canopy cover, with nested 20%,
40%, 60% and 80% credible intervals.

Expected richness barely changes, from 23.5 species at the most open
sites to 22.9 under the densest canopy. The posterior probability that
dense canopy holds fewer species is 0.60.

Richness counts species without regard to how common they are. Each
species weighs into diversity and traits by its expected count. A Hill
number gives how many equally common species would match the community’s
evenness. Each trait panel gives the trait of an average individual ant:

``` r

plot(cm, type = "diversity")
```

![Hill numbers and abundance-weighted mean traits along canopy cover,
with nested 20%, 40%, 60% and 80% credible
intervals.](jsdgam_files/figure-html/community-diversity-1.png)

Hill numbers and abundance-weighted mean traits along canopy cover, with
nested 20%, 40%, 60% and 80% credible intervals.

The exponential of Shannon entropy, the Hill number of order one, peaks
at 27 species near a canopy cover of -0.1 and falls to 19 under the
densest canopy. Dense canopy keeps a list of species as long as open
ground does, while a few of them come to dominate the counts.

`plot(cm, type = "cooccurrence")` draws the pairs of species found
together more or less often than canopy cover predicts.

## How many factors?

The models so far fixed two factors. A multiplicative gamma process
prior (Bhattacharya and Dunson
[2011](https://doi.org/10.1093/biomet/asr013)) shrinks each column of
loadings more strongly than the one before. With a generous ceiling, the
data decide how many columns stay away from zero. The prior combines
with the trait kernel through `loadings_prior`:

``` r

mod_mgp <- jsdgam(
  counts ~ 1,
  factor_formula = ~ -1,
  data = ant_data,
  family = negbinomial(),
  n_lv = 6,
  loadings_prior = list(column_shrinkage = "mgp", features = traits),
  unit = site,
  species = species,
  chains = 4, seed = 2025, silent = 2
)
```

[`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md)
splits the covariance the factors give the species into principal
dimensions. A dimension is active when its variance is at least a tenth
of the leading dimension’s, with posterior probability of at least one
half.

``` r

af <- active_factors(mod_mgp)
plot(af)
```

![Posterior probability that each factor, largest first, is active. The
dashed line marks the
threshold.](jsdgam_files/figure-html/active-factors-1.png)

Posterior probability that each factor, largest first, is active. The
dashed line marks the threshold.

In this fit, 2 of the 6 factors are active, against the 2 the earlier
models fixed.

## Summary

The trait prior and column shrinkage tested two assumptions of the first
ant community models: independent loadings and two factors.
Leave-one-out ranked the trait-informed prior first, by 30.0 in expected
log predictive density against a standard error of 7.3. Given a ceiling
of 6 factors, the shrinkage prior kept 2 active, against the 2 the first
models assumed. Each extension is one argument to a single
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
call, as are the environmental slopes that gave all 41 species a canopy
response from three covariates.

## Where to next

Each function used here has a help page, such as
[`?jsdgam`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md),
[`?lv_axis`](https://nicholasjclark.github.io/mvgam/dev/reference/lv_axis.md)
or
[`?ordinate`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md),
with its arguments and output.
[`how_to_cite()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md)
writes a methods paragraph for a fitted model, naming the architecture
it used.
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
also takes a phylogeny through `phylo`, trait-mediated environmental
responses through `trait_slopes` and imperfect detection through the
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
family.

Related articles:

- [Dynamic factor
  models](https://nicholasjclark.github.io/mvgam/dev/articles/dfm.md)
  give the factors dynamics over time.
- [N-mixture
  models](https://nicholasjclark.github.io/mvgam/dev/articles/nmix.md)
  cover the closure-unit families that
  [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
  belongs to.
- [mvgam
  overview](https://nicholasjclark.github.io/mvgam/dev/articles/mvgam_overview.md)
  introduces the observation and trend formulas.

## References

Bhattacharya A and Dunson DB (2011). Sparse Bayesian infinite factor
models. *Biometrika* 98(2), 291-306.
[doi:10.1093/biomet/asr013](https://doi.org/10.1093/biomet/asr013)

Gibb H, Stoklosa J, Warton DI, Brown AM, Andrew NR and Cunningham SA
(2015). Does morphology predict trophic position and habitat use of ant
species and assemblages? *Oecologia* 177(2), 519-531.
[doi:10.1007/s00442-014-3101-9](https://doi.org/10.1007/s00442-014-3101-9)

Heaps SE and Jermyn IH (2024). Structured prior distributions for the
covariance matrix in latent factor models. *Statistics and Computing*
34, 143.
[doi:10.1007/s11222-024-10454-0](https://doi.org/10.1007/s11222-024-10454-0)

Ovaskainen O, Tikhonov G, Norberg A, Blanchet FG, Duan L, Dunson D,
Roslin T and Abrego N (2017). How to make more out of community data? A
conceptual framework and its implementation as models and software.
*Ecology Letters* 20(5), 561-576.
[doi:10.1111/ele.12757](https://doi.org/10.1111/ele.12757)

Warton DI, Blanchet FG, O’Hara RB, Ovaskainen O, Taskinen S, Walker SC
and Hui FKC (2015). So many variables: joint modeling in community
ecology. *Trends in Ecology and Evolution* 30(12), 766-779.
[doi:10.1016/j.tree.2015.09.007](https://doi.org/10.1016/j.tree.2015.09.007)
