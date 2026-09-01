# Dev Tasks: mvgam 2.0 release survey

**Source:** post-fit sweep over the cached fixtures plus three
read-only survey agents. No TRD.
**Generated:** 2026-08-25

## Relevant Files

**Created** — `tests/local/postfit_sweep.R` drives every post-fit
method against every cached fit and logs a status and shape per call
(`--fixtures=`, `--groups=` to filter).
`tests/local/test-hierarchical-trends.R` covers `gr`/`subgr` fits end
to end. `R/mvgam_families.R` restores the family hub topic.
`R/stan_source.R` holds the rules for reading the Stan source brms
generated. `tests/local/test-new-levels.R`,
`tests/local/test-update-trend-args.R`,
`tests/testthat/test-multi-response-kernels.R` and
`tests/testthat/test-mvgam-core.R` cover what the sweep could not
reach. `mvgam_codegen_options()` in `R/brms_integration.R` carries the
brms code-generation settings from one place to every generator that
needs them, and `tests/testthat/test-stancode-standata.R` pins each of
their journeys.

**Notes** — long commands run in the background and write to a log
that is then read. Fixture-dependent tests live in `tests/local/`; CI
tests never skip on a missing fixture and the CI sweep stays under ten
minutes. Cached fits are read once, never re-fitted to inspect.

## Tasks

- [ ] **14.0 The factor labels move between chains, and `ar1_trend` carries it**
  > Recorded first as two free scales on a column, then as a
  > rotation. Neither. Every route draws `Z` at a fixed scale, so
  > there is one free scale per column and no ridge; and arbitrary
  > rotation is not a symmetry here, because mixing two AR(1)
  > factors with different coefficients does not give an AR(1).
  > The autoregressive dynamics break rotation and leave the
  > discrete symmetry: column permutation with sign.
  >
  > Measured on the cached four-series `n_lv = 2` AR(1) fit. The
  > per-chain posterior means of `ar1_trend` are (0.652, 0.367) and
  > (0.403, 0.614): the same pair, swapped. Chain 1's first
  > loadings column sits on series 1 and 2; chain 2's second column
  > sits there with the sign flipped. Label switching, not a
  > continuous family.
  >
  > The QR step handles it for what it covers. `Z_tilde` agrees
  > across chains to two decimals, so its triangular form fixes
  > both the sign and the ordering, and every post-fit surface
  > reads it: `ordinate()`, `compare_loadings()`, `residual_cor()`
  > and `active_factors()` are unaffected.
  >
  > `ar1_trend` is not covered. It belongs to the unpermuted
  > column, nothing relabels it, and `summary()` reports it: R-hat
  > 1.12 to 1.17 and bulk ESS 9 to 14 on a fit whose `Z_tilde` is
  > clean and whose `sigma_trend` reaches 1.02 and ESS 76 to 293.
  > A user reads that as a model that has not converged. Raw `Z`
  > has the same problem for the same reason.
  >
  > Identifying rotation at sampling time is declined: it would
  > make inference depend on the order the series arrive in, and
  > truncate rather than match the kernel prior
  > `Z[, k] ~ MVN(0, Phi)`.
  >
  > What is left is relabelling after the fact, which is the usual
  > treatment for label switching and needs no constraint. Match
  > each chain's columns to a reference by their loadings, then
  > permute `Z`, `lv_trend` and the per-column dynamics parameters
  > together. `sign_canonicalise_factors()` already walks the
  > per-chain samples to do exactly this for sign, and skips
  > whenever `Z_tilde` is present on the grounds that QR settled
  > it; that reasoning holds for `Z` and not for `ar1_trend`.
  > Failing that, say in `summary()` that per-column dynamics
  > parameters carry label switching on a factor model and that
  > `Z_tilde` is the identified quantity.
  >
  > Separately and cheaply: the default `Z` prior is
  > `student_t(3, 0, 0.5)` on four routes and Gaussian on the
  > other four. Harmonising to `std_normal()` is one string in
  > `common_trend_priors` and makes the default agree with the
  > kernel and MGP branches. A prior harmonisation, not a
  > geometry fix.
  >
  > Judge any change here across seeds. An earlier entry chased an
  > E-BFMI of 0.295 on a single `VAR()` draw, implemented two
  > parameterisations against it and reverted both: across four
  > seeds the E-BFMI, the divergence count and the ESS each moved
  > further between seeds than between parameterisations.

- [ ] **15.5b Describe a fixed or partial `trend_map` in the table**
  > `get_prior()` on a jsdgam specification reports `b`, `Intercept`
  > and `shape` and no trend classes at all, so a user cannot see
  > the priors they might set. The cause is structural:
  > `mvgam_formula()` holds `formula` and `trend_formula` only,
  > while `jsdgam()` carries its factor structure in `trend_map` and
  > `loadings_prior`. No `mvgam_formula` can describe a jsdgam fit.
  >
  > The plain path is correct: `~ ZMVN(n_lv = 2)` does report `Z`.
  > Give `get_prior.mvgam_formula()` the `trend_map` and
  > `loadings_prior` arguments `stancode.mvgam_formula()` already
  > takes, which also closes 15.0.

- [ ] **16.0 Smaller things the sweeps turned up**
  > - [x] `prior(constant(1), class = sigma_trend)` was accepted and
  >   emitted `sigma_trend ~ constant(1);`, which names no density
  >   Stan can evaluate, so the model failed at `stanc` with the
  >   class nowhere in the message. brms implements `constant()` by
  >   moving the parameter out of the parameters block and assigning
  >   it, which every mvgam emitter would have to do too. Refused
  >   instead, at `get_trend_parameter_prior()`, the one point all
  >   ten emission sites resolve through.
  > - [x] `pkgdown/jsdgam_cache/figs/` held 11 tracked PNGs, 1.4 MB.
  >   Removed. Nothing referenced them and no script wrote them; the
  >   only mention in the repo was this line.
  > - [x] `ZMVN()` on a single series makes `sigma_trend^2 +
  >   sigma^2` an exact sum with nothing to split it.
  >   `zmvn_scale_confounded()` decides, and `mvgam()` warns once
  >   per session rather than refusing, since the model is still
  >   fittable and the split is a prior choice a user may mean to
  >   make. Two series identify it through the cross-series
  >   covariance; a family with no residual scale never had the
  >   problem, and a negative binomial's `shape` does not count,
  >   being a dispersion rather than an additive scale.
  >
  > Two entries are struck, both parameterisation changes and
  > neither wanted:
  >
  > `AR(p >= 2)` boxes each coefficient in `(-1, 1)` independently
  > while the stationary region of an AR(2) is a triangle strictly
  > inside that box, so the prior admits non-stationary draws. The
  > Heaps `AtoP` and `rev_mapping` functions the VAR generator uses
  > would fix it and reduce to the scalar case at `m = 1`. Not
  > adopting them is a decision, not an oversight: it would move
  > what a user's `prior(class = ar2_trend)` constrains onto the
  > partial-autocorrelation scale and rename posterior columns. The
  > coefficient scale stays.
  >
  > `CAR()` and `AR(p = 2)` start the state from `Normal(0,
  > sigma_trend)` rather than the stationary variance, where AR(1)
  > divides by `sqrt(1 - phi^2)`. The comment at the branch names
  > the Yule-Walker solve this needs. Left alone for the same
  > reason: it changes the posterior of every AR(p>1) and CAR fit,
  > so it belongs with a deliberate parameterisation pass rather
  > than a release sweep.

- [ ] **13.1 The migration the names still need**
  > Deferred from 13.0 because each renames something a fitted object
  > carries, so cached fixtures stop being readable until they are
  > rebuilt. Belongs with 5.0's refit rather than ahead of it.
  >
  > `trend_model` means three things. It is a constructor when passed
  > (`sim_mvgam(trend_model = AR())`), a character type name on a
  > trend spec, and a `brmsfit` on the fitted object: `class(
  > fit$trend_model)` is `"brmsfit"`, the trend-side prefit, across
  > 74 read sites. `tidier_methods.R:170` reaches
  > `x$trend_model$trend_model`, which is the collision in one
  > expression. `trend_prefit` is the name that describes the slot.
  >
  > `process_error` still means two things. 13.0 gave it the
  > innovation axis everywhere, but `log_lik()` keeps it as the
  > superseded spelling of the surface, where innovations are pinned
  > off. Back-compatibility is the reason and `incl_dynamics` is the
  > 1.x name that would serve instead.
  >
  > `trend_arg_metadata` names the trend-constructor arguments whose
  > values a fit stores, `trend_map` and `n_lv`, and
  > `mvgam_update_inheritance` names the top-level ones. Those are two
  > lists of one idea, an argument whose value the object can give
  > back, and nothing makes them agree. A constructor naming any other
  > out-of-scope variable, `AR(p = 1, gr = my_grouping)` say, still
  > fails on refit.
  >
  > Three Stan names still break the `_trend` suffix rule:
  > `time_dis`, `theta_features` and `varrho_inv`. `time_dis` reaches
  > into the compiled C++ signatures in `RcppExports.R`, and the
  > other two are parameters, so renaming them changes posterior
  > column names and every fit carrying a structured loadings prior
  > stops being readable. `N_free_Z` puts its qualifier before the
  > noun where every sibling puts it after.

- [ ] **5.2 The published site is not the site these sources describe**
  > Three separate faults, and only the first was visible from the
  > vignette sources alone.
  >
  > `data.Rmd`, `dfm.Rmd` and `mvgam_overview.Rmd` gate every chunk
  > on `params$EVAL`, which reads `NOT_CRAN`. The gate is right for
  > CRAN, where a vignette must not fit Stan models, but
  > `.github/workflows/pkgdown.yaml` sets `GITHUB_PAT` and
  > `R_KEEP_PKG_SOURCE` and never sets `NOT_CRAN`, so a CI build
  > renders all three as code listings with no results.
  >
  > The tracked `docs/` build appears to contradict that:
  > `mvgam_overview.html` carries real figures, so its chunks ran.
  > The gate predates it, having been in place since September 2024
  > against a build committed in February 2025. The explanation is
  > that `docs/` was built locally and committed, and an interactive
  > `devtools` session sets `NOT_CRAN` itself. The evidence that the
  > site is fine therefore comes from a build that never went
  > through the workflow, which is the same trap 5.0 records for
  > articles rendered against a stale installed package.
  >
  > Second, and worse: eight of the ten articles have no published
  > page at all. `docs/articles/` holds `data_in_mvgam`,
  > `nmixtures`, `shared_states`, `time_varying_effects` and
  > `trend_formulas`, none of which still exists as a source, and
  > lacks `data`, `dfm`, `var`, `hierarchical_var`, `mvbf`,
  > `jsdgam`, `idm` and `nmix`, all of which do. `_pkgdown.yml`
  > names every one of the missing ones in its `articles:` section,
  > so the article index links to pages that are not there.
  >
  > Third, two workflow settings keep it that way. The deploy step
  > passes `clean: false`, so the five renamed pages stay on
  > `gh-pages` with nothing to remove them, and
  > `build_site_github_pages(lazy = TRUE)` skips any article whose
  > HTML is newer than its source, which is exactly the stale page
  > that needs rebuilding.
  >
  > So this is not the one-line change it was written as. It needs
  > `NOT_CRAN: true`, `clean: true` on the deploy or an explicit
  > purge of the five dead pages, and a decision on `lazy`. Sequence
  > it with 5.0: turning the gate on now would publish articles
  > built on sampling that 14.0 changes.
  >
  > Unverified, worth checking during 5.0: the workflow pins
  > `pkgdown` to 2.0.9, released in 2022, while `_pkgdown.yml` now
  > carries a full `reference:` and `articles:` structure.
  >
  > The manual points both ways across the same gap.
  > `man/mvgam_use_cases.Rd` links to `shared_states`,
  > `time_varying_effects` and `trend_formulas`, which resolve only
  > because the published site is the January build and have no
  > source in `vignettes/`; they die when the site is rebuilt.
  > Meanwhile `man/ZMVN.Rd`, `man/jsdgam.Rd`, `man/mvgam.Rd`,
  > `man/irf.mvgam.Rd`, `man/fevd.mvgam.Rd`, `man/nmix.Rd` and
  > others link to `idm`, `jsdgam`, `mvbf`, `nmix` and `var`, which
  > have source and no published page, so they 404 today and the
  > rebuild fixes them. Decide what the first three should point at
  > before rebuilding, since afterwards nothing will resolve them.
  >
  > 5.1 is why this matters. The one article whose chunks had never
  > executed was hiding two package bugs and four false claims. All
  > three gated vignettes have since been rendered with the gate
  > open and each carried something: `data.Rmd` overstated a claim
  > the validator does not support, which is 5.3; `dfm.Rmd` drew a
  > conclusion its own scores contradict; `mvgam_overview.Rmd`
  > displays priors the model does not use and builds its comparison
  > on a simulation that cannot be recovered, which is 5.4.

- [ ] **5.0 Rebuild every vignette and the pkgdown site**
  > Five articles still hold numbers from June and July caches:
  > `var.Rmd`, `hierarchical_var.Rmd`, `mvbf.Rmd`, `jsdgam.Rmd` and
  > `idm.Rmd`. Each carries one inline R expression, so every figure
  > in its prose is frozen text. `nmix.Rmd` (5.1) and
  > `forecast_evaluation.Rmd` (5.5) are done; `data.Rmd`, `dfm.Rmd`
  > and `mvgam_overview.Rmd` are rendered but still need the
  > workflow gate in 5.2.
  >
  > Per article, in order:
  >
  > 1. `R CMD INSTALL --preclean --no-multiarch .` first. See the
  >    rule below; skipping this invalidates everything after it.
  > 2. Re-run the builder in `tests/local/*_vignette_fits.R` where
  >    one exists. `idm_cache` and `var_cache` have no builder and
  >    need one written, or they cannot be regenerated from a clean
  >    clone.
  > 3. Render, then read the rendered output and check every prose
  >    claim against the block printed directly above it.
  > 4. Replace each quoted number with inline R computed from the
  >    object, so the next stale cache cannot go unnoticed.
  > 5. Check the sampler diagnostics the article prints, and say
  >    something about them if they are poor.
  > 6. `prose_lint.py`, then a writing pass.
  >
  > Two rules, both learned by getting them wrong.
  >
  > Install from HEAD before rendering. Every article calls
  > `library(mvgam)`, so it renders against the installed build
  > rather than the working tree, and the installed build carries
  > the same `2.0.0` version string as HEAD whatever its age.
  > Nothing warns. The build in place during 5.5 dated from 24 July
  > and still scored the marginal likelihood, so the article
  > re-rendered to its original numbers and read as consistent with
  > prose written against them. Refitting does not save you: the
  > builders use `load_all()`, but every `loo()`, `score()` and
  > `lfo_cv()` call inside the article goes through the installed
  > code.
  >
  > Do not retire a model on its forecast score. In 5.5 the energy
  > score said the VAR earned nothing over a per-species AR, and
  > that was read as a verdict on the model. The transition matrix
  > and the innovation covariance answer different questions and are
  > not equally identified: `A_trend` reached a bulk ESS of 26 where
  > the covariance reached 322, and the VAR recovered a residual
  > correlation of 0.58 between two congeneric pocket mice that an
  > AR cannot represent at any horizon. Wherever an article compares
  > models, check what the losing model estimates before dropping
  > it, and keep forecast accuracy and structural inference apart.

- [ ] **17.0 What `R CMD check --as-cran` reports**
  > The check reports no WARNING and one actionable NOTE:
  > `predict(type = "variance")` answers, `ev` is declared in the
  > single `globalVariables()` block, and `Rplots.pdf` stays out of
  > the tarball. What remains is the
  > non-portable compiler flag, which comes from the local
  > Makevars rather than the package, and the URL NOTE.
  >
  > Two things the run turned up that are worth more than the
  > NOTEs.
  >
  > `--run-donttest` fits Stan models. There are 168 `mvgam()` or
  > `jsdgam()` calls across 59 help files, 55 of them inside
  > `\donttest{}`, and CRAN runs those blocks. The check has been
  > sampling for over an hour without reaching the end of them.
  > This wants a decision before submission: move the fits behind
  > `\dontrun{}`, cut them to a handful of tiny ones, or accept a
  > check time that CRAN may refuse.
  >
  > Five of the flagged URLs are
  > `nicholasjclark.github.io/mvgam/articles/{idm,jsdgam,mvbf,nmix,var}.html`,
  > returning 404 from `man/ZMVN.Rd`, `man/jsdgam.Rd`,
  > `man/mvgam.Rd`, `man/irf.mvgam.Rd`, `man/fevd.mvgam.Rd` and
  > others. Those articles have no published page, which is 5.2
  > read from the manual rather than from the site. The remaining
  > three are handled: both GitHub redirects in `man/occ.Rd` point
  > at `ecoverseR` after confirming the 301s, and the Stan forum
  > thread in `man/mvgam_use_cases.Rd` is removed, having 404'd on
  > both its id and its slug.

- [ ] **19.0 The family scales have the defect 15.6 closed for the kernel**
  > `Psi`, the residual scale of `mvn()` and `mvt()`, and `nu`, the
  > mvt degrees of freedom, are emitted as literals in
  > `R/families.R`. `prior_summary()` lists them, tagged
  > `source = "mvgam"`; `get_prior()` does not; and a prior set on
  > either is refused by brms with a message naming
  > `default_prior()`, which is where the class name was read.
  > Verified on `mvn()` with `ZMVN(n_lv = 2)`: `Psi ~
  > exponential(1);` is in the program, `Psi` is absent from
  > `get_prior()`, and `prior(exponential(5), class = Psi)` is
  > refused.
  >
  > Harder than the kernel case. The family stanvar builders reach
  > `Psi` through `prepare_closure_unit_family()`, which dispatches
  > on family name and passes only `arrays`, so making the prior
  > settable means threading one through nine branches. `nu` is
  > harder still: its prior is written
  > `target += gamma_lpdf(nu - 2 | 2, 0.1)`, and the shift is the
  > model rather than a formatting choice, since the mvt has no
  > finite variance below 2. A plain `nu ~ dist(args)` would drop
  > it, so `nu` needs the shift kept and the distribution made
  > settable around it, or an argument on the family.
  >
  > The declaration and prior are written once now:
  > `make_psi_stanvars()` replaces two byte-identical blocks that
  > differed only in a stanvar name prefix.

- [ ] **18.0 mvgam's own priors are unnormalised, and the guard cannot see it**
  > `bridge_sampler.mvgam()` rejects a program containing `_lupdf`
  > or `_lupmf`, matching `brms:::is_normalized()`, and says
  > mvgam's emitters produce normalised Stan. Stan drops the
  > normalising constant for any `~` statement, and every prior
  > mvgam emits is written that way, while brms writes its own as
  > `lprior += student_t_lpdf(...)`. The guard tests a spelling
  > mvgam never uses, so it passes on a program that is not
  > normalised.
  >
  > A single marginal likelihood is then offset by a fixed amount.
  > A Bayes factor is wrong only when the two models' trend priors
  > differ in family, hyperparameters or dimension, since otherwise
  > the offsets cancel. So comparisons across trend structures, the
  > interesting ones, are the ones that break.
  >
  > Scoped: 22 emission sites, about 30 Stan lines, covering every
  > trend type, the loadings and MGP paths and the `mvn()` and
  > `mvt()` families. `normalize` reaches none of them. It stops at
  > the two `setup_brms_lightweight()` calls that build the brms
  > skeleton; the trend path runs through
  > `generate_combined_stancode()` and
  > `generate_trend_specific_stanvars()`, neither of which takes
  > it. Confirmed by diffing `stancode()` at both settings: only
  > two lines move, both brms-native. Threading it touches roughly
  > 18 signatures.
  >
  > `stan_density_suffix()` is not the tool. It preserves a
  > spelling brms already chose, which is right for the rewrites in
  > `glm_analysis.R` and wrong here, where mvgam authors the
  > statement and picks the suffix from `normalize` itself. This
  > wants a statement builder.
  >
  > Posterior draws do not move: adding a parameter-independent
  > constant leaves every gradient and accept step unchanged. So no
  > fixture needs refitting, and `loo()`, predictions and forecasts
  > are unaffected. Only `lp__` shifts, and only under
  > `normalize = TRUE`. About a dozen tests assert the literal `~`
  > spelling and would need rewriting.
  >
  > Two things to fix in the same pass. `mvgam_stancode_prior_rows()`
  > reads the `target +=` form with a regex that matches a bare
  > identifier only, so it would stop reporting `to_vector(Z)` and
  > `varrho_inv[1]` once those move; its `~` sibling already
  > handles containers and indices. And the guard should test what
  > it means rather than one spelling, reusing
  > `is_mvgam_managed_class()` so it and the prior table cannot
  > disagree about what counts as a prior.

- [ ] **20.0 ZMVN and CAR forecasts drop the trend linear predictor**
  > What is left of a six-part forecasting audit. The other five
  > are done: the forecast grid is sorted so `score()` cannot pair
  > a truth with the wrong horizon; a gap between the training end
  > and the forecast times is refused rather than answered as a
  > shorter horizon; `ensemble()` takes one draw selection across
  > every series and both arms; `score(log = TRUE)` errors on the
  > five scorers that cannot honour it; and `clean_ll()` seeds its
  > refill and says how many values it replaced.
  >
  > `propagate_zmvn()` and `propagate_car()` take no `linpreds`,
  > though `propagate_one_draw()` builds and passes one, so a
  > `trend_formula` carrying covariates forecasts as though every
  > trend coefficient were zero. `jsdgam()`'s `factor_formula`
  > maps onto a ZMVN `trend_formula`, so this is not a corner.
  > Reported by the auditor and not yet reproduced here; no
  > fixture pairs either trend with a non-trivial
  > `trend_formula`, which is why the suite cannot see it.
  >
  > `lfo_sum_rows()` also has two NA conventions: `log p = 0` in
  > the multi-column branch, and a dropped draw in the
  > single-column branch, which then recycles against the full
  > weight vector. Same audit, not yet reproduced here.

- [ ] **23.0 Two local test files do not pass**
  > Neither failure comes from the simulator change. Reverting
  > `R/sim_mvgam.R` to `HEAD` and rerunning gives the same counts,
  > so both predate it.
  >
  > `tests/local/test-sim-mvgam-recovery.R` fails 9 of 9 with no
  > passes: it calls `mvgam(trend_model = ...)`, an argument 2.0
  > removed, so `reject_removed_args()` stops every case before it
  > fits. The recovery coverage it claims is not running. It
  > exercises `type = 1` and `type = 7`, the two recipes whose
  > variance the rescale now governs, so it is worth repairing
  > rather than deleting.
  >
  > `tests/local/test-empty-obs-formula.R` fails 3 of 13, the same
  > 3 either side of the simulator change. Two are median quantile
  > residuals of 0.92 and 1.23 against a threshold of 0.5, on a
  > model deliberately misspecified as `AR(p = 1)` against
  > random-walk data at 30 timepoints. Raising `prop_trend` to
  > 0.95 does not bring them under the threshold either, so the
  > threshold is not tracking trend dominance and something else
  > sets the residual level. The third is a placeholder-name
  > assertion. Diagnose before adjusting: a threshold moved to fit
  > the observed number tests nothing.
  >
  > Neither file runs in CI, and the local suite has no runner
  > that sweeps every file, so nothing reports a stale one.

- [ ] **24.0 What the loadings basis still touches**
  > `residual_cor()`, `shared_variation()`, `active_factors()` and
  > `sample_innovations()` combined factor loadings with a
  > covariance, and all four read the QR-identified `Z_tilde`
  > while the covariance is stated for the `Z` the model sampled.
  > `Z Z'` is invariant to that rotation, which is why it went
  > unseen until a scale sat between the loadings. All four now
  > take `basis = "model"`; reporting and plotting keep
  > `"identified"`, and a fixed `trend_map` has no rotation so
  > both agree.
  >
  > What is left is the pair that were judged rather than fixed.
  > `compare_loadings()` Procrustes-aligns median `Z_tilde` with no
  > scale, so under shrinkage a near-dead column weighs the same as
  > the dominant one: on a ten-factor fit the raw column norms sit
  > flat around 41 while the scales run 2.06 down to 0.013.
  > Defensible if the intent is to compare loading patterns, wrong
  > if it is to compare what the fits say. Decide which, and say so
  > in the roxygen either way. `ordinate()` needs nothing: it pairs
  > `lv_trend_tilde` with `Z_tilde`, so the scale is already in the
  > site scores, confirmed to 6.8e-07 against the stored trend.

- [ ] **25.0 One fixture stores a Cholesky factor as a covariance**
  > `tests/local/fixtures/val_jsdgam_trait.rds` was built when the
  > generator emitted
  > `Sigma_trend = diag_pre_multiply(sigma_trend, L_Omega_trend)`,
  > so its stored `Sigma_trend` is the Cholesky factor rather than
  > the covariance: not symmetric, not positive semi-definite.
  > Current codegen emits
  > `multiply_lower_tri_self_transpose(...)`, so only the fixture
  > is stale. Rebuilding it moves its agreement with a simulated
  > truth from 0.80 to 0.999982.
  >
  > The rule the fixture teaches is worth keeping either way: read
  > `sigma_trend` and `L_Omega_trend` and rebuild, rather than
  > reading `Sigma_trend[i, j]` off the posterior of a
  > cholesky-scaled trend. `extract_cov_draws_flat()` already does
  > that.

- [ ] **26.0 No `VAR(n_lv)` or `RW(n_lv)` fixture exists**
  > Every factor fixture is `ZMVN` or `AR(p = 1)`. The latent
  > covariance for a factor VAR reads the leading block of
  > `Omega_trend`, and a factor RW has no stationary law so it
  > falls back to the innovation covariance; both were read off
  > generated Stan rather than confirmed against a fit. Two small
  > fixtures would close that, and they are the two cases where
  > `residual_cor()` on a factor model is least exercised.

- [ ] **6.0 Final release verification**
  > Clean `document()`, clean test sweep, `R CMD check --as-cran`,
  > tarball under the size limit, sweep green.

## Behaviour confirmed, worth documenting

`posterior_epred()` and `posterior_predict()` default to
`process_error = FALSE` and `incl_autocor = FALSE`, so the trend
contributes its deterministic submodel and the answer is the same on
every call. Passing the training data back as `newdata` is an exact
no-op. Under `process_error = TRUE` innovations are drawn afresh, so
two calls on one fit and the same `draw_ids` differ, by as much as
160 units on a cached Poisson fit; reproducible output there needs a
seed.
