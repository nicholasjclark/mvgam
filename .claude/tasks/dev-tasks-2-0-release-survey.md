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

- [ ] **14.0 One column, one scale, everywhere**
  > A latent factor column carries a loadings scale and an
  > innovation scale, and only their product reaches the
  > likelihood. Fixed for `column_shrinkage = "mgp"`, where
  > `sigma_trend` is now derived as `sqrt(Psi_diag)` and the
  > loadings prior draws `Z` at unit scale. Every other factor path
  > still has it: `RW()`, `AR()` and `ZMVN()` factor models, and the
  > jsdgam default, `"iid"` and distances-only routes, all sample a
  > free `Z` against a free `sigma_trend`.
  >
  > Measured on `AR(p = 1, n_lv = 2)`, four series, 40 timepoints:
  > `Z[1,2]` at bulk ESS 6.1 and R-hat 1.27, `sigma_trend[1]` at
  > 21.5, while the scale- and rotation-invariant
  > `sum_k sigma_trend[k]^2 * ||Z_tilde[, k]||^2` reaches 202. The
  > pathology sits in the split, not in the quantity.
  >
  > The QR step fixes rotation for reporting but runs in generated
  > quantities, so it does nothing for the sampler's geometry along
  > the scale direction. Only the priors pin the split.
  >
  > Apply the device the MGP branch now uses: give `Z` a fixed unit
  > scale and let `sigma_trend` carry the column magnitude alone.
  > The kernel routes need it too, since `Phi` has unit diagonal and
  > so contributes no scale of its own.

- [x] **14.1 VAR's E-BFMI warning does not reproduce**
  > A sweep reported E-BFMI of 0.295 on a `VAR()` fit, below Stan's
  > 0.3 threshold, and read the centred latent state as the cause.
  > Two parameterisations were implemented against it and both are
  > reverted. Four seeds, same data and settings, medians:
  >
  > | | E-BFMI | divergences | max R-hat | min ESS |
  > |---|---|---|---|---|
  > | centred | 0.416 | 4.5 | 1.17 | 5.2 |
  > | init-state non-centred | 0.438 | 7.0 | 1.17 | 12.7 |
  >
  > Neither the difference nor the original finding survives. Across
  > those seeds the centred form's lowest E-BFMI was 0.343, and no
  > run fell below 0.3; the 0.295 that prompted this came from one
  > draw. Within-variant spread swamps the between-variant
  > difference: the non-centred initial state produced both the best
  > run of the eight, at no divergences and 78 effective draws, and
  > the worst, at 57 divergences and an E-BFMI of 0.256.
  >
  > Non-centring the whole recursion was tried first and is much
  > worse: max R-hat 2.125 against 1.090, one effective draw against
  > 24, because the raw draws then couple to `A_trend` at every step.
  > That is a real effect and reproduced, unlike the warning it was
  > meant to cure.
  >
  > The comment at the declaration, that `lv_trend` belongs in the
  > parameters block, records a choice the evidence supports. Left
  > as it stands.
  >
  > Worth keeping from this: a single fit is not evidence about
  > sampler geometry. Three of the numbers that drove this entry,
  > the E-BFMI, the divergence count and the ESS, each moved further
  > across seeds than between parameterisations.

- [x] **14.2 `VAR(n_lv = k)` does not compile**
  > `R/stan_assembly.R:3307` declares
  > `array[size(A_trend)] matrix[N_lv_trend, N_lv_trend] A_trend_tilde;`
  > in generated quantities. Stan rejects a non-data expression as a
  > top-level array size; `N_lags_trend` is already a data integer
  > and is what was meant. Every VAR and VARMA factor model fails at
  > `stanc`, so a capability the registry advertises
  > (`supports_factors = TRUE` for VAR) has never worked.
  >
  > Reproduced identically before and after 14.0's fix, so it is not
  > a regression from it. Blocks testing 14.0 and 14.1 together.

- [x] **15.0 The prior a model reports is not always the prior it samples**
  > Decision 3.1 fixed this for `sigma_trend` on the fitted object.
  > The same shape is still live on `Z`, which carries the whole
  > factor and JSDM story. One registry entry, three emission sites,
  > four strings:
  >
  > | path | reported | sampled |
  > |---|---|---|
  > | free factor `n_lv` | `student_t(3, 0, 0.5)` | agrees |
  > | MGP | `student_t(3, 0, 0.5)` | `std_normal()` |
  > | partial `trend_map` | `student_t(3, 0, 0.5)` | `student_t(3, 0, 1)` |
  > | kernel | `student_t(3, 0, 0.5)` | `multi_normal_cholesky` |
  >
  > `get_prior.mvgam_formula()` takes neither `loadings_prior` nor
  > `trend_map`, so it cannot know which branch will fire; passing
  > either is swallowed by the dots and the table is unchanged.
  >
  > Do 15.1 first: it supplies the predicate this needs.

- [x] **15.1 Which classes belong to mvgam, written four times**
  > `filter_obs_priors()` and `filter_trend_priors()` (R/priors.R:763,
  > 793) test a bare `_trend$` regex;
  > `get_all_mvgam_trend_parameters()` (R/priors.R:809) builds a list;
  > `mvgam_unsuffixed_params` (R/brms_integration.R:645) carries the
  > regex plus an exception list; `suffix_trend_prior_classes()`
  > (R/priors.R:1034) adds a `sigma`-with-empty-coef special case.
  >
  > They disagree exactly on `Z`, which has no `_trend` suffix. A
  > user prior on `Z` is filed as observation-side, handed to brms,
  > and rejected with a message pointing them at `default_prior()`,
  > which is where they read the class name. `Psi` is routed the
  > same way.
  >
  > One predicate built from `mvgam_unsuffixed_params` plus the
  > suffix rule, read by both filters, the suffixer and
  > `mvgam_stancode_prior_rows()`.

- [x] **15.2 `PW()` advertises a parameter it does not sample**
  > `get_prior()` on a `PW()` spec lists `sigma_trend`. The emitted
  > program contains no such parameter, and
  > `prior(exponential(9), class = sigma_trend)` produces neither an
  > error nor a `9` anywhere in the Stan. The sibling of the MGP case
  > 14.0 fixed, except that one now refuses and this one is silent.
  >
  > There were two instances, not one. Multiplicative gamma process
  > shrinkage does the same thing from the other direction: it
  > derives the scale as `sqrt(Psi_diag)`, and the emitter already
  > refused a user prior on it, so the table was offering a row
  > `mvgam()` would reject.
  >
  > The innovation scale is now a property of the trend
  > registration, and `samples_innovation_scale()` combines it with
  > the shrinkage case so both instances have one answer. The catch
  > was that `monitor_params` is computed when the trend object is
  > built, before any loadings prior exists, so a cache taken
  > beforehand names a parameter the attached spec takes away.
  > `attach_loadings_spec_to_trend()` refreshes it at the attach
  > point rather than patching the one consumer that noticed.
  >
  > `get_all_mvgam_trend_parameters()` still names both
  > unconditionally, deliberately. It decides what must never reach
  > brms, which is a different question from what a model samples,
  > and dropping a name there would send `sigma_trend` to brms as an
  > observation-side `sigma`.

- [x] **15.3 AR bounds stated three times, with two values**
  > The registry says `c(-1, 1)` (R/priors.R:64), the type fallback
  > says `[-1, 1]` (R/priors.R:521), and `get_ar_parameter_prior()`
  > (R/priors.R:593) says `[-0.99, 0.99]`, which is what the user is
  > shown. Stan declares `vector<lower=-1,upper=1>`. Nothing reads
  > the tightened bound. `get_car_parameter_prior()` is the same
  > shape and does agree with its declaration, so the pattern was
  > right and AR was the outlier. The resolver is deleted; both
  > remaining statements already said what Stan declares.
  >
  > Checked by sweeping every reported bound against its emitted
  > declaration across six trend configurations rather than the one
  > parameter named here, which was worth doing twice over: it
  > confirmed AR was the only mismatch, and it cleared a suspicion
  > formed by reading, that `theta1_trend` had the same defect
  > through a `[0, 1]` type fallback. A resolver catches it first,
  > so the fallback branch never fires for it. That sweep is now a
  > contract test, since the next drift will be somewhere else.

- [x] **15.4 `get_prior(fit)` and `prior_summary(fit)` disagree**
  > Both document themselves as returning the table the model was
  > fitted with; both re-derive. `get_prior()` drops the
  > stanvar-lifted rows and invents a `b_trend` the fit never
  > sampled, because the no-trend-predictors guard tests one
  > spelling: `~ -1 + AR(p = 1)` is not `~ 0`, so brms is asked and
  > returns a `b` row. `~ 0 + AR()` and `~ AR()` do not.
  >
  > The guard also read `!all.equal(...) == TRUE`, which worked only
  > because `all.equal` happened to return length one here.
  >
  > Both are gone. `formula_has_population_terms()` reads the terms
  > object, so every spelling of an empty predictor answers the same
  > way and an intercept still counts, leaving `~ 1` to report its
  > `Intercept_trend`. It replaced an inline copy in
  > `R/brms_integration.R` as well. `has_obs_intercept()` stays
  > separate: it asks only about the intercept.
  >
  > What remains of this entry is the second half of its title. The
  > two surfaces still re-derive rather than reading one table, and
  > that is worth closing once `prior_summary()` is touched again.

- [x] **15.5 The jsdgam prior surface is unreachable**
  > Closed with 15.0: `get_prior.mvgam_formula()` takes
  > `loadings_prior` and attaches the spec the way the fit path
  > does, so a structured-loadings model reports the prior it
  > samples. A `trend_map` is refused rather than answered, because
  > fixed loadings move `Z` into the data block and partial ones
  > replace it with `Z_free_vec`; the free-loadings table has the
  > wrong rows for both, not merely the wrong values. The refusal
  > names `stancode()`, which does take it.

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

- [x] **15.6 Length-scale priors cannot be set**
  > `theta_features` and each `theta_dist_<name>` were emitted as
  > literal `lognormal(0, 1)` targets with no registry entry, so
  > `prior_summary()` reported a row `get_prior()` never offered
  > and a prior set on one was refused. For a phylogenetic kernel
  > that length-scale is a modelling choice, not a nuisance.
  >
  > They now resolve through the chain that also builds the
  > reported table, so an override reaches the program per distance
  > source. Which length-scales exist is derived from the loadings
  > spec rather than listed again, so a model cannot report one it
  > has no distance source for.
  >
  > The claim that the `normal(0, 1)` default matches Heaps'
  > practice is left standing but is unverified here; the paper was
  > not consulted. The default now matters less, since it can be
  > overridden, but the citation should be checked against the
  > source before release.

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
  > - [ ] `ZMVN()` on a single series makes `sigma_trend^2 +
  >   sigma^2` an exact sum with nothing to split it. Applies only
  >   to families carrying a residual scale, since a Poisson has no
  >   `sigma` for the trend to trade against. A validator question
  >   rather than a code-generation one.
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
  > three are a dead Stan forum thread in
  > `man/mvgam_use_cases.Rd` and two GitHub redirects in
  > `man/occ.Rd`, all fixable now.

- [ ] **18.0 mvgam's own priors are unnormalised, and the guard cannot see it**
  > `bridge_sampler.mvgam()` rejects a program containing `_lupdf`
  > or `_lupmf`, matching `brms:::is_normalized()`, and its message
  > says mvgam's shipped emitters produce normalised Stan. Neither
  > half holds. Stan drops the normalising constant for any `~`
  > statement, and every prior mvgam emits is written that way,
  > while brms writes its own as `lprior += student_t_lpdf(...)`.
  > On `~ AR(p = 1, cor = TRUE)` with `normalize = TRUE`:
  >
  > | statement | constants |
  > |---|---|
  > | `lprior += student_t_lpdf(Intercept \| 3, 1.4, 2.5);` | kept |
  > | `sigma_trend ~ exponential(2);` | dropped |
  > | `L_Omega_trend ~ lkj_corr_cholesky(2);` | dropped |
  > | `ar1_trend ~ normal(0, 0.5);` | dropped |
  >
  > The guard finds no `_lupdf`, passes, and bridge sampling then
  > runs on a log density missing each trend prior's constant. A
  > single marginal likelihood is offset by a fixed amount, which
  > is harmless on its own; a Bayes factor between two models whose
  > trend priors differ is not, because the offsets do not cancel.
  >
  > `normalize = FALSE` is equally unheard: brms switches to
  > `_lupdf` and the mvgam statements do not change, because there
  > is nothing in a `~` statement to switch.
  >
  > The fix is to emit mvgam's priors the way brms does, choosing
  > the suffix from the `normalize` setting, and to have the guard
  > test what it means rather than one spelling of it. Sizeable,
  > and it changes `lp__` for every model, so it wants its own
  > verification pass rather than riding along with a prior fix.

- [ ] **20.0 Forecasting returns wrong numbers, five ways**
  > From an audit of `forecast`, `score`, `lfo_cv`, `kfold`, `loo`
  > and `ensemble`. The first two are confirmed here by execution;
  > the rest are the auditor's, verified by it and not yet by me.
  >
  > - [x] Row order of `newdata` permuted the truths and the
  >   forecast columns independently, so `score()` paired each
  >   truth with a horizon it did not belong to. `fc_times` was
  >   already sorted; the observations and the grid rows were not.
  > - [ ] The horizon is `nrow(newdata)`, not the distance from
  >   the last training time. Asking for t = 41:42 alone gives sd
  >   0.87 / 0.93 where the same times inside a t = 31:42 request
  >   give 1.02 / 1.00. Uncertainty is understated whenever the
  >   forecast rows do not start at the training boundary, and
  >   `lfo_cv()` hits it on every fold between refits, so every
  >   non-ELPD LFO score is optimistic. `compute_car_forecast_time()`
  >   already computes true gaps; every other trend re-derives the
  >   horizon as a row count.
  > - [ ] `propagate_zmvn()` and `propagate_car()` take no
  >   `linpreds`, so a `trend_formula` carrying covariates
  >   forecasts as though every trend coefficient were zero.
  >   `jsdgam()`'s `factor_formula` maps onto a ZMVN
  >   `trend_formula`, so this is not a corner.
  > - [ ] `ensemble()` samples rows independently per series and
  >   per arm, so cross-series dependence is destroyed: two
  >   perfectly correlated series came back at 0.017. Any joint
  >   score on an ensemble is wrong, and an ensemble scores worse
  >   than its own members on `energy` and `variogram`.
  > - [ ] `score(log = TRUE)` is forwarded to `crps`, `drps` and
  >   `sis` only. `logs`, `dss`, `qs` and `twcrps` return the
  >   unlogged score with nothing said. `interval_width` is
  >   dropped for those four plus `brier`, yet the column still
  >   echoes the value that governed nothing.
  > - [ ] `clean_ll()` replaces non-finite log-densities with
  >   resamples of the finite ones, unseeded, so `loo()` returns a
  >   different number on each call for any fit with one bad draw.
  >   `lfo_sum_rows()` treats a missing density as `log p = 0` in
  >   its multi-column branch and drops the draw in its
  >   single-column branch, which then recycles against the full
  >   weight vector.

- [ ] **21.0 `sim_mvgam()` does not draw the trend it reports**
  > `prop_trend` sets how much of the variance the trend should
  > carry. A random walk with two or more series carries far more:
  > measured across Monte Carlo replicates at `prop_trend = 0.5`,
  > `var(trend)` came to 2.52 at two series and 5.31 at twenty
  > against a target of 0.5. One series is correct.
  >
  > `is_nonstationary_trend()` classifies RW and sparse-lag
  > `AR(p = c(1, 12))` as non-stationary, which skips the rescale
  > every stationary trend receives, while the upfront correction
  > assumes a single series and the matrix is centred on one
  > global mean rather than per series. Sparse-lag AR is wrong at
  > one series too, since it reuses a scale calibrated for AR(1).
  >
  > This reaches `eta` and so the simulated `y`, not just a
  > metadata field, and these are the recipes the recovery checks
  > are built on.

- [ ] **22.0 `allow_new_levels` is documented as working**
  > `population_random_pred()` raises for any grouping level the
  > model has not seen, whatever `allow_new_levels` and
  > `sample_new_levels` say. That is a deliberate limitation with
  > tests pinning it, so it fails loudly rather than answering
  > wrongly. But `predict.R`, `fitted.R`, `posterior_predict.R`,
  > `posterior_epred.R` and `posterior_linpred.R` all describe the
  > argument as allowing predictions for new levels, and name
  > three sampling strategies for `sample_new_levels`, with no
  > caveat in any of the five.

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
