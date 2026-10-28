#' Observation families supported by mvgam
#'
#' @description
#' `mvgam()` and `jsdgam()` accept the observation families that
#' \pkg{brms} and \pkg{stats} define. \pkg{mvgam} adds families for
#' data that ecological and epidemiological series tend to produce.
#' They cover counts observed with imperfect detection, compositions
#' and counts whose dispersion a negative binomial cannot reach.
#'
#' Pass a family as you would to `brms::brm()` or `stats::glm()`, for
#' example `mvgam(y ~ x, trend_formula = ~ AR(p = 1), family = nmix())`.
#' The `r mvgam_article("nmix", "N-mixture article")` fits [nmix()].
#' The `r mvgam_article("jsdgam", "JSDM article")` fits a joint species
#' distribution model.
#'
#' @section Families mvgam adds:
#' Each fixes its link. `com_binomial()` takes a `link` argument that
#' accepts `"logit"` alone. The table lists every argument.
#'
#' \tabular{lll}{
#'   \strong{Family} \tab \strong{Link} \tab \strong{Arguments} \cr
#'   [tweedie()] \tab log \tab `M`, the truncation point of the
#'     compound Poisson sum \cr
#'   [beta_nb()] \tab log \tab none \cr
#'   [com_binomial()] \tab logit \tab `link`, `"logit"` only \cr
#'   [nmix()] \tab log (state), logit (detection) \tab `type`, one of
#'     `"poisson_binomial"`, `"royle_nichols"` or
#'     `"poisson_poisson"`; `multi_season` \cr
#'   [occ()] \tab logit (state and detection) \tab `multi_season` \cr
#'   [diri()] \tab softmax over each unit's categories \tab none \cr
#'   [multi()] \tab softmax over each unit's categories \tab none \cr
#'   [categ()] \tab softmax over each unit's categories \tab none \cr
#'   [mvn()] \tab identity \tab none \cr
#'   [mvt()] \tab identity \tab none
#' }
#'
#' @section Choosing by the shape of the response:
#' \describe{
#'   \item{Real-valued}{[stats::gaussian()], [brms::student()],
#'     [brms::skew_normal()], [brms::exgaussian()],
#'     [brms::asym_laplace()] and [brms::von_mises()] for angles.}
#'   \item{Non-negative real-valued}{[stats::Gamma()],
#'     [brms::lognormal()], [brms::exponential()],
#'     [brms::weibull()], [brms::frechet()] and
#'     [stats::inverse.gaussian()]. [tweedie()] takes continuous data
#'     with a point mass at zero. With a trend, give these families
#'     `link = "log"`. Their mean must stay positive. Under the
#'     `identity`, `inverse` or `1/mu^2` link it stays positive only
#'     where the linear predictor is positive on every row. A latent
#'     trend is centred near zero and takes negative values, where the
#'     likelihood is undefined. `stats::Gamma()` defaults to the
#'     `inverse` link. `mvgam()` warns when a trend meets one of these
#'     links. `lognormal()` is the exception. Its `mu` is a log-scale
#'     location that takes any sign. `identity` is the right link for
#'     it.}
#'   \item{A single proportion on `(0, 1)`}{[Beta()], with
#'     [brms::zero_inflated_beta()] or
#'     [brms::zero_one_inflated_beta()] when the bounds are
#'     attainable.}
#'   \item{Compositions and choices among categories}{[diri()] for
#'     proportions summing to one, [multi()] for counts across
#'     categories and [categ()] for one chosen category per unit.}
#'   \item{Binary}{[brms::bernoulli()].}
#'   \item{Counts out of a known number of trials}{
#'     [stats::binomial()]. [brms::beta_binomial()] takes overdispersed
#'     trials. [com_binomial()] takes trials that are under- or
#'     over-dispersed.}
#'   \item{Unbounded counts}{[stats::poisson()],
#'     [brms::negbinomial()], [brms::geometric()] and their
#'     zero-inflated and hurdle counterparts (see
#'     [brms::brmsfamily()]). [beta_nb()] takes tails heavier than a
#'     negative binomial reaches.}
#'   \item{Repeat visits to a site}{[nmix()] estimates latent
#'     abundance from repeated counts. [occ()] estimates latent
#'     occupancy from repeated detection histories.}
#'   \item{Several responses measured together}{[mvn()] and [mvt()],
#'     whose residual correlations come from a latent factor
#'     structure.}
#'   \item{Ordered}{[brms::cumulative()], [brms::sratio()],
#'     [brms::cratio()] and [brms::acat()].}
#' }
#'
#' @section Link functions:
#' A \pkg{brms} or \pkg{stats} family takes a `link` argument as
#' usual. `poisson(link = "sqrt")` and `Gamma(link = "log")` both
#' work. The families mvgam adds each have the one link the table
#' gives.
#'
#' @section Data layout:
#' [nmix()] and [occ()] are closure-unit families. A closure unit is a
#' site, or a site in one season, whose true occupancy or abundance
#' holds across its repeat visits. The families expect several rows
#' per closure unit, one per visit. They model the state and the
#' detection process with separate linear predictors. `bf()` supplies
#' the second, as in `bf(y ~ elev, p ~ tod)`.
#' [sim_closure_unit_data()] simulates the layout they take.
#' [pivot_detection_array()] reshapes a site-by-visit array into it.
#' [diri()], [multi()], [categ()], [mvn()] and [mvt()] also take
#' several rows per unit, one per category or response.
#'
#' @section What the family changes after fitting:
#' A closure-unit family adds `predict(type = "latent_state")` and
#' `predict(type = "detection")`. It supports
#' `pp_check(type = "fit_stat")`. It refuses the `pp_check()` types
#' that assume one observation per row.
#'
#' `brms::dirichlet()`, `brms::multinomial()` and `brms::categorical()`
#' are refused with a pointer to [diri()], [multi()] and [categ()].
#' These fit the same responses in mvgam's long-format layout.
#' `brms::logistic_normal()` is refused with a pointer to [mvn()],
#' which fits the additive log-ratios of the proportions.
#'
#' @examples
#' # Families are constructed, then passed to `family`
#' nmix(type = "royle_nichols")
#' com_binomial()
#' tweedie(M = 40L)
#'
#' # A brms or stats family still takes its own link
#' poisson(link = "sqrt")
#'
#' # The layout occ() takes: one row per visit to each site
#' head(sim_closure_unit_data(family = occ())$data_train)
#'
#' # Next, get_prior() lists the priors a family's parameters take
#'
#' @seealso [mvgam()], [jsdgam()], [brms::brmsfamily()],
#'   [get_prior()], [posterior_predict.mvgam()],
#'   [sim_closure_unit_data()]
#'
#' @name mvgam_families
#' @rdname mvgam_families
#' @family observation families
NULL
