#' Observation families supported by mvgam
#'
#' @description
#' `mvgam()` and `jsdgam()` accept the observation families that
#' \pkg{brms} and \pkg{stats} define, together with a set that
#' \pkg{mvgam} adds for data that ecological and epidemiological
#' series tend to produce: counts observed with imperfect detection,
#' compositions that sum to one, and counts whose dispersion a
#' negative binomial cannot reach.
#'
#' Pass a family the same way you would in `brms::brm()` or
#' `stats::glm()`, for example
#' `mvgam(y ~ x, trend_formula = ~ AR(p = 1), family = nmix())`.
#'
#' @section Families mvgam adds:
#' Each fixes its link, and each takes the arguments listed here
#' rather than a `link` argument.
#'
#' \tabular{lll}{
#'   \strong{Family} \tab \strong{Link} \tab \strong{Arguments} \cr
#'   [tweedie()] \tab log \tab `M`, the truncation point of the
#'     compound Poisson sum \cr
#'   [beta_nb()] \tab log \tab none \cr
#'   [com_binomial()] \tab logit \tab `link`, which accepts
#'     `"logit"` only \cr
#'   [nmix()] \tab log (state), logit (detection) \tab `type`, one of
#'     `"poisson_binomial"`, `"royle_nichols"` or
#'     `"poisson_poisson"`; `multi_season` \cr
#'   [occ()] \tab logit (state and detection) \tab `multi_season` \cr
#'   [diri()] \tab identity \tab none \cr
#'   [multi()] \tab identity \tab none \cr
#'   [categ()] \tab identity \tab none \cr
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
#'     [brms::weibull()], [brms::frechet()],
#'     [stats::inverse.gaussian()], and [tweedie()] when the data
#'     are continuous but carry a point mass at zero.}
#'   \item{A single proportion on `(0, 1)`}{[Beta()], with
#'     [brms::zero_inflated_beta()] or
#'     [brms::zero_one_inflated_beta()] when the bounds are
#'     attainable.}
#'   \item{A vector of proportions summing to one}{[diri()] for a
#'     Dirichlet, [multi()] for a multinomial, [categ()] for a
#'     categorical response.}
#'   \item{Binary}{[brms::bernoulli()].}
#'   \item{Counts out of a known number of trials}{
#'     [stats::binomial()], [brms::beta_binomial()] when trials are
#'     overdispersed, and [com_binomial()] when they are either
#'     under- or over-dispersed.}
#'   \item{Unbounded counts}{[stats::poisson()],
#'     [brms::negbinomial()], [brms::geometric()], their
#'     zero-inflated and hurdle counterparts (see
#'     [brms::brmsfamily()]), and [beta_nb()] for tails heavier
#'     than a negative binomial reaches.}
#'   \item{Repeat visits to a site}{[nmix()] estimates latent
#'     abundance from repeated counts and [occ()] estimates latent
#'     occupancy from repeated detection histories.}
#'   \item{Several responses measured together}{[mvn()] and [mvt()],
#'     whose residual correlations are informed by a latent factor
#'     structure.}
#'   \item{Ordered}{[brms::cumulative()], [brms::sratio()],
#'     [brms::cratio()] and [brms::acat()].}
#' }
#'
#' @section Link functions:
#' A \pkg{brms} or \pkg{stats} family takes a `link` argument as
#' usual, so `poisson(link = "sqrt")` and `Gamma(link = "log")` both
#' work. The families in the table above fix their link, because the
#' Stan code mvgam generates for them is written against that scale.
#'
#' @section Data layout:
#' [nmix()] and [occ()] are closure-unit families: they expect
#' several rows per closure unit, one per visit, and model the state
#' and the detection process separately. `bf()` supplies the second
#' linear predictor, as in `bf(y ~ elev, p ~ tod)`. Use
#' [sim_closure_unit_data()] to see the layout they want, and
#' [pivot_detection_array()] to reshape a site-by-visit array into
#' it. [diri()], [multi()], [categ()], [mvn()] and [mvt()] also read
#' several rows per unit, one per response category.
#'
#' @section What the family changes after fitting:
#' A closure-unit family adds `predict(type = "latent_state")` and
#' `predict(type = "detection")`, supports `pp_check(type =
#' "fit_stat")`, and restricts the `pp_check()` types that work per
#' row. Families on a discrete support are drawn from with the
#' integer sampler, which [posterior_predict.mvgam()] selects on its
#' own.
#'
#' `brms::dirichlet()`, `brms::multinomial()`, `brms::categorical()`
#' and `brms::logistic_normal()` are refused with a message pointing
#' at [diri()], [multi()], [categ()] and [mvn()], which fit the same
#' responses through mvgam's long-format layout.
#'
#' @examples
#' # Families are constructed, then passed to `family`.
#' nmix(type = "royle_nichols")
#' com_binomial()
#' tweedie(M = 40L)
#'
#' # A brms or stats family still takes its own link.
#' poisson(link = "sqrt")
#'
#' @seealso [mvgam()], [jsdgam()], [brms::brmsfamily()],
#'   [get_prior()], [posterior_predict.mvgam()],
#'   [sim_closure_unit_data()]
#'
#' @name mvgam_families
#' @rdname mvgam_families
NULL
