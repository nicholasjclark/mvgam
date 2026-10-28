#' A link to an article on the package website
#'
#' Roxygen calls this in inline R code, as
#' `` `r mvgam_article("var", "the VAR article")` ``. The site's address
#' is written here once. A name outside the site's articles fails the
#' documentation build.
#'
#' @param name The article's file name, without its extension
#' @param text The link text
#' @return A Markdown link
#' @noRd
mvgam_article <- function(name, text) {
  checkmate::assert_choice(name, c(
    "mvgam_overview", "data", "dfm", "forecast_evaluation",
    "hierarchical_var", "idm", "jsdgam", "mvbf", "nmix", "var"
  ))
  checkmate::assert_string(text, min.chars = 1L)
  # The 2.0 site builds into /dev/ and the CRAN 1.x site holds the root.
  # The address moves to the root when 2.0 reaches CRAN, with the
  # site's development mode.
  paste0("[", text,
         "](https://nicholasjclark.github.io/mvgam/dev/articles/",
         name, ".html)")
}
