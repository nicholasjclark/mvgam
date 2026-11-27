# Knits the articles of the pkgdown site before the site build.
#
# Each article's source is <name>.Rmd.orig. Knitting it writes
# <name>.Rmd, which holds the code, its printed output and links to the
# figures in figures/, with no chunks left to run. pkgdown renders that
# file without evaluating it. The site build fits no models for the
# articles.
#
# Run from the package root with the package installed:
#   Rscript vignettes/articles/precompile.R            # every article
#   Rscript vignettes/articles/precompile.R var nmix   # named articles

Sys.setenv(NOT_CRAN = "true")
setwd("vignettes/articles")

sources <- sub("\\.Rmd\\.orig$", "", list.files(pattern = "\\.Rmd\\.orig$"))
requested <- commandArgs(trailingOnly = TRUE)
if (length(requested) == 0L) {
  requested <- sources
}
unknown <- setdiff(requested, sources)
if (length(unknown) > 0L) {
  stop("Each article needs a source <name>.Rmd.orig. Missing: ",
       paste(unknown, collapse = ", "), call. = FALSE)
}

for (name in requested) {
  message("Knitting ", name, " at ", format(Sys.time(), "%H:%M"))
  # The article's figures are deleted first. A figure the new knit does
  # not write would otherwise stay in figures/ unused.
  unlink(list.files("figures", pattern = paste0("^", name, "-"),
                    full.names = TRUE))
  # knitr picks its syntax from the file extension. The .orig extension
  # names none. The R Markdown patterns and hooks are set here instead.
  knitr::pat_md()
  knitr::render_markdown()
  # A figure with a caption is written as HTML with a visible caption,
  # as rmarkdown writes it for an HTML page. Plain markdown keeps the
  # caption only as alt text.
  knitr::opts_knit$set(rmarkdown.pandoc.to = "html")
  knitr::knit(paste0(name, ".Rmd.orig"), paste0(name, ".Rmd"),
              envir = new.env(), quiet = TRUE)
  knitr::knit_patterns$restore()
  knitr::knit_hooks$restore()
  # pkgdown evaluates an inline date in the header when it renders the
  # page. The knit date replaces it.
  out <- readLines(paste0(name, ".Rmd"))
  out <- sub("^date: \"`r Sys.Date\\(\\)`\"$",
             paste0("date: \"", format(Sys.Date()), "\""), out)
  writeLines(out, paste0(name, ".Rmd"))
}
message("Done at ", format(Sys.time(), "%H:%M"))
