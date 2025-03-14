# Vignette names
vignettes <- list.files('./vignettes', pattern = '.Rmd')

# Generate R script versions of vignettes
purl_vignettes <- function(x) {
  # Generate the R script version
  knitr::purl(
    input = paste0('vignettes/', x),
    output = paste0('doc/', sub('.Rmd', '.R', x))
  )

  # Copy this version to inst/doc
  file.copy(
    from = paste0('doc/', sub('.Rmd', '.R', x)),
    to = paste0('inst/doc/', sub('.Rmd', '.R', x)),
    overwrite = TRUE
  )
}

# Build vignette htmls
build_vignettes = function(x) {
  # Build the vignette html file
  devtools::build_rmd(
    paste0('vignettes/', x)
  )

  # Copy the .Rmd to doc
  file.copy(
    from = paste0('vignettes/', x),
    to = paste0('doc/', x),
    overwrite = TRUE
  )

  # Copy the .Rmd to inst/doc
  file.copy(
    from = paste0('vignettes/', x),
    to = paste0('inst/doc/', x),
    overwrite = TRUE
  )

  # Copy the .html to inst/doc
  file.copy(
    from = sub('.Rmd', '.html', paste0('vignettes/', x)),
    to = paste0('inst/doc/', sub('.Rmd', '.html', x)),
    overwrite = TRUE
  )

  # Copy the .html to doc
  file.copy(
    from = sub('.Rmd', '.html', paste0('vignettes/', x)),
    to = paste0('doc/', sub('.Rmd', '.html', x)),
    overwrite = TRUE
  )

  # Remove the .html
  file.remove(sub('.Rmd', '.html', paste0('vignettes/', x)))
}

# Apply these functions to all vignette .Rmds
lapply(vignettes, purl_vignettes)
lapply(vignettes, build_vignettes)
