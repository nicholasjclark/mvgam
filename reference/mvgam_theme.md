# The ggplot theme mvgam draws with

Every figure the package draws uses this theme, including the figures it
draws through bayesplot. It extends
[`ggplot2::theme_classic()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).
Apply it to a plot of your own to give it the look of mvgam output.

mvgam's figures take their colours from the `"red"` scheme of bayesplot,
which `bayesplot::color_scheme_get("red")` returns. The example colours
its points with it.

## Usage

``` r
mvgam_theme(base_size = 11, base_family = "")
```

## Arguments

- base_size:

  Base font size in points. Defaults to `11`, matching
  [`ggplot2::theme_classic()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

- base_family:

  Base font family. Defaults to `""`, the ggplot2 default.

## Value

A
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
object.

## See also

[`bayesplot::color_scheme_get()`](https://mc-stan.org/bayesplot/reference/bayesplot-colors.html)

## Author

Nicholas J Clark

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(colour = bayesplot::color_scheme_get("red")$dark) +
  mvgam_theme()

```
