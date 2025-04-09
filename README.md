
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flexoki <img src="man/figures/logo.png" align="right" height="123" alt="" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/flexoki)](https://CRAN.R-project.org/package=flexoki)
[![flexoki status
badge](https://christopherkenny.r-universe.dev/badges/flexoki)](https://christopherkenny.r-universe.dev/flexoki)
[![R-CMD-check](https://github.com/christopherkenny/flexoki/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/christopherkenny/flexoki/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

`flexoki` provides a set of color palettes from Steph Ango’s
<https://stephango.com/flexoki>.

This package provides:

- Dark and light [`ggplot2`](https://ggplot2.tidyverse.org/index.html)
  themes that match the aesthetic of Flexoki’s code editor styles.
- Custom color and fill scales for discrete, continuous, and binned data
  using Flexoki’s extended color schemes.
- Color palettes in the `flexoki` object, which is a list of
  [`palette`](https://christophertkenny.com/palette/) vectors.

## Installation

You can install the stable version of `flexoki` from
[CRAN](https://cran.r-project.org/package=flexoki) with:

``` r
install.packages('flexoki')
```

You can install the development version of `flexoki` from
[GitHub](https://github.com/christopherkenny/flexoki) with:

``` r
pak::pak('christopherkenny/flexoki')
```

## Example

`flexoki` provides basic ggplot2 scales for fill and color (also aliased
as colour). It similarly provides corresponding theme functions.

``` r
library(ggplot2)
library(flexoki)

mpg |>
  ggplot() +
  geom_point(aes(displ, hwy, colour = class)) +
  scale_color_flexoki_d(palette = 'light') +
  theme_flexoki_light()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
