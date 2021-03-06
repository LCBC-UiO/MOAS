
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LCBC MOAS functions <img src="man/figures/hex.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![CircleCI build
status](https://circleci.com/gh/LCBC-UiO/MOAS.svg)](https://circleci.com/gh/LCBC-UiO/MOAS)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/LCBC-UiO/MOAS.svg?branch=master)](https://travis-ci.org/LCBC-UiO/MOAS)
[![Codecov test
coverage](https://codecov.io/gh/LCBC-UiO/MOAS/branch/master/graph/badge.svg)](https://codecov.io/gh/LCBC-UiO/MOAS?branch=master)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/LCBC-UiO/MOAS?branch=master&svg=true)](https://ci.appveyor.com/project/LCBC-UiO/MOAS)
<!-- badges: end -->

The functions in this package are intended for use on the Mother of All
Spreadsheets (and related files) in LCBC. The functions will mostly not
work on any other data, it is an in-house package of functions.

The package can be installed using devtools:

``` r
install.packages("remotes")
remotes::install_github("LCBC-UiO/MOAS", build_vignettes = TRUE)
```

The functions are now installed, and you may load them when you want to
use them. All functions are documented in standard R fashion.

The package also has a vignette, to help you get started using it.

``` r
library(MOAS)
vignette("MOAS")
```

The vignette and other presentations or tutorials of the package can
also be found online [here](https://lcbc-uio.github.io/MOAS/)
