# metasurvey

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/metasSurvey)](https://cran.r-project.org/package=metasurvey)
[![pkgdown](https://github.com/metasurveyr/metasurvey/actions/workflows/pkgdown.yaml/badge.svg?branch=main)](https://github.com/metasurveyr/metasurvey/actions/workflows/pkgdown.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidyverse/dplyr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/metasurveyr/metasurvey?branch=main)
<!-- badges: end -->

This package provides a set of functions to facilitate the analysis of survey sampling data using meta-programming techniques. In the package you can create reproducible analysis pipelines, and easily generate reports and tables. The package is designed to work with the `survey` package, and it is particularly useful for complex survey designs.

Adittionally, the package provides a set of functions to facilitate the variance estimation of complex survey designs, and to facilitate the analysis of complex survey designs using the `survey` package.

The package is currently under development, and it is not yet available on CRAN. You can install the development version of the package using the `devtools` package.

## Installation

You can install the released version of metaSurvey from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("metasurvey")
```

And the development version from Github with:

``` r
# install.packages("devtools")
devtools::install_github("metasurveyR/metaSurvey")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r

library(metaSurvey)

# Load the data

eph2022_3 <- load_survey(
  path = load_survey_example("eph2022_3.csv"),
  svy_type = "eph",
  svy_edition = "2022_3",
  svy_weight = "PONDERA"
) %>%
  metasurvey::step_recode(
    "pea",
    ESTADO %in% 1:2 ~ 1,
    .default = 0
  ) %>%
  metasurvey::step_recode(
    "pet",
    ESTADO != 4 ~ 1,
    .default = 0
  ) %>%
  metasurvey::step_recode(
    "po",
    ESTADO == 1 ~ 1,
    .default = 0
  ) %>%
  metasurvey::step_recode(
    "pd",
    ESTADO == 2 ~ 1,
    .default = 0
  )

````
