# Evaluate estimation with Coefficient of Variation

Evaluate estimation with Coefficient of Variation

## Usage

``` r
evaluate_cv(cv)
```

## Arguments

- cv:

  Numeric coefficient of variation value.

## Value

Character string with the quality category (e.g. "Excelente", "Bueno").

## Examples

``` r
evaluate_cv(3) # "Excelente"
#> [1] "Excelente"
evaluate_cv(12) # "Bueno"
#> [1] "Bueno"
evaluate_cv(30) # "Utilizar con precaucion"
#> [1] "Utilizar con precaucion"
```
