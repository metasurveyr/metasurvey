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

Character string with the quality category (e.g. "Excellent", "Good").

## Examples

``` r
evaluate_cv(3) # "Excellent"
#> [1] "Excellent"
evaluate_cv(12) # "Good"
#> [1] "Good"
evaluate_cv(30) # "Use with caution"
#> [1] "Use with caution"
```
