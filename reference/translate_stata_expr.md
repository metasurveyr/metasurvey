# Translate a STATA expression to an R expression string

Converts STATA-specific syntax (inrange, inlist, missing values, etc.)
to equivalent R expressions suitable for data.table evaluation.

## Usage

``` r
translate_stata_expr(expr)
```

## Arguments

- expr:

  STATA expression string

## Value

R expression string
