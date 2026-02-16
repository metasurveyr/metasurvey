# Expand STATA foreach and forvalues loops

Detects foreach/forvalues blocks and unrolls them by substituting loop
variables into body lines.

## Usage

``` r
expand_stata_loops(lines)
```

## Arguments

- lines:

  Character vector of source lines

## Value

Character vector with loops expanded
