# Expand a STATA variable range to individual variable names

STATA allows variable ranges like suma1-suma4 which means suma1 suma2
suma3 suma4. This function detects and expands such ranges by
incrementing the trailing numeric suffix.

## Usage

``` r
expand_var_range(spec)
```

## Arguments

- spec:

  Variable specification (may contain ranges with -)

## Value

Character vector of individual variable names
