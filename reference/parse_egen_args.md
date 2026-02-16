# Parse a STATA egen command

Parse a STATA egen command

## Usage

``` r
parse_egen_args(args, by_group = NULL, options = NULL)
```

## Arguments

- args:

  Arguments string after "egen"

- by_group:

  By-group from bysort prefix or by() option

- options:

  Options string

## Value

List with var_name, func, func_arg, by_group
