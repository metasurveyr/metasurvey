# get_engine

This function retrieves the currently configured engine for loading
surveys. It returns the engine configured in the system options or
environment variables.

## Usage

``` r
get_engine()
```

## Value

Character vector with the name of the configured engine.

## Examples

``` r
get_engine()
#> [1] "data.table"
```
