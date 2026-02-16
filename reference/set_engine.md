# set_engine

This function configures the engine to be used for loading surveys. It
checks if the provided engine is supported, sets the default engine if
none is specified, and generates a message indicating the configured
engine. If the engine is not supported, it throws an error.

## Usage

``` r
set_engine(.engine = show_engines())
```

## Arguments

- .engine:

  Character vector with the name of the engine to configure. By default,
  the engine returned by the
  [`show_engines()`](https://metasurveyr.github.io/metasurvey/reference/show_engines.md)
  function is used.

## Value

Invisibly, the previous engine name (for restoring).

## Examples

``` r
if (FALSE) { # \dontrun{
set_engine("data.table")
} # }
```
