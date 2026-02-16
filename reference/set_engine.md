# Configure the survey data engine

Configures the engine to be used for loading surveys. Checks if the
provided engine is supported, sets the default engine if none is
specified, and generates a message indicating the configured engine. If
the engine is not supported, it throws an error.

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

## See also

Other options:
[`get_engine()`](https://metasurveyr.github.io/metasurvey/reference/get_engine.md),
[`lazy_default()`](https://metasurveyr.github.io/metasurvey/reference/lazy_default.md),
[`set_lazy_processing()`](https://metasurveyr.github.io/metasurvey/reference/set_lazy_processing.md),
[`set_use_copy()`](https://metasurveyr.github.io/metasurvey/reference/set_use_copy.md),
[`show_engines()`](https://metasurveyr.github.io/metasurvey/reference/show_engines.md),
[`use_copy_default()`](https://metasurveyr.github.io/metasurvey/reference/use_copy_default.md)

## Examples

``` r
# \donttest{
old <- set_engine("data.table")
#> Engine: data.table
get_engine()
#> [1] "data.table"
# }
```
