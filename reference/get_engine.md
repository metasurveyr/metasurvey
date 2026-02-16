# Get the current survey data engine

Retrieves the currently configured engine for loading surveys from
system options or environment variables.

## Usage

``` r
get_engine()
```

## Value

Character vector with the name of the configured engine.

## See also

Other options:
[`lazy_default()`](https://metasurveyr.github.io/metasurvey/reference/lazy_default.md),
[`set_engine()`](https://metasurveyr.github.io/metasurvey/reference/set_engine.md),
[`set_lazy_processing()`](https://metasurveyr.github.io/metasurvey/reference/set_lazy_processing.md),
[`set_use_copy()`](https://metasurveyr.github.io/metasurvey/reference/set_use_copy.md),
[`show_engines()`](https://metasurveyr.github.io/metasurvey/reference/show_engines.md),
[`use_copy_default()`](https://metasurveyr.github.io/metasurvey/reference/use_copy_default.md)

## Examples

``` r
get_engine()
#> [1] "data.table"
```
