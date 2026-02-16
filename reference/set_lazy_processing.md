# Set lazy processing

Set lazy processing

## Usage

``` r
set_lazy_processing(lazy)
```

## Arguments

- lazy:

  Logical. If TRUE, steps are deferred until bake_steps() is called.

## Value

Invisibly, the previous value.

## See also

Other options:
[`get_engine()`](https://metasurveyr.github.io/metasurvey/reference/get_engine.md),
[`lazy_default()`](https://metasurveyr.github.io/metasurvey/reference/lazy_default.md),
[`set_engine()`](https://metasurveyr.github.io/metasurvey/reference/set_engine.md),
[`set_use_copy()`](https://metasurveyr.github.io/metasurvey/reference/set_use_copy.md),
[`show_engines()`](https://metasurveyr.github.io/metasurvey/reference/show_engines.md),
[`use_copy_default()`](https://metasurveyr.github.io/metasurvey/reference/use_copy_default.md)

## Examples

``` r
old <- lazy_default()
set_lazy_processing(FALSE)
lazy_default() # now FALSE
#> [1] FALSE
set_lazy_processing(old) # restore
```
