# Set lazy processing

Set lazy processing

## Usage

``` r
set_lazy_processing(lazy)
```

## Arguments

- lazy:

  Logical. If TRUE, steps are deferred until bake_steps() is called.

## Examples

``` r
old <- lazy_default()
set_lazy_processing(FALSE)
lazy_default() # now FALSE
#> [1] FALSE
set_lazy_processing(old) # restore
```
