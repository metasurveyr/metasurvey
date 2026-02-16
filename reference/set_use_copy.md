# Set data copy option

Configures whether survey operations should create copies of the data or
modify existing data in place. This setting affects memory usage and
performance across the metasurvey package.

## Usage

``` r
set_use_copy(use_copy)
```

## Arguments

- use_copy:

  Logical value: TRUE to create data copies (safer), FALSE to modify
  data in place (more efficient)

## Details

Setting use_copy affects all subsequent survey operations:

- TRUE (default): Operations create data copies, preserving original
  data

- FALSE: Operations modify data in place, reducing memory usage

Use FALSE for large datasets where memory is a concern, but ensure you
don't need the original data after operations.

## See also

[`use_copy_default`](https://metasurveyr.github.io/metasurvey/reference/use_copy_default.md)
to check current setting

## Examples

``` r
# Set to use copies (default behavior)
set_use_copy(TRUE)
use_copy_default()
#> [1] TRUE

# Set to modify in place for better performance
set_use_copy(FALSE)
use_copy_default()
#> [1] FALSE

# Reset to default
set_use_copy(TRUE)
```
