# Get data copy option

Retrieves the current setting for the use_copy option, which controls
whether survey operations create copies of the data or modify in place.

## Usage

``` r
use_copy_default()
```

## Value

Logical value indicating whether to use data copies (TRUE) or modify
data in place (FALSE). Default is TRUE.

## Details

The use_copy option affects memory usage and performance:

- TRUE: Creates copies, safer but uses more memory

- FALSE: Modifies in place, more efficient but requires caution

## See also

[`set_use_copy`](https://metasurveyr.github.io/metasurvey/reference/set_use_copy.md)
to change the setting

## Examples

``` r
# Check current setting
current_setting <- use_copy_default()
print(current_setting)
#> [1] TRUE
```
