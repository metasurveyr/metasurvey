# show_engines

This function returns a list of available engines that can be used for
loading surveys. The available engines are "data.table", "tidyverse",
and "dplyr".

## Usage

``` r
show_engines()
```

## Value

Character vector with the names of the available engines.

## Examples

``` r
show_engines()
#> [1] "data.table" "tidyverse"  "dplyr"     
```
