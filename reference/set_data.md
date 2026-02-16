# Set data on a Survey

Tidy wrapper for `svy$set_data(data)`.

## Usage

``` r
set_data(svy, data, .copy = FALSE)
```

## Arguments

- svy:

  Survey object

- data:

  A data.frame or data.table with survey microdata

- .copy:

  Logical; if TRUE, clone the Survey before modifying (default FALSE)

## Value

The Survey object (invisibly). If `.copy=TRUE`, returns a new clone.
