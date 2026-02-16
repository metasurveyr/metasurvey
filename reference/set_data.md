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

## Examples

``` r
dt <- data.table::data.table(id = 1:5, x = rnorm(5), w = rep(1, 5))
svy <- Survey$new(
  data = dt, edition = "2023", type = "test",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w")
)
new_dt <- data.table::data.table(id = 1:3, x = rnorm(3), w = rep(1, 3))
svy <- set_data(svy, new_dt)
```
