# get_data

Get data from survey

## Usage

``` r
get_data(svy)
```

## Arguments

- svy:

  Survey object

## Value

Data

## Examples

``` r
dt <- data.table::data.table(id = 1:5, age = c(25, 30, 45, 50, 60), w = rep(1, 5))
svy <- Survey$new(data = dt, edition = "2023", type = "ech",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w"))
head(get_data(svy))
#>       id   age     w
#>    <int> <num> <num>
#> 1:     1    25     1
#> 2:     2    30     1
#> 3:     3    45     1
#> 4:     4    50     1
#> 5:     5    60     1
```
