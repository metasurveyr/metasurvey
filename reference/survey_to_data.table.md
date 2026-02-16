# survey_to_data.table

Convert survey to data.table

## Usage

``` r
survey_to_data.table(svy)
```

## Arguments

- svy:

  Survey object

## Value

data.table

## Examples

``` r
dt <- data.table::data.table(id = 1:5, age = c(25, 30, 45, 50, 60), w = rep(1, 5))
svy <- Survey$new(
  data = dt, edition = "2023", type = "ech",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w")
)
result <- survey_to_data.table(svy)
data.table::is.data.table(result) # TRUE
#> [1] TRUE
```
