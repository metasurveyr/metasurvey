# survey_to_data_frame

Convert survey to data.frame

## Usage

``` r
survey_to_data_frame(svy)
```

## Arguments

- svy:

  Survey object

## Value

data.frame

## Examples

``` r
dt <- data.table::data.table(id = 1:5, age = c(25, 30, 45, 50, 60), w = rep(1, 5))
svy <- Survey$new(
  data = dt, edition = "2023", type = "ech",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w")
)
df <- survey_to_data_frame(svy)
class(df) # "data.frame"
#> [1] "data.frame"
```
