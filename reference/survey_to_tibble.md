# survey_to_tibble

Convert survey to tibble

## Usage

``` r
survey_to_tibble(svy)
```

## Arguments

- svy:

  Survey object

## Value

tibble

## Examples

``` r
if (FALSE) { # \dontrun{
dt <- data.table::data.table(id = 1:5, age = c(25, 30, 45, 50, 60), w = rep(1, 5))
svy <- Survey$new(data = dt, edition = "2023", type = "ech",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w"))
tbl <- survey_to_tibble(svy)
class(tbl)
} # }
```
