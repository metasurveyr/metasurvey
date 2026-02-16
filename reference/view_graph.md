# View graph

View graph

## Usage

``` r
view_graph(svy, init_step = "Load survey")
```

## Arguments

- svy:

  Survey object

- init_step:

  Initial step label (default: "Load survey")

## Value

A visNetwork interactive graph of the survey processing steps.

## Examples

``` r
if (FALSE) { # \dontrun{
dt <- data.table::data.table(id = 1:5, age = c(25, 30, 45, 50, 60), w = rep(1, 5))
svy <- Survey$new(
  data = dt, edition = "2023", type = "ech",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w")
)
svy <- step_compute(svy, age2 = age * 2)
view_graph(svy)
} # }
```
