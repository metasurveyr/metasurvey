# get_metadata

Get metadata from survey

## Usage

``` r
get_metadata(self)
```

## Arguments

- self:

  Object of class Survey

## Examples

``` r
dt <- data.table::data.table(id = 1:5, age = c(25, 30, 45, 50, 60), w = rep(1, 5))
svy <- Survey$new(data = dt, edition = "2023", type = "ech",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w"))
get_metadata(svy)
#> Type: ECH
#> Edition: 2023
#> Periodicity: Annual
#> Engine: data.table
#> Design: 
#>   Design: Not initialized (lazy initialization - will be created when needed)
#> 
#> Steps: None
#> Recipes: None
```
