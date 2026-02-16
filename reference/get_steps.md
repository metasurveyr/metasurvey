# get_steps

Get steps from survey

## Usage

``` r
get_steps(svy)
```

## Arguments

- svy:

  Survey object

## Value

List of Step objects

## Examples

``` r
dt <- data.table::data.table(id = 1:5, age = c(25, 30, 45, 50, 60), w = rep(1, 5))
svy <- Survey$new(
  data = dt, edition = "2023", type = "ech",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w")
)
svy <- step_compute(svy, age2 = age * 2)
get_steps(svy) # list of Step objects
#> $`step_1 Compute: age2`
#> <Step>
#>   Public:
#>     bake: FALSE
#>     call: call
#>     clone: function (deep = FALSE) 
#>     comments: Compute step
#>     default_engine: data.table
#>     depends_on: age
#>     edition: 2023
#>     exprs: call
#>     initialize: function (name, edition, survey_type, type, new_var, exprs, call, 
#>     name: step_1 Compute: age2
#>     new_var: age2
#>     survey_type: ech
#>     svy_before: Survey, R6
#>     type: compute
#> 
```
