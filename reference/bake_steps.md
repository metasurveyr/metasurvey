# Execute all pending steps

Iterates over all pending (lazy) steps attached to a Survey or
RotativePanelSurvey and executes them sequentially, mutating the
underlying data.table. Each step is validated before execution (checks
that required variables exist).

## Usage

``` r
bake_steps(svy)
```

## Arguments

- svy:

  A `Survey` or `RotativePanelSurvey` object with pending steps

## Value

The same object with all steps materialized in the data and each step
marked as `bake = TRUE`.

## Details

Steps are executed in the order they were added. Each step's expressions
can reference variables created by previous steps.

For RotativePanelSurvey objects, steps are applied to both the
implantation and all follow-up surveys.

## Examples

``` r
dt <- data.table::data.table(id = 1:5, age = c(15, 30, 45, 50, 70), w = 1)
svy <- Survey$new(data = dt, edition = "2023", type = "test",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w"))
svy <- step_compute(svy, age2 = age * 2)
svy <- bake_steps(svy)
get_data(svy)
#>       id   age     w  age2 record
#>    <int> <num> <num> <num> <lgcl>
#> 1:     1    15     1    30  FALSE
#> 2:     2    30     1    60  FALSE
#> 3:     3    45     1    90  FALSE
#> 4:     4    50     1   100  FALSE
#> 5:     5    70     1   140  FALSE
```
