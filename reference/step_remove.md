# Remove variables from survey data (step)

Creates a step that removes one or more variables from the survey data
when baked.

## Usage

``` r
step_remove(
  svy = survey_empty(),
  ...,
  vars = NULL,
  use_copy = use_copy_default(),
  comment = "Remove variables",
  lazy = lazy_default(),
  record = TRUE
)
```

## Arguments

- svy:

  A Survey or RotativePanelSurvey object

- ...:

  Unquoted variable names to remove, or a character vector

- vars:

  Character vector of variable names to remove.

- use_copy:

  Whether to operate on a copy (default: use_copy_default())

- comment:

  Optional description for the step

- lazy:

  Logical, whether to delay execution.

- record:

  Logical, whether to record the step.

## Value

Survey object with the specified variables removed (or queued for
removal).

## Examples

``` r
dt <- data.table::data.table(id = 1:5, age = c(25, 30, 45, 50, 60), w = rep(1, 5))
svy <- Survey$new(data = dt, edition = "2023", type = "ech",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w"))
svy2 <- step_remove(svy, age)
svy2 <- bake_steps(svy2)
"age" %in% names(get_data(svy2)) # FALSE
#> [1] FALSE
```
