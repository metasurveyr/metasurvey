# Rename variables in survey data (step)

Creates a step that renames variables in the survey data when baked.

## Usage

``` r
step_rename(
  svy = survey_empty(),
  ...,
  mapping = NULL,
  use_copy = use_copy_default(),
  comment = "Rename variables",
  lazy = lazy_default(),
  record = TRUE
)
```

## Arguments

- svy:

  A Survey or RotativePanelSurvey object

- ...:

  Pairs in the form new_name = old_name (unquoted or character)

- mapping:

  A named character vector of the form `c(new_name = "old_name")`.

- use_copy:

  Whether to operate on a copy (default: use_copy_default())

- comment:

  Optional description for the step

- lazy:

  Logical, whether to delay execution.

- record:

  Logical, whether to record the step.

## Value

Survey object with the specified variables renamed (or queued for
renaming).

## See also

Other steps:
[`bake_steps()`](https://metasurveyr.github.io/metasurvey/reference/bake_steps.md),
[`get_steps()`](https://metasurveyr.github.io/metasurvey/reference/get_steps.md),
[`step_compute()`](https://metasurveyr.github.io/metasurvey/reference/step_compute.md),
[`step_join()`](https://metasurveyr.github.io/metasurvey/reference/step_join.md),
[`step_recode()`](https://metasurveyr.github.io/metasurvey/reference/step_recode.md),
[`step_remove()`](https://metasurveyr.github.io/metasurvey/reference/step_remove.md),
[`view_graph()`](https://metasurveyr.github.io/metasurvey/reference/view_graph.md)

## Examples

``` r
dt <- data.table::data.table(
  id = 1:5, age = c(25, 30, 45, 50, 60),
  w = rep(1, 5)
)
svy <- Survey$new(
  data = dt, edition = "2023", type = "ech",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w")
)
svy2 <- step_rename(svy, edad = age)
svy2 <- bake_steps(svy2)
"edad" %in% names(get_data(svy2)) # TRUE
#> [1] TRUE
```
