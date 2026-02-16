# Create computation steps for survey variables

This function uses optimized expression evaluation with automatic
dependency detection and error prevention. All computations are
validated before execution.

## Usage

``` r
step_compute(
  svy = NULL,
  ...,
  .by = NULL,
  use_copy = use_copy_default(),
  comment = "Compute step",
  .level = "auto"
)
```

## Arguments

- svy:

  A `Survey` or `RotativePanelSurvey` object. If NULL, creates a step
  that can be applied later using the pipe operator (%\>%)

- ...:

  Computation expressions with automatic optimization. Names are
  assigned using `new_var = expression`

- .by:

  Vector of variables to group computations by. The system automatically
  validates these variables exist before execution

- use_copy:

  Logical indicating whether to create a copy of the object before
  applying transformations. Defaults to
  [`use_copy_default()`](https://metasurveyr.github.io/metasurvey/reference/use_copy_default.md)

- comment:

  Descriptive text for the step for documentation and traceability.
  Compatible with Markdown syntax. Defaults to "Compute step"

- .level:

  For RotativePanelSurvey objects, specifies the level where
  computations are applied: "implantation", "follow_up", "quarter",
  "month", or "auto"

## Value

Same type of input object (`Survey` or `RotativePanelSurvey`) with new
computed variables and the step added to the history

## Details

**CORE ENGINE FEATURES**:

**1. Automatic Expression Processing:**

- All expressions are evaluated using R's native evaluation

- Dependency detection using variable name analysis

- Runtime validation prevents errors

**2. Enhanced Error Prevention:**

- Missing variables detected before execution

- Type checking when possible

- Precise error locations with context

**3. Performance Benefits:**

- Direct evaluation for minimal overhead

- Efficient data.table operations

- Optimized for large survey datasets

For RotativePanelSurvey objects, validation ensures computations are
compatible with the specified hierarchical level:

- "implantation": Household/dwelling level computations

- "follow_up": Individual/person level computations

- "quarter": Quarterly aggregated computations

- "month": Monthly aggregated computations

- "auto": Automatically detects appropriate level

## See also

[`step_recode`](https://metasurveyr.github.io/metasurvey/reference/step_recode.md)
for categorical recodings
[`bake_steps`](https://metasurveyr.github.io/metasurvey/reference/bake_steps.md)
to execute all pending steps

## Examples

``` r
# Basic computation
dt <- data.table::data.table(id = 1:5, age = c(25, 30, 45, 50, 60), w = 1)
svy <- Survey$new(data = dt, edition = "2023", type = "test",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w"))
svy <- svy |> step_compute(age_squared = age^2, comment = "Age squared")
svy <- bake_steps(svy)
get_data(svy)
#>       id   age     w age_squared record
#>    <int> <num> <num>       <num> <lgcl>
#> 1:     1    25     1         625  FALSE
#> 2:     2    30     1         900  FALSE
#> 3:     3    45     1        2025  FALSE
#> 4:     4    50     1        2500  FALSE
#> 5:     5    60     1        3600  FALSE

if (FALSE) { # \dontrun{
# ECH example: labor indicator
ech <- ech |>
  step_compute(
    unemployed = ifelse(POBPCOAC %in% 3:5, 1, 0),
    comment = "Unemployment indicator"
  )
} # }
```
