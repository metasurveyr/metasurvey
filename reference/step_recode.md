# Create recoding steps for categorical variables

This function uses optimized expression evaluation for all recoding
conditions. All conditional expressions are validated and optimized for
efficient execution.

## Usage

``` r
step_recode(
  svy = survey_empty(),
  new_var,
  ...,
  .default = NA_character_,
  .name_step = NULL,
  ordered = FALSE,
  use_copy = use_copy_default(),
  comment = "Recode step",
  .to_factor = FALSE,
  .level = "auto"
)
```

## Arguments

- svy:

  A `Survey` or `RotativePanelSurvey` object. If NULL, creates a step
  that can be applied later using the pipe operator (%\>%)

- new_var:

  Name of the new variable to create (unquoted)

- ...:

  Sequence of two-sided formulas defining recoding rules. Left-hand side
  (LHS) is a conditional expression, right-hand side (RHS) defines the
  replacement value. Format: `condition ~ value`

- .default:

  Default value assigned when no condition is met. Defaults to
  `NA_character_`

- .name_step:

  Custom name for the step to identify it in the history. If not
  provided, generated automatically with "Recode" prefix

- ordered:

  Logical indicating whether the new variable should be an ordered
  factor. Defaults to FALSE

- use_copy:

  Logical indicating whether to create a copy of the object before
  applying transformations. Defaults to
  [`use_copy_default()`](https://metasurveyr.github.io/metasurvey/reference/use_copy_default.md)

- comment:

  Descriptive text for the step for documentation and traceability.
  Compatible with Markdown syntax. Defaults to "Recode step"

- .to_factor:

  Logical indicating whether the new variable should be converted to a
  factor. Defaults to FALSE

- .level:

  For RotativePanelSurvey objects, specifies the level where recoding is
  applied: "implantation", "follow_up", "quarter", "month", or "auto"

## Value

Same type of input object (`Survey` or `RotativePanelSurvey`) with the
new recoded variable and the step added to the history

## Details

**CORE ENGINE FOR RECODING**:

**1. Automatic Condition Processing:**

- All LHS conditions are automatically analyzed

- Static analysis of logical expressions

- Dependency detection for all referenced variables

- Optimization of conditional logic

**2. Enhanced Condition Evaluation:**

- Conditions evaluated in order

- First matching condition determines assignment

- Optimized short-circuit evaluation

- Better error reporting with expression context

**3. Performance Features:**

- Direct evaluation using R's native conditional logic

- Efficient execution for all condition types

- Dependency validation prevents runtime errors

Condition examples:

- Simple: `variable == 1`

- Complex: `age >= 18 & income > 12000`

- Vectorized: `variable %in% c(1,2,3)`

- Vectorized: `variable %in% c(1,2,3)` (validates `variable` exists)

- Logical:
  `!is.na(education) & education > mean(education, na.rm = TRUE)`

## See also

[`step_compute`](https://metasurveyr.github.io/metasurvey/reference/step_compute.md)
for more complex calculations
[`bake_steps`](https://metasurveyr.github.io/metasurvey/reference/bake_steps.md)
to execute all pending steps
[`get_steps`](https://metasurveyr.github.io/metasurvey/reference/get_steps.md)
to view step history

## Examples

``` r
if (FALSE) { # \dontrun{
# Create labor force status variable
ech <- ech |>
  step_recode(
    labor_status,
    POBPCOAC == 2 ~ "Employed",
    POBPCOAC %in% 3:5 ~ "Unemployed",
    POBPCOAC %in% 6:8 ~ "Inactive",
    .default = "Missing",
    comment = "Labor force status from ECH"
  )

# Create age groups
ech <- ech |>
  step_recode(
    age_group,
    e27 < 18 ~ "Under 18",
    e27 >= 18 & e27 < 65 ~ "Working age",
    e27 >= 65 ~ "Senior",
    .default = "Missing",
    .to_factor = TRUE,
    ordered = TRUE,
    comment = "Standard age groups"
  )

# Dummy variable
ech <- ech |>
  step_recode(
    household_head,
    e30 == 1 ~ 1,
    .default = 0,
    comment = "Household head indicator"
  )

# For rotative panel
panel <- panel |>
  step_recode(
    region_simple,
    REGION_4 == 1 ~ "Montevideo",
    REGION_4 != 1 ~ "Interior",
    .level = "implantation",
    comment = "Simplified region"
  )
} # }
```
