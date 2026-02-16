# Step Class Represents a step in a survey workflow.

The `Step` class is used to define and manage individual steps in a
survey workflow. Each step can include operations such as recoding
variables, computing new variables, or validating dependencies.

## Details

The `Step` class is part of the survey workflow system and is designed
to encapsulate all the information and operations required for a single
step in the workflow. Steps can be chained together to form a complete
workflow.

## Public fields

- `name`:

  The name of the step.

- `edition`:

  The edition of the survey associated with the step.

- `survey_type`:

  The type of survey associated with the step.

- `type`:

  The type of operation performed by the step (e.g., "compute",
  "recode").

- `new_var`:

  The name of the new variable created by the step, if applicable.

- `exprs`:

  A list of expressions defining the step's operations.

- `call`:

  The function call associated with the step.

- `svy_before`:

  The survey object before the step is applied.

- `default_engine`:

  The default engine used for processing the step.

- `depends_on`:

  A list of variables that the step depends on.

- `comments`:

  Comments or notes about the step.

- `bake`:

  A logical value indicating whether the step has been executed.

## Methods

### Public methods

- [`Step$new()`](#method-Step-new)

- [`Step$clone()`](#method-Step-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Step object

#### Usage

    Step$new(
      name,
      edition,
      survey_type,
      type,
      new_var,
      exprs,
      call,
      svy_before,
      default_engine,
      depends_on,
      comments = NULL,
      bake = !lazy_default(),
      comment = NULL
    )

#### Arguments

- `name`:

  The name of the step.

- `edition`:

  The edition of the survey associated with the step.

- `survey_type`:

  The type of survey associated with the step.

- `type`:

  The type of operation performed by the step (e.g., "compute" or
  "recode").

- `new_var`:

  The name of the new variable created by the step, if applicable.

- `exprs`:

  A list of expressions defining the step's operations.

- `call`:

  The function call associated with the step.

- `svy_before`:

  The survey object before the step is applied.

- `default_engine`:

  The default engine used for processing the step.

- `depends_on`:

  A list of variables that the step depends on.

- `comments`:

  Comments or notes about the step.

- `bake`:

  A logical value indicating whether the step has been executed.

- `comment`:

  Optional alias of `comments` for backwards compatibility.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Step$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Step objects are created internally by step_compute(), step_recode(), etc.
# Use the tidy API:
dt <- data.table::data.table(id = 1:3, age = c(25, 30, 45), w = 1)
svy <- Survey$new(data = dt, edition = "2023", type = "test",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w"))
svy <- step_compute(svy, age2 = age * 2)
get_steps(svy)
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
#>     survey_type: test
#>     svy_before: Survey, R6
#>     type: compute
#> 
```
