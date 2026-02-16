# Survey R6 class

Survey R6 class

Survey R6 class

## Format

An R6 class with public fields: `data`, `edition`, `type`,
`periodicity`, `default_engine`, `weight`, `steps`, `recipes`,
`workflows`, `design`; and methods such as `initialize()`,
[`get_data()`](https://metasurveyr.github.io/metasurvey/reference/get_data.md),
[`set_data()`](https://metasurveyr.github.io/metasurvey/reference/set_data.md),
`add_step()`,
[`add_recipe()`](https://metasurveyr.github.io/metasurvey/reference/add_recipe.md),
`bake()`, [`head()`](https://rdrr.io/r/utils/head.html),
[`str()`](https://rdrr.io/r/utils/str.html), `update_design()`.

An R6 class generator (R6ClassGenerator)

## Value

R6 class generator for Survey.

## Details

R6 class that encapsulates survey data, metadata (type, edition,
periodicity), sampling design (simple/replicate),
steps/recipes/workflows, and utilities to manage them.

Only copies the data; reuses design objects and other metadata. Much
faster than clone(deep=TRUE) but design objects are shared.

## Methods

- initialize(data, edition, type, psu, engine, weight, design, steps,
  recipes):

  Create a Survey object

- get_data(), get_edition(), get_type():

  Basic accessors

- set_data(), set_edition(), set_type(), set_weight():

  Mutators

- add_step(step), add_recipe(recipe), add_workflow(wf):

  Pipeline management

- bake():

  Bake associated recipes

## Active bindings

- design_active:

  Recompute design on-demand from current data and weights.

## Main methods

- \$new(data, edition, type, psu, engine, weight, design = NULL, steps =
  NULL, recipes = list()):

  Constructor.

- \$get_data():

  Return data.

- \$get_edition():

  Return edition.

- \$get_type():

  Return type.

- \$set_data(data):

  Set data.

- \$set_edition(edition):

  Set edition.

- \$set_type(type):

  Set type.

- \$set_weight(weight):

  Set weight specification.

- \$print():

  Print summarized metadata.

- \$add_step(step):

  Add a step and update design.

- \$add_recipe(recipe, bake = lazy_default()):

  Add a recipe.

- \$add_workflow(workflow):

  Add a workflow.

- \$bake():

  Apply recipes and return updated Survey.

- \$head():

  Return data head.

- \$str():

  Structure of data.

- \$set_design(design):

  Set design.

- \$update_design():

  Update design variables with current data.

## See also

[`survey_empty`](https://metasurveyr.github.io/metasurvey/reference/survey_empty.md),
[`bake_recipes`](https://metasurveyr.github.io/metasurvey/reference/bake_recipes.md),
[`cat_design`](https://metasurveyr.github.io/metasurvey/reference/cat_design.md),
[`cat_recipes`](https://metasurveyr.github.io/metasurvey/reference/cat_recipes.md)

## Public fields

- `data`:

  Survey data (data.frame/data.table).

- `edition`:

  Reference edition or period.

- `type`:

  Survey type, e.g., "ech" (character).

- `periodicity`:

  Periodicity detected by validate_time_pattern.

- `default_engine`:

  Default engine (character).

- `weight`:

  List with weight specifications per estimation type.

- `steps`:

  List of steps applied to the survey.

- `recipes`:

  List of
  [Recipe](https://metasurveyr.github.io/metasurvey/reference/Recipe-class.md)
  objects associated.

- `workflows`:

  List of workflows.

- `design`:

  List of survey design objects (survey/surveyrep).

- `psu`:

  Primary Sampling Unit specification (formula or character).

- `design_initialized`:

  Logical flag for lazy design initialization.

## Active bindings

- `design_active`:

  Active binding that recomputes design on-demand from current data and
  weights.

## Methods

### Public methods

- [`Survey$new()`](#method-Survey-new)

- [`Survey$get_data()`](#method-Survey-get_data)

- [`Survey$get_edition()`](#method-Survey-get_edition)

- [`Survey$get_type()`](#method-Survey-get_type)

- [`Survey$set_data()`](#method-Survey-set_data)

- [`Survey$set_edition()`](#method-Survey-set_edition)

- [`Survey$set_type()`](#method-Survey-set_type)

- [`Survey$set_weight()`](#method-Survey-set_weight)

- [`Survey$print()`](#method-Survey-print)

- [`Survey$add_step()`](#method-Survey-add_step)

- [`Survey$add_recipe()`](#method-Survey-add_recipe)

- [`Survey$add_workflow()`](#method-Survey-add_workflow)

- [`Survey$bake()`](#method-Survey-bake)

- [`Survey$head()`](#method-Survey-head)

- [`Survey$str()`](#method-Survey-str)

- [`Survey$set_design()`](#method-Survey-set_design)

- [`Survey$ensure_design()`](#method-Survey-ensure_design)

- [`Survey$update_design()`](#method-Survey-update_design)

- [`Survey$shallow_clone()`](#method-Survey-shallow_clone)

- [`Survey$clone()`](#method-Survey-clone)

------------------------------------------------------------------------

### Method `new()`

Create a Survey object

#### Usage

    Survey$new(
      data,
      edition,
      type,
      psu = NULL,
      engine,
      weight,
      design = NULL,
      steps = NULL,
      recipes = list()
    )

#### Arguments

- `data`:

  Survey data

- `edition`:

  Edition or period

- `type`:

  Survey type (character)

- `psu`:

  PSU variable or formula (optional)

- `engine`:

  Default engine

- `weight`:

  Weight specification(s) per estimation type

- `design`:

  Pre-built design (optional)

- `steps`:

  Initial steps list (optional)

- `recipes`:

  List of Recipe (optional)

------------------------------------------------------------------------

### Method [`get_data()`](https://metasurveyr.github.io/metasurvey/reference/get_data.md)

Return the underlying data

#### Usage

    Survey$get_data()

------------------------------------------------------------------------

### Method `get_edition()`

Return the survey edition/period

#### Usage

    Survey$get_edition()

------------------------------------------------------------------------

### Method `get_type()`

Return the survey type identifier

#### Usage

    Survey$get_type()

------------------------------------------------------------------------

### Method [`set_data()`](https://metasurveyr.github.io/metasurvey/reference/set_data.md)

Set the underlying data

#### Usage

    Survey$set_data(data)

#### Arguments

- `data`:

  New survey data

------------------------------------------------------------------------

### Method `set_edition()`

Set the survey edition/period

#### Usage

    Survey$set_edition(edition)

#### Arguments

- `edition`:

  New edition or period

------------------------------------------------------------------------

### Method `set_type()`

Set the survey type

#### Usage

    Survey$set_type(type)

#### Arguments

- `type`:

  New type identifier

------------------------------------------------------------------------

### Method `set_weight()`

Set weight specification(s) per estimation type

#### Usage

    Survey$set_weight(weight)

#### Arguments

- `weight`:

  Weight specification list or character

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print summarized metadata to console

#### Usage

    Survey$print()

------------------------------------------------------------------------

### Method `add_step()`

Add a step and update design

#### Usage

    Survey$add_step(step)

#### Arguments

- `step`:

  Step object

------------------------------------------------------------------------

### Method [`add_recipe()`](https://metasurveyr.github.io/metasurvey/reference/add_recipe.md)

Add a recipe

#### Usage

    Survey$add_recipe(recipe, bake = lazy_default())

#### Arguments

- `recipe`:

  Recipe object

- `bake`:

  Whether to bake lazily (internal flag)

------------------------------------------------------------------------

### Method `add_workflow()`

Add a workflow to the survey

#### Usage

    Survey$add_workflow(workflow)

#### Arguments

- `workflow`:

  Workflow object

------------------------------------------------------------------------

### Method `bake()`

Apply recipes and return updated Survey

#### Usage

    Survey$bake()

------------------------------------------------------------------------

### Method [`head()`](https://rdrr.io/r/utils/head.html)

Return the head of the underlying data

#### Usage

    Survey$head()

------------------------------------------------------------------------

### Method [`str()`](https://rdrr.io/r/utils/str.html)

Display the structure of the underlying data

#### Usage

    Survey$str()

------------------------------------------------------------------------

### Method `set_design()`

Set the survey design object

#### Usage

    Survey$set_design(design)

#### Arguments

- `design`:

  Survey design object or list

------------------------------------------------------------------------

### Method `ensure_design()`

Ensure survey design is initialized (lazy initialization)

#### Usage

    Survey$ensure_design()

#### Returns

Invisibly returns self

------------------------------------------------------------------------

### Method `update_design()`

Update design variables using current data and weight

#### Usage

    Survey$update_design()

------------------------------------------------------------------------

### Method `shallow_clone()`

Create a shallow copy of the Survey (optimized for performance)

#### Usage

    Survey$shallow_clone()

#### Returns

New Survey object with copied data but shared design

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Survey$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
