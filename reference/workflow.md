# Execute estimation workflow for surveys

This function executes a sequence of statistical estimations on Survey
objects, applying functions from the R survey package with appropriate
metadata. Automatically handles different survey types and
periodicities.

## Usage

``` r
workflow(survey, ..., estimation_type = "monthly")
```

## Arguments

- survey:

  Survey object, list of Survey objects, or PoolSurvey. Must contain
  properly configured sample design

- ...:

  Calls to survey package functions (such as `svymean`, `svytotal`,
  `svyratio`, etc.) that will be executed sequentially

- estimation_type:

  Type of estimation that determines which weight to use. Options:
  "monthly", "quarterly", "annual", or vector with multiple types

## Value

`data.table` with results from all estimations, including columns:

- `stat`: Name of estimated statistic

- `value`: Estimation value

- `se`: Standard error

- `cv`: Coefficient of variation

- `estimation_type`: Type of estimation used

- `survey_edition`: Survey edition

- Other columns depending on estimation type

## Details

The function automatically selects the appropriate sample design
according to the specified `estimation_type`. For each Survey in the
input list, it executes all functions specified in `...` and combines
the results.

Supported estimation types:

- "monthly": Monthly estimations

- "quarterly": Quarterly estimations

- "annual": Annual estimations

For PoolSurvey objects, it uses a specialized methodology that handles
pooling of multiple surveys.

## See also

[`svymean`](https://rdrr.io/pkg/survey/man/surveysummary.html) for
population means
[`svytotal`](https://rdrr.io/pkg/survey/man/surveysummary.html) for
population totals
[`svyratio`](https://rdrr.io/pkg/survey/man/svyratio.html) for ratios
[`svyby`](https://rdrr.io/pkg/survey/man/svyby.html) for domain
estimations
[`PoolSurvey`](https://metasurveyr.github.io/metasurvey/reference/PoolSurvey.md)
for survey pooling

Other workflows:
[`RecipeWorkflow-class`](https://metasurveyr.github.io/metasurvey/reference/RecipeWorkflow-class.md),
[`evaluate_cv()`](https://metasurveyr.github.io/metasurvey/reference/evaluate_cv.md),
[`print.RecipeWorkflow()`](https://metasurveyr.github.io/metasurvey/reference/print.RecipeWorkflow.md),
[`publish_workflow()`](https://metasurveyr.github.io/metasurvey/reference/publish_workflow.md),
[`read_workflow()`](https://metasurveyr.github.io/metasurvey/reference/read_workflow.md),
[`reproduce_workflow()`](https://metasurveyr.github.io/metasurvey/reference/reproduce_workflow.md),
[`save_workflow()`](https://metasurveyr.github.io/metasurvey/reference/save_workflow.md),
[`workflow_from_list()`](https://metasurveyr.github.io/metasurvey/reference/workflow_from_list.md)

## Examples

``` r
# Simple estimation with a test survey
dt <- data.table::data.table(
  x = rnorm(100), g = sample(c("a", "b"), 100, TRUE),
  w = rep(1, 100)
)
svy <- Survey$new(
  data = dt, edition = "2023", type = "test",
  psu = NULL, engine = "data.table",
  weight = add_weight(annual = "w")
)
result <- workflow(
  survey = list(svy),
  survey::svymean(~x, na.rm = TRUE),
  estimation_type = "annual"
)
#> Warning: CV may not be useful for negative statistics

# \donttest{
# ECH example with domain estimations
# result <- workflow(
#   survey = list(ech_2023),
#   svyby(~unemployed, ~region, svymean, na.rm = TRUE),
#   estimation_type = "annual")
# }
```
