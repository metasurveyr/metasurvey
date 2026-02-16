# Construct a RecipeWorkflow from a plain list

Construct a RecipeWorkflow from a plain list

## Usage

``` r
workflow_from_list(lst)
```

## Arguments

- lst:

  A list (typically from JSON) with workflow fields

## Value

A RecipeWorkflow object

## See also

Other workflows:
[`RecipeWorkflow-class`](https://metasurveyr.github.io/metasurvey/reference/RecipeWorkflow-class.md),
[`evaluate_cv()`](https://metasurveyr.github.io/metasurvey/reference/evaluate_cv.md),
[`print.RecipeWorkflow()`](https://metasurveyr.github.io/metasurvey/reference/print.RecipeWorkflow.md),
[`publish_workflow()`](https://metasurveyr.github.io/metasurvey/reference/publish_workflow.md),
[`read_workflow()`](https://metasurveyr.github.io/metasurvey/reference/read_workflow.md),
[`reproduce_workflow()`](https://metasurveyr.github.io/metasurvey/reference/reproduce_workflow.md),
[`save_workflow()`](https://metasurveyr.github.io/metasurvey/reference/save_workflow.md),
[`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)

## Examples

``` r
lst <- list(
  name = "example", user = "test", survey_type = "ech",
  edition = "2023", estimation_type = "svymean"
)
wf <- workflow_from_list(lst)
```
