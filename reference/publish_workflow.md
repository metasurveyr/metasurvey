# Publish a workflow to the active backend

Publishes a RecipeWorkflow to the configured workflow backend.

## Usage

``` r
publish_workflow(wf)
```

## Arguments

- wf:

  A RecipeWorkflow object.

## Value

NULL (called for side effect).

## See also

[`set_workflow_backend`](https://metasurveyr.github.io/metasurvey/reference/set_workflow_backend.md),
[`RecipeWorkflow`](https://metasurveyr.github.io/metasurvey/reference/RecipeWorkflow-class.md)

Other workflows:
[`RecipeWorkflow-class`](https://metasurveyr.github.io/metasurvey/reference/RecipeWorkflow-class.md),
[`evaluate_cv()`](https://metasurveyr.github.io/metasurvey/reference/evaluate_cv.md),
[`print.RecipeWorkflow()`](https://metasurveyr.github.io/metasurvey/reference/print.RecipeWorkflow.md),
[`read_workflow()`](https://metasurveyr.github.io/metasurvey/reference/read_workflow.md),
[`reproduce_workflow()`](https://metasurveyr.github.io/metasurvey/reference/reproduce_workflow.md),
[`save_workflow()`](https://metasurveyr.github.io/metasurvey/reference/save_workflow.md),
[`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md),
[`workflow_from_list()`](https://metasurveyr.github.io/metasurvey/reference/workflow_from_list.md)

## Examples

``` r
set_workflow_backend("local", path = tempfile(fileext = ".json"))
wf <- RecipeWorkflow$new(
  name = "Example", description = "Test",
  survey_type = "ech", edition = "2023",
  recipe_ids = "r_001", estimation_type = "svymean"
)
publish_workflow(wf)
```
