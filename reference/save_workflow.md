# Save a RecipeWorkflow to a JSON file

Save a RecipeWorkflow to a JSON file

## Usage

``` r
save_workflow(wf, file)
```

## Arguments

- wf:

  A RecipeWorkflow object

- file:

  Character file path

## Value

NULL (called for side-effect)

## See also

Other workflows:
[`RecipeWorkflow-class`](https://metasurveyr.github.io/metasurvey/reference/RecipeWorkflow-class.md),
[`evaluate_cv()`](https://metasurveyr.github.io/metasurvey/reference/evaluate_cv.md),
[`print.RecipeWorkflow()`](https://metasurveyr.github.io/metasurvey/reference/print.RecipeWorkflow.md),
[`publish_workflow()`](https://metasurveyr.github.io/metasurvey/reference/publish_workflow.md),
[`read_workflow()`](https://metasurveyr.github.io/metasurvey/reference/read_workflow.md),
[`reproduce_workflow()`](https://metasurveyr.github.io/metasurvey/reference/reproduce_workflow.md),
[`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md),
[`workflow_from_list()`](https://metasurveyr.github.io/metasurvey/reference/workflow_from_list.md)

## Examples

``` r
if (FALSE) { # \dontrun{
save_workflow(wf, "my_workflow.json")
} # }
```
