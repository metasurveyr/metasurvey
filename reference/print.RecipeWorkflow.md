# Print method for RecipeWorkflow objects

Print method for RecipeWorkflow objects

## Usage

``` r
# S3 method for class 'RecipeWorkflow'
print(x, ...)
```

## Arguments

- x:

  A RecipeWorkflow object

- ...:

  Additional arguments (unused)

## Value

Invisibly returns the object

## See also

Other workflows:
[`RecipeWorkflow-class`](https://metasurveyr.github.io/metasurvey/reference/RecipeWorkflow-class.md),
[`evaluate_cv()`](https://metasurveyr.github.io/metasurvey/reference/evaluate_cv.md),
[`publish_workflow()`](https://metasurveyr.github.io/metasurvey/reference/publish_workflow.md),
[`read_workflow()`](https://metasurveyr.github.io/metasurvey/reference/read_workflow.md),
[`reproduce_workflow()`](https://metasurveyr.github.io/metasurvey/reference/reproduce_workflow.md),
[`save_workflow()`](https://metasurveyr.github.io/metasurvey/reference/save_workflow.md),
[`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md),
[`workflow_from_list()`](https://metasurveyr.github.io/metasurvey/reference/workflow_from_list.md)

## Examples

``` r
wf <- RecipeWorkflow$new(
  id = "w1", name = "Example Workflow", user = "tester",
  edition = "2023", survey_type = "test",
  recipe_ids = "r1",
  calls = list(), description = "Demo"
)
print(wf)
#> 
#> ── Workflow: Example Workflow ──
#> Author:  tester
#> Survey:  test / 2023
#> Version: 1.0.0
#> Description: Demo
#> Certification: community
#> 
#> ── Uses Recipes (1) ──
#>   r1
#> 
```
