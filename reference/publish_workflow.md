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

## Examples

``` r
if (FALSE) { # \dontrun{
set_workflow_backend("local", path = "workflows.json")
publish_workflow(my_workflow)
} # }
```
