# Get a single workflow by ID

Retrieve a workflow from the API by its ID.

## Usage

``` r
api_get_workflow(id)
```

## Arguments

- id:

  Workflow ID

## Value

RecipeWorkflow object or NULL

## See also

Other api-workflows:
[`api_list_workflows()`](https://metasurveyr.github.io/metasurvey/reference/api_list_workflows.md),
[`api_publish_workflow()`](https://metasurveyr.github.io/metasurvey/reference/api_publish_workflow.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_get_workflow("w_1739654400_123")
} # }
```
