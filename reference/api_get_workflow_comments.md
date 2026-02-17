# Get comments for a workflow

List all comments on a workflow, sorted by creation date.

## Usage

``` r
api_get_workflow_comments(id)
```

## Arguments

- id:

  Workflow ID

## Value

List of comment objects.

## See also

Other api-comments:
[`api_comment_recipe()`](https://metasurveyr.github.io/metasurvey/reference/api_comment_recipe.md),
[`api_comment_workflow()`](https://metasurveyr.github.io/metasurvey/reference/api_comment_workflow.md),
[`api_delete_comment()`](https://metasurveyr.github.io/metasurvey/reference/api_delete_comment.md),
[`api_get_recipe_comments()`](https://metasurveyr.github.io/metasurvey/reference/api_get_recipe_comments.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_get_workflow_comments("w_1739654400_123")
} # }
```
