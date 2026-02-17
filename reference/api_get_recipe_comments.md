# Get comments for a recipe

List all comments on a recipe, sorted by creation date.

## Usage

``` r
api_get_recipe_comments(id)
```

## Arguments

- id:

  Recipe ID

## Value

List of comment objects.

## See also

Other api-comments:
[`api_comment_recipe()`](https://metasurveyr.github.io/metasurvey/reference/api_comment_recipe.md),
[`api_comment_workflow()`](https://metasurveyr.github.io/metasurvey/reference/api_comment_workflow.md),
[`api_delete_comment()`](https://metasurveyr.github.io/metasurvey/reference/api_delete_comment.md),
[`api_get_workflow_comments()`](https://metasurveyr.github.io/metasurvey/reference/api_get_workflow_comments.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_get_recipe_comments("r_1739654400_742")
} # }
```
