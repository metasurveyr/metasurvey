# Delete a comment

Delete a comment by its ID. Only the comment author can delete it.
Requires authentication.

## Usage

``` r
api_delete_comment(comment_id)
```

## Arguments

- comment_id:

  Comment ID

## Value

Invisibly, the API response.

## See also

Other api-comments:
[`api_comment_recipe()`](https://metasurveyr.github.io/metasurvey/reference/api_comment_recipe.md),
[`api_comment_workflow()`](https://metasurveyr.github.io/metasurvey/reference/api_comment_workflow.md),
[`api_get_recipe_comments()`](https://metasurveyr.github.io/metasurvey/reference/api_get_recipe_comments.md),
[`api_get_workflow_comments()`](https://metasurveyr.github.io/metasurvey/reference/api_get_workflow_comments.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_delete_comment("c_abc123")
} # }
```
