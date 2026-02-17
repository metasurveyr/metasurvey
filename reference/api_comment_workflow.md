# Add a comment to a workflow

Post a text comment on a workflow. Requires authentication.

## Usage

``` r
api_comment_workflow(id, text)
```

## Arguments

- id:

  Workflow ID

- text:

  Comment text (max 2000 characters)

## Value

Invisibly, the API response.

## See also

Other api-comments:
[`api_comment_recipe()`](https://metasurveyr.github.io/metasurvey/reference/api_comment_recipe.md),
[`api_delete_comment()`](https://metasurveyr.github.io/metasurvey/reference/api_delete_comment.md),
[`api_get_recipe_comments()`](https://metasurveyr.github.io/metasurvey/reference/api_get_recipe_comments.md),
[`api_get_workflow_comments()`](https://metasurveyr.github.io/metasurvey/reference/api_get_workflow_comments.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_comment_workflow("w_1739654400_123", "Very useful workflow!")
} # }
```
