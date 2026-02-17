# Add a comment to a recipe

Post a text comment on a recipe. Requires authentication.

## Usage

``` r
api_comment_recipe(id, text)
```

## Arguments

- id:

  Recipe ID

- text:

  Comment text (max 2000 characters)

## Value

Invisibly, the API response.

## See also

Other api-comments:
[`api_comment_workflow()`](https://metasurveyr.github.io/metasurvey/reference/api_comment_workflow.md),
[`api_delete_comment()`](https://metasurveyr.github.io/metasurvey/reference/api_delete_comment.md),
[`api_get_recipe_comments()`](https://metasurveyr.github.io/metasurvey/reference/api_get_recipe_comments.md),
[`api_get_workflow_comments()`](https://metasurveyr.github.io/metasurvey/reference/api_get_workflow_comments.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_comment_recipe("r_1739654400_742", "Great recipe!")
} # }
```
