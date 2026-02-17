# Get star summary for a workflow

Returns average rating, count, and the current user's rating (if
authenticated).

## Usage

``` r
api_get_workflow_stars(id)
```

## Arguments

- id:

  Workflow ID

## Value

List with `average`, `count`, and optionally `user_value`.

## See also

Other api-stars:
[`api_get_recipe_stars()`](https://metasurveyr.github.io/metasurvey/reference/api_get_recipe_stars.md),
[`api_star_recipe()`](https://metasurveyr.github.io/metasurvey/reference/api_star_recipe.md),
[`api_star_workflow()`](https://metasurveyr.github.io/metasurvey/reference/api_star_workflow.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_get_workflow_stars("w_1739654400_123")
} # }
```
