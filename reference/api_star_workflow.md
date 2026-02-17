# Rate a workflow

Give a star rating (1-5) to a workflow. Requires authentication.

## Usage

``` r
api_star_workflow(id, value)
```

## Arguments

- id:

  Workflow ID

- value:

  Integer rating from 1 to 5

## Value

Invisibly, the API response.

## See also

Other api-stars:
[`api_get_recipe_stars()`](https://metasurveyr.github.io/metasurvey/reference/api_get_recipe_stars.md),
[`api_get_workflow_stars()`](https://metasurveyr.github.io/metasurvey/reference/api_get_workflow_stars.md),
[`api_star_recipe()`](https://metasurveyr.github.io/metasurvey/reference/api_star_recipe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_star_workflow("w_1739654400_123", 4)
} # }
```
