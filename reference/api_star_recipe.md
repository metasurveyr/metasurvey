# Rate a recipe

Give a star rating (1-5) to a recipe. Requires authentication. Each user
can have one rating per recipe (upserts on subsequent calls).

## Usage

``` r
api_star_recipe(id, value)
```

## Arguments

- id:

  Recipe ID

- value:

  Integer rating from 1 to 5

## Value

Invisibly, the API response.

## See also

Other api-stars:
[`api_get_recipe_stars()`](https://metasurveyr.github.io/metasurvey/reference/api_get_recipe_stars.md),
[`api_get_workflow_stars()`](https://metasurveyr.github.io/metasurvey/reference/api_get_workflow_stars.md),
[`api_star_workflow()`](https://metasurveyr.github.io/metasurvey/reference/api_star_workflow.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_star_recipe("r_1739654400_742", 5)
} # }
```
