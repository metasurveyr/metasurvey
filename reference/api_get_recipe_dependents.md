# Get recipes that depend on a recipe

Find all recipes whose `depends_on_recipes` field references the given
recipe ID (backlinks).

## Usage

``` r
api_get_recipe_dependents(id)
```

## Arguments

- id:

  Recipe ID

## Value

List of recipe summaries (id, name, user).

## See also

Other api-recipes:
[`api_get_recipe()`](https://metasurveyr.github.io/metasurvey/reference/api_get_recipe.md),
[`api_list_recipes()`](https://metasurveyr.github.io/metasurvey/reference/api_list_recipes.md),
[`api_publish_recipe()`](https://metasurveyr.github.io/metasurvey/reference/api_publish_recipe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_get_recipe_dependents("r_1739654400_742")
} # }
```
