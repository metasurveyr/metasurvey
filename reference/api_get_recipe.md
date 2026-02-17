# Get recipe(s) by ID

Get recipe(s) by ID

## Usage

``` r
api_get_recipe(id)
```

## Arguments

- id:

  Character vector of recipe ID(s). If length \> 1, returns a list.

## Value

A single Recipe object (or NULL) when `length(id) == 1`. A list of
Recipe objects when `length(id) > 1` (NULLs are dropped).

## See also

Other api-recipes:
[`api_get_recipe_dependents()`](https://metasurveyr.github.io/metasurvey/reference/api_get_recipe_dependents.md),
[`api_list_recipes()`](https://metasurveyr.github.io/metasurvey/reference/api_list_recipes.md),
[`api_publish_recipe()`](https://metasurveyr.github.io/metasurvey/reference/api_publish_recipe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_get_recipe("r_1739654400_742")
} # }
```
