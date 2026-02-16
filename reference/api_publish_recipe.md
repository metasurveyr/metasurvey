# Publish a recipe

Publish a Recipe object to the API. Requires authentication (call
[`api_login()`](https://metasurveyr.github.io/metasurvey/reference/api_login.md)
first).

## Usage

``` r
api_publish_recipe(recipe)
```

## Arguments

- recipe:

  A Recipe object

## Value

Invisibly, the API response with the assigned ID.

## See also

Other api-recipes:
[`api_get_recipe()`](https://metasurveyr.github.io/metasurvey/reference/api_get_recipe.md),
[`api_list_recipes()`](https://metasurveyr.github.io/metasurvey/reference/api_list_recipes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_publish_recipe(my_recipe)
} # }
```
