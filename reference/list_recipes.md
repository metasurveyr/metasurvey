# List all recipes

List all recipes from the active backend.

## Usage

``` r
list_recipes()
```

## Value

List of all Recipe objects.

## See also

[`search_recipes`](https://metasurveyr.github.io/metasurvey/reference/search_recipes.md),
[`filter_recipes`](https://metasurveyr.github.io/metasurvey/reference/filter_recipes.md)

## Examples

``` r
set_backend("local", path = tempfile(fileext = ".json"))
all <- list_recipes()
length(all)
#> [1] 0
```
