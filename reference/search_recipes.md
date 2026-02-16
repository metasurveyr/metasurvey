# Search recipes

Search for recipes by name or description in the active backend.

## Usage

``` r
search_recipes(query)
```

## Arguments

- query:

  Character search string.

## Value

List of matching Recipe objects.

## See also

[`filter_recipes`](https://metasurveyr.github.io/metasurvey/reference/filter_recipes.md),
[`rank_recipes`](https://metasurveyr.github.io/metasurvey/reference/rank_recipes.md),
[`set_backend`](https://metasurveyr.github.io/metasurvey/reference/set_backend.md)

## Examples

``` r
set_backend("local", path = tempfile(fileext = ".json"))
r <- recipe(
  name = "Labor Market", user = "Test",
  svy = survey_empty(type = "ech", edition = "2023"),
  description = "Labor market indicators"
)
publish_recipe(r)
results <- search_recipes("labor")
length(results)
#> [1] 1
```
