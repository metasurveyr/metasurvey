# Rank recipes by downloads

Get the top recipes ranked by download count from the active backend.

## Usage

``` r
rank_recipes(n = NULL)
```

## Arguments

- n:

  Integer. Maximum number of recipes to return, or NULL for all.

## Value

List of Recipe objects sorted by downloads (descending).

## See also

[`search_recipes`](https://metasurveyr.github.io/metasurvey/reference/search_recipes.md),
[`filter_recipes`](https://metasurveyr.github.io/metasurvey/reference/filter_recipes.md)

## Examples

``` r
set_backend("local", path = tempfile(fileext = ".json"))
top10 <- rank_recipes(n = 10)
```
