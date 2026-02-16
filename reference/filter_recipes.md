# Filter recipes by criteria

Filter recipes in the active backend by survey type, edition, category,
or certification level.

## Usage

``` r
filter_recipes(
  survey_type = NULL,
  edition = NULL,
  category = NULL,
  certification_level = NULL
)
```

## Arguments

- survey_type:

  Character survey type or NULL.

- edition:

  Character edition or NULL.

- category:

  Character category name or NULL.

- certification_level:

  Character certification level or NULL.

## Value

List of matching Recipe objects.

## See also

[`search_recipes`](https://metasurveyr.github.io/metasurvey/reference/search_recipes.md),
[`rank_recipes`](https://metasurveyr.github.io/metasurvey/reference/rank_recipes.md)

## Examples

``` r
set_backend("local", path = tempfile(fileext = ".json"))
ech_recipes <- filter_recipes(survey_type = "ech")
length(ech_recipes)
#> [1] 0
```
