# Remove a category from a recipe

Pipe-friendly function to remove a category from a Recipe by name.

## Usage

``` r
remove_category(recipe, name)
```

## Arguments

- recipe:

  A Recipe object.

- name:

  Character. Category name to remove.

## Value

The modified Recipe object.

## See also

[`add_category`](https://metasurveyr.github.io/metasurvey/reference/add_category.md)

## Examples

``` r
r <- recipe(
  name = "Example", user = "Test",
  svy = survey_empty(type = "ech", edition = "2023"),
  description = "Example recipe"
)
r <- r |>
  add_category("labor_market") |>
  remove_category("labor_market")
```
