# Add a category to a recipe

Pipe-friendly function to add a category to a Recipe object. Accepts
either a category name (string) or a
[`RecipeCategory`](https://metasurveyr.github.io/metasurvey/reference/RecipeCategory.md)
object.

## Usage

``` r
add_category(recipe, category, description = "")
```

## Arguments

- recipe:

  A Recipe object.

- category:

  Character category name or RecipeCategory object.

- description:

  Character. Description for the category (used when `category` is a
  string). Defaults to empty.

## Value

The modified Recipe object (invisibly for piping).

## See also

[`remove_category`](https://metasurveyr.github.io/metasurvey/reference/remove_category.md),
[`recipe_category`](https://metasurveyr.github.io/metasurvey/reference/recipe_category.md),
[`default_categories`](https://metasurveyr.github.io/metasurvey/reference/default_categories.md)

## Examples

``` r
r <- recipe(
  name = "Example", user = "Test",
  svy = survey_empty(type = "ech", edition = "2023"),
  description = "Example recipe"
)
r <- r |>
  add_category("labor_market", "Labor market indicators") |>
  add_category("income")
```
