# Create a recipe category

Creates a
[`RecipeCategory`](https://metasurveyr.github.io/metasurvey/reference/RecipeCategory.md)
object for classifying recipes.

## Usage

``` r
recipe_category(name, description = "", parent = NULL)
```

## Arguments

- name:

  Character. Category identifier (e.g. `"labor_market"`).

- description:

  Character. Human-readable description. Defaults to empty.

- parent:

  RecipeCategory object or character parent category name. If a string
  is provided, it creates a parent category with that name.

## Value

A
[`RecipeCategory`](https://metasurveyr.github.io/metasurvey/reference/RecipeCategory.md)
object.

## See also

[`RecipeCategory`](https://metasurveyr.github.io/metasurvey/reference/RecipeCategory.md),
[`add_category`](https://metasurveyr.github.io/metasurvey/reference/add_category.md),
[`default_categories`](https://metasurveyr.github.io/metasurvey/reference/default_categories.md)

## Examples

``` r
cat <- recipe_category("labor_market", "Labor market indicators")

# With parent hierarchy
sub <- recipe_category("employment", "Employment stats", parent = "labor_market")
```
