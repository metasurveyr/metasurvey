# Bake recipes

Bake recipes

## Usage

``` r
bake_recipes(svy)
```

## Arguments

- svy:

  Survey object

## Value

Survey object with all recipes applied

## Examples

``` r
if (FALSE) { # \dontrun{
svy <- load_survey("data.csv", svy_type = "ech", svy_edition = "2023",
  recipes = my_recipe)
processed <- bake_recipes(svy)
} # }
```
