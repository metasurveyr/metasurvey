# Add a recipe to a Survey

Tidy wrapper for `svy$add_recipe(recipe)`.

## Usage

``` r
add_recipe(svy, recipe, bake = lazy_default())
```

## Arguments

- svy:

  Survey object

- recipe:

  A Recipe object

- bake:

  Logical; whether to bake immediately (default: lazy_default())

## Value

The Survey object (invisibly), modified in place

## Examples

``` r
if (FALSE) { # \dontrun{
svy <- add_recipe(svy, my_recipe)
} # }
```
