# Publish Recipe

Publishes a Recipe object to the active backend (local JSON registry or
remote API).

## Usage

``` r
publish_recipe(recipe)
```

## Arguments

- recipe:

  A Recipe object.

## Value

The Recipe object (invisibly).

## Examples

``` r
set_backend("local", path = tempfile(fileext = ".json"))
r <- recipe(
  name = "Example", user = "Test",
  svy = survey_empty(type = "ech", edition = "2023"),
  description = "Example recipe"
)
publish_recipe(r)
length(list_recipes())
#> [1] 1
```
