# Set version on a recipe

Pipe-friendly function to set the version string on a Recipe.

## Usage

``` r
set_version(recipe, version)
```

## Arguments

- recipe:

  A Recipe object.

- version:

  Character version string (e.g. `"2.0.0"`).

## Value

The modified Recipe object.

## Examples

``` r
r <- recipe(
  name = "Example", user = "Test",
  svy = survey_empty(type = "ech", edition = "2023"),
  description = "Example recipe"
)
r <- r |> set_version("2.0.0")
```
