# Certify a recipe

Pipe-friendly function to certify a Recipe at a given quality level.

## Usage

``` r
certify_recipe(recipe, user, level)
```

## Arguments

- recipe:

  A Recipe object.

- user:

  RecipeUser who is certifying.

- level:

  Character. Certification level: `"reviewed"` or `"official"`.

## Value

The modified Recipe object.

## See also

[`recipe_certification`](https://metasurveyr.github.io/metasurvey/reference/recipe_certification.md),
[`recipe_user`](https://metasurveyr.github.io/metasurvey/reference/recipe_user.md)

## Examples

``` r
r <- recipe(
  name = "Example", user = "Test",
  svy = survey_empty(type = "ech", edition = "2023"),
  description = "Example recipe"
)
inst <- recipe_user("IECON", type = "institution")
r <- r |> certify_recipe(inst, "official")
```
