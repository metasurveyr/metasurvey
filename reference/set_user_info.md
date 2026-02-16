# Set user info on a recipe

Pipe-friendly function to assign a
[`RecipeUser`](https://metasurveyr.github.io/metasurvey/reference/RecipeUser.md)
to a Recipe.

## Usage

``` r
set_user_info(recipe, user)
```

## Arguments

- recipe:

  A Recipe object.

- user:

  A RecipeUser object.

## Value

The modified Recipe object.

## See also

[`recipe_user`](https://metasurveyr.github.io/metasurvey/reference/recipe_user.md)

## Examples

``` r
r <- recipe(
  name = "Example", user = "Test",
  svy = survey_empty(type = "ech", edition = "2023"),
  description = "Example recipe"
)
user <- recipe_user("Juan Perez", email = "juan@example.com")
r <- r |> set_user_info(user)
```
