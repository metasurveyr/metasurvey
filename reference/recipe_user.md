# Create a recipe user

Creates a
[`RecipeUser`](https://metasurveyr.github.io/metasurvey/reference/RecipeUser.md)
object with a simple functional interface.

## Usage

``` r
recipe_user(
  name,
  type = "individual",
  email = NULL,
  affiliation = NULL,
  institution = NULL,
  url = NULL,
  verified = FALSE
)
```

## Arguments

- name:

  Character. User or institution name.

- type:

  Character. One of `"individual"` (default), `"institutional_member"`,
  or `"institution"`.

- email:

  Character or NULL. Email address.

- affiliation:

  Character or NULL. Organizational affiliation.

- institution:

  RecipeUser object or character institution name. Required for
  `"institutional_member"` type. If a string is provided, it creates an
  institution user with that name automatically.

- url:

  Character or NULL. Institution URL.

- verified:

  Logical. Whether the account is verified.

## Value

A
[`RecipeUser`](https://metasurveyr.github.io/metasurvey/reference/RecipeUser.md)
object.

## See also

[`RecipeUser`](https://metasurveyr.github.io/metasurvey/reference/RecipeUser.md),
[`set_user_info`](https://metasurveyr.github.io/metasurvey/reference/set_user_info.md),
[`certify_recipe`](https://metasurveyr.github.io/metasurvey/reference/certify_recipe.md)

## Examples

``` r
# Individual user
user <- recipe_user("Juan Perez", email = "juan@example.com")

# Institution
inst <- recipe_user("Instituto de Economia", type = "institution", verified = TRUE)

# Member linked to institution
member <- recipe_user("Maria", type = "institutional_member", institution = inst)

# Member with institution name shortcut
member2 <- recipe_user("Pedro", type = "institutional_member", institution = "IECON")
```
