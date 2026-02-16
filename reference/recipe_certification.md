# Create a recipe certification

Creates a
[`RecipeCertification`](https://metasurveyr.github.io/metasurvey/reference/RecipeCertification.md)
object. Typically you would use
[`certify_recipe`](https://metasurveyr.github.io/metasurvey/reference/certify_recipe.md)
to certify a recipe in a pipeline instead.

## Usage

``` r
recipe_certification(level = "community", certified_by = NULL, notes = NULL)
```

## Arguments

- level:

  Character. One of `"community"` (default), `"reviewed"`, or
  `"official"`.

- certified_by:

  RecipeUser or NULL. Required for reviewed/official.

- notes:

  Character or NULL. Additional notes.

## Value

A
[`RecipeCertification`](https://metasurveyr.github.io/metasurvey/reference/RecipeCertification.md)
object.

## See also

[`RecipeCertification`](https://metasurveyr.github.io/metasurvey/reference/RecipeCertification.md),
[`certify_recipe`](https://metasurveyr.github.io/metasurvey/reference/certify_recipe.md)

## Examples

``` r
# Default community certification
cert <- recipe_certification()

# Official certification
inst <- recipe_user("IECON", type = "institution")
cert <- recipe_certification("official", certified_by = inst)
```
