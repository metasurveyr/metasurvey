# Find workflows that use a specific recipe

Cross-reference query: find all workflows that reference a given recipe
ID.

## Usage

``` r
find_workflows_for_recipe(recipe_id)
```

## Arguments

- recipe_id:

  Character recipe ID to search for.

## Value

List of RecipeWorkflow objects that reference this recipe.

## See also

[`filter_workflows`](https://metasurveyr.github.io/metasurvey/reference/filter_workflows.md)

## Examples

``` r
if (FALSE) { # \dontrun{
wfs <- find_workflows_for_recipe("recipe_001")
} # }
```
