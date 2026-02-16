# Filter workflows by criteria

Filter workflows in the active backend by survey type, edition, recipe
ID, or certification level.

## Usage

``` r
filter_workflows(
  survey_type = NULL,
  edition = NULL,
  recipe_id = NULL,
  certification_level = NULL
)
```

## Arguments

- survey_type:

  Character survey type or NULL.

- edition:

  Character edition or NULL.

- recipe_id:

  Character recipe ID or NULL (find workflows using this recipe).

- certification_level:

  Character certification level or NULL.

## Value

List of matching RecipeWorkflow objects.

## See also

[`search_workflows`](https://metasurveyr.github.io/metasurvey/reference/search_workflows.md),
[`find_workflows_for_recipe`](https://metasurveyr.github.io/metasurvey/reference/find_workflows_for_recipe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ech_wf <- filter_workflows(survey_type = "ech")
for_recipe <- filter_workflows(recipe_id = "recipe_001")
} # }
```
