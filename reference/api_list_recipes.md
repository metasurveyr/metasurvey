# List recipes from API

Fetch recipes with optional search and filters.

## Usage

``` r
api_list_recipes(
  search = NULL,
  survey_type = NULL,
  topic = NULL,
  certification = NULL,
  user = NULL,
  limit = 50,
  offset = 0
)
```

## Arguments

- search:

  Text search (matches name/description)

- survey_type:

  Filter by survey type (e.g., `"ech"`)

- topic:

  Filter by topic

- certification:

  Filter by certification level

- user:

  Filter by author email

- limit:

  Maximum results (default 50)

- offset:

  Skip first N results (default 0)

## Value

List of Recipe objects

## See also

Other api-recipes:
[`api_get_recipe()`](https://metasurveyr.github.io/metasurvey/reference/api_get_recipe.md),
[`api_publish_recipe()`](https://metasurveyr.github.io/metasurvey/reference/api_publish_recipe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
configure_api("https://metasurvey-api.example.com")
recipes <- api_list_recipes(survey_type = "ech")
} # }
```
