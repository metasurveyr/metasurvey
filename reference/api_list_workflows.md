# List workflows from API

Fetch workflows with optional search and filters.

## Usage

``` r
api_list_workflows(
  search = NULL,
  survey_type = NULL,
  recipe_id = NULL,
  user = NULL,
  limit = 50,
  offset = 0
)
```

## Arguments

- search:

  Text search

- survey_type:

  Filter by survey type

- recipe_id:

  Filter workflows that reference this recipe

- user:

  Filter by author email

- limit:

  Maximum results

- offset:

  Skip first N results

## Value

List of RecipeWorkflow objects
