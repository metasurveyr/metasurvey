# WorkflowRegistry

Local JSON-backed catalog for workflow discovery, ranking, and
filtering.

## Methods

### Public methods

- [`WorkflowRegistry$new()`](#method-WorkflowRegistry-new)

- [`WorkflowRegistry$register()`](#method-WorkflowRegistry-register)

- [`WorkflowRegistry$unregister()`](#method-WorkflowRegistry-unregister)

- [`WorkflowRegistry$search()`](#method-WorkflowRegistry-search)

- [`WorkflowRegistry$filter()`](#method-WorkflowRegistry-filter)

- [`WorkflowRegistry$find_by_recipe()`](#method-WorkflowRegistry-find_by_recipe)

- [`WorkflowRegistry$rank_by_downloads()`](#method-WorkflowRegistry-rank_by_downloads)

- [`WorkflowRegistry$get()`](#method-WorkflowRegistry-get)

- [`WorkflowRegistry$list_all()`](#method-WorkflowRegistry-list_all)

- [`WorkflowRegistry$save()`](#method-WorkflowRegistry-save)

- [`WorkflowRegistry$load()`](#method-WorkflowRegistry-load)

- [`WorkflowRegistry$stats()`](#method-WorkflowRegistry-stats)

- [`WorkflowRegistry$print()`](#method-WorkflowRegistry-print)

- [`WorkflowRegistry$clone()`](#method-WorkflowRegistry-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new empty WorkflowRegistry

#### Usage

    WorkflowRegistry$new()

------------------------------------------------------------------------

### Method `register()`

Register a workflow in the catalog

#### Usage

    WorkflowRegistry$register(wf)

#### Arguments

- `wf`:

  RecipeWorkflow object to register

------------------------------------------------------------------------

### Method `unregister()`

Remove a workflow from the catalog by id

#### Usage

    WorkflowRegistry$unregister(workflow_id)

#### Arguments

- `workflow_id`:

  Workflow id to remove

------------------------------------------------------------------------

### Method [`search()`](https://rdrr.io/r/base/search.html)

Search workflows by name or description (case-insensitive)

#### Usage

    WorkflowRegistry$search(query)

#### Arguments

- `query`:

  Character search query

#### Returns

List of matching RecipeWorkflow objects

------------------------------------------------------------------------

### Method [`filter()`](https://rdrr.io/r/stats/filter.html)

Filter workflows by criteria

#### Usage

    WorkflowRegistry$filter(
      survey_type = NULL,
      edition = NULL,
      recipe_id = NULL,
      certification_level = NULL
    )

#### Arguments

- `survey_type`:

  Character survey type or NULL

- `edition`:

  Character edition or NULL

- `recipe_id`:

  Character recipe ID or NULL (find workflows using this recipe)

- `certification_level`:

  Character certification level or NULL

#### Returns

List of matching RecipeWorkflow objects

------------------------------------------------------------------------

### Method `find_by_recipe()`

Find workflows that reference a specific recipe

#### Usage

    WorkflowRegistry$find_by_recipe(recipe_id)

#### Arguments

- `recipe_id`:

  Character recipe ID

#### Returns

List of RecipeWorkflow objects referencing this recipe

------------------------------------------------------------------------

### Method `rank_by_downloads()`

Rank workflows by download count (descending)

#### Usage

    WorkflowRegistry$rank_by_downloads(n = NULL)

#### Arguments

- `n`:

  Integer max number to return, or NULL for all

#### Returns

List of RecipeWorkflow objects sorted by downloads

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get a single workflow by id

#### Usage

    WorkflowRegistry$get(workflow_id)

#### Arguments

- `workflow_id`:

  Workflow id

#### Returns

RecipeWorkflow object or NULL

------------------------------------------------------------------------

### Method `list_all()`

List all registered workflows

#### Usage

    WorkflowRegistry$list_all()

#### Returns

List of all RecipeWorkflow objects

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save the registry catalog to a JSON file

#### Usage

    WorkflowRegistry$save(path)

#### Arguments

- `path`:

  Character file path

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

Load a registry catalog from a JSON file

#### Usage

    WorkflowRegistry$load(path)

#### Arguments

- `path`:

  Character file path

------------------------------------------------------------------------

### Method `stats()`

Get registry statistics

#### Usage

    WorkflowRegistry$stats()

#### Returns

List with total, by_survey_type, by_certification counts

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print registry summary

#### Usage

    WorkflowRegistry$print(...)

#### Arguments

- `...`:

  Additional arguments (not used)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    WorkflowRegistry$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Use the tidy API instead:
# set_workflow_backend("local", path = "workflows.json")
# publish_workflow(my_workflow)
# search_workflows("labor")
```
