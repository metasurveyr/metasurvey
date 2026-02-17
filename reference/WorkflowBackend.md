# WorkflowBackend

Backend-agnostic factory for workflow storage and retrieval. Supports
"local" (JSON-backed WorkflowRegistry) and "api" (remote plumber API)
backends.

## Public fields

- `type`:

  Character backend type ("local" or "api").

## Methods

### Public methods

- [`WorkflowBackend$new()`](#method-WorkflowBackend-new)

- [`WorkflowBackend$publish()`](#method-WorkflowBackend-publish)

- [`WorkflowBackend$search()`](#method-WorkflowBackend-search)

- [`WorkflowBackend$get()`](#method-WorkflowBackend-get)

- [`WorkflowBackend$increment_downloads()`](#method-WorkflowBackend-increment_downloads)

- [`WorkflowBackend$find_by_recipe()`](#method-WorkflowBackend-find_by_recipe)

- [`WorkflowBackend$rank()`](#method-WorkflowBackend-rank)

- [`WorkflowBackend$filter()`](#method-WorkflowBackend-filter)

- [`WorkflowBackend$list_all()`](#method-WorkflowBackend-list_all)

- [`WorkflowBackend$save()`](#method-WorkflowBackend-save)

- [`WorkflowBackend$load()`](#method-WorkflowBackend-load)

- [`WorkflowBackend$clone()`](#method-WorkflowBackend-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new WorkflowBackend

#### Usage

    WorkflowBackend$new(type, path = NULL)

#### Arguments

- `type`:

  Character. "local" or "api".

- `path`:

  Character. File path for local backend (optional).

------------------------------------------------------------------------

### Method `publish()`

Publish a workflow to the backend

#### Usage

    WorkflowBackend$publish(wf)

#### Arguments

- `wf`:

  RecipeWorkflow object

------------------------------------------------------------------------

### Method [`search()`](https://rdrr.io/r/base/search.html)

Search workflows

#### Usage

    WorkflowBackend$search(query)

#### Arguments

- `query`:

  Character search string

#### Returns

List of matching RecipeWorkflow objects

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get a workflow by id

#### Usage

    WorkflowBackend$get(id)

#### Arguments

- `id`:

  Workflow id

#### Returns

RecipeWorkflow object or NULL

------------------------------------------------------------------------

### Method `increment_downloads()`

Increment download count for a workflow

#### Usage

    WorkflowBackend$increment_downloads(id)

#### Arguments

- `id`:

  Workflow id

------------------------------------------------------------------------

### Method `find_by_recipe()`

Find workflows that reference a specific recipe

#### Usage

    WorkflowBackend$find_by_recipe(recipe_id)

#### Arguments

- `recipe_id`:

  Character recipe ID

#### Returns

List of RecipeWorkflow objects

------------------------------------------------------------------------

### Method [`rank()`](https://rdrr.io/r/base/rank.html)

Rank workflows by downloads

#### Usage

    WorkflowBackend$rank(n = NULL)

#### Arguments

- `n`:

  Integer max to return

#### Returns

List of RecipeWorkflow objects

------------------------------------------------------------------------

### Method [`filter()`](https://rdrr.io/r/stats/filter.html)

Filter workflows by criteria

#### Usage

    WorkflowBackend$filter(
      survey_type = NULL,
      edition = NULL,
      recipe_id = NULL,
      certification_level = NULL
    )

#### Arguments

- `survey_type`:

  Character or NULL

- `edition`:

  Character or NULL

- `recipe_id`:

  Character or NULL

- `certification_level`:

  Character or NULL

#### Returns

List of matching RecipeWorkflow objects

------------------------------------------------------------------------

### Method `list_all()`

List all workflows

#### Usage

    WorkflowBackend$list_all()

#### Returns

List of RecipeWorkflow objects

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save local backend to disk

#### Usage

    WorkflowBackend$save()

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

Load local backend from disk

#### Usage

    WorkflowBackend$load()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    WorkflowBackend$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Local backend (internal class, use set_workflow_backend() instead)
backend <- WorkflowBackend$new("local", path = tempfile(fileext = ".json"))
backend$search("labor")
} # }
```
