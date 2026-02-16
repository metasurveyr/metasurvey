# Changelog

## metasurvey 0.0.5

### Breaking changes

- **AST engine removed**: The Abstract Syntax Tree evaluation engine
  (`R/ast.R`) has been replaced by direct `data.table` expression
  evaluation. This simplifies the step pipeline and removes ~760 lines
  of internal code. Users who relied on internal AST functions
  (`parse_ast`, `evaluate_ast_node`, `recode_with_ast`, `optimize_node`)
  will need to update their code. The public API
  ([`step_compute()`](https://metasurveyr.github.io/metasurvey/reference/step_compute.md),
  [`step_recode()`](https://metasurveyr.github.io/metasurvey/reference/step_recode.md),
  [`bake_steps()`](https://metasurveyr.github.io/metasurvey/reference/bake_steps.md))
  remains unchanged.

- **API client migrated to JWT**: The API client (`R/api_client.R`) now
  uses JWT authentication with a self-hosted Plumber backend, replacing
  the previous Atlas Data API approach.

### New features

- **Lazy design initialization**: `Survey$new()` no longer builds
  `svydesign` objects at construction time. Designs are created
  on-demand when needed (e.g., during
  [`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
  or
  [`bake_steps()`](https://metasurveyr.github.io/metasurvey/reference/bake_steps.md)),
  significantly improving construction performance for large datasets.
  The `psu` parameter is now optional (default `NULL`).

- **Recipe ecosystem**: New infrastructure for publishing, discovering,
  and managing reproducible survey processing recipes:

  - `RecipeRegistry` R6 class with pluggable backends (JSON file,
    MongoDB).
  - `RecipeCategory` for hierarchical categorization of recipes.
  - `RecipeCertification` for quality certification levels (official,
    reviewed, community).
  - `RecipeUser` for author/maintainer profiles with institutional
    affiliations.
  - Tidy API:
    [`list_recipes()`](https://metasurveyr.github.io/metasurvey/reference/list_recipes.md),
    [`search_recipes()`](https://metasurveyr.github.io/metasurvey/reference/search_recipes.md),
    [`filter_recipes()`](https://metasurveyr.github.io/metasurvey/reference/filter_recipes.md),
    [`rank_recipes()`](https://metasurveyr.github.io/metasurvey/reference/rank_recipes.md).
  - `Recipe$doc()` for auto-generated documentation.
  - `Recipe$validate(svy)` for dependency checking against a survey.
  - `print.Recipe` S3 method for human-readable output.
  - [`save_recipe()`](https://metasurveyr.github.io/metasurvey/reference/save_recipe.md)
    /
    [`read_recipe()`](https://metasurveyr.github.io/metasurvey/reference/read_recipe.md)
    for JSON serialization.

- **Workflow system**: New estimation workflow infrastructure:

  - `RecipeWorkflow` R6 class capturing
    `svymean`/`svytotal`/`svyratio`/`svyby` calls with recipe references
    and reproducibility metadata.
  - `WorkflowRegistry` with pluggable backends (JSON, MongoDB).
  - Auto-capture of workflow metadata via `.capture_workflow()` when
    surveys contain recipes.
  - [`workflow_from_list()`](https://metasurveyr.github.io/metasurvey/reference/workflow_from_list.md)
    for reconstructing workflows from JSON/API responses.
  - Tidy API:
    [`list_workflows()`](https://metasurveyr.github.io/metasurvey/reference/list_workflows.md),
    [`search_workflows()`](https://metasurveyr.github.io/metasurvey/reference/search_workflows.md),
    [`filter_workflows()`](https://metasurveyr.github.io/metasurvey/reference/filter_workflows.md),
    [`rank_workflows()`](https://metasurveyr.github.io/metasurvey/reference/rank_workflows.md),
    [`find_workflows_for_recipe()`](https://metasurveyr.github.io/metasurvey/reference/find_workflows_for_recipe.md),
    [`publish_workflow()`](https://metasurveyr.github.io/metasurvey/reference/publish_workflow.md).
  - [`save_workflow()`](https://metasurveyr.github.io/metasurvey/reference/save_workflow.md)
    /
    [`read_workflow()`](https://metasurveyr.github.io/metasurvey/reference/read_workflow.md)
    for serialization.

- **REST API** (`inst/api/plumber.R`): Self-hosted Plumber API with JWT
  authentication for recipe and workflow publishing. Endpoints for CRUD
  operations on recipes, workflows, and user management. Includes Docker
  deployment configuration.

- **ANDA catalog integration** (`R/anda.R`): Functions for accessing INE
  Uruguay’s ANDA5 catalog metadata:

  - [`anda_catalog_search()`](https://metasurveyr.github.io/metasurvey/reference/anda_catalog_search.md)
    for searching the catalog.
  - [`anda_list_editions()`](https://metasurveyr.github.io/metasurvey/reference/anda_list_editions.md)
    for listing available survey editions.
  - [`anda_fetch_ddi()`](https://metasurveyr.github.io/metasurvey/reference/anda_fetch_ddi.md)
    for fetching DDI metadata.
  - [`anda_parse_variables()`](https://metasurveyr.github.io/metasurvey/reference/anda_parse_variables.md)
    for extracting variable definitions.
  - [`anda_variable_detail()`](https://metasurveyr.github.io/metasurvey/reference/anda_variable_detail.md)
    for detailed variable information.

- **Shiny explorer app** (`inst/shiny/`): Interactive recipe and
  workflow browser launched via `run_metasurvey_app()`:

  - Recipe gallery with search, filtering by survey type/certification,
    and detail modals showing pipeline visualization and R code
    snippets.
  - Workflow gallery with estimation timeline and cross-references to
    recipes.
  - User authentication with registration, login, and profile
    management.
  - Token generation for programmatic API access from R scripts.

- **[`bake_recipes()`](https://metasurveyr.github.io/metasurvey/reference/bake_recipes.md)
  handles string steps**: Steps stored as strings (from JSON/API) are
  now parsed to call objects before evaluation, fixing the previous
  “attempt to apply non-function” error.

### Bug fixes

- `is_blank()` now handles `NULL` and zero-length inputs without error.
- Fixed regex in
  [`extract_time_pattern()`](https://metasurveyr.github.io/metasurvey/reference/extract_time_pattern.md):
  type prefix detection now correctly matches only at string start.
- Multianual year-range patterns (e.g., `"2019_2021"`) are now correctly
  parsed when separated by underscore.
- [`validate_time_pattern()`](https://metasurveyr.github.io/metasurvey/reference/validate_time_pattern.md)
  gracefully handles `NULL`/empty edition strings.
- Improved `public_key()` error handling with informative messages.
- [`workflow_from_list()`](https://metasurveyr.github.io/metasurvey/reference/workflow_from_list.md)
  is now exported (was `@keywords internal`), fixing Shiny app workflow
  parsing from API responses.

### Documentation

- Complete vignette rewrite with `eval=TRUE` examples:
  - `getting-started`: Survey creation, steps, recipes, and workflows.
  - `recipes`: Recipe ecosystem, publishing, and discovery.
  - `workflows-and-estimation`: Estimation workflows and registry.
  - `complex-designs`: Replicate weights, stratified designs.
  - `panel-analysis`: Rotating panel surveys.
  - `ech-case-study`: Full ECH processing example.
  - `shiny-explorer`: Interactive app usage guide.
  - `api-database`: REST API and database architecture.
- All vignettes available in Spanish (`*-es.Rmd`).
- Added `references.bib` for academic citations.
- Added `pkgdown` site configuration with rOpenSci template.

## metasurvey 0.0.3

## metasurvey 0.0.2

## metasurvey 0.0.1

- Initial CRAN submission.
