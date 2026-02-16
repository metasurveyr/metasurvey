# metasurvey 0.0.7

## New features

* `Recipe$to_list()` and `RecipeWorkflow$to_list()` now include
  `metasurvey_version` for reproducibility tracking.

## Bug fixes

* Fixed `bake_step()` crash when executing `step_recode()`: removed invalid
  `record=FALSE` argument passed to internal `recode()`.
* Fixed `bake_steps_survey()` iterating over the original survey's steps
  instead of the cloned copy, causing inconsistent state.
* Fixed `bake_recipes()` environment leakage: recipe step evaluation now
  uses an isolated `new.env()` instead of the calling `environment()`.
* Removed redundant `copy()` in `compute()` and `recode()` — the
  `shallow_clone()` already handles data copying.
* `step_join()` now uses `merge.data.table()` directly and `set_data()`
  instead of assigning to `$data`.
* Fixed Shiny admin panel memory leak: replaced dynamic
  `observe→lapply→observeEvent` pattern with delegated event handlers.
* Removed `invalidateLater(0)` from admin panel refresh.

## Security

* `store_token()` no longer calls `Sys.setenv()` — API tokens are stored
  in R options only, not leaked to environment variables.
* `token_expires_soon()` returns `TRUE` for `NULL`, malformed, or missing
  `exp` claims, forcing token refresh instead of using potentially invalid tokens.
* API error messages are sanitized to 200 characters maximum.
* Added `validate_api_id()` with alphanumeric whitelist for recipe/workflow IDs.
* Added password length validation (8–128 characters) in `api_register()`.
* `api_download_recipe()` and `api_download_workflow()` now warn on failure
  instead of silently swallowing errors.

## Performance

* Added `debounce(300)` on Shiny recipe search input to reduce excessive
  reactivity.

## Internal

* Replaced `sapply()` with `vapply()` or `lapply()` across the codebase
  for type-safe return values.
* `set_engine()` uses `requireNamespace()` instead of `eval(install.packages(...))`.
* `read_recipe()` now warns when step parsing falls back to raw strings.
* Added ORCID identifiers for authors in DESCRIPTION.
* Added ANDA fetch failure notification in Shiny explore module.

# metasurvey 0.0.6

## Documentation

* All exported functions and R6 classes now have `@return` and `@examples`
  roxygen2 tags, meeting rOpenSci documentation requirements.
* Added `@return` to R6 class exports: `Recipe`, `RecipeWorkflow`,
  `RecipeCategory`, `RecipeCertification`, `RecipeUser`, `PoolSurvey`,
  `RotativePanelSurvey`, and `Survey`.
* Added `@examples` to `set_engine()`, `show_engines()`, `get_engine()`,
  and all API/backend functions.
* Added `@return` to `get_metadata()`, `set_lazy_processing()`,
  `set_use_copy()`, `set_backend()`, `set_workflow_backend()`, and
  `explore_recipes()`.

## Bug fixes

* Fixed Codecov GitHub Actions workflow: replaced deprecated `covr::codecov()`
  with `covr::package_coverage()` + `codecov/codecov-action@v5` for reliable
  coverage uploads with token authentication.
* Added CI guard to `.Rprofile` to skip example-data setup in GitHub Actions,
  preventing potential download failures during CI builds.
* Added missing `archive`, `haven`, and `openxlsx` to `Suggests` in
  DESCRIPTION, fixing R CMD check warnings about undeclared dependencies.

## Internal

* Added `.claude/` directory with development agents and commands for
  code review, testing, documentation auditing, and rOpenSci preparation.

# metasurvey 0.0.5

## Breaking changes

* **AST engine removed**: The Abstract Syntax Tree evaluation engine (`R/ast.R`)
  has been replaced by direct `data.table` expression evaluation. This simplifies
  the step pipeline and removes ~760 lines of internal code. Users who relied on
  internal AST functions (`parse_ast`, `evaluate_ast_node`, `recode_with_ast`,
  `optimize_node`) will need to update their code. The public API
  (`step_compute()`, `step_recode()`, `bake_steps()`) remains unchanged.

* **API client migrated to JWT**: The API client (`R/api_client.R`) now uses
  JWT authentication with a self-hosted Plumber backend, replacing the previous
  Atlas Data API approach.

## New features

* **Lazy design initialization**: `Survey$new()` no longer builds `svydesign`
  objects at construction time. Designs are created on-demand when needed
  (e.g., during `workflow()` or `bake_steps()`), significantly improving
  construction performance for large datasets. The `psu` parameter is now
  optional (default `NULL`).

* **Recipe ecosystem**: New infrastructure for publishing, discovering, and
  managing reproducible survey processing recipes:
  - `RecipeRegistry` R6 class with pluggable backends (JSON file, MongoDB).
  - `RecipeCategory` for hierarchical categorization of recipes.
  - `RecipeCertification` for quality certification levels (official, reviewed,
    community).
  - `RecipeUser` for author/maintainer profiles with institutional affiliations.
  - Tidy API: `list_recipes()`, `search_recipes()`, `filter_recipes()`,
    `rank_recipes()`.
  - `Recipe$doc()` for auto-generated documentation.
  - `Recipe$validate(svy)` for dependency checking against a survey.
  - `print.Recipe` S3 method for human-readable output.
  - `save_recipe()` / `read_recipe()` for JSON serialization.

* **Workflow system**: New estimation workflow infrastructure:
  - `RecipeWorkflow` R6 class capturing `svymean`/`svytotal`/`svyratio`/`svyby`
    calls with recipe references and reproducibility metadata.
  - `WorkflowRegistry` with pluggable backends (JSON, MongoDB).
  - Auto-capture of workflow metadata via `.capture_workflow()` when surveys
    contain recipes.
  - `workflow_from_list()` for reconstructing workflows from JSON/API responses.
  - Tidy API: `list_workflows()`, `search_workflows()`, `filter_workflows()`,
    `rank_workflows()`, `find_workflows_for_recipe()`, `publish_workflow()`.
  - `save_workflow()` / `read_workflow()` for serialization.

* **REST API** (`inst/api/plumber.R`): Self-hosted Plumber API with JWT
  authentication for recipe and workflow publishing. Endpoints for CRUD
  operations on recipes, workflows, and user management. Includes Docker
  deployment configuration.

* **ANDA catalog integration** (`R/anda.R`): Functions for accessing INE
  Uruguay's ANDA5 catalog metadata:
  - `anda_catalog_search()` for searching the catalog.
  - `anda_list_editions()` for listing available survey editions.
  - `anda_fetch_ddi()` for fetching DDI metadata.
  - `anda_parse_variables()` for extracting variable definitions.
  - `anda_variable_detail()` for detailed variable information.

* **Shiny explorer app** (`inst/shiny/`): Interactive recipe and workflow
  browser launched via `explore_recipes()`:
  - Recipe gallery with search, filtering by survey type/certification, and
    detail modals showing pipeline visualization and R code snippets.
  - Workflow gallery with estimation timeline and cross-references to recipes.
  - User authentication with registration, login, and profile management.
  - Token generation for programmatic API access from R scripts.

* **`bake_recipes()` handles string steps**: Steps stored as strings (from
  JSON/API) are now parsed to call objects before evaluation, fixing the
  previous "attempt to apply non-function" error.

## Bug fixes

* `is_blank()` now handles `NULL` and zero-length inputs without error.
* Fixed regex in `extract_time_pattern()`: type prefix detection now correctly
  matches only at string start.
* Multianual year-range patterns (e.g., `"2019_2021"`) are now correctly
  parsed when separated by underscore.
* `validate_time_pattern()` gracefully handles `NULL`/empty edition strings.
* Improved `public_key()` error handling with informative messages.
* `workflow_from_list()` is now exported (was `@keywords internal`), fixing
  Shiny app workflow parsing from API responses.

## Documentation

* Complete vignette rewrite with `eval=TRUE` examples:
  - `getting-started`: Survey creation, steps, recipes, and workflows.
  - `recipes`: Recipe ecosystem, publishing, and discovery.
  - `workflows-and-estimation`: Estimation workflows and registry.
  - `complex-designs`: Replicate weights, stratified designs.
  - `panel-analysis`: Rotating panel surveys.
  - `ech-case-study`: Full ECH processing example.
  - `shiny-explorer`: Interactive app usage guide.
  - `api-database`: REST API and database architecture.
* All vignettes available in Spanish (`*-es.Rmd`).
* Added `references.bib` for academic citations.
* Added `pkgdown` site configuration with rOpenSci template.

# metasurvey 0.0.3

# metasurvey 0.0.2

# metasurvey 0.0.1

* Initial CRAN submission.
