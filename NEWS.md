# metasurvey 0.0.4

## Breaking changes

* **AST engine removed**: The Abstract Syntax Tree evaluation engine (`R/ast.R`)
  has been replaced by direct `data.table` expression evaluation. This simplifies
  the step pipeline and removes ~760 lines of internal code. Users who relied on
  internal AST functions (`parse_ast`, `evaluate_ast_node`, `recode_with_ast`,
  `optimize_node`) will need to update their code. The public API
  (`step_compute()`, `step_recode()`, `bake_steps()`) remains unchanged.

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
  - `RecipeCertification` for quality certification levels.
  - `RecipeUser` for author/maintainer profiles.
  - Tidy API: `list_recipes()`, `search_recipes()`, `filter_recipes()`,
    `rank_recipes()`, `explore_recipes()`.
  - `Recipe$doc()` for auto-generated documentation.
  - `Recipe$validate(svy)` for dependency checking against a survey.
  - `print.Recipe` S3 method for human-readable output.

* **Workflow registry and publishing**: New system for managing survey
  estimation workflows:
  - `RecipeWorkflow` R6 class capturing `svymean`/`svytotal`/`svyby` calls
    with recipe references and reproducibility metadata.
  - `WorkflowRegistry` with pluggable backends (JSON, MongoDB).
  - Tidy API: `list_workflows()`, `search_workflows()`, `filter_workflows()`,
    `rank_workflows()`, `find_workflows_for_recipe()`.
  - `save_workflow()` / `read_workflow()` for serialization.
  - `publish_workflow()` for sharing via registry.

* **Shiny explorer app**: Interactive recipe and workflow browser
  (`run_metasurvey_app()`).

## Bug fixes

* `is_blank()` now handles `NULL` and zero-length inputs without error.
* Fixed regex in `extract_time_pattern()`: type prefix detection now correctly
  matches only at string start (`"^[^0-9]"` instead of `"$[^0-9]*"`).
* Multianual year-range patterns (e.g., `"2019_2021"`) are now correctly
  parsed when separated by underscore.
* `validate_time_pattern()` gracefully handles `NULL`/empty edition strings.
* Improved `public_key()` error handling with informative messages.

## Documentation

* Expanded `getting-started` and `metasurvey-es` vignettes with Recipe
  ecosystem, Workflow, and new API coverage.
* Added `references.bib` for academic citations.

# metasurvey 0.0.3

# metasurvey 0.0.2

# metasurvey 0.0.1

* Initial CRAN submission.
