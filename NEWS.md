# metasurvey 0.0.14

## Breaking changes
* `Step$comments` field renamed to `Step$comment`. The old name still works
  as a legacy alias but will be removed in a future version.
* `workflow()` first parameter renamed from `survey` to `svy` for consistency.
  Positional usage is unchanged.

## New features
* `use_copy` parameter in all step functions (`step_compute`, `step_recode`,
  `step_join`, `step_remove`, `step_rename`) is deprecated in favour of `.copy`,
  following dplyr's dot-prefix convention for secondary arguments.
* Added survey state predicates: `has_steps()`, `has_recipes()`, `is_baked()`,
  `has_design()`.
* `step_recode()`, `step_join()`, `step_remove()`, `step_rename()` no longer
  default `svy` to `survey_empty()`; the survey argument is now required.
* `extract_time_pattern()` now recognises quarter/trimester editions
  (`"2023-T3"`, `"2023-Q1"`), parsed as `Quarterly` periodicity.

## Bug fixes
* Fixed `validate_time_pattern()` crash when `svy_edition` is `NA`.
* Fixed `shallow_clone()` re-running `validate_time_pattern()` on
  already-parsed editions, causing cascading NULL → NA → crash.
* Fixed `get_metadata()` crash when edition is NULL or NA.
* Fixed `cat_estimation.svyby()` producing wrong output: margins index was
  integer (not column name), SE column `"se"` was missed by grep, and
  `cv()` data.frame was not handled.  Output now shows group labels in
  the `stat` column (e.g. `svyby: x [sexo=1]`).
* Mixed estimation types (`svymean` + `svyby`) no longer fail on
  `rbindlist` due to mismatched columns.

## Documentation
* Updated vignettes to use `Step$comment` instead of `Step$comments`.
* Vignettes now use real ECH 2023 sample data instead of simulated data.
* Fixed ECH variable names for 2023 edition (`e31` → `e30`).

# metasurvey 0.0.12

## Bug fixes
* Fixed critical memory leak when chaining N steps: eliminated `svy_before`
  retention chain that prevented GC from freeing N copies of the data.table.
* Fixed `bake_step` re-recording step_join/step_remove/step_rename during bake,
  which caused duplicate steps.

## Performance
* `bake_steps()` uses `shallow_clone()` instead of `clone(deep=TRUE)`,
  avoiding duplication of the entire step chain on every bake.
* `add_step()` invalidates design lazily instead of rebuilding `svydesign`
  on every step addition.
* Removed wasteful `copy()` calls in `step_compute` and `step_recode`.

## Internal
* Added 500+ tests: memory behavior (test-memory.R), expanded integration
  pipelines (test-integration.R), and stress tests with 100 steps on 10k+
  rows (test-stress.R, skipped on CRAN).

# metasurvey 0.0.11

## Documentation
* Fixed examples across 12 R files for strict CRAN compliance: functions
  requiring an API server, external files, or interactive sessions now use
  `\dontrun{}` instead of `\donttest{}`.
* Fixed `RecipeWorkflow` examples: corrected `recipe_ids` parameter name,
  removed non-existent `variables` parameter.
* Fixed `add_weight`/`add_replicate` examples: removed double-escaped regex
  patterns, removed `load_survey()` calls that depend on external files.
* Made `set_workflow_backend`/`get_workflow_backend` examples runnable
  without `\dontrun{}` wrapper.

## Internal
* Fixed GitHub Actions: added `main` to R-CMD-check PR triggers, replaced
  `master` with `main` in test-coverage and pkgdown, removed broken
  `setup-r-test.yml`.
* Fixed deploy workflow: secrets passed via env vars instead of inline
  shell interpolation, Railway CLI installed via npm with pinned version.

# metasurvey 0.0.10

## New features
* Docker deployment pipeline with GitHub Actions: builds and pushes images
  to GHCR, deploys to Railway on push to main.
* `docker-compose.yml` for local development (API + Shiny stack).

## Bug fixes
* `api_url()` no longer falls back to a hardcoded production URL; returns
  NULL when no URL is configured via option or environment variable.

## Documentation
* Replaced remaining `\dontrun{}` with `\donttest{}` across all exported
  examples for CRAN compliance.
* Added runnable examples for `RecipeWorkflow`, `save_workflow`,
  `read_workflow`, `search_workflows`, `rank_workflows`, `RecipeBackend`.
* Added STATA transpiler usage section to README.
* Added Spanish vignettes: `stata-transpiler-es`, `ech-demographics-recipe-es`.

## Internal
* Updated API Dockerfile with curl-based healthcheck.
* Updated Shiny Dockerfile: configurable `METASURVEY_REF` build arg,
  added `jose` and `git` dependencies.

# metasurvey 0.0.9

## CRAN compliance

* Namespaced all global options: `use_copy` → `metasurvey.use_copy`,
  `lazy_processing` → `metasurvey.lazy_processing`.
* `set_use_copy()` and `set_lazy_processing()` return the previous value
  invisibly, matching `set_engine()` behavior.
* `set_backend()` and `set_workflow_backend()` return the previous value
  invisibly.
* Replaced all `\dontrun{}` examples with runnable or `\donttest{}` where
  appropriate (`set_engine()`, `view_graph()`, `survey_empty()`,
  `set_workflow_backend()`, `get_workflow_backend()`).
* Fixed `print.RecipeWorkflow` example: removed non-existent `recipe_name`
  and `results` arguments.
* Fixed `extract_time_pattern()` roxygen block: was accidentally attached to
  internal `validate_monthly()`, causing the export to be dropped.
* Fixed malformed `@keywords` tags: removed comma-separated values in
  `PoolSurvey` and `RotativePanelSurvey`, removed duplicate description
  block in `Survey` that was parsed as keyword entries.
* Simplified `.onAttach()` to a single version message.
* Removed hardcoded production API URL from `.onLoad()`.
* Deleted orphan Spanish-only vignette `metasurvey-es.Rmd`.
* Added `.Rbuildignore` entries for deployment artifacts (`inst/scripts`,
  `inst/seed-data`, Dockerfiles, `inst/shiny-auth.R`).
* Deleted empty `inst/extdata/` directory.
* Fixed `add_weight()` regex example.
* Fixed `load_survey()` example.
* Fixed `set_lazy()` → `set_lazy_processing()` in vignettes.
* Enabled `eval=TRUE` on in-memory vignette chunks (`bake`, `get-steps`).
* `load_survey_example()`: removed dead `file.exists()` branch, wrapped
  `download.file()` in `tryCatch()` for graceful failure.
* Added `requireNamespace("htmltools")` check in `view_graph()`.
* Removed version constraint from `parallel` in Suggests.
* R CMD check: 0 errors, 0 warnings, 1 note (new submission).

## Translations

* All remaining Spanish return values translated to English:
  `"Trianual"` → `"Triennial"`, `"Multianual"` → `"Multi-year"`.
* All Spanish inline comments translated to English.
* Translated roxygen documentation for `recipe()`, `extract_surveys()`,
  `get_implantation()`, `get_follow_up()`.

## New features

* Added STATA-to-metasurvey transpiler: `transpile_stata()`,
  `transpile_stata_module()`, `parse_do_file()`, `parse_stata_labels()`,
  and `transpile_coverage()`. Converts `.do` files into Recipe JSON,
  supporting gen/replace chains, recode, egen, foreach loops, mvencode,
  destring, labels, and more.
* Added `labels` field to `Recipe` class for storing variable and value
  labels from STATA transpilation.
* Added Shiny DAG graph loading spinner and disclaimer for recipes with
  more than 20 steps.

## Bug fixes

* Fixed `validate_weight_time_pattern()` crash when `weight` is NULL
  (triggered by `shallow_clone()` on surveys without weights).
* Fixed `steps_to_recipe()` producing unparseable step strings: long
  `step_recode` calls were split across multiple lines by `deparse()`,
  breaking JSON round-trip.
* Fixed `bake_recipes()` not executing `step_rename` and `step_remove`:
  recipe step replay now temporarily disables lazy processing so all
  steps execute immediately.
* Fixed Shiny `category_tag()` crash when recipe topic is NULL.

## Documentation

* Added `@family` tags to all exported functions across 16 groups.
* Standardized `@keywords` tags (removed commas, fixed multi-word entries).
* Removed redundant `@title` tags in `R/set_engine.R`.
* Updated `cran-comments.md` to reflect 1 NOTE (new submission).
* Added `stata-transpiler` vignette covering all supported STATA patterns.
* Added `ech-demographics-recipe` vignette showing hand-crafted vs
  transpiled recipe workflows.

# metasurvey 0.0.8

## Bug fixes

* Fixed 5 occurrences of `stop(message(...))` which silently called `stop(NULL)`
  instead of raising a proper error (`set_engine()`, `load_survey()`, `recipe()`).
* Fixed `.Rbuildignore` double-escape bug: patterns had `\\\\.` instead of `\\.`,
  causing hidden files and non-standard files to leak into the tarball.
* Replaced unsafe `1:n` sequences with `seq_len()`/`seq.int()` to avoid
  `c(1, 0)` when n=0 (7 occurrences across survey.R, workflow.R, steps.R).
* Removed duplicate internal `set_data()` definition in survey.R.
* Replaced raw ANSI escape codes with `crayon::red()`/`crayon::green()` in
  `cat_design()` for cross-platform compatibility.
* Removed unreachable dead code in `load_survey_example()`.
* `requireNamespace("parallel")` now uses `quietly = TRUE` and checks the
  return value before using the package.
* `requireNamespace("rio")` return value is now checked before calling
  `rio::convert()`.

## Security

* SSL certificate verification in ANDA client is now enabled by default.
  Previously hardcoded `ssl_verifypeer = FALSE`; now user-controllable
  via `options(metasurvey.ssl_verify = FALSE)`.
* `api_logout()` no longer calls `Sys.unsetenv("METASURVEY_TOKEN")` — only
  clears the R option, per CRAN policy on environment variables.

## CRAN compliance

* `set_engine()`, `set_use_copy()`, `set_lazy_processing()`, and
  `configure_api()` now return the previous value invisibly, allowing users
  to restore global options.
* All user-facing messages translated from Spanish to English.
* `evaluate_cv()` returns English labels ("Excellent", "Good", etc.).
* `extract_time_pattern()` returns "Invalid format"/"Unknown format".
* DESCRIPTION title and description rewritten to be substantive.
* `inst/CITATION` updated: removed deprecated `citHeader()`, dynamic year.
* Fixed inconsistent maintainer email across DESCRIPTION and package docs.
* R CMD check now passes with 0 errors, 0 warnings, 0 NOTEs.

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

* Added `step_join()` for merging external reference data into surveys.
* Added `step_remove()` and `step_rename()` for variable management.

# metasurvey 0.0.2

* Added `RotativePanelSurvey` and `PoolSurvey` R6 classes for complex designs.
* Added `extract_surveys()`, `get_implantation()`, `get_follow_up()`.

# metasurvey 0.0.1

* Initial CRAN submission.
