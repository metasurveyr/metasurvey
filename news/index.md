# Changelog

## metasurvey 0.0.9

### CRAN compliance

- Namespaced all global options: `use_copy` → `metasurvey.use_copy`,
  `lazy_processing` → `metasurvey.lazy_processing`.
- [`set_use_copy()`](https://metasurveyr.github.io/metasurvey/reference/set_use_copy.md)
  and
  [`set_lazy_processing()`](https://metasurveyr.github.io/metasurvey/reference/set_lazy_processing.md)
  return the previous value invisibly, matching
  [`set_engine()`](https://metasurveyr.github.io/metasurvey/reference/set_engine.md)
  behavior.
- [`set_backend()`](https://metasurveyr.github.io/metasurvey/reference/set_backend.md)
  and
  [`set_workflow_backend()`](https://metasurveyr.github.io/metasurvey/reference/set_workflow_backend.md)
  return the previous value invisibly.
- Replaced all `\dontrun{}` examples with runnable or `\donttest{}`
  where appropriate
  ([`set_engine()`](https://metasurveyr.github.io/metasurvey/reference/set_engine.md),
  [`view_graph()`](https://metasurveyr.github.io/metasurvey/reference/view_graph.md),
  [`survey_empty()`](https://metasurveyr.github.io/metasurvey/reference/survey_empty.md),
  [`set_workflow_backend()`](https://metasurveyr.github.io/metasurvey/reference/set_workflow_backend.md),
  [`get_workflow_backend()`](https://metasurveyr.github.io/metasurvey/reference/get_workflow_backend.md)).
- Fixed `print.RecipeWorkflow` example: removed non-existent
  `recipe_name` and `results` arguments.
- Fixed
  [`extract_time_pattern()`](https://metasurveyr.github.io/metasurvey/reference/extract_time_pattern.md)
  roxygen block: was accidentally attached to internal
  `validate_monthly()`, causing the export to be dropped.
- Fixed malformed `@keywords` tags: removed comma-separated values in
  `PoolSurvey` and `RotativePanelSurvey`, removed duplicate description
  block in `Survey` that was parsed as keyword entries.
- Simplified `.onAttach()` to a single version message.
- Removed hardcoded production API URL from `.onLoad()`.
- Deleted orphan Spanish-only vignette `metasurvey-es.Rmd`.
- Added `.Rbuildignore` entries for deployment artifacts
  (`inst/scripts`, `inst/seed-data`, Dockerfiles, `inst/shiny-auth.R`).
- Deleted empty `inst/extdata/` directory.
- Fixed
  [`add_weight()`](https://metasurveyr.github.io/metasurvey/reference/add_weight.md)
  regex example.
- Fixed
  [`load_survey()`](https://metasurveyr.github.io/metasurvey/reference/load_survey.md)
  example.
- Fixed `set_lazy()` →
  [`set_lazy_processing()`](https://metasurveyr.github.io/metasurvey/reference/set_lazy_processing.md)
  in vignettes.
- Enabled `eval=TRUE` on in-memory vignette chunks (`bake`,
  `get-steps`).
- [`load_survey_example()`](https://metasurveyr.github.io/metasurvey/reference/load_survey_example.md):
  removed dead [`file.exists()`](https://rdrr.io/r/base/files.html)
  branch, wrapped
  [`download.file()`](https://rdrr.io/r/utils/download.file.html) in
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) for graceful
  failure.
- Added
  [`requireNamespace("htmltools")`](https://github.com/rstudio/htmltools)
  check in
  [`view_graph()`](https://metasurveyr.github.io/metasurvey/reference/view_graph.md).
- Removed version constraint from `parallel` in Suggests.
- R CMD check: 0 errors, 0 warnings, 1 note (new submission).

### Translations

- All remaining Spanish return values translated to English:
  `"Trianual"` → `"Triennial"`, `"Multianual"` → `"Multi-year"`.
- All Spanish inline comments translated to English.
- Translated roxygen documentation for
  [`recipe()`](https://metasurveyr.github.io/metasurvey/reference/recipe.md),
  [`extract_surveys()`](https://metasurveyr.github.io/metasurvey/reference/extract_surveys.md),
  [`get_implantation()`](https://metasurveyr.github.io/metasurvey/reference/get_implantation.md),
  [`get_follow_up()`](https://metasurveyr.github.io/metasurvey/reference/get_follow_up.md).

### New features

- Added STATA-to-metasurvey transpiler:
  [`transpile_stata()`](https://metasurveyr.github.io/metasurvey/reference/transpile_stata.md),
  [`transpile_stata_module()`](https://metasurveyr.github.io/metasurvey/reference/transpile_stata_module.md),
  [`parse_do_file()`](https://metasurveyr.github.io/metasurvey/reference/parse_do_file.md),
  [`parse_stata_labels()`](https://metasurveyr.github.io/metasurvey/reference/parse_stata_labels.md),
  and
  [`transpile_coverage()`](https://metasurveyr.github.io/metasurvey/reference/transpile_coverage.md).
  Converts `.do` files into Recipe JSON, supporting gen/replace chains,
  recode, egen, foreach loops, mvencode, destring, labels, and more.
- Added `labels` field to `Recipe` class for storing variable and value
  labels from STATA transpilation.
- Added Shiny DAG graph loading spinner and disclaimer for recipes with
  more than 20 steps.

### Bug fixes

- Fixed `validate_weight_time_pattern()` crash when `weight` is NULL
  (triggered by `shallow_clone()` on surveys without weights).
- Fixed
  [`steps_to_recipe()`](https://metasurveyr.github.io/metasurvey/reference/steps_to_recipe.md)
  producing unparseable step strings: long `step_recode` calls were
  split across multiple lines by
  [`deparse()`](https://rdrr.io/r/base/deparse.html), breaking JSON
  round-trip.
- Fixed
  [`bake_recipes()`](https://metasurveyr.github.io/metasurvey/reference/bake_recipes.md)
  not executing `step_rename` and `step_remove`: recipe step replay now
  temporarily disables lazy processing so all steps execute immediately.
- Fixed Shiny `category_tag()` crash when recipe topic is NULL.

### Documentation

- Added `@family` tags to all exported functions across 16 groups.
- Standardized `@keywords` tags (removed commas, fixed multi-word
  entries).
- Removed redundant `@title` tags in `R/set_engine.R`.
- Updated `cran-comments.md` to reflect 1 NOTE (new submission).
- Added `stata-transpiler` vignette covering all supported STATA
  patterns.
- Added `ech-demographics-recipe` vignette showing hand-crafted vs
  transpiled recipe workflows.

## metasurvey 0.0.8

### Bug fixes

- Fixed 5 occurrences of `stop(message(...))` which silently called
  `stop(NULL)` instead of raising a proper error
  ([`set_engine()`](https://metasurveyr.github.io/metasurvey/reference/set_engine.md),
  [`load_survey()`](https://metasurveyr.github.io/metasurvey/reference/load_survey.md),
  [`recipe()`](https://metasurveyr.github.io/metasurvey/reference/recipe.md)).
- Fixed `.Rbuildignore` double-escape bug: patterns had `\\\\.` instead
  of `\\.`, causing hidden files and non-standard files to leak into the
  tarball.
- Replaced unsafe `1:n` sequences with
  [`seq_len()`](https://rdrr.io/r/base/seq.html)/[`seq.int()`](https://rdrr.io/r/base/seq.html)
  to avoid `c(1, 0)` when n=0 (7 occurrences across survey.R,
  workflow.R, steps.R).
- Removed duplicate internal
  [`set_data()`](https://metasurveyr.github.io/metasurvey/reference/set_data.md)
  definition in survey.R.
- Replaced raw ANSI escape codes with
  [`crayon::red()`](http://r-lib.github.io/crayon/reference/crayon.md)/[`crayon::green()`](http://r-lib.github.io/crayon/reference/crayon.md)
  in
  [`cat_design()`](https://metasurveyr.github.io/metasurvey/reference/cat_design.md)
  for cross-platform compatibility.
- Removed unreachable dead code in
  [`load_survey_example()`](https://metasurveyr.github.io/metasurvey/reference/load_survey_example.md).
- [`requireNamespace("parallel")`](https://rdrr.io/r/base/ns-load.html)
  now uses `quietly = TRUE` and checks the return value before using the
  package.
- [`requireNamespace("rio")`](https://gesistsa.github.io/rio/) return
  value is now checked before calling
  [`rio::convert()`](http://gesistsa.github.io/rio/reference/convert.md).

### Security

- SSL certificate verification in ANDA client is now enabled by default.
  Previously hardcoded `ssl_verifypeer = FALSE`; now user-controllable
  via `options(metasurvey.ssl_verify = FALSE)`.
- [`api_logout()`](https://metasurveyr.github.io/metasurvey/reference/api_logout.md)
  no longer calls `Sys.unsetenv("METASURVEY_TOKEN")` — only clears the R
  option, per CRAN policy on environment variables.

### CRAN compliance

- [`set_engine()`](https://metasurveyr.github.io/metasurvey/reference/set_engine.md),
  [`set_use_copy()`](https://metasurveyr.github.io/metasurvey/reference/set_use_copy.md),
  [`set_lazy_processing()`](https://metasurveyr.github.io/metasurvey/reference/set_lazy_processing.md),
  and
  [`configure_api()`](https://metasurveyr.github.io/metasurvey/reference/configure_api.md)
  now return the previous value invisibly, allowing users to restore
  global options.
- All user-facing messages translated from Spanish to English.
- [`evaluate_cv()`](https://metasurveyr.github.io/metasurvey/reference/evaluate_cv.md)
  returns English labels (“Excellent”, “Good”, etc.).
- [`extract_time_pattern()`](https://metasurveyr.github.io/metasurvey/reference/extract_time_pattern.md)
  returns “Invalid format”/“Unknown format”.
- DESCRIPTION title and description rewritten to be substantive.
- `inst/CITATION` updated: removed deprecated
  [`citHeader()`](https://rdrr.io/r/utils/citation.html), dynamic year.
- Fixed inconsistent maintainer email across DESCRIPTION and package
  docs.
- R CMD check now passes with 0 errors, 0 warnings, 0 NOTEs.

## metasurvey 0.0.7

### New features

- `Recipe$to_list()` and `RecipeWorkflow$to_list()` now include
  `metasurvey_version` for reproducibility tracking.

### Bug fixes

- Fixed `bake_step()` crash when executing
  [`step_recode()`](https://metasurveyr.github.io/metasurvey/reference/step_recode.md):
  removed invalid `record=FALSE` argument passed to internal `recode()`.
- Fixed `bake_steps_survey()` iterating over the original survey’s steps
  instead of the cloned copy, causing inconsistent state.
- Fixed
  [`bake_recipes()`](https://metasurveyr.github.io/metasurvey/reference/bake_recipes.md)
  environment leakage: recipe step evaluation now uses an isolated
  [`new.env()`](https://rdrr.io/r/base/environment.html) instead of the
  calling [`environment()`](https://rdrr.io/r/base/environment.html).
- Removed redundant
  [`copy()`](https://rdrr.io/pkg/data.table/man/copy.html) in
  `compute()` and `recode()` — the `shallow_clone()` already handles
  data copying.
- [`step_join()`](https://metasurveyr.github.io/metasurvey/reference/step_join.md)
  now uses
  [`merge.data.table()`](https://rdrr.io/pkg/data.table/man/merge.html)
  directly and
  [`set_data()`](https://metasurveyr.github.io/metasurvey/reference/set_data.md)
  instead of assigning to `$data`.
- Fixed Shiny admin panel memory leak: replaced dynamic
  `observe→lapply→observeEvent` pattern with delegated event handlers.
- Removed `invalidateLater(0)` from admin panel refresh.

### Security

- `store_token()` no longer calls
  [`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html) — API tokens
  are stored in R options only, not leaked to environment variables.
- `token_expires_soon()` returns `TRUE` for `NULL`, malformed, or
  missing `exp` claims, forcing token refresh instead of using
  potentially invalid tokens.
- API error messages are sanitized to 200 characters maximum.
- Added `validate_api_id()` with alphanumeric whitelist for
  recipe/workflow IDs.
- Added password length validation (8–128 characters) in
  [`api_register()`](https://metasurveyr.github.io/metasurvey/reference/api_register.md).
- [`api_download_recipe()`](https://metasurveyr.github.io/metasurvey/reference/api_download_recipe.md)
  and
  [`api_download_workflow()`](https://metasurveyr.github.io/metasurvey/reference/api_download_workflow.md)
  now warn on failure instead of silently swallowing errors.

### Performance

- Added `debounce(300)` on Shiny recipe search input to reduce excessive
  reactivity.

### Internal

- Replaced [`sapply()`](https://rdrr.io/r/base/lapply.html) with
  [`vapply()`](https://rdrr.io/r/base/lapply.html) or
  [`lapply()`](https://rdrr.io/r/base/lapply.html) across the codebase
  for type-safe return values.
- [`set_engine()`](https://metasurveyr.github.io/metasurvey/reference/set_engine.md)
  uses [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html)
  instead of `eval(install.packages(...))`.
- [`read_recipe()`](https://metasurveyr.github.io/metasurvey/reference/read_recipe.md)
  now warns when step parsing falls back to raw strings.
- Added ORCID identifiers for authors in DESCRIPTION.
- Added ANDA fetch failure notification in Shiny explore module.

## metasurvey 0.0.6

### Documentation

- All exported functions and R6 classes now have `@return` and
  `@examples` roxygen2 tags, meeting rOpenSci documentation
  requirements.
- Added `@return` to R6 class exports: `Recipe`, `RecipeWorkflow`,
  `RecipeCategory`, `RecipeCertification`, `RecipeUser`, `PoolSurvey`,
  `RotativePanelSurvey`, and `Survey`.
- Added `@examples` to
  [`set_engine()`](https://metasurveyr.github.io/metasurvey/reference/set_engine.md),
  [`show_engines()`](https://metasurveyr.github.io/metasurvey/reference/show_engines.md),
  [`get_engine()`](https://metasurveyr.github.io/metasurvey/reference/get_engine.md),
  and all API/backend functions.
- Added `@return` to
  [`get_metadata()`](https://metasurveyr.github.io/metasurvey/reference/get_metadata.md),
  [`set_lazy_processing()`](https://metasurveyr.github.io/metasurvey/reference/set_lazy_processing.md),
  [`set_use_copy()`](https://metasurveyr.github.io/metasurvey/reference/set_use_copy.md),
  [`set_backend()`](https://metasurveyr.github.io/metasurvey/reference/set_backend.md),
  [`set_workflow_backend()`](https://metasurveyr.github.io/metasurvey/reference/set_workflow_backend.md),
  and
  [`explore_recipes()`](https://metasurveyr.github.io/metasurvey/reference/explore_recipes.md).

### Bug fixes

- Fixed Codecov GitHub Actions workflow: replaced deprecated
  `covr::codecov()` with `covr::package_coverage()` +
  `codecov/codecov-action@v5` for reliable coverage uploads with token
  authentication.
- Added CI guard to `.Rprofile` to skip example-data setup in GitHub
  Actions, preventing potential download failures during CI builds.
- Added missing `archive`, `haven`, and `openxlsx` to `Suggests` in
  DESCRIPTION, fixing R CMD check warnings about undeclared
  dependencies.

### Internal

- Added `.claude/` directory with development agents and commands for
  code review, testing, documentation auditing, and rOpenSci
  preparation.

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
  workflow browser launched via
  [`explore_recipes()`](https://metasurveyr.github.io/metasurvey/reference/explore_recipes.md):

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

- Added
  [`step_join()`](https://metasurveyr.github.io/metasurvey/reference/step_join.md)
  for merging external reference data into surveys.
- Added
  [`step_remove()`](https://metasurveyr.github.io/metasurvey/reference/step_remove.md)
  and
  [`step_rename()`](https://metasurveyr.github.io/metasurvey/reference/step_rename.md)
  for variable management.

## metasurvey 0.0.2

- Added `RotativePanelSurvey` and `PoolSurvey` R6 classes for complex
  designs.
- Added
  [`extract_surveys()`](https://metasurveyr.github.io/metasurvey/reference/extract_surveys.md),
  [`get_implantation()`](https://metasurveyr.github.io/metasurvey/reference/get_implantation.md),
  [`get_follow_up()`](https://metasurveyr.github.io/metasurvey/reference/get_follow_up.md).

## metasurvey 0.0.1

- Initial CRAN submission.
