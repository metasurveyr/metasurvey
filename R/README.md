# `R/` Directory Guide

The source code is organized in six layers that mirror the package's architecture:

```
                      STATA .do files        ANDA catalog
                           |                     |
                       transpile()          fetch metadata
                           |                     |
                           v                     v
                  +-----------------+   +-----------------+
                  |   5. Interop    |   |   5. Interop    |
                  |  stata_parser   |   |    anda.R       |
                  |  stata_mappings |   |                 |
                  +--------+--------+   +--------+--------+
                           |                     |
             produces Recipe|          enriches Survey
                           v                     v
+-----------------------------------------------------------+
|              4. Registry & Backends                       |
|       publish, discover, share (JSON / MongoDB)           |
|  RecipeRegistry · WorkflowRegistry · api_client           |
+----------------------------+------------------------------+
                             |
              stores & retrieves
                             |
+----------------------------v------------------------------+
|            3. Recipes & Workflows                         |
|  Recipe = portable bundle of steps + metadata             |
|  Workflow = estimation (svymean/svytotal) + provenance    |
+----------------------------+------------------------------+
                             |
                 bundles & executes
                             |
+----------------------------v------------------------------+
|              2. Step Pipeline                             |
|  step_compute · step_recode · step_rename                 |
|  step_remove  · step_join   · step_filter                 |
|                    bake_steps()                            |
+----------------------------+------------------------------+
                             |
                 operates on
                             |
+----------------------------v------------------------------+
|              1. Survey Objects                            |
|  Survey · RotativePanelSurvey · PoolSurvey               |
|         data.table + survey::svydesign                    |
+-----------------------------------------------------------+
```

## The problem metasurvey solves

Research institutes and university departments that work with national household surveys (ECH in Uruguay, EPH in Argentina, CASEN in Chile, CPS in the US) typically follow a well-defined methodology to process raw microdata: harmonize variables across editions, construct derived indicators (income, poverty lines, employment categories), apply sampling weights, and produce estimates. This processing is implemented in STATA `.do` files that accumulate years of institutional knowledge.

The results — processed datasets, indicator tables — are shared informally, often by email, as flat files or spreadsheets. External researchers, policy monitors, and government reports then use these processed variables to make decisions, publish papers, and build dashboards. But the processing pipeline itself is opaque: at best, you receive a `.do` file that rarely runs outside the original machine (hardcoded paths, missing dependencies, undocumented STATA version requirements). There is no standard way to inspect what transformations were applied, validate them against a new survey edition, or reproduce them independently.

metasurvey replaces this workflow with transparent, reproducible infrastructure: every transformation is recorded as a step, bundled into a portable recipe with metadata and dependency tracking, and published to a shared registry where others can discover, validate, and apply it — without needing STATA or chasing email attachments.

## Why R6?

Survey objects are inherently stateful: they accumulate steps, hold a mutable sampling design, attach recipes, and track provenance. A Survey starts empty, gets data loaded, receives a weight specification, builds a `survey::svydesign`, records a sequence of lazy transformations, and eventually materializes them — all as mutations on the same object. R6 reference semantics match this lifecycle naturally: the object identity stays the same while its internal state evolves, which avoids copying multi-million-row data.tables on every operation. The same reasoning applies to Recipes (which accumulate steps and metadata) and Registries (which maintain a connection to a backend).

## 1. Survey Objects — Data Model

A household survey is not just a data frame. It carries a sampling design (strata, PSUs, weights or replicate weights), metadata (survey type, edition, periodicity), and — for rotating panels — a structure of implantation and follow-up waves. If you lose the design context during processing, any subsequent estimation (`svymean`, `svytotal`) will produce wrong standard errors. The Survey class bundles all of this together so that every downstream operation preserves it automatically.

| File | Description |
|------|-------------|
| `survey.R` | `Survey` (base class) and `PoolSurvey` (groups surveys by time hierarchy). Holds data, metadata, weight spec, and sampling design. |
| `PanelSurvey.R` | `RotativePanelSurvey` — rotating panels with implantation + follow-up waves. |
| `load_survey.R` | Constructors: `survey_empty()`, `load_survey()`, `load_panel_survey()`. Reads CSV/SAV/DTA into Survey objects. |

## 2. Step Pipeline — Lazy Transformations

Raw survey microdata requires substantial processing before analysis: computing derived variables (per-capita income from household income and household size), recoding categorical variables (education levels, employment status), renaming columns to harmonize across editions, removing administrative fields, and joining external tables (deflators, geographic codes). These operations are recorded lazily — nothing executes until `bake_steps()` — so the pipeline can be inspected, optimized (collapsing consecutive computes), and serialized as part of a Recipe before touching the data.

| File | Description |
|------|-------------|
| `Step.R` | `Step` R6 class — stores one transformation (type, expressions, dependencies, bake status). |
| `steps.R` | Step functions (`step_compute`, `step_recode`, `step_rename`, `step_remove`, `step_join`, `step_filter`) and `bake_steps()` to materialize them. Internal `compute()` and `recode()` handle data.table mutations. |

## 3. Recipes & Workflows — Reproducibility

Surveys are re-released annually, but editions are not minor updates — questionnaires change, variables are renamed, added, or removed, and the way a given indicator must be constructed can differ substantially from one year to the next. The indicators themselves (poverty rate, income inequality, employment) stay the same; what changes is *how* to build them from the raw microdata. A Recipe captures the processing pipeline for a specific edition as a portable object. When a new edition arrives, `validate()` checks the recipe against the new schema and tells you exactly what broke — which variables are missing, which were renamed — instead of failing silently mid-analysis.

Workflows close the loop: they record the estimation call, the recipe that produced the input data, and a provenance trail (timestamp, package version, data hash). When a policy monitor publishes "poverty rate by department, ECH 2023", the full chain — from raw microdata through every transformation to the final estimate — is traceable and reproducible.

| File | Description |
|------|-------------|
| `Recipes.R` | `Recipe` R6 class — bundles metadata + step calls. Has `doc()` for auto-documentation and `validate()` to check dependencies against a survey's schema. |
| `RecipeWorkflow.R` | `RecipeWorkflow` R6 class — captures estimation calls (`svymean`/`svytotal`/`svyratio`/`svyby`) and references the recipe that produced the input data. |
| `workflow.R` | `workflow()` — executes estimation on Survey objects, returns a data.table with metadata. |
| `workflow_table.R` | `workflow_table()` — formatted output tables (gt) for workflow results. |
| `provenance.R` | `provenance()` — tracks data lineage (steps applied, recipe used, timestamp, diffs). |

## 4. Registry & Backends — Sharing Infrastructure

The registry replaces the informal sharing (email attachments, shared drives) described above with a structured, searchable system. Recipes and workflows can be published, searched, filtered (by survey type, edition, author, certification status), and ranked. The backend is pluggable — a local JSON file for individual use, or a MongoDB-backed REST API for institutional deployment where multiple teams need a shared catalog.

| File | Description |
|------|-------------|
| `RecipeRegistry.R` | `RecipeRegistry` — registry pattern with pluggable backends. |
| `RecipeAPI.R` | `RecipeBackend` — JSON-file and MongoDB backends for recipe storage. |
| `WorkflowRegistry.R` | `WorkflowRegistry` — registry pattern for workflows. |
| `WorkflowBackend.R` | `WorkflowBackend` — JSON-file and MongoDB backends for workflow storage. |
| `RecipeCategory.R` | `RecipeCategory` — hierarchical categories for organizing recipes. |
| `RecipeCertification.R` | `RecipeCertification` — trust/quality metadata for recipes. |
| `RecipeUser.R` | `RecipeUser` — author metadata for recipe authors. |
| `api_client.R` | REST API client — authentication and recipe/workflow CRUD against the optional MongoDB-backed server. |
| `recipe_tidy_api.R` | Tidy wrappers: `list_recipes()`, `search_recipes()`, `filter_recipes()`, `rank_recipes()`, etc. |
| `workflow_tidy_api.R` | Tidy wrappers: `list_workflows()`, `search_workflows()`, etc. |

## 5. Interoperability — Bridges to External Systems

Adoption requires meeting researchers where they are. In Latin America, the dominant tool for survey processing is STATA, and decades of institutional knowledge are locked in `.do` files. The transpiler converts these scripts into Recipe objects, providing a migration path that preserves existing logic instead of requiring researchers to rewrite everything from scratch. ANDA (Archivo Nacional de Datos, INE Uruguay) is the national microdata catalog; the client fetches DDI metadata (variable descriptions, survey structure) to enrich Survey objects automatically.

| File | Description |
|------|-------------|
| `transpile.R` | `transpile_stata()` — converts STATA `.do` files into Recipe objects. Main entry point. |
| `stata_parser.R` | Parses STATA syntax: commands, loops, labels, expressions. |
| `stata_mappings.R` | Mapping tables from STATA commands/functions to metasurvey step equivalents. |
| `anda.R` | ANDA (INE Uruguay) catalog client — search, download, and parse DDI metadata from the national microdata archive. |

## 6. Package Infrastructure

| File | Description |
|------|-------------|
| `meta.R` | Package-level options set on `.onLoad()` (engine, copy semantics, lazy processing). |
| `set_engine.R` | `set_engine()` / `show_engines()` — configure the data backend (default: data.table). |
| `utils.R` | Shared helpers: ID generation, validation, date parsing, display formatting. |
| `run_app.R` | `explore_recipes()` — launches the Shiny explorer app (code in `inst/shiny/`). |
| `metaSurvey-package.R` | Package-level roxygen documentation. |
