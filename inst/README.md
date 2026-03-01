# `inst/` Directory Guide

Installed files that ship with the package, organized by purpose:

```
inst/
│
├── Sample Data
│   └── extdata/            ech_2023_sample.csv (vignettes & examples)
│
├── Deployable Services (optional, Docker-based)
│   ├── api/                Plumber REST API (recipe/workflow CRUD, MongoDB)
│   ├── shiny/              Recipe explorer app (launched via explore_recipes())
│   └── worker/             Background job runner (validation, transpilation)
│
├── STATA Transpiler Fixtures
│   ├── do-files-ech/       Real ECH 2022 .do files (integration tests)
│   └── stata-test-cases/   Minimal .do files per construct (unit tests)
│
├── Registry Bootstrap
│   ├── seed-data/          JSON seeds (recipes, workflows, users, indicators)
│   └── scripts/            Setup scripts (MongoDB, ANDA, batch transpilation)
│
└── Tooling
    ├── diagrams/           Architecture diagrams (draw.io + SVG)
    ├── rstudio/            RStudio addin registration
    ├── CITATION
    └── WORDLIST
```

## Sample Data

`extdata/` contains `ech_2023_sample.csv` — a 500-row subset of Uruguay's Encuesta Continua de Hogares. Used in vignettes and runnable examples.

## Deployable Services

Three Docker-based services that enable the collaborative recipe ecosystem. All optional — the core package works entirely locally without them.

| Directory | Service | Entry point |
|-----------|---------|-------------|
| `api/` | REST API server | `plumber.R` — recipe/workflow CRUD, user auth, search. Backed by MongoDB. |
| `shiny/` | Explorer app | `app.R` + `R/` modules — browse, search, and preview recipes/workflows. |
| `worker/` | Background worker | `plumber.R` — async recipe validation and transpilation jobs. |

**Why a Shiny app?** The recipe registry can hold hundreds of recipes across survey types, editions, and authors. Discovering the right recipe for your survey programmatically (filtering by type, edition, variable coverage, certification status) works well for users comfortable with the tidy API (`search_recipes()`, `filter_recipes()`). But for researchers who are new to a survey or exploring what processing pipelines exist, a visual interface is more effective: they can browse categories, preview step-by-step documentation, compare recipes side by side, and generate the R code snippet to apply a recipe — all without writing a single line of code. The app is launched locally via `explore_recipes()` and connects to whichever registry backend is configured (local JSON or remote API).

## STATA Transpiler Fixtures

In Latin America, most national statistical offices and economics research groups process household survey microdata using STATA `.do` scripts. These scripts accumulate institutional knowledge — variable harmonization, income construction, label definitions — but are not portable or reproducible outside STATA.

metasurvey's transpiler (`transpile_stata()`) converts these `.do` files into Recipe objects, preserving the processing logic in a format that can be versioned, validated, and applied to new survey editions from R.

The test fixtures come from two sources:

- **`do-files-ech/`** — 12 real `.do` files from Uruguay's ECH 2022, authored by IECON (Instituto de Economia, Universidad de la Republica). These are the actual scripts used to process the national household survey for academic research. They cover complex patterns: multi-file pipelines, income decomposition, variable compatibilization across editions, and label assignment. Used as integration tests for the transpiler.
- **`stata-test-cases/`** — 10 minimal `.do` files, each isolating one STATA construct (foreach loops, egen, destring, gen/replace with expressions, recode, labels, lag/lead, mvencode, variable ranges). Used as unit test fixtures for the parser.

## Registry Bootstrap

Scripts and seed data for setting up a recipe registry from scratch:

- **`seed-data/`** — JSON files: `recipes.json` (community-contributed ECH recipes), `workflows.json`, `indicators.json`, `users.json`, `anda_variables.json`.
- **`scripts/`** — `setup_mongodb.js` (collection indexes), `seed_anda_metadata.R`, `seed_ech_recipes.R`, `transpile_iecon_all.R`. See `scripts/README.md` for details.

## Tooling

- **`diagrams/`** — Architecture diagram (draw.io source + SVG) used in package documentation.
- **`rstudio/`** — `addins.dcf` for RStudio addin registration.
- **`CITATION`** — Citation metadata for `citation("metasurvey")`.
