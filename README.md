# metasurvey

<!-- badges: start -->
[![pkgdown](https://github.com/metasurveyr/metasurvey/actions/workflows/pkgdown.yaml/badge.svg?branch=main)](https://github.com/metasurveyr/metasurvey/actions/workflows/pkgdown.yaml)
[![Codecov test coverage](https://codecov.io/gh/metasurveyr/metasurvey/branch/main/graph/badge.svg)](https://app.codecov.io/gh/metasurveyr/metasurvey?branch=main)
<!-- badges: end -->

**metasurvey** is an R package for processing and analysing complex survey
data using metaprogramming and reproducible pipelines. It integrates with the
[`survey`](https://cran.r-project.org/package=survey) package and is designed
for **complex sampling designs** and **recurring estimations** over time
(rotating panels, repeated cross-sections).

### Key features

- **Steps**: lazy transformation pipeline (`step_compute`, `step_recode`,
  `step_rename`, `step_remove`, `step_join`) executed via `bake_steps()`.
- **Recipes**: portable, versioned objects that encapsulate harmonisation
  pipelines with automatic documentation (`doc()`) and validation
  (`validate()`).
- **Workflows**: estimation with `survey::svymean`, `svytotal`, `svyratio`
  and `svyby` integrated in `workflow()`, returning a `data.table` with
  value, standard error and coefficient of variation.
- **Rotating panels**: support for `RotativePanelSurvey` with implantation
  and follow-ups, and `PoolSurvey` for combined estimation.
- **Replicate weights**: bootstrap replicate configuration via
  `add_replicate()` for robust variance with `survey::svrepdesign`.
- **Recipe registry**: publish, search and discover recipes and workflows
  through a REST API or a local JSON registry.
- **Shiny app**: interactive recipe and workflow explorer with
  `explore_recipes()`.

---

## Installation

Development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("metasurveyR/metasurvey")
```

---

## Quick example

```r
library(metasurvey)

# Create a survey with sample data
data(api, package = "survey")

svy <- Survey$new(
  data    = apistrat,
  edition = "2000",
  type    = "api",
  psu     = NULL,
  engine  = "data.table",
  weight  = add_weight(annual = "pw")
)

# Lazy transformations
svy <- step_compute(svy, growth = api00 - api99, comment = "API growth")
svy <- step_recode(svy, school_level,
  stype == "E" ~ "Elementary",
  stype == "M" ~ "Middle",
  stype == "H" ~ "High",
  .default = NA_character_
)
svy <- bake_steps(svy)

# Estimation
workflow(
  list(svy),
  survey::svymean(~growth, na.rm = TRUE),
  estimation_type = "annual"
)
```

---

## Full example: ECH panel with bootstrap replicate weights

This example uses the rotating panel from Uruguay's
[*Encuesta Continua de Hogares*](https://www.ine.gub.uy/encuesta-continua-de-hogares)
(ECH) with bootstrap replicate weights. First, download the example data:

```r
download_example_ech <- function() {
  zip_url <- "https://informe-tfg.s3.us-east-2.amazonaws.com/example-data.zip"
  dest_zip <- "example-data.zip"
  temp_dir <- tempfile("example-data")
  download.file(zip_url, destfile = dest_zip, mode = "wb")
  dir.create(temp_dir)
  unzip(dest_zip, exdir = temp_dir)
  target_dir <- "example-data"
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  file.rename(
    list.files(file.path(temp_dir, "example-data"), full.names = TRUE),
    file.path(target_dir, basename(list.files(file.path(temp_dir, "example-data"))))
  )
  unlink(dest_zip)
  unlink(temp_dir, recursive = TRUE)
}
download_example_ech()
```

With the data downloaded:

```r
library(metasurvey)
library(magrittr)

path_dir <- file.path("example-data", "ech", "ech_2023")

ech_2023 <- load_panel_survey(
  path_implantation = file.path(path_dir, "ECH_implantacion_2023.csv"),
  path_follow_up = file.path(path_dir, "seguimiento"),
  svy_type = "ECH_2023",
  svy_weight_implantation = add_weight(annual = "W_ANO"),
  svy_weight_follow_up = add_weight(
    monthly = add_replicate(
      "W",
      replicate_path = file.path(
        path_dir,
        c(
          "Pesos replicados Bootstrap mensuales enero_junio 2023",
          "Pesos replicados Bootstrap mensuales julio_diciembre 2023"
        ),
        c(
          "Pesos replicados mensuales enero_junio 2023",
          "Pesos replicados mensuales Julio_diciembre 2023"
        )
      ),
      replicate_id = c("ID" = "ID"),
      replicate_pattern = "wr[0-9]+",
      replicate_type = "bootstrap"
    )
  )
)

# Build labour market indicators
ech_2023 <- ech_2023 %>%
  step_recode("pea", POBPCOAC %in% 2:5 ~ 1, .default = 0,
              comment = "EAP", .level = "follow_up") %>%
  step_recode("pet", e27 >= 14 ~ 1, .default = 0,
              comment = "WAP", .level = "follow_up") %>%
  step_recode("po", POBPCOAC == 2 ~ 1, .default = 0,
              comment = "Employed", .level = "follow_up") %>%
  step_recode("pd", POBPCOAC %in% 3:5 ~ 1, .default = 0,
              comment = "Unemployed", .level = "follow_up")

ech_2023_bake <- bake_steps(ech_2023)

# Quarterly rates: activity, employment and unemployment
workflow_result <- workflow(
  survey = extract_surveys(ech_2023_bake, quarterly = 1:4),
  survey::svyratio(~pea, denominator = ~pet),
  survey::svyratio(~po, denominator = ~pet),
  survey::svyratio(~pd, denominator = ~pea),
  estimation_type = "quarterly:monthly",
  rho = 0.5,
  R = 5 / 6
)

workflow_result
```

This pipeline loads a rotating panel with bootstrap replicate weights, builds
binary labour market indicators (EAP, WAP, employed, unemployed), and
estimates activity, employment and unemployment rates by quarter with robust
variance.

---

## Documentation

- [Getting started](https://metasurveyr.github.io/metasurvey/articles/getting-started.html)
- [Recipes](https://metasurveyr.github.io/metasurvey/articles/recipes.html)
- [Workflows and estimation](https://metasurveyr.github.io/metasurvey/articles/workflows-and-estimation.html)
- [Complex designs](https://metasurveyr.github.io/metasurvey/articles/complex-designs.html)
- [Rotating panels](https://metasurveyr.github.io/metasurvey/articles/panel-analysis.html)
- [Case study: ECH](https://metasurveyr.github.io/metasurvey/articles/ech-case-study.html)
- [Interactive explorer](https://metasurveyr.github.io/metasurvey/articles/shiny-explorer.html)
- [API and database](https://metasurveyr.github.io/metasurvey/articles/api-database.html)
