# metasurvey

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/metasSurvey)](https://cran.r-project.org/package=metasurvey)
[![pkgdown](https://github.com/metasurveyr/metasurvey/actions/workflows/pkgdown.yaml/badge.svg?branch=main)](https://github.com/metasurveyr/metasurvey/actions/workflows/pkgdown.yaml)
[![Codecov test coverage](https://codecov.io/gh/metasurveyr/metasurvey/branch/main/graph/badge.svg)](https://app.codecov.io/gh/metasurveyr/metasurvey?branch=main)
[![pre-commit.ci status](https://results.pre-commit.ci/badge/github/metasurveyr/metasurvey/main.svg)](https://results.pre-commit.ci/latest/github/metasurveyr/metasurvey/main)
<!-- badges: end -->

**metasurvey** provides tools to streamline the analysis of complex survey data using metaprogramming and reproducible pipelines.  
It integrates tightly with the [`survey`](https://cran.r-project.org/package=survey) package and is especially helpful when working with **complex survey designs** and **recurrent estimations** over time (e.g., rotating panels or repeated cross-sections).

Key features:

- Tidy-style API for survey processing and estimation.
- Reproducible survey pipelines with steps and workflows.
- Support for advanced variance estimation methods (e.g., bootstrap replicates).
- Recipes and reusable metadata to manage harmonized survey definitions over time.

---

## Installation

Install the stable version from CRAN:

```r
install.packages("metasurvey")
```

Or the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("metasurveyR/metasurvey")
```

---

## Quick example

```r
library(metasurvey)
library(magrittr)

ech_2022 <- load_survey(
    metasurvey::load_survey_example(
        "ech",
        "ech_2022"
    ),
    svy_edition = "2022",
    svy_type = "ech",
    svy_weight = add_weight(annual = "w_ano"),
    recipes = get_recipe(
        "ech",
        "2022"
    )
)


```

---

## Full panel example (with bootstrap replicates)

This example uses the Uruguayan [*ECH*](https://www.google.com/search?client=safari&rls=en&q=ECH&ie=UTF-8&oe=UTF-8) panel with bootstrap replicate weights.  
First, download and unzip the example data:

```bash
curl -o example-data.zip "https://informe-tfg.s3.us-east-2.amazonaws.com/example-data.zip"
mkdir example-data
mkdir temp && unzip example-data.zip -d temp && mv temp/example-data/* example-data/ && rm -rf temp
rm example-data.zip
```

Or run the following R script to download and unzip the data:

```r
# download_example_data.R

# This script downloads and unzips the example-data.zip from S3

download_example_ech <- function() {
  zip_url <- "https://informe-tfg.s3.us-east-2.amazonaws.com/example-data.zip"
  dest_zip <- "example-data.zip"
  temp_dir <- tempfile("example-data")

  # Download the zip file
  download.file(zip_url, destfile = dest_zip, mode = "wb")
  message("Downloaded example-data.zip")

  # Unzip to temporary directory
  dir.create(temp_dir)
  unzip(dest_zip, exdir = temp_dir)
  message("Unzipped example-data.zip")

  target_dir <- file.path("example-data")
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  file.rename(list.files(file.path(temp_dir, "example-data"), full.names = TRUE), 
              file.path(target_dir, basename(list.files(file.path(temp_dir, "example-data")))))
  message("Moved unzipped data to chapters/example-data")

  # Cleanup
  unlink(dest_zip)
  unlink(temp_dir, recursive = TRUE)
  message("Cleanup done")
}

# Run the function
download_example_ech()
```

Then, you can run the following analysis:

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

ech_2023 <- ech_2023 %>%
  step_recode("pea", POBPCOAC %in% 2:5 ~ 1, .default = 0, comment = "PEA", .level = "follow_up") %>%
  step_recode("pet", e27 >= 14 ~ 1, .default = 0, comment = "PET", .level = "follow_up") %>%
  step_recode("po", POBPCOAC == 2 ~ 1, .default = 0, comment = "PO", .level = "follow_up") %>%
  step_recode("pd", POBPCOAC %in% 3:5 ~ 1, .default = 0, comment = "PD", .level = "follow_up")

ech_2023_bake <- bake_steps(ech_2023)

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
