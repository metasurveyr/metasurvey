# Contributing to metasurvey

Thank you for your interest in contributing to metasurvey. This document
outlines how to propose changes, report bugs, and contribute code.

## Bug reports

If you find a bug, please file an issue on
[GitHub](https://github.com/metasurveyr/metasurvey/issues) with a
minimal reproducible example using the
[reprex](https://reprex.tidyverse.org/) package.

Include: - A description of the expected behavior vs. actual behavior. -
The output of
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html). - A minimal
dataset or use `make_test_survey()` from the test helpers.

## Feature requests and improvements

**Use [GitHub
Discussions](https://github.com/orgs/metasurveyr/discussions)** to
propose new features, improvements, and ideas. Post in the “Ideas”
category with a clear description of the problem and your proposed
solution. If the feature relates to a specific survey (ECH, EPH, EAII),
include the relevant variable names and edition.

The metasurvey team reviews discussions and decides which ideas get
prioritized for implementation. Issues are reserved for confirmed bugs —
the team manages their prioritization.

## Contributing recipes

The most impactful way to contribute to metasurvey is by **publishing a
recipe** to the community registry. A recipe captures the full
processing pipeline for a household survey — variable construction,
recodes, filters — so other researchers can reproduce your work or apply
it to new editions.

### Publish a recipe to the registry

``` r
library(metasurvey)

# 1. Connect to the registry
configure_api("https://metasurvey-api.onrender.com")

# 2. Create an account (once)
api_register("Your Name", "you@example.com", "your_password")

# 3. Build your recipe
svy <- survey_empty("eph", "2023T1")
r <- recipe(
  name = "Labor Market Indicators",
  user = "you@example.com",
  svy = svy,
  description = "Activity, employment, and unemployment rates from EPH"
)

# 4. Publish
api_publish_recipe(r)
```

### Browse existing recipes

``` r
# From R
configure_api("https://metasurvey-api.onrender.com")
list_recipes()
search_recipes("labor")

# Or launch the Shiny explorer
explore_recipes()
```

### Surveys we’re looking for

We are building a community collection for Latin American household
surveys:

| Survey | Country   | Office |
|--------|-----------|--------|
| EPH    | Argentina | INDEC  |
| PNADc  | Brazil    | IBGE   |
| CASEN  | Chile     | MDS    |
| GEIH   | Colombia  | DANE   |
| ENIGH  | Mexico    | INEGI  |

If you work with any of these (or any other complex survey), publish
your recipe and [open an
issue](https://github.com/metasurveyr/metasurvey/issues/new?template=new_recipe.md)
to let us know.

## Pull requests

1.  Fork the repo and create a branch from `develop`.
2.  Run `devtools::document()` if you changed any roxygen comments.
3.  Add or update tests in `tests/testthat/`.
4.  Run `devtools::test()` and ensure all tests pass.
5.  Run `devtools::check()` and fix any warnings or notes.
6.  Submit a PR to the `develop` branch.

### Code style

- Use `data.table` syntax for internal data operations.
- Follow existing naming conventions: `step_*` for step functions, `R6`
  classes use PascalCase.
- Keep functions focused and avoid over-engineering.

### Testing

- Use testthat edition 3.
- Test helpers are in `tests/testthat/helper-survey.R`
  (`make_test_survey()`, `make_test_panel()`).
- Integration tests that need external data should use `skip_on_cran()`.

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/metasurveyr/metasurvey/blob/main/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
