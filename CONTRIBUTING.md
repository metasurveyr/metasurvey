# Contributing to metasurvey

Thank you for your interest in contributing to metasurvey. This document
outlines how to propose changes, report bugs, and contribute code.

## Bug reports

If you find a bug, please file an issue on
[GitHub](https://github.com/metasurveyr/metasurvey/issues) with a minimal
reproducible example using the [reprex](https://reprex.tidyverse.org/) package.

Include:
- A description of the expected behavior vs. actual behavior.
- The output of `sessionInfo()`.
- A minimal dataset or use `make_test_survey()` from the test helpers.

## Feature requests and improvements

**Use [GitHub Discussions](https://github.com/orgs/metasurveyr/discussions)** to
propose new features, improvements, and ideas. Post in the "Ideas" category with
a clear description of the problem and your proposed solution. If the feature
relates to a specific survey (ECH, EPH, EAII), include the relevant variable
names and edition.

The metasurvey team reviews discussions and decides which ideas get prioritized
for implementation. Issues are reserved for confirmed bugs — the team manages
their prioritization.

## Pull requests

1. Fork the repo and create a branch from `develop`.
2. Run `devtools::document()` if you changed any roxygen comments.
3. Add or update tests in `tests/testthat/`.
4. Run `devtools::test()` and ensure all tests pass.
5. Run `devtools::check()` and fix any warnings or notes.
6. Submit a PR to the `develop` branch.

### Code style

- Use `data.table` syntax for internal data operations.
- Follow existing naming conventions: `step_*` for step functions, `R6` classes
  use PascalCase.
- Keep functions focused and avoid over-engineering.

### Testing

- Use testthat edition 3.
- Test helpers are in `tests/testthat/helper-survey.R` (`make_test_survey()`,
  `make_test_panel()`).
- Integration tests that need external data should use `skip_on_cran()`.

## Code of Conduct

Please note that this project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this
project you agree to abide by its terms.
