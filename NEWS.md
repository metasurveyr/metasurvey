# metasurvey 0.3.0

* The `metasurvey` package is now a **meta-package** for the modularized
  ecosystem, in the style of the tidyverse. A single `library(metasurvey)`
  loads and re-exports metasurvey.core, metasurvey.fromstata, metasurvey.anda,
  and metasurvey.explorer.backend. See `MIGRATION.md`.
* The monolithic code moved to
  [`metasurvey-legacy`](https://github.com/metasurveyr/metasurvey-legacy) and was
  split following the rOpenSci review's modularization suggestion.
* High-level orchestration `reproduce_workflow()` and `resolve_weight_spec()`
  live here (they span core + anda + the API backend).
* The Shiny explorer is optional:
  `metasurvey.explorer.frontend::explore_recipes()`.
