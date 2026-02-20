## R CMD check results

0 errors | 0 warnings | 1 note

The NOTE is "New submission" plus a URL timeout for the INE Uruguay
government website (see below).

## Resubmission

This is a resubmission. Changes since the previous attempt:

* Fixed invalid file URI (`api-database.html` from `inst/doc/recipes.html`):
  included the vignette in the build instead of excluding it via .Rbuildignore.
* Removed `@examples` from unexported internal functions (`cat_recipes`,
  `Step`, `RecipeBackend`, `RecipeRegistry`, `WorkflowBackend`,
  `WorkflowRegistry`).

## Test environments

* local macOS (aarch64-apple-darwin24.2.0), R 4.4.3
* win-builder (R-devel), Debian (R-devel)
* GitHub Actions: ubuntu-latest (release, devel, oldrel-1),
  macOS-latest (release), windows-latest (release)

## Notes

* This is a new submission (first CRAN release).
* Package contains 2,973 tests with 0 failures.
* The `eph`, `PNADcIBGE`, and `ipumsr` packages in Suggests are used
  conditionally in the international-surveys vignette; sections are
  skipped when these packages are not installed.
* URL `https://www.gub.uy/instituto-nacional-estadistica/` may
  trigger a timeout NOTE. This is the official website of Uruguay's
  National Statistics Institute (INE) -- a valid government URL that
  is occasionally slow to respond from outside Latin America.
