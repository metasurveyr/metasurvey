## R CMD check results

0 errors | 0 warnings | 1 note

The NOTE is "New submission" plus a URL timeout for the INE Uruguay
government website (see below).

## Resubmission

This is a resubmission. Changes since the previous version (0.0.22):

* `workflow()` now supports `conf.level` inside individual estimation calls
  (e.g., `svymean(~x, na.rm = TRUE, conf.level = 0.80)`), overriding the
  `workflow()` default for that estimation.
* New `harmonize()` function for automatic recipe-based survey harmonization.
* New `convey-inequality` vignette for poverty and inequality estimation.
* Replaced `\dontrun{}` with `\donttest{}` in `load_survey_example()` and
  `harmonize()` examples.
* Fixed global state leaks in test-harmonize.R (backend restored via `on.exit()`).

## Test environments

* local macOS (aarch64-apple-darwin24.2.0), R 4.4.3
* GitHub Actions: ubuntu-latest (release, devel, oldrel-1),
  macOS-latest (release), windows-latest (release)

## Notes

* This is a new submission (first CRAN release).
* Package contains 3,024 tests with 0 failures.
* The `eph`, `PNADcIBGE`, and `ipumsr` packages in Suggests are used
  conditionally in the international-surveys vignette; sections are
  skipped when these packages are not installed.
* URL `https://www.gub.uy/instituto-nacional-estadistica/` may
  trigger a timeout NOTE. This is the official website of Uruguay's
  National Statistics Institute (INE) -- a valid government URL that
  is occasionally slow to respond from outside Latin America.
