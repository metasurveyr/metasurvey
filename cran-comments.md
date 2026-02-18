## R CMD check results

0 errors | 0 warnings | 0 notes

## Resubmission

This is a resubmission. The previous submission (0.0.21) was flagged
for an invalid file URI (`CONTRIBUTING.md` referenced from `README.md`).
Fixed:

* Replaced relative `CONTRIBUTING.md` link in README with full GitHub URL.
* Replaced relative `CODE_OF_CONDUCT.md` link in CONTRIBUTING.md with
  full GitHub URL.
* Wrapped bare URL in roxygen with `\url{}` (`load_survey_example.Rd`).

## Test environments

* local macOS (aarch64-apple-darwin24.2.0), R 4.4.3
* GitHub Actions: ubuntu-latest (release, devel, oldrel-1),
  macOS-latest (release), windows-latest (release)

## Notes

* This is a new submission (first CRAN release).
* Package contains 2,972 tests with 0 failures.
* The `eph`, `PNADcIBGE`, and `ipumsr` packages in Suggests are used
  conditionally in the international-surveys vignette; sections are
  skipped when these packages are not installed.
* URL `https://www.gub.uy/instituto-nacional-estadistica/` may
  trigger a timeout NOTE. This is the official website of Uruguay's
  National Statistics Institute (INE) — a valid government URL that
  is occasionally slow to respond from outside Latin America.
