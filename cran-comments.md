## R CMD check results

0 errors | 0 warnings | 1 note

* checking installed package size ... NOTE
  installed size is 9.8Mb
  sub-directories of 1Mb or more:
    doc 7.8Mb

  The package ships 9 vignettes with rendered HTML output demonstrating
  survey processing workflows with 7 international household surveys.
  These vignettes are essential for user onboarding and cannot be
  meaningfully reduced without losing documentation value.

## Test environments

* local macOS (aarch64-apple-darwin24.2.0), R 4.4.3
* GitHub Actions: ubuntu-latest (release, devel, oldrel-1),
  macOS-latest (release), windows-latest (release)

## Notes

* This is a new submission.
* Package contains 2,810 tests with 0 failures.
* The `eph`, `PNADcIBGE`, and `ipumsr` packages in Suggests are used
  conditionally in the international-surveys vignette; sections are
  skipped when these packages are not installed.
