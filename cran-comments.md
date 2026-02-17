## R CMD check results

0 errors | 0 warnings | 0 notes

## Resubmission

This is a resubmission. In the previous submission (0.0.17) the package
received a NOTE about invalid URLs and file URIs. All issues have been
fixed:

* Updated redirected URLs (Azure AKS, INE Uruguay) to their current
  destinations.
* Removed links to resources that are no longer available (Railway API,
  infrastructure repository).
* Replaced relative cross-vignette links to excluded vignettes with
  full pkgdown URLs.
* Replaced relative `CODE_OF_CONDUCT.md` link in README with full
  GitHub URL.
* Updated bibliography entry (`references.bib`) with current INE URL.
* Included the `api-database` vignette in the build (was previously
  excluded, causing broken file URI references from other vignettes).
* Fixed SVG diagram branding text that referenced a non-existent
  repository URL.

## Test environments

* local macOS (aarch64-apple-darwin24.2.0), R 4.4.3
* GitHub Actions: ubuntu-latest (release, devel, oldrel-1),
  macOS-latest (release), windows-latest (release)

## Notes

* This is a new submission (first CRAN release).
* Package contains 2,959 tests with 0 failures.
* The `eph`, `PNADcIBGE`, and `ipumsr` packages in Suggests are used
  conditionally in the international-surveys vignette; sections are
  skipped when these packages are not installed.
