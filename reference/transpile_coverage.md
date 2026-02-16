# Analyze transpilation coverage for STATA do-files

Reports what percentage of commands in a .do file (or directory of
files) can be automatically transpiled vs require manual review.

## Usage

``` r
transpile_coverage(path, recursive = TRUE)
```

## Arguments

- path:

  Path to a .do file or directory of .do files

- recursive:

  If TRUE and path is a directory, search subdirectories

## Value

A data.frame with columns: file, total_commands, translated, skipped,
manual_review, coverage_pct
