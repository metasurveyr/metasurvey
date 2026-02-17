# Transpile a STATA .do file to metasurvey steps

Parses a STATA .do file and translates its commands into metasurvey step
call strings suitable for use in Recipe objects.

## Usage

``` r
transpile_stata(do_file, survey_type = "ech", user = "iecon", strict = FALSE)
```

## Arguments

- do_file:

  Path to a STATA .do file

- survey_type:

  Survey type (default "ech")

- user:

  Author name for the recipe

- strict:

  If TRUE, stops on untranslatable commands; if FALSE, inserts
  MANUAL_REVIEW comments as warnings

## Value

A list with:

- steps: character vector of step call strings

- labels: list with var_labels and val_labels (if label commands found)

- warnings: character vector of MANUAL_REVIEW items

- stats: list with command counts

## Examples

``` r
# \donttest{
tf <- tempfile(fileext = ".do")
writeLines(c("gen age2 = edad^2", "replace sexo = 1 if sexo == ."), tf)
result <- transpile_stata(tf)
result$steps
#> [1] "step_compute(svy, age2 = edad^2, sexo = data.table::fifelse(is.na(sexo), 1, sexo))"
result$stats
#> $translated
#> [1] 2
#> 
#> $skipped
#> [1] 0
#> 
#> $manual_review
#> [1] 0
#> 
# }
```
