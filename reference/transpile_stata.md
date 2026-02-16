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
