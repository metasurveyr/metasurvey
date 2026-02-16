# Query ANDA variable metadata from the API

Fetches variable metadata (labels, types, value labels) from the
metasurvey API's ANDA endpoint.

## Usage

``` r
anda_variables(survey_type = "ech", var_names = NULL)
```

## Arguments

- survey_type:

  Character survey type (default "ech")

- var_names:

  Character vector of variable names to look up. If NULL, returns all
  variables for the survey type.

## Value

A data.frame with columns: name, label, type

## Examples

``` r
if (FALSE) { # \dontrun{
anda_variables("ech", c("pobpcoac", "e27"))
} # }
```
