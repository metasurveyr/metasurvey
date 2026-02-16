# Get ANDA variable metadata from the API

Get ANDA variable metadata from the API

## Usage

``` r
api_get_anda_variables(survey_type = "ech", var_names = NULL)
```

## Arguments

- survey_type:

  Character survey type (default "ech")

- var_names:

  Character vector of variable names. If NULL, returns all.

## Value

A list of variable metadata objects

## See also

Other anda:
[`anda_download_microdata()`](https://metasurveyr.github.io/metasurvey/reference/anda_download_microdata.md),
[`anda_variables()`](https://metasurveyr.github.io/metasurvey/reference/anda_variables.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_get_anda_variables("ech", c("pobpcoac", "e27"))
} # }
```
