# Load survey example data

Downloads and loads example survey data from the metasurvey data
repository. This function provides access to sample datasets for testing
and demonstration purposes, including ECH (Continuous Household Survey)
and other survey types.

## Usage

``` r
load_survey_example(svy_type, svy_edition)
```

## Arguments

- svy_type:

  Character string specifying the survey type (e.g., "ech")

- svy_edition:

  Character string specifying the survey edition/year (e.g., "2023")

## Value

Character string with the path to the downloaded CSV file containing the
example survey data

## Details

This function downloads example data from the official metasurvey data
repository on GitHub. The data is cached locally in a temporary file to
avoid repeated downloads in the same session.

Available survey types and editions can be found at:
https://github.com/metasurveyr/metasurvey_data

## See also

[`load_survey`](https://metasurveyr.github.io/metasurvey/reference/load_survey.md)
for loading the downloaded data

## Examples

``` r
if (FALSE) { # \dontrun{
# Load ECH 2023 example data
ech_path <- load_survey_example("ech", "2023")

# Use with load_survey
ech_data <- load_survey(
  path = load_survey_example("ech", "2023"),
  svy_type = "ech",
  svy_edition = "2023"
)
} # }
```
