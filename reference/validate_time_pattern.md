# Validate time pattern

Validate time pattern

## Usage

``` r
validate_time_pattern(svy_type = NULL, svy_edition = NULL)
```

## Arguments

- svy_type:

  Survey type (e.g. "ech").

- svy_edition:

  Survey edition string (e.g. "2023", "2023-06").

## Value

List with components: svy_type, svy_edition (parsed), svy_periodicity.

## See also

Other survey-loading:
[`extract_time_pattern()`](https://metasurveyr.github.io/metasurvey/reference/extract_time_pattern.md),
[`group_dates()`](https://metasurveyr.github.io/metasurvey/reference/group_dates.md),
[`load_panel_survey()`](https://metasurveyr.github.io/metasurvey/reference/load_panel_survey.md),
[`load_survey()`](https://metasurveyr.github.io/metasurvey/reference/load_survey.md),
[`load_survey_example()`](https://metasurveyr.github.io/metasurvey/reference/load_survey_example.md)

## Examples

``` r
validate_time_pattern(svy_type = "ech", svy_edition = "2023")
#> $svy_type
#> [1] "ech"
#> 
#> $svy_edition
#> [1] 2023
#> 
#> $svy_periodicity
#> [1] "Annual"
#> 
validate_time_pattern(
  svy_type = "ech", svy_edition = "2023-06"
)
#> $svy_type
#> [1] "ech"
#> 
#> $svy_edition
#> [1] "2023-06-01"
#> 
#> $svy_periodicity
#> [1] "Monthly"
#> 
```
