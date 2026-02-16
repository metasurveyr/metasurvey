# Extract time pattern

Extract time pattern

## Usage

``` r
extract_time_pattern(svy_edition)
```

## Arguments

- svy_edition:

  Survey edition string (e.g. "2023", "2023-06", "2023_Q1").

## Value

List with components: periodicity, year, month (when applicable).

## See also

Other survey-loading:
[`group_dates()`](https://metasurveyr.github.io/metasurvey/reference/group_dates.md),
[`load_panel_survey()`](https://metasurveyr.github.io/metasurvey/reference/load_panel_survey.md),
[`load_survey()`](https://metasurveyr.github.io/metasurvey/reference/load_survey.md),
[`load_survey_example()`](https://metasurveyr.github.io/metasurvey/reference/load_survey_example.md),
[`validate_time_pattern()`](https://metasurveyr.github.io/metasurvey/reference/validate_time_pattern.md)

## Examples

``` r
# Annual edition
extract_time_pattern("2023")
#> $year
#> [1] 2023
#> 
#> $periodicity
#> [1] "Annual"
#> 

# Monthly edition
extract_time_pattern("2023-06")
#> $year
#> [1] 2023
#> 
#> $month
#> [1] 6
#> 
#> $periodicity
#> [1] "Monthly"
#> 
```
