# survey_to_data_frame

Convert survey to data.frame

## Usage

``` r
survey_to_data_frame(svy)
```

## Arguments

- svy:

  Survey object

## Value

data.frame

## See also

Other survey-objects:
[`Survey`](https://metasurveyr.github.io/metasurvey/reference/Survey.md),
[`cat_design()`](https://metasurveyr.github.io/metasurvey/reference/cat_design.md),
[`cat_design_type()`](https://metasurveyr.github.io/metasurvey/reference/cat_design_type.md),
[`get_data()`](https://metasurveyr.github.io/metasurvey/reference/get_data.md),
[`get_metadata()`](https://metasurveyr.github.io/metasurvey/reference/get_metadata.md),
[`set_data()`](https://metasurveyr.github.io/metasurvey/reference/set_data.md),
[`survey_empty()`](https://metasurveyr.github.io/metasurvey/reference/survey_empty.md),
[`survey_to_data.table()`](https://metasurveyr.github.io/metasurvey/reference/survey_to_data.table.md),
[`survey_to_tibble()`](https://metasurveyr.github.io/metasurvey/reference/survey_to_tibble.md)

## Examples

``` r
dt <- data.table::data.table(
  id = 1:5, age = c(25, 30, 45, 50, 60),
  w = rep(1, 5)
)
svy <- Survey$new(
  data = dt, edition = "2023", type = "ech",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w")
)
df <- survey_to_data_frame(svy)
class(df) # "data.frame"
#> [1] "data.frame"
```
