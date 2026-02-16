# Set data on a Survey

Tidy wrapper for `svy$set_data(data)`.

## Usage

``` r
set_data(svy, data, .copy = FALSE)
```

## Arguments

- svy:

  Survey object

- data:

  A data.frame or data.table with survey microdata

- .copy:

  Logical; if TRUE, clone the Survey before modifying (default FALSE)

## Value

The Survey object (invisibly). If `.copy=TRUE`, returns a new clone.

## See also

Other survey-objects:
[`Survey`](https://metasurveyr.github.io/metasurvey/reference/Survey.md),
[`cat_design()`](https://metasurveyr.github.io/metasurvey/reference/cat_design.md),
[`cat_design_type()`](https://metasurveyr.github.io/metasurvey/reference/cat_design_type.md),
[`get_data()`](https://metasurveyr.github.io/metasurvey/reference/get_data.md),
[`get_metadata()`](https://metasurveyr.github.io/metasurvey/reference/get_metadata.md),
[`survey_empty()`](https://metasurveyr.github.io/metasurvey/reference/survey_empty.md),
[`survey_to_data.table()`](https://metasurveyr.github.io/metasurvey/reference/survey_to_data.table.md),
[`survey_to_data_frame()`](https://metasurveyr.github.io/metasurvey/reference/survey_to_data_frame.md),
[`survey_to_tibble()`](https://metasurveyr.github.io/metasurvey/reference/survey_to_tibble.md)

## Examples

``` r
dt <- data.table::data.table(id = 1:5, x = rnorm(5), w = rep(1, 5))
svy <- Survey$new(
  data = dt, edition = "2023", type = "test",
  psu = NULL, engine = "data.table", weight = add_weight(annual = "w")
)
new_dt <- data.table::data.table(id = 1:3, x = rnorm(3), w = rep(1, 3))
svy <- set_data(svy, new_dt)
```
