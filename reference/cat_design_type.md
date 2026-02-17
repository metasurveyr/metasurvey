# cat_design_type

Cast design type from survey

## Usage

``` r
cat_design_type(self, design_name)
```

## Arguments

- self:

  Object of class Survey

- design_name:

  Name of design

## Value

Character string describing the design type, or "None".

## See also

Other survey-objects:
[`Survey`](https://metasurveyr.github.io/metasurvey/reference/Survey.md),
[`cat_design()`](https://metasurveyr.github.io/metasurvey/reference/cat_design.md),
[`get_data()`](https://metasurveyr.github.io/metasurvey/reference/get_data.md),
[`get_metadata()`](https://metasurveyr.github.io/metasurvey/reference/get_metadata.md),
[`has_design()`](https://metasurveyr.github.io/metasurvey/reference/has_design.md),
[`has_recipes()`](https://metasurveyr.github.io/metasurvey/reference/has_recipes.md),
[`has_steps()`](https://metasurveyr.github.io/metasurvey/reference/has_steps.md),
[`is_baked()`](https://metasurveyr.github.io/metasurvey/reference/is_baked.md),
[`set_data()`](https://metasurveyr.github.io/metasurvey/reference/set_data.md),
[`survey_empty()`](https://metasurveyr.github.io/metasurvey/reference/survey_empty.md),
[`survey_to_data.table()`](https://metasurveyr.github.io/metasurvey/reference/survey_to_data.table.md),
[`survey_to_data_frame()`](https://metasurveyr.github.io/metasurvey/reference/survey_to_data_frame.md),
[`survey_to_tibble()`](https://metasurveyr.github.io/metasurvey/reference/survey_to_tibble.md)

## Examples

``` r
# \donttest{
library(data.table)
dt <- data.table(id = 1:20, x = rnorm(20), w = runif(20, 0.5, 2))
svy <- Survey$new(
  data = dt, edition = "2023", type = "demo",
  psu = NULL, engine = "data.table",
  weight = add_weight(annual = "w")
)
svy$ensure_design()
cat_design_type(svy, "annual")
#> 
#> * Package: survey
#> * Variance estimation: Ultimate cluster
# }
```
