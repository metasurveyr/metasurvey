# Display survey design information

Pretty-prints the sampling design configuration for each estimation type
in a Survey object, showing PSU, strata, weights, and other design
elements in a color-coded, readable format.

## Usage

``` r
cat_design(self)
```

## Arguments

- self:

  Survey object containing design information

## Value

Invisibly returns NULL; called for side effect of printing design info

## Details

This function displays design information including:

- Primary Sampling Units (PSU/clusters)

- Stratification variables

- Weight variables for each estimation type

- Finite Population Correction (FPC) if used

- Calibration formulas if applied

- Overall design type classification

Output is color-coded for better readability in supporting terminals.

## See also

[`cat_design_type`](https://metasurveyr.github.io/metasurvey/reference/cat_design_type.md)
for design type classification

Other survey-objects:
[`Survey`](https://metasurveyr.github.io/metasurvey/reference/Survey.md),
[`cat_design_type()`](https://metasurveyr.github.io/metasurvey/reference/cat_design_type.md),
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
cat_design(svy)
#> [1] "\n  Design: Not initialized (lazy initialization - will be created when needed)\n"
# }
```
