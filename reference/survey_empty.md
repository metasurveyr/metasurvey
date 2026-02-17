# survey_empty

Create an empty survey

## Usage

``` r
survey_empty(
  edition = NULL,
  type = NULL,
  weight = NULL,
  engine = NULL,
  psu = NULL,
  strata = NULL
)
```

## Arguments

- edition:

  Edition of survey

- type:

  Type of survey

- weight:

  Weight of survey

- engine:

  Engine of survey

- psu:

  PSU variable or formula (optional)

- strata:

  Stratification variable name (optional)

## Value

Survey object

## See also

Other survey-objects:
[`Survey`](https://metasurveyr.github.io/metasurvey/reference/Survey.md),
[`cat_design()`](https://metasurveyr.github.io/metasurvey/reference/cat_design.md),
[`cat_design_type()`](https://metasurveyr.github.io/metasurvey/reference/cat_design_type.md),
[`get_data()`](https://metasurveyr.github.io/metasurvey/reference/get_data.md),
[`get_metadata()`](https://metasurveyr.github.io/metasurvey/reference/get_metadata.md),
[`has_design()`](https://metasurveyr.github.io/metasurvey/reference/has_design.md),
[`has_recipes()`](https://metasurveyr.github.io/metasurvey/reference/has_recipes.md),
[`has_steps()`](https://metasurveyr.github.io/metasurvey/reference/has_steps.md),
[`is_baked()`](https://metasurveyr.github.io/metasurvey/reference/is_baked.md),
[`set_data()`](https://metasurveyr.github.io/metasurvey/reference/set_data.md),
[`survey_to_data.table()`](https://metasurveyr.github.io/metasurvey/reference/survey_to_data.table.md),
[`survey_to_data_frame()`](https://metasurveyr.github.io/metasurvey/reference/survey_to_data_frame.md),
[`survey_to_tibble()`](https://metasurveyr.github.io/metasurvey/reference/survey_to_tibble.md)

## Examples

``` r
empty <- survey_empty(edition = "2023", type = "test")
empty_typed <- survey_empty(edition = "2023", type = "ech")
```
