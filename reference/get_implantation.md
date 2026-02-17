# Get implantation survey from a rotating panel

Extracts the implantation (baseline) survey from a RotativePanelSurvey
object. The implantation survey represents the first data collection
wave and is essential for establishing the baseline and structural
characteristics of the panel.

## Usage

``` r
get_implantation(RotativePanelSurvey)
```

## Arguments

- RotativePanelSurvey:

  A `RotativePanelSurvey` object from which to extract the implantation
  survey

## Value

A `Survey` object containing the implantation survey with all its
metadata, data, and design configuration

## Details

The implantation survey is special in a rotating panel because:

- Establishes the baseline: Defines initial characteristics of all panel
  units

- Contains the full sample: Includes all units that will participate in
  the different panel waves

- Defines temporal structure: Establishes rotation and follow-up
  patterns

- Configures metadata: Contains information about periodicity, key
  variables, and stratification

- Serves as tracking reference: Basis for unit tracking in subsequent
  waves

This function is essential for analysis requiring:

- Temporal comparisons from the baseline

- Analysis of the complete panel structure

- Configuration of longitudinal models

- Evaluation of sampling design quality

## See also

[`get_follow_up`](https://metasurveyr.github.io/metasurvey/reference/get_follow_up.md)
for obtaining follow-up surveys
[`extract_surveys`](https://metasurveyr.github.io/metasurvey/reference/extract_surveys.md)
for extracting multiple surveys by criteria
[`load_panel_survey`](https://metasurveyr.github.io/metasurvey/reference/load_panel_survey.md)
for loading rotating panels
[`workflow`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
for analysis with the implantation survey

Other panel-surveys:
[`PoolSurvey`](https://metasurveyr.github.io/metasurvey/reference/PoolSurvey.md),
[`RotativePanelSurvey`](https://metasurveyr.github.io/metasurvey/reference/RotativePanelSurvey.md),
[`extract_surveys()`](https://metasurveyr.github.io/metasurvey/reference/extract_surveys.md),
[`get_follow_up()`](https://metasurveyr.github.io/metasurvey/reference/get_follow_up.md)

## Examples

``` r
impl <- Survey$new(
  data = data.table::data.table(id = 1:5, w = 1),
  edition = "2023", type = "test", psu = NULL,
  engine = "data.table", weight = add_weight(annual = "w")
)
fu <- Survey$new(
  data = data.table::data.table(id = 1:5, w = 1),
  edition = "2023_01", type = "test", psu = NULL,
  engine = "data.table", weight = add_weight(annual = "w")
)
panel <- RotativePanelSurvey$new(
  implantation = impl, follow_up = list(fu),
  type = "test", default_engine = "data.table",
  steps = list(), recipes = list(), workflows = list(), design = NULL
)
get_implantation(panel)
#> Type: TEST
#> Edition: 2023
#> Periodicity: Annual
#> Engine: data.table
#> Design: 
#>   Design: Not initialized (lazy initialization - will be created when needed)
#> 
#> Steps: None
#> Recipes: None 
```
