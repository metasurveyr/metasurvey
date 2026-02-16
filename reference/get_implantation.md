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
if (FALSE) { # \dontrun{
# Load ECH rotating panel
panel_ech <- load_panel_survey(
  path = "ech_panel_2023.dta",
  svy_type = "ech_panel",
  svy_edition = "2023"
)

# Get implantation survey
ech_baseline <- get_implantation(panel_ech)

# Check implantation characteristics
cat("Implantation sample size:", nrow(ech_baseline$data))
cat("Available variables:", ncol(ech_baseline$data))

# Use in baseline analysis
baseline_stats <- workflow(
  survey = ech_baseline,
  svymean(~activity_rate, na.rm = TRUE),
  estimation_type = "baseline"
)

# Compare with follow-up
followup_1 <- get_follow_up(panel_ech, index = 1)[[1]]
} # }
```
