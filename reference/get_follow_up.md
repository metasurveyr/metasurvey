# Get follow-up surveys from a rotating panel

Extracts one or more follow-up surveys (waves after the implantation)
from a RotativePanelSurvey object. Follow-up surveys represent
subsequent data collections and are essential for longitudinal and
temporal change analysis.

## Usage

``` r
get_follow_up(
  RotativePanelSurvey,
  index = seq_along(RotativePanelSurvey$follow_up)
)
```

## Arguments

- RotativePanelSurvey:

  A `RotativePanelSurvey` object from which to extract the follow-up
  surveys

- index:

  Integer vector specifying which follow-up surveys to extract. Defaults
  to all available (1:length(follow_up)). Can be a single index or a
  vector of indices

## Value

A list of `Survey` objects corresponding to the specified follow-up
surveys. If a single index is specified, returns a list with one element

## Details

Follow-up surveys are fundamental in rotating panels because:

- Enable longitudinal analysis: Track the same units over time

- Capture temporal changes: Evolution of economic, social, and
  demographic variables

- Maintain representativeness: Each wave preserves population
  representativeness through controlled rotation

- Optimize resources: Reuse information from previous waves to reduce
  collection costs

- Facilitate comparisons: Consistent temporal structure for trend
  analysis

In rotating panels like ECH:

- Each follow-up wave covers a specific period (monthly/quarterly)

- Units rotate gradually maintaining temporal overlap

- Indices correspond to the chronological collection order

- Each follow-up maintains methodological consistency with implantation

## See also

[`get_implantation`](https://metasurveyr.github.io/metasurvey/reference/get_implantation.md)
for obtaining the implantation survey
[`extract_surveys`](https://metasurveyr.github.io/metasurvey/reference/extract_surveys.md)
for extracting surveys by temporal criteria
[`load_panel_survey`](https://metasurveyr.github.io/metasurvey/reference/load_panel_survey.md)
for loading rotating panels
[`workflow`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
for analysis with follow-up surveys

Other panel-surveys:
[`PoolSurvey`](https://metasurveyr.github.io/metasurvey/reference/PoolSurvey.md),
[`RotativePanelSurvey`](https://metasurveyr.github.io/metasurvey/reference/RotativePanelSurvey.md),
[`extract_surveys()`](https://metasurveyr.github.io/metasurvey/reference/extract_surveys.md),
[`get_implantation()`](https://metasurveyr.github.io/metasurvey/reference/get_implantation.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load rotating panel
panel_ech <- load_panel_survey(
  path = "ech_panel_2023.dta",
  svy_type = "ech_panel",
  svy_edition = "2023"
)

# Get first follow-up survey
followup_1 <- get_follow_up(panel_ech, index = 1)[[1]]

# Get multiple follow-ups
followups_q1 <- get_follow_up(panel_ech, index = c(1, 2, 3))

# Get all available follow-ups
all_followups <- get_follow_up(panel_ech)

# Check number of available follow-ups
n_followups <- length(get_follow_up(panel_ech))
cat("Available follow-ups:", n_followups)

# Longitudinal analysis with follow-ups
baseline <- get_implantation(panel_ech)
final_followup <- get_follow_up(panel_ech, index = n_followups)[[1]]

# Compare rates between implantation and final follow-up
initial_rate <- workflow(
  survey = baseline,
  svymean(~unemployment_rate, na.rm = TRUE)
)

final_rate <- workflow(
  survey = final_followup,
  svymean(~unemployment_rate, na.rm = TRUE)
)
} # }
```
