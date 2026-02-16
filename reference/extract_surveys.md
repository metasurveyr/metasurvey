# Extract surveys by periodicity from a rotating panel

Extracts subsets of surveys from a RotativePanelSurvey object based on
temporal criteria. Allows obtaining surveys for different types of
analysis (monthly, quarterly, annual) respecting the rotating panel's
temporal structure.

## Usage

``` r
extract_surveys(
  RotativePanelSurvey,
  index = NULL,
  monthly = NULL,
  annual = NULL,
  quarterly = NULL,
  biannual = NULL,
  use.parallel = FALSE
)
```

## Arguments

- RotativePanelSurvey:

  A `RotativePanelSurvey` object containing the rotating panel surveys
  organized temporally

- index:

  Integer vector specifying survey indices to extract. If a single
  value, returns that survey; if a vector, returns a list

- monthly:

  Integer vector specifying which months to extract for monthly analysis
  (1-12)

- annual:

  Integer vector specifying which years to extract for annual analysis

- quarterly:

  Integer vector specifying which quarters to extract for quarterly
  analysis (1-4)

- biannual:

  Integer vector specifying which semesters to extract for biannual
  analysis (1-2)

- use.parallel:

  Logical indicating whether to use parallel processing for intensive
  operations. Default FALSE

## Value

A list of `Survey` objects matching the specified criteria, or a single
`Survey` object if a single index is specified

## Details

This function is essential for working with rotating panels because:

- Enables periodicity-based analysis: Extract data for different types
  of temporal estimations

- Preserves temporal structure: Respects temporal relationships between
  different panel waves

- Optimizes memory: Only loads surveys needed for the analysis

- Facilitates comparisons: Extract specific periods for comparative
  analysis

- Supports parallelization: For operations with large data volumes

Extraction criteria are interpreted according to survey frequency:

- For monthly ECH: monthly=c(1,3,6) extracts January, March and June

- For annual analysis: annual=1 typically extracts the first available
  year

- For quarterly analysis: quarterly=c(1,4) extracts Q1 and Q4

If no criteria are specified, the function returns the implantation
survey with a warning.

## See also

[`load_panel_survey`](https://metasurveyr.github.io/metasurvey/reference/load_panel_survey.md)
for loading rotating panels
[`get_implantation`](https://metasurveyr.github.io/metasurvey/reference/get_implantation.md)
for obtaining implantation data
[`get_follow_up`](https://metasurveyr.github.io/metasurvey/reference/get_follow_up.md)
for obtaining follow-up data
[`workflow`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
for using extracted surveys in analysis

Other panel-surveys:
[`PoolSurvey`](https://metasurveyr.github.io/metasurvey/reference/PoolSurvey.md),
[`RotativePanelSurvey`](https://metasurveyr.github.io/metasurvey/reference/RotativePanelSurvey.md),
[`get_follow_up()`](https://metasurveyr.github.io/metasurvey/reference/get_follow_up.md),
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

# Extract specific monthly surveys
ech_q1 <- extract_surveys(
  panel_ech,
  monthly = c(1, 2, 3) # January, February, March
)

# Extract by index
ech_first <- extract_surveys(panel_ech, index = 1)
ech_several <- extract_surveys(panel_ech, index = c(1, 3, 6))

# Quarterly analysis
ech_Q1_Q4 <- extract_surveys(
  panel_ech,
  quarterly = c(1, 4)
)

# Annual analysis (typically all surveys for the year)
ech_annual <- extract_surveys(
  panel_ech,
  annual = 1
)

# With parallel processing for large volumes
ech_full <- extract_surveys(
  panel_ech,
  monthly = 1:12,
  use.parallel = TRUE
)

# Use in workflow
results <- workflow(
  survey = extract_surveys(panel_ech, quarterly = c(1, 2)),
  svymean(~unemployed, na.rm = TRUE),
  estimation_type = "quarterly"
)
} # }
```
