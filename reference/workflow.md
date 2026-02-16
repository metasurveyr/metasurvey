# Execute estimation workflow for surveys

This function executes a sequence of statistical estimations on Survey
objects, applying functions from the R survey package with appropriate
metadata. Automatically handles different survey types and
periodicities.

## Usage

``` r
workflow(survey, ..., estimation_type = "monthly")
```

## Arguments

- survey:

  Survey object, list of Survey objects, or PoolSurvey. Must contain
  properly configured sample design

- ...:

  Calls to survey package functions (such as `svymean`, `svytotal`,
  `svyratio`, etc.) that will be executed sequentially

- estimation_type:

  Type of estimation that determines which weight to use. Options:
  "monthly", "quarterly", "annual", or vector with multiple types

## Value

`data.table` with results from all estimations, including columns:

- `stat`: Name of estimated statistic

- `value`: Estimation value

- `se`: Standard error

- `cv`: Coefficient of variation

- `estimation_type`: Type of estimation used

- `survey_edition`: Survey edition

- Other columns depending on estimation type

Data

## Details

The function automatically selects the appropriate sample design
according to the specified `estimation_type`. For each Survey in the
input list, it executes all functions specified in `...` and combines
the results.

Supported estimation types:

- "monthly": Monthly estimations

- "quarterly": Quarterly estimations

- "annual": Annual estimations

For PoolSurvey objects, it uses a specialized methodology that handles
pooling of multiple surveys.

## See also

[`svymean`](https://rdrr.io/pkg/survey/man/surveysummary.html) for
population means
[`svytotal`](https://rdrr.io/pkg/survey/man/surveysummary.html) for
population totals
[`svyratio`](https://rdrr.io/pkg/survey/man/svyratio.html) for ratios
[`svyby`](https://rdrr.io/pkg/survey/man/svyby.html) for domain
estimations
[`PoolSurvey`](https://metasurveyr.github.io/metasurvey/reference/PoolSurvey.md)
for survey pooling

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic estimations
result <- workflow(
  survey = list(ech_2023),
  svymean(~unemployed, na.rm = TRUE),
  svytotal(~active_population, na.rm = TRUE),
  estimation_type = "annual"
)

# Multiple periods
result_multiple <- workflow(
  survey = list(ech_jan, ech_feb, ech_mar),
  svymean(~labor_income, na.rm = TRUE),
  svyratio(~total_income, ~persons, na.rm = TRUE),
  estimation_type = "monthly"
)

# Domain estimations
result_domains <- workflow(
  survey = list(ech_2023),
  svyby(~unemployed, ~region_4, svymean, na.rm = TRUE),
  estimation_type = "annual"
)

# Multiple estimation types
result_complete <- workflow(
  survey = list(ech_quarter),
  svymean(~activity_rate, na.rm = TRUE),
  estimation_type = c("monthly", "quarterly")
)
} # }
```
