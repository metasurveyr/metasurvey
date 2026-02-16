# Configure weights by periodicity for Survey objects

This function creates a weight structure that allows specifying
different weight variables according to estimation periodicity. It is
essential for proper functioning of workflows with multiple temporal
estimation types.

## Usage

``` r
add_weight(monthly = NULL, annual = NULL, quarterly = NULL, biannual = NULL)
```

## Arguments

- monthly:

  String with monthly weight variable name, or replicate list created
  with
  [`add_replicate`](https://metasurveyr.github.io/metasurvey/reference/add_replicate.md)
  for monthly weights

- annual:

  String with annual weight variable name, or replicate list for annual
  weights

- quarterly:

  String with quarterly weight variable name, or replicate list for
  quarterly weights

- biannual:

  String with biannual weight variable name, or replicate list for
  biannual weights

## Value

Named list with weight configuration by periodicity, which will be used
by
[`load_survey`](https://metasurveyr.github.io/metasurvey/reference/load_survey.md)
and
[`workflow`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
to automatically select the appropriate weight

## Details

This function is fundamental for surveys that require different weights
according to temporal estimation type. For example, Uruguay's ECH has
specific weights for monthly, quarterly, and annual estimations.

Each parameter can be:

- A simple string with the weight variable name

- A replicate structure created with
  [`add_replicate()`](https://metasurveyr.github.io/metasurvey/reference/add_replicate.md)
  for bootstrap or jackknife estimations

Weights are automatically selected in
[`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
according to the specified `estimation_type` parameter.

## See also

[`add_replicate`](https://metasurveyr.github.io/metasurvey/reference/add_replicate.md)
to configure bootstrap/jackknife replicates
[`load_survey`](https://metasurveyr.github.io/metasurvey/reference/load_survey.md)
where this configuration is used
[`workflow`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
that automatically selects weights

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic configuration with simple weight variables
ech_weights <- add_weight(
  monthly = "pesomes",
  quarterly = "pesotri",
  annual = "pesoano"
)

# With bootstrap replicates for variance estimation
weights_with_replicates <- add_weight(
  monthly = add_replicate(
    weight = "pesomes",
    replicate_pattern = "wr\\\\d+",
    replicate_path = "monthly_replicate_weights.xlsx",
    replicate_id = c("ID_HOGAR" = "ID"),
    replicate_type = "bootstrap"
  ),
  annual = "pesoano"
)

# Usage in load_survey
ech <- load_survey(
  path = "ech_2023.dta",
  svy_type = "ech",
  svy_edition = "2023",
  svy_weight = add_weight(
    monthly = "pesomes",
    annual = "pesoano"
  )
)
} # }
```
