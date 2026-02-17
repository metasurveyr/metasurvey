# Configure replicate weights for variance estimation

This function configures replicate weights (bootstrap, jackknife, etc.)
that allow estimation of variance for complex statistics in surveys with
complex sampling designs. It is essential for obtaining correct standard
errors in population estimates.

## Usage

``` r
add_replicate(
  weight,
  replicate_pattern,
  replicate_path = NULL,
  replicate_id = NULL,
  replicate_type
)
```

## Arguments

- weight:

  String with the name of the main weight variable in the survey (e.g.,
  "pesoano", "pesomes")

- replicate_pattern:

  String with regex pattern to identify replicate weight columns.
  Examples: "wr\d+" for columns wr1, wr2, etc.

- replicate_path:

  Path to the file containing replicate weights. If NULL, assumes they
  are in the same main dataset

- replicate_id:

  Named vector specifying how to join between the main dataset and
  replicate file. Format: c("main_var" = "replicate_var")

- replicate_type:

  Type of replication used. Options: "bootstrap", "jackknife", "BRR"
  (Balanced Repeated Replication)

## Value

List with replicate configuration that will be used by the sampling
design for variance estimation

## Details

Replicate weights are essential for:

- Correctly estimating variance in complex designs

- Calculating appropriate confidence intervals

- Obtaining reliable coefficients of variation

- Performing valid statistical tests

The regex pattern must exactly match the replicate weight column names
in the file. For example, if columns are named "wr001", "wr002", etc.,
use the pattern "wr\\d+".

This function is typically used within
[`add_weight()`](https://metasurveyr.github.io/metasurvey/reference/add_weight.md)
for more complex weight configurations.

## See also

[`add_weight`](https://metasurveyr.github.io/metasurvey/reference/add_weight.md)
for complete weight configuration
[`svrepdesign`](https://rdrr.io/pkg/survey/man/svrepdesign.html) for
replicate design in survey package
[`load_survey`](https://metasurveyr.github.io/metasurvey/reference/load_survey.md)
where this configuration is used

Other weights:
[`add_weight()`](https://metasurveyr.github.io/metasurvey/reference/add_weight.md),
[`resolve_weight_spec()`](https://metasurveyr.github.io/metasurvey/reference/resolve_weight_spec.md)

## Examples

``` r
# Basic configuration with external file
annual_replicates <- add_replicate(
  weight = "pesoano",
  replicate_pattern = "wr\\d+",
  replicate_path = "bootstrap_weights_2023.xlsx",
  replicate_id = c("ID_HOGAR" = "ID"),
  replicate_type = "bootstrap"
)

# With replicates in same dataset
integrated_replicates <- add_replicate(
  weight = "main_weight",
  replicate_pattern = "rep_\\d{3}",
  replicate_type = "jackknife"
)

# Use within add_weight
weight_config <- add_weight(
  annual = add_replicate(
    weight = "pesoano",
    replicate_pattern = "wr\\d+",
    replicate_path = "bootstrap_annual.xlsx",
    replicate_id = c("numero" = "ID_HOGAR"),
    replicate_type = "bootstrap"
  ),
  monthly = "pesomes"
)
```
