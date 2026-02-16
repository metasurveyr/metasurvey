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

## Examples

``` r
if (FALSE) { # \dontrun{
# Display design for survey with multiple estimation types
ech_survey <- load_survey("ech_2023.dta",
  svy_type = "ech",
  svy_edition = "2023"
)
cat_design(ech_survey)
} # }
```
