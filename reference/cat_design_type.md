# cat_design_type

Cast design type from survey

## Usage

``` r
cat_design_type(self, design_name)
```

## Arguments

- self:

  Object of class Survey

- design_name:

  Name of design

## Value

Character string describing the design type, or "None".

## Examples

``` r
if (FALSE) { # \dontrun{
svy <- load_survey("data.csv", svy_type = "ech", svy_edition = "2023")
cat_design_type(svy, "annual")
} # }
```
