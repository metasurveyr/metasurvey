# Get detailed metadata for a single ANDA variable

Get detailed metadata for a single ANDA variable

## Usage

``` r
anda_variable_detail(survey_type = "ech", var_name)
```

## Arguments

- survey_type:

  Character survey type (default "ech")

- var_name:

  Character variable name

## Value

A list with: name, label, type, value_labels, description. NULL if not
found.
