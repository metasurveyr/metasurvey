# Parse STATA label commands from source lines

Extracts variable labels, value label definitions, and value label
assignments from label commands.

## Usage

``` r
parse_stata_labels(lines)
```

## Arguments

- lines:

  Character vector of source lines (already comment-stripped)

## Value

A list with var_labels (named list) and val_labels (named list of named
lists)

## Examples

``` r
# \donttest{
lines <- c(
  'label variable edad "Age in years"',
  'label define sexo_lbl 1 "Male" 2 "Female"',
  "label values sexo sexo_lbl"
)
labels <- parse_stata_labels(lines)
labels$var_labels
#> $edad
#> [1] "Age in years"
#> 
labels$val_labels
#> $sexo
#> $sexo$`1`
#> [1] "Male"
#> 
#> $sexo$`2`
#> [1] "Female"
#> 
#> 
# }
```
