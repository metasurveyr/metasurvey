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
