# Transpile and group do-files by thematic module

Processes all do-files in a year directory and groups them into thematic
Recipe objects (demographics, income, etc.).

## Usage

``` r
transpile_stata_module(year_dir, year, user = "iecon", output_dir = NULL)
```

## Arguments

- year_dir:

  Path to a year directory (e.g., "do_files_iecon/2022")

- year:

  Year of the edition (character or numeric)

- user:

  Author name

- output_dir:

  Directory to write JSON recipes (NULL = no file output)

## Value

A named list of Recipe objects, one per thematic module

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires a directory of .do files organized by year
recipes <- transpile_stata_module("do_files_iecon/2022", year = 2022)
names(recipes)
} # }
```
