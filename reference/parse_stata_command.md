# Parse a single STATA command line into structured data

Parse a single STATA command line into structured data

## Usage

``` r
parse_stata_command(line, line_num = NA_integer_)
```

## Arguments

- line:

  A single STATA command string

- line_num:

  Original line number (for error reporting)

## Value

A list with cmd, args, if_clause, options, raw_line, line_num, capture,
or NULL for empty/skippable lines
