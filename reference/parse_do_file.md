# Parse a STATA .do file into structured commands

Reads a .do file and returns a list of parsed command objects. Handles
comment stripping, line continuation, loop expansion, and command
tokenization.

## Usage

``` r
parse_do_file(do_file, encoding = "latin1")
```

## Arguments

- do_file:

  Path to a STATA .do file

- encoding:

  File encoding (default "latin1" for legacy STATA files)

## Value

A list of StataCommand lists, each with fields: cmd, args, if_clause,
options, raw_line, line_num, capture
