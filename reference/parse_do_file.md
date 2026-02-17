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

## Examples

``` r
# \donttest{
tf <- tempfile(fileext = ".do")
writeLines(c("gen age2 = edad^2", "replace sexo = 1 if sexo == ."), tf)
cmds <- parse_do_file(tf)
length(cmds)
#> [1] 2
cmds[[1]]$cmd
#> [1] "gen"
# }
```
