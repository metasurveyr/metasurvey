# Collect lines inside a loop body until closing brace

Collect lines inside a loop body until closing brace

## Usage

``` r
collect_loop_body(lines, start_idx)
```

## Arguments

- lines:

  All source lines

- start_idx:

  Index of the line with opening brace

## Value

List with body (character vector) and end_idx
