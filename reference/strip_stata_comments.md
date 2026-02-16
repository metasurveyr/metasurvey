# Strip STATA comments from source lines

Removes single-line comments (// and \* at start) and multi-line block
comments (/\* ... \*/).

## Usage

``` r
strip_stata_comments(lines)
```

## Arguments

- lines:

  Character vector of source lines

## Value

Character vector with comments removed
