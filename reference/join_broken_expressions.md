# Join broken expression lines after block comment removal

After removing /\* \*/ block comments, expressions can be split across
lines. This function re-joins lines where the previous line ends with an
operator or open paren, or the next line starts with an operator or
close paren.

## Usage

``` r
join_broken_expressions(lines)
```

## Arguments

- lines:

  Character vector of source lines

## Value

Character vector with broken expressions re-joined
