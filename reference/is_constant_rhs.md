# Detect if a STATA replace RHS is a constant value

Returns TRUE if the expression is a simple numeric constant, string
literal, or negative number. Used to decide between step_recode
(constants) and step_compute (expressions).

## Usage

``` r
is_constant_rhs(expr)
```

## Arguments

- expr:

  STATA expression string

## Value

Logical
