# Print provenance diff

Print provenance diff

## Usage

``` r
# S3 method for class 'metasurvey_provenance_diff'
print(x, ...)
```

## Arguments

- x:

  A `metasurvey_provenance_diff` list.

- ...:

  Additional arguments (unused).

## Value

Invisibly returns `x`.

## See also

Other provenance:
[`print.metasurvey_provenance()`](https://metasurveyr.github.io/metasurvey/reference/print.metasurvey_provenance.md),
[`provenance()`](https://metasurveyr.github.io/metasurvey/reference/provenance.md),
[`provenance_diff()`](https://metasurveyr.github.io/metasurvey/reference/provenance_diff.md),
[`provenance_to_json()`](https://metasurveyr.github.io/metasurvey/reference/provenance_to_json.md)

## Examples

``` r
s1 <- survey_empty("ech", "2022")
#> Type does not match. Please provide a valid type in the survey edition or as an argument
s1 <- set_data(s1, data.table::data.table(age = 18:65))
s1 <- step_compute(s1, age2 = age * 2)
s1 <- bake_steps(s1)

s2 <- survey_empty("ech", "2023")
#> Type does not match. Please provide a valid type in the survey edition or as an argument
s2 <- set_data(s2, data.table::data.table(age = 20:70))
s2 <- step_compute(s2, age2 = age * 2)
s2 <- bake_steps(s2)

diff_result <- provenance_diff(provenance(s1), provenance(s2))
print(diff_result)
#> ── Provenance Diff ─────────────────────────────────────────────────────────────
#> Final N: 48 -> 51 
#> Steps: 1 -> 1 
```
