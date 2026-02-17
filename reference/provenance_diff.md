# Compare two provenance objects

Shows differences between two provenance records, useful for comparing
processing across survey editions.

## Usage

``` r
provenance_diff(prov1, prov2)
```

## Arguments

- prov1:

  First provenance list.

- prov2:

  Second provenance list.

## Value

A `metasurvey_provenance_diff` list with detected differences.

## See also

Other provenance:
[`print.metasurvey_provenance()`](https://metasurveyr.github.io/metasurvey/reference/print.metasurvey_provenance.md),
[`print.metasurvey_provenance_diff()`](https://metasurveyr.github.io/metasurvey/reference/print.metasurvey_provenance_diff.md),
[`provenance()`](https://metasurveyr.github.io/metasurvey/reference/provenance.md),
[`provenance_to_json()`](https://metasurveyr.github.io/metasurvey/reference/provenance_to_json.md)

## Examples

``` r
svy1 <- Survey$new(
  data = data.table::data.table(id = 1:5, w = rep(1, 5)),
  edition = "2023", type = "test",
  engine = "data.table", weight = add_weight(annual = "w")
)
svy2 <- Survey$new(
  data = data.table::data.table(id = 1:5, w = rep(2, 5)),
  edition = "2024", type = "test",
  engine = "data.table", weight = add_weight(annual = "w")
)
provenance_diff(provenance(svy1), provenance(svy2))
#> ── Provenance Diff ─────────────────────────────────────────────────────────────
#> Steps: 0 -> 0 
```
