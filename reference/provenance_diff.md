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
[`provenance()`](https://metasurveyr.github.io/metasurvey/reference/provenance.md),
[`provenance_to_json()`](https://metasurveyr.github.io/metasurvey/reference/provenance_to_json.md)
