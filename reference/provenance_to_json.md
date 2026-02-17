# Export provenance to JSON

Serializes a provenance object to JSON format, optionally writing to a
file.

## Usage

``` r
provenance_to_json(prov, path = NULL)
```

## Arguments

- prov:

  A `metasurvey_provenance` list.

- path:

  File path to write JSON. If `NULL`, returns the JSON string.

## Value

JSON string (invisibly if `path` is provided).

## See also

Other provenance:
[`print.metasurvey_provenance()`](https://metasurveyr.github.io/metasurvey/reference/print.metasurvey_provenance.md),
[`provenance()`](https://metasurveyr.github.io/metasurvey/reference/provenance.md),
[`provenance_diff()`](https://metasurveyr.github.io/metasurvey/reference/provenance_diff.md)
