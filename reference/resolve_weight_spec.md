# Resolve a portable weight specification to a usable weight configuration

Converts the portable weight_spec from a RecipeWorkflow back into the
format expected by
[`load_survey()`](https://metasurveyr.github.io/metasurvey/reference/load_survey.md)
and
[`add_weight()`](https://metasurveyr.github.io/metasurvey/reference/add_weight.md).
For replicate weights with ANDA sources, automatically downloads the
replicate file.

## Usage

``` r
resolve_weight_spec(weight_spec, dest_dir = tempdir())
```

## Arguments

- weight_spec:

  Named list from RecipeWorkflow\$weight_spec

- dest_dir:

  Character directory for downloaded files (default: tempdir())

## Value

Named list compatible with add_weight() output
