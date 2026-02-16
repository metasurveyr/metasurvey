# Set recipe backend

Configure the active recipe backend via options.

## Usage

``` r
set_backend(type, path = NULL)
```

## Arguments

- type:

  Character. "local" or "api" (also accepts "mongo" for backward
  compat).

- path:

  Character. File path for local backend.

## Value

Invisibly, the RecipeBackend object created.

## See also

Other backends:
[`get_backend()`](https://metasurveyr.github.io/metasurvey/reference/get_backend.md),
[`get_workflow_backend()`](https://metasurveyr.github.io/metasurvey/reference/get_workflow_backend.md),
[`set_workflow_backend()`](https://metasurveyr.github.io/metasurvey/reference/set_workflow_backend.md)

## Examples

``` r
set_backend("local", path = tempfile(fileext = ".json"))
```
