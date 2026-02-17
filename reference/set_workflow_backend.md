# Set workflow backend

Configure the active workflow backend via options.

## Usage

``` r
set_workflow_backend(type, path = NULL)
```

## Arguments

- type:

  Character. "local" or "api" (also accepts "mongo" for backward
  compat).

- path:

  Character. File path for local backend.

## Value

Invisibly, the WorkflowBackend object created.

## See also

Other backends:
[`get_backend()`](https://metasurveyr.github.io/metasurvey/reference/get_backend.md),
[`get_workflow_backend()`](https://metasurveyr.github.io/metasurvey/reference/get_workflow_backend.md),
[`set_backend()`](https://metasurveyr.github.io/metasurvey/reference/set_backend.md)

## Examples

``` r
set_workflow_backend("local", path = tempfile(fileext = ".json"))
```
