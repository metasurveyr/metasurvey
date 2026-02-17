# Get workflow backend

Returns the currently configured workflow backend. Defaults to "local"
if not configured.

## Usage

``` r
get_workflow_backend()
```

## Value

WorkflowBackend object

## See also

Other backends:
[`get_backend()`](https://metasurveyr.github.io/metasurvey/reference/get_backend.md),
[`set_backend()`](https://metasurveyr.github.io/metasurvey/reference/set_backend.md),
[`set_workflow_backend()`](https://metasurveyr.github.io/metasurvey/reference/set_workflow_backend.md)

## Examples

``` r
backend <- get_workflow_backend()
```
