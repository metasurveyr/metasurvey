# Get workflow backend

Returns the currently configured workflow backend. Defaults to "local"
if not configured.

## Usage

``` r
get_workflow_backend()
```

## Value

WorkflowBackend object

## Examples

``` r
if (FALSE) { # \dontrun{
backend <- get_workflow_backend()
backend$search("labor")
} # }
```
