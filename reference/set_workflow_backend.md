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

## Examples

``` r
if (FALSE) { # \dontrun{
set_workflow_backend("local", path = "my_workflows.json")
set_workflow_backend("api")
} # }
```
