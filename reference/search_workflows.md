# Search workflows

Search for workflows by name or description in the active workflow
backend.

## Usage

``` r
search_workflows(query)
```

## Arguments

- query:

  Character search string.

## Value

List of matching RecipeWorkflow objects.

## See also

[`filter_workflows`](https://metasurveyr.github.io/metasurvey/reference/filter_workflows.md),
[`rank_workflows`](https://metasurveyr.github.io/metasurvey/reference/rank_workflows.md)

## Examples

``` r
if (FALSE) { # \dontrun{
results <- search_workflows("labor market")
} # }
```
