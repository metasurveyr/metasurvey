# Rank workflows by downloads

Get the top workflows ranked by download count.

## Usage

``` r
rank_workflows(n = NULL)
```

## Arguments

- n:

  Integer. Maximum number to return, or NULL for all.

## Value

List of RecipeWorkflow objects sorted by downloads.

## See also

[`search_workflows`](https://metasurveyr.github.io/metasurvey/reference/search_workflows.md),
[`filter_workflows`](https://metasurveyr.github.io/metasurvey/reference/filter_workflows.md)

## Examples

``` r
if (FALSE) { # \dontrun{
top5 <- rank_workflows(n = 5)
} # }
```
