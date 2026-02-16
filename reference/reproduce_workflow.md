# Reproduce a workflow from its published specification

Given a RecipeWorkflow (typically fetched from the registry), downloads
the data, resolves the weight configuration, fetches referenced recipes,
and returns a Survey object ready for
[`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
estimation.

## Usage

``` r
reproduce_workflow(wf, data_path = NULL, dest_dir = tempdir())
```

## Arguments

- wf:

  RecipeWorkflow object

- data_path:

  Character path to survey microdata. If NULL, attempts to download from
  ANDA for ECH surveys.

- dest_dir:

  Character directory for downloaded files

## Value

Survey object with recipes applied and weight configuration set

## See also

Other workflows:
[`RecipeWorkflow-class`](https://metasurveyr.github.io/metasurvey/reference/RecipeWorkflow-class.md),
[`evaluate_cv()`](https://metasurveyr.github.io/metasurvey/reference/evaluate_cv.md),
[`print.RecipeWorkflow()`](https://metasurveyr.github.io/metasurvey/reference/print.RecipeWorkflow.md),
[`publish_workflow()`](https://metasurveyr.github.io/metasurvey/reference/publish_workflow.md),
[`read_workflow()`](https://metasurveyr.github.io/metasurvey/reference/read_workflow.md),
[`save_workflow()`](https://metasurveyr.github.io/metasurvey/reference/save_workflow.md),
[`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md),
[`workflow_from_list()`](https://metasurveyr.github.io/metasurvey/reference/workflow_from_list.md)

## Examples

``` r
if (FALSE) { # \dontrun{
wf <- api_get_workflow("w_123")
svy <- reproduce_workflow(wf)
} # }
```
