# Construct a RecipeWorkflow from a plain list

Construct a RecipeWorkflow from a plain list

## Usage

``` r
workflow_from_list(lst)
```

## Arguments

- lst:

  A list (typically from JSON) with workflow fields

## Value

A RecipeWorkflow object

## Examples

``` r
lst <- list(name = "example", user = "test", survey_type = "ech",
            edition = "2023", estimation_type = "svymean")
wf <- workflow_from_list(lst)
```
