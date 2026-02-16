# Convert a list of steps to a recipe

Convert a list of steps to a recipe

## Usage

``` r
steps_to_recipe(
  name,
  user,
  svy = survey_empty(type = "eaii", edition = "2019-2021"),
  description,
  steps,
  doi = NULL,
  topic = NULL
)
```

## Arguments

- name:

  A character string with the name of the recipe

- user:

  A character string with the user of the recipe

- svy:

  A Survey object

- description:

  A character string with the description of the recipe

- steps:

  A list with the steps of the recipe

- doi:

  A character string with the DOI of the recipe

- topic:

  A character string with the topic of the recipe

## Value

A Recipe object

## Examples

``` r
if (FALSE) { # \dontrun{
svy <- load_survey("data.csv", svy_type = "ech", svy_edition = "2023")
svy <- step_compute(svy, employed = ifelse(status == 1, 1, 0))
my_recipe <- steps_to_recipe(
  name = "employment", user = "analyst",
  svy = svy, description = "Employment indicators",
  steps = get_steps(svy)
)
} # }
```
