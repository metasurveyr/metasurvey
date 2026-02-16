# survey_empty

Create an empty survey

## Usage

``` r
survey_empty(
  edition = NULL,
  type = NULL,
  weight = NULL,
  engine = NULL,
  psu = NULL
)
```

## Arguments

- edition:

  Edition of survey

- type:

  Type of survey

- weight:

  Weight of survey

- engine:

  Engine of survey

- psu:

  PSU variable or formula (optional)

## Value

Survey object

## Examples

``` r
if (FALSE) { # \dontrun{
empty <- survey_empty()
empty_typed <- survey_empty(edition = "2023", type = "ech")
} # }
```
