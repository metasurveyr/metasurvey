# cat_recipes

Cast recipes from survey

## Usage

``` r
cat_recipes(self)
```

## Arguments

- self:

  Object of class Survey

## Value

Character string listing recipe names, or "None".

## Examples

``` r
if (FALSE) { # \dontrun{
svy <- load_survey("data.csv", svy_type = "ech", svy_edition = "2023")
cat_recipes(svy)
} # }
```
