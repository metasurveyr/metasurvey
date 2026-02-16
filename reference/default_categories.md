# Default recipe categories

Returns a list of standard built-in categories for recipe
classification.

## Usage

``` r
default_categories()
```

## Value

List of RecipeCategory objects

## Examples

``` r
cats <- default_categories()
vapply(cats, function(c) c$name, character(1))
#> [1] "labor_market" "income"       "education"    "health"       "demographics"
#> [6] "housing"     
```
