# Read Recipe

Reads a Recipe object from a JSON file.

## Usage

``` r
read_recipe(file)
```

## Arguments

- file:

  A character string specifying the file path.

## Value

A Recipe object.

## Details

This function reads a JSON file and decodes it into a Recipe object.

## Examples

``` r
r <- recipe(
  name = "Example", user = "Test",
  svy = survey_empty(type = "ech", edition = "2023"),
  description = "Example recipe"
)
f <- tempfile(fileext = ".json")
save_recipe(r, f)
#> The recipe has been saved in /tmp/RtmpomrfhV/file1a7e63f17849.json
r2 <- read_recipe(f)
#> Warning: Failed to parse recipe steps: invalid length 0 argument. Using raw strings as fallback.
r2
#> 
#> ── Recipe: Example ──
#> Author:  Test
#> Survey:  ech / 2023
#> Version: 1.0.0
#> Topic:   
#> DOI:     
#> Description: Example recipe
#> Certification: community
#> 
```
