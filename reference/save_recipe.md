# Save Recipe

Saves a Recipe object to a file in JSON format.

## Usage

``` r
save_recipe(recipe, file)
```

## Arguments

- recipe:

  A Recipe object.

- file:

  A character string specifying the file path.

## Value

NULL.

## Details

This function encodes the Recipe object and writes it to a JSON file.

## Examples

``` r
r <- recipe(
  name = "Example", user = "Test",
  svy = survey_empty(type = "ech", edition = "2023"),
  description = "Example recipe"
)
f <- tempfile(fileext = ".json")
save_recipe(r, f)
#> The recipe has been saved in /tmp/RtmpHzL1ea/file658b766dee13.json
```
