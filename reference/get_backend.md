# Get recipe backend

Returns the currently configured recipe backend. Defaults to "api" if
not configured.

## Usage

``` r
get_backend()
```

## Value

RecipeBackend object

## Examples

``` r
set_backend("local", path = tempfile(fileext = ".json"))
backend <- get_backend()
backend
#> <RecipeBackend>
#>   Public:
#>     clone: function (deep = FALSE) 
#>     filter: function (survey_type = NULL, edition = NULL, category = NULL, 
#>     get: function (id) 
#>     increment_downloads: function (id) 
#>     initialize: function (type, path = NULL) 
#>     list_all: function () 
#>     load: function () 
#>     publish: function (recipe) 
#>     rank: function (n = NULL) 
#>     save: function () 
#>     search: function (query) 
#>     type: local
#>   Private:
#>     .path: /tmp/RtmpKSpfjQ/file65d4102e2af1.json
#>     .registry: RecipeRegistry, R6
```
