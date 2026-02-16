# Search ANDA5 catalog

Queries the ANDA5 REST API to list available survey datasets.

## Usage

``` r
anda_catalog_search(
  keyword = "ECH",
  base_url = "https://www4.ine.gub.uy/Anda5",
  limit = 50
)
```

## Arguments

- keyword:

  Character search term (e.g., "ECH")

- base_url:

  Character base URL of the ANDA5 instance

- limit:

  Integer max results to return

## Value

A data.frame with columns: id, title, year_start, year_end
