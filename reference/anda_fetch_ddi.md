# Fetch DDI XML from ANDA5 catalog

Downloads the DDI (Data Documentation Initiative) XML file for a given
catalog entry from INE Uruguay's ANDA5 catalog.

## Usage

``` r
anda_fetch_ddi(
  catalog_id,
  base_url = "https://www4.ine.gub.uy/Anda5",
  dest_file = NULL
)
```

## Arguments

- catalog_id:

  Integer catalog ID (e.g., 767 for ECH 2024)

- base_url:

  Character base URL of the ANDA5 instance

- dest_file:

  Character path where to save the XML file. If NULL, uses a temporary
  file.

## Value

Character path to the downloaded DDI XML file
