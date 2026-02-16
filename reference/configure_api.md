# Configure metasurvey API

Set API base URL and optionally load stored credentials. The URL can
also be set via the `METASURVEY_API_URL` environment variable, and the
token via `METASURVEY_TOKEN`.

## Usage

``` r
configure_api(url)
```

## Arguments

- url:

  API base URL (e.g., `"https://metasurvey-api.example.com"`)

## Value

Invisibly, the previous URL (for restoring).

## Examples

``` r
if (FALSE) { # \dontrun{
configure_api(url = "https://metasurvey-api.example.com")
} # }
```
