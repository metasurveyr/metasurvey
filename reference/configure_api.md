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

## See also

Other api-auth:
[`api_login()`](https://metasurveyr.github.io/metasurvey/reference/api_login.md),
[`api_logout()`](https://metasurveyr.github.io/metasurvey/reference/api_logout.md),
[`api_me()`](https://metasurveyr.github.io/metasurvey/reference/api_me.md),
[`api_refresh_token()`](https://metasurveyr.github.io/metasurvey/reference/api_refresh_token.md),
[`api_register()`](https://metasurveyr.github.io/metasurvey/reference/api_register.md)

## Examples

``` r
if (FALSE) { # \dontrun{
configure_api(url = "https://metasurvey-api.example.com")
} # }
```
