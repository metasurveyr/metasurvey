# Refresh JWT token

Request a new JWT token using the current (still valid) token. The new
token is stored automatically. This is called internally by
`api_request()` when the current token is close to expiry (within 5
minutes).

## Usage

``` r
api_refresh_token()
```

## Value

The new token string (invisibly), or NULL if refresh fails.

## See also

Other api-auth:
[`api_login()`](https://metasurveyr.github.io/metasurvey/reference/api_login.md),
[`api_logout()`](https://metasurveyr.github.io/metasurvey/reference/api_logout.md),
[`api_me()`](https://metasurveyr.github.io/metasurvey/reference/api_me.md),
[`api_register()`](https://metasurveyr.github.io/metasurvey/reference/api_register.md),
[`configure_api()`](https://metasurveyr.github.io/metasurvey/reference/configure_api.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_refresh_token()
} # }
```
