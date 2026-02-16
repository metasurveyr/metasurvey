# Login

Authenticate with the metasurvey API. On success the JWT token is stored
automatically.

## Usage

``` r
api_login(email, password)
```

## Arguments

- email:

  Email address

- password:

  Password

## Value

Invisibly, the API response.

## See also

Other api-auth:
[`api_logout()`](https://metasurveyr.github.io/metasurvey/reference/api_logout.md),
[`api_me()`](https://metasurveyr.github.io/metasurvey/reference/api_me.md),
[`api_refresh_token()`](https://metasurveyr.github.io/metasurvey/reference/api_refresh_token.md),
[`api_register()`](https://metasurveyr.github.io/metasurvey/reference/api_register.md),
[`configure_api()`](https://metasurveyr.github.io/metasurvey/reference/configure_api.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_login("ana@example.com", "s3cret")
} # }
```
