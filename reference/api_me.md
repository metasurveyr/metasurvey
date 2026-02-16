# Get current user profile

Returns profile info for the currently authenticated user.

## Usage

``` r
api_me()
```

## Value

List with user fields (name, email, user_type, etc.)

## See also

Other api-auth:
[`api_login()`](https://metasurveyr.github.io/metasurvey/reference/api_login.md),
[`api_logout()`](https://metasurveyr.github.io/metasurvey/reference/api_logout.md),
[`api_refresh_token()`](https://metasurveyr.github.io/metasurvey/reference/api_refresh_token.md),
[`api_register()`](https://metasurveyr.github.io/metasurvey/reference/api_register.md),
[`configure_api()`](https://metasurveyr.github.io/metasurvey/reference/configure_api.md)

## Examples

``` r
if (FALSE) { # \dontrun{
api_me()
} # }
```
