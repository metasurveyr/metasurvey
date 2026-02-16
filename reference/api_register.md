# Register a new user

Create an account on the metasurvey API. On success the JWT token is
stored automatically via `options(metasurvey.api_token)`.

## Usage

``` r
api_register(
  name,
  email,
  password,
  user_type = "individual",
  institution = NULL
)
```

## Arguments

- name:

  Display name

- email:

  Email address

- password:

  Password

- user_type:

  One of `"individual"`, `"institutional_member"`, `"institution"`

- institution:

  Institution name (required for `"institutional_member"`)

## Value

Invisibly, the API response (list with `ok`, `token`, `user`).

## See also

Other api-auth:
[`api_login()`](https://metasurveyr.github.io/metasurvey/reference/api_login.md),
[`api_logout()`](https://metasurveyr.github.io/metasurvey/reference/api_logout.md),
[`api_me()`](https://metasurveyr.github.io/metasurvey/reference/api_me.md),
[`api_refresh_token()`](https://metasurveyr.github.io/metasurvey/reference/api_refresh_token.md),
[`configure_api()`](https://metasurveyr.github.io/metasurvey/reference/configure_api.md)

## Examples

``` r
if (FALSE) { # \dontrun{
configure_api("https://metasurvey-api.example.com")
api_register("Ana Garcia", "ana@example.com", "s3cret")
} # }
```
