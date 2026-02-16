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

## Examples

``` r
if (FALSE) { # \dontrun{
api_login("ana@example.com", "s3cret")
} # }
```
