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
