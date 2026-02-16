# Launch the Recipe Explorer Shiny App

Opens an interactive web application to explore, search, and browse
metasurvey recipes with visual documentation cards. Supports user
registration and login via MongoDB Atlas.

## Usage

``` r
explore_recipes(port = NULL, host = "127.0.0.1", launch.browser = TRUE)
```

## Arguments

- port:

  Integer port number, or NULL for automatic.

- host:

  Character. The host to listen on. Defaults to `"127.0.0.1"` for local
  use. Set to `"0.0.0.0"` for server deployments (Railway, etc.).

- launch.browser:

  Logical. Open the app in a browser?

## Value

NULL (called for side effect of launching the app).

## Examples

``` r
if (FALSE) { # \dontrun{
# Local / RStudio viewer
explore_recipes()

# Server deployment (Railway, Docker, etc.)
explore_recipes(host = "0.0.0.0", port = 3838, launch.browser = FALSE)
} # }
```
