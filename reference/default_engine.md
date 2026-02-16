# Set default engine

Sets a default engine for loading surveys. If an engine is already
configured, it keeps it; otherwise, it sets "data.table" as the default
engine.

## Usage

``` r
default_engine(.engine = "data.table")
```

## Arguments

- .engine:

  Character vector with the name of the default engine. By default,
  "data.table" is used.
