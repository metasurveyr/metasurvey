# Group dates

Group dates

## Usage

``` r
group_dates(dates, type = c("monthly", "quarterly", "biannual"))
```

## Arguments

- dates:

  Vector of Date objects.

- type:

  Grouping type: "monthly", "quarterly", or "biannual".

## Value

Integer vector of group indices (e.g. 1-12 for monthly, 1-4 for
quarterly).

## Examples

``` r
dates <- as.Date(c("2023-01-15", "2023-04-20", "2023-07-10", "2023-11-05"))
group_dates(dates, "quarterly")
#> 2023-01-15 2023-04-20 2023-07-10 2023-11-05 
#>          1          2          3          4 
group_dates(dates, "biannual")
#> 2023-01-15 2023-04-20 2023-07-10 2023-11-05 
#>          1          1          2          2 
```
