# Extract time pattern

Extract time pattern

## Usage

``` r
extract_time_pattern(svy_edition)
```

## Arguments

- svy_edition:

  Survey edition string (e.g. "2023", "2023-06", "2023_Q1").

## Value

List with components: periodicity, year, month (when applicable).

## Examples

``` r
# Annual edition
extract_time_pattern("2023")
#> $year
#> [1] 2023
#> 
#> $periodicity
#> [1] "Annual"
#> 

# Monthly edition
extract_time_pattern("2023-06")
#> $year
#> [1] 2023
#> 
#> $month
#> [1] 6
#> 
#> $periodicity
#> [1] "Monthly"
#> 
```
