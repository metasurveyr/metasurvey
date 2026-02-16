# Download ECH microdata from ANDA5

Downloads microdata files for a given ECH edition from INE Uruguay's
ANDA5 catalog. Automatically accepts the terms of use, parses available
resources, and downloads the appropriate file.

## Usage

``` r
anda_download_microdata(
  edition,
  resource = "implantation",
  dest_dir = tempdir(),
  base_url = "https://www4.ine.gub.uy/Anda5"
)
```

## Arguments

- edition:

  Character year (e.g., "2023")

- resource:

  Character type of resource to download. One of:

  "implantation"

  :   (default) Main implantation file. For editions \< 2022, downloads
      the main microdata file.

  "monthly"

  :   Monthly follow-up files (editions \>= 2022 only). Returns a
      character vector of paths, one per month.

  "bootstrap_annual"

  :   Annual bootstrap replicate weights.

  "bootstrap_monthly"

  :   Monthly bootstrap replicate weights.

  "bootstrap_quarterly"

  :   Quarterly bootstrap replicate weights.

  "bootstrap_semestral"

  :   Semestral bootstrap replicate weights.

  "poverty"

  :   Poverty line microdata (Microdatos_LP).

- dest_dir:

  Character directory where to save files. Defaults to a temporary
  directory.

- base_url:

  Character base URL of the ANDA5 instance

## Value

Character path (or vector of paths for monthly) to the downloaded
file(s), ready to pass to
[`load_survey()`](https://metasurveyr.github.io/metasurvey/reference/load_survey.md)
or
[`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html).

## Details

For editions \>= 2022, ANDA provides separate files for implantation,
monthly follow-ups, and bootstrap replicate weights. Use the `resource`
parameter to select which file to download.

## See also

Other anda:
[`anda_variables()`](https://metasurveyr.github.io/metasurvey/reference/anda_variables.md),
[`api_get_anda_variables()`](https://metasurveyr.github.io/metasurvey/reference/api_get_anda_variables.md)

## Examples

``` r
if (FALSE) { # \dontrun{
path <- anda_download_microdata("2023", resource = "implantation")
svy <- load_survey(path, svy_type = "ech", svy_edition = "2023")
} # }
```
