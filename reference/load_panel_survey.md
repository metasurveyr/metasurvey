# Read panel survey files from different formats and create a RotativePanelSurvey object

Read panel survey files from different formats and create a
RotativePanelSurvey object

## Usage

``` r
load_panel_survey(
  path_implantation,
  path_follow_up,
  svy_type,
  svy_weight_implantation,
  svy_weight_follow_up,
  ...
)
```

## Arguments

- path_implantation:

  Survey implantation path, file can be in different formats, csv, xtsx,
  dta, sav and rds

- path_follow_up:

  Path with all the needed files with only survey valid files but also
  can be character vector with path files.

- svy_type:

  String with the survey type, supported types; "ech" (Encuensta
  Continua de Hogares, Uruguay), "eph" ( Encuesta Permanente de Hogares,
  Argentina), "eai" (Encuesta de Actividades de Innovación, Uruguay)

- svy_weight_implantation:

  List with survey implantation weights information specifing
  periodicity and the name of the weight variable. Recomended to use the
  helper function add_weight().

- svy_weight_follow_up:

  List with survey follow_up weights information specifing periodicity
  and the name of the weight variable. Recomended to use the helper
  function add_weight().

- ...:

  Further arguments to be passed to load_panel_survey

## Value

RotativePanelSurvey object

## Examples

``` r
if (FALSE) { # \dontrun{
# example code
path_dir <- here::here("example-data", "ech", "ech_2023")
ech_2023 <- load_panel_survey(
  path_implantation = file.path(
    path_dir,
    "ECH_implantacion_2023.csv"
  ),
  path_follow_up = file.path(
    path_dir,
    "seguimiento"
  ),
  svy_type = "ECH_2023",
  svy_weight_implantation = add_weight(
    annual = "W_ANO"
  ),
  svy_weight_follow_up = add_weight(
    monthly = add_replicate(
      "W",
      replicate_path = file.path(
        path_dir,
        c(
          "Pesos replicados Bootstrap mensuales enero_junio 2023",
          "Pesos replicados Bootstrap mensuales julio_diciembre 2023"
        ),
        c(
          "Pesos replicados mensuales enero_junio 2023",
          "Pesos replicados mensuales Julio_diciembre 2023"
        )
      ),
      replicate_id = c("ID" = "ID"),
      replicate_pattern = "wr[0-9]+",
      replicate_type = "bootstrap"
    )
  )
)
} # }
if (FALSE) { # \dontrun{
# Example of loading a panel survey
panel_survey <- load_panel_survey(
  path_implantation = "path/to/implantation.csv",
  path_follow_up = "path/to/follow_up",
  svy_type = "ech",
  svy_weight_implantation = add_weight(annual = "w_ano"),
  svy_weight_follow_up = add_weight(monthly = "w_monthly")
)
print(panel_survey)
} # }
```
