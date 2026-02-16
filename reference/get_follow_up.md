# Obtener encuestas de seguimiento de panel rotativo

Esta función extrae una o múltiples encuestas de seguimiento (ondas
posteriores a la implantación) de un objeto RotativePanelSurvey. Las
encuestas de seguimiento representan las recolecciones posteriores del
panel y son esenciales para análisis longitudinales y de cambio
temporal.

## Usage

``` r
get_follow_up(
  RotativePanelSurvey,
  index = seq_along(RotativePanelSurvey$follow_up)
)
```

## Arguments

- RotativePanelSurvey:

  Objeto `RotativePanelSurvey` del cual extraer las encuestas de
  seguimiento

- index:

  Vector de enteros que especifica cuáles encuestas de seguimiento
  extraer. Por defecto extrae todas las disponibles
  (1:length(follow_up)). Puede ser un solo índice o un vector de índices

## Value

Lista de objetos `Survey` correspondientes a las encuestas de
seguimiento especificadas. Si se especifica un solo índice, devuelve una
lista con un elemento

## Details

Las encuestas de seguimiento son fundamentales en paneles rotativos
porque:

- Permiten análisis longitudinal: Seguimiento de las mismas unidades a
  través del tiempo

- Capturan cambios temporales: Evolución de variables económicas,
  sociales y demográficas

- Mantienen representatividad: Cada onda mantiene representatividad
  poblacional mediante rotación controlada

- Optimizan recursos: Reutilizan información de ondas anteriores para
  reducir costos de recolección

- Facilitan comparaciones: Estructura temporal consistente para análisis
  de tendencias

En paneles rotativos como ECH:

- Cada onda de seguimiento cubre un período específico
  (mensual/trimestral)

- Las unidades rotan gradualmente manteniendo overlap temporal

- Los índices corresponden al orden cronológico de recolección

- Cada seguimiento mantiene consistencia metodológica con implantación

## See also

[`get_implantation`](https://metasurveyr.github.io/metasurvey/reference/get_implantation.md)
para obtener la encuesta de implantación
[`extract_surveys`](https://metasurveyr.github.io/metasurvey/reference/extract_surveys.md)
para extraer encuestas por criterios temporales
[`load_panel_survey`](https://metasurveyr.github.io/metasurvey/reference/load_panel_survey.md)
para cargar paneles rotativos
[`workflow`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
para análisis con encuestas de seguimiento

## Examples

``` r
if (FALSE) { # \dontrun{
# Cargar panel rotativo
panel_ech <- load_panel_survey(
  path = "ech_panel_2023.dta",
  svy_type = "ech_panel",
  svy_edition = "2023"
)

# Obtener primera encuesta de seguimiento
seguimiento_1 <- get_follow_up(panel_ech, index = 1)[[1]]

# Obtener múltiples seguimientos
seguimientos_trim1 <- get_follow_up(panel_ech, index = c(1, 2, 3))

# Obtener todos los seguimientos disponibles
todos_seguimientos <- get_follow_up(panel_ech)

# Verificar número de seguimientos disponibles
n_seguimientos <- length(get_follow_up(panel_ech))
cat("Seguimientos disponibles:", n_seguimientos)

# Análisis longitudinal con seguimientos
implantacion <- get_implantation(panel_ech)
seguimiento_final <- get_follow_up(panel_ech, index = n_seguimientos)[[1]]

# Comparar tasas entre implantación y seguimiento final
tasa_inicial <- workflow(
  survey = implantacion,
  svymean(~tasa_desempleo, na.rm = TRUE)
)

tasa_final <- workflow(
  survey = seguimiento_final,
  svymean(~tasa_desempleo, na.rm = TRUE)
)

# Análisis trimestral con seguimientos específicos
trimestre_actual <- workflow(
  survey = seguimientos_trim1,
  svymean(~ingreso_laboral, na.rm = TRUE),
  estimation_type = "quarterly"
)
} # }
```
