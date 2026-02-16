# Obtener encuesta de implantación de panel rotativo

Esta función extrae la encuesta de implantación (primera onda) de un
objeto RotativePanelSurvey. La encuesta de implantación representa la
primera recolección de datos del panel y es fundamental para establecer
la línea base y las características estructurales del panel.

## Usage

``` r
get_implantation(RotativePanelSurvey)
```

## Arguments

- RotativePanelSurvey:

  Objeto `RotativePanelSurvey` del cual extraer la encuesta de
  implantación

## Value

Objeto `Survey` que contiene la encuesta de implantación con todos sus
metadatos, datos y configuración de diseño

## Details

La encuesta de implantación es especial en un panel rotativo porque:

- Establece la línea base: Define las características iniciales de todas
  las unidades del panel

- Contiene toda la muestra: Incluye todas las unidades que participarán
  en las diferentes ondas del panel

- Define estructura temporal: Establece los patrones de rotación y
  seguimiento del panel

- Configura metadatos: Contiene información sobre periodicidad,
  variables clave y estratificación

- Base para seguimiento: Sirve como referencia para tracking de unidades
  en ondas posteriores

Esta función es esencial para análisis que requieren:

- Comparaciones temporales desde la línea base

- Análisis de la estructura completa del panel

- Configuración de modelos longitudinales

- Evaluación de la calidad del diseño muestral

## See also

[`get_follow_up`](https://metasurveyr.github.io/metasurvey/reference/get_follow_up.md)
para obtener encuestas de seguimiento
[`extract_surveys`](https://metasurveyr.github.io/metasurvey/reference/extract_surveys.md)
para extraer múltiples encuestas por criterios
[`load_panel_survey`](https://metasurveyr.github.io/metasurvey/reference/load_panel_survey.md)
para cargar paneles rotativos
[`workflow`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
para análisis con la encuesta de implantación

## Examples

``` r
if (FALSE) { # \dontrun{
# Cargar panel rotativo de ECH
panel_ech <- load_panel_survey(
  path = "ech_panel_2023.dta",
  svy_type = "ech_panel",
  svy_edition = "2023"
)

# Obtener encuesta de implantación
ech_implantacion <- get_implantation(panel_ech)

# Verificar características de la implantación
cat("Tamaño muestra implantación:", nrow(ech_implantacion$data))
cat("Variables disponibles:", ncol(ech_implantacion$data))

# Usar en análisis de línea base
baseline_stats <- workflow(
  survey = ech_implantacion,
  svymean(~tasa_actividad, na.rm = TRUE),
  estimation_type = "baseline"
)

# Comparar con follow-up
followup_1 <- get_follow_up(panel_ech, index = 1)[[1]]

# Análisis de cambios desde implantación
panel_comparison <- list(
  implantacion = ech_implantacion,
  seguimiento = followup_1
)
} # }
```
