# Extraer encuestas por periodicidad de panel rotativo

Esta función extrae subconjuntos de encuestas de un objeto
RotativePanelSurvey basándose en criterios temporales específicos.
Permite obtener encuestas para diferentes tipos de análisis (mensual,
trimestral, anual) respetando la estructura temporal del panel rotativo.

## Usage

``` r
extract_surveys(
  RotativePanelSurvey,
  index = NULL,
  monthly = NULL,
  annual = NULL,
  quarterly = NULL,
  biannual = NULL,
  use.parallel = FALSE
)
```

## Arguments

- RotativePanelSurvey:

  Objeto `RotativePanelSurvey` que contiene las encuestas del panel
  rotativo organizadas temporalmente

- index:

  Vector de enteros que especifica índices específicos de encuestas a
  extraer. Si es un solo valor, devuelve esa encuesta; si es un vector,
  devuelve una lista

- monthly:

  Vector de enteros que especifica qué meses extraer para análisis
  mensual (1-12)

- annual:

  Vector de enteros que especifica qué años extraer para análisis anual

- quarterly:

  Vector de enteros que especifica qué trimestres extraer para análisis
  trimestral (1-4)

- biannual:

  Vector de enteros que especifica qué semestres extraer para análisis
  semestral (1-2)

- use.parallel:

  Lógico que indica si usar procesamiento en paralelo para operaciones
  intensivas. Por defecto FALSE

## Value

Lista de objetos `Survey` que corresponden a los criterios
especificados, o un solo objeto `Survey` si se especifica un índice
único

## Details

Esta función es esencial para trabajar con paneles rotativos porque:

- Facilita análisis por periodicidad: Permite extraer datos para
  diferentes tipos de estimaciones temporales

- Mantiene estructura temporal: Respeta las relaciones temporales entre
  las diferentes ondas del panel

- Optimiza memoria: Solo carga las encuestas necesarias para el análisis

- Facilita comparaciones: Permite extraer períodos específicos para
  análisis comparativos

- Soporta paralelización: Para operaciones con grandes volúmenes de
  datos

Los criterios de extracción se interpretan según la frecuencia de la
encuesta:

- Para ECH mensual: monthly=c(1,3,6) extrae enero, marzo y junio

- Para análisis anual: annual=1 típicamente extrae el primer año
  disponible

- Para análisis trimestral: quarterly=c(1,4) extrae Q1 y Q4

Si no se especifica ningún criterio, la función devuelve la encuesta de
implantación con una advertencia.

## See also

[`load_panel_survey`](https://metasurveyr.github.io/metasurvey/reference/load_panel_survey.md)
para cargar paneles rotativos
[`get_implantation`](https://metasurveyr.github.io/metasurvey/reference/get_implantation.md)
para obtener datos de implantación
[`get_follow_up`](https://metasurveyr.github.io/metasurvey/reference/get_follow_up.md)
para obtener datos de seguimiento
[`workflow`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
para usar las encuestas extraídas en análisis

## Examples

``` r
if (FALSE) { # \dontrun{
# Cargar panel rotativo
panel_ech <- load_panel_survey(
  path = "ech_panel_2023.dta",
  svy_type = "ech_panel",
  svy_edition = "2023"
)

# Extraer encuestas mensuales específicas
ech_trimestre1 <- extract_surveys(
  panel_ech,
  monthly = c(1, 2, 3) # Enero, febrero, marzo
)

# Extraer por índice
ech_primera <- extract_surveys(panel_ech, index = 1)
ech_varias <- extract_surveys(panel_ech, index = c(1, 3, 6))

# Análisis trimestral
ech_Q1_Q4 <- extract_surveys(
  panel_ech,
  quarterly = c(1, 4)
)

# Para análisis anual (típicamente todas las encuestas del año)
ech_anual <- extract_surveys(
  panel_ech,
  annual = 1
)

# Con procesamiento paralelo para grandes volúmenes
ech_completo <- extract_surveys(
  panel_ech,
  monthly = 1:12,
  use.parallel = TRUE
)

# Usar en workflow
resultados <- workflow(
  survey = extract_surveys(panel_ech, quarterly = c(1, 2)),
  svymean(~desocupado, na.rm = TRUE),
  estimation_type = "quarterly"
)
} # }
```
