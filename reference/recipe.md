# Crear receta de transformación de datos de encuesta

Esta función crea un objeto Recipe que encapsula una secuencia de
transformaciones de datos que pueden ser aplicadas a encuestas de manera
reproducible. Las recetas permiten documentar, compartir y reutilizar
workflows de procesamiento de datos.

## Usage

``` r
recipe(...)
```

## Arguments

- ...:

  A list with the following metadata: name, user, svy, description

## Value

Objeto `Recipe` que contiene:

- Metadata completa de la receta

- Lista de steps de transformación

- Información de dependencias

- Configuración de motor por defecto

A Recipe object

## Details

Las recetas son fundamentales para:

- Reproducibilidad: Garantizar que las transformaciones se apliquen
  consistentemente

- Documentación: Mantener registro de qué transformaciones se realizan y
  por qué

- Colaboración: Compartir workflows entre usuarios y equipos

- Versionado: Mantener diferentes versiones de procesamiento para
  distintas ediciones

- Automatización: Aplicar transformaciones complejas automáticamente

Los steps incluidos en la receta pueden ser cualquier combinación de
`step_compute`, `step_recode`, u otros steps de transformación.

Las recetas se pueden guardar con
[`save_recipe()`](https://metasurveyr.github.io/metasurvey/reference/save_recipe.md),
cargar con
[`read_recipe()`](https://metasurveyr.github.io/metasurvey/reference/read_recipe.md),
y aplicar automáticamente con
[`bake_recipes()`](https://metasurveyr.github.io/metasurvey/reference/bake_recipes.md).

## See also

[`Recipe`](https://metasurveyr.github.io/metasurvey/reference/Recipe-class.md)
para la definición de la clase
[`save_recipe`](https://metasurveyr.github.io/metasurvey/reference/save_recipe.md)
para guardar recetas
[`read_recipe`](https://metasurveyr.github.io/metasurvey/reference/read_recipe.md)
para cargar recetas
[`get_recipe`](https://metasurveyr.github.io/metasurvey/reference/get_recipe.md)
para obtener recetas del repositorio
[`bake_recipes`](https://metasurveyr.github.io/metasurvey/reference/bake_recipes.md)
para aplicar recetas a datos

## Examples

``` r
# Basic recipe without steps
r <- recipe(
  name = "Basic ECH Indicators",
  user = "Analyst",
  svy = survey_empty(type = "ech", edition = "2023"),
  description = "Basic labor indicators for ECH 2023"
)
r
#> 
#> ── Recipe: Basic ECH Indicators ──
#> Author:  Analyst
#> Survey:  ech / 2023
#> Version: 1.0.0
#> Description: Basic labor indicators for ECH 2023
#> Certification: community
#> 

if (FALSE) { # \dontrun{
# Recipe with steps
r2 <- recipe(
  name = "Labor Market ECH",
  user = "Labor Team",
  svy = survey_empty(type = "ech", edition = "2023"),
  description = "Full labor market analysis",
  step_recode(
    labor_status,
    POBPCOAC == 2 ~ "Employed",
    POBPCOAC %in% 3:5 ~ "Unemployed",
    .default = "Other"
  ),
  step_compute(activity_rate = active / total * 100)
)
} # }
```
