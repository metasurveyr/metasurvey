# Introducción a metasurvey

## ¿Qué es metasurvey?

**metasurvey** es un framework para procesar datos de encuestas
complejas en R. Combina el paquete `survey` ([Lumley
2004](#ref-lumley2004)) con un sistema de:

- **Pasos**: Transformaciones aplicadas a los datos
- **Recetas**: Colecciones reutilizables de pasos para
  reproducibilidad  
- **Workflows**: Estimaciones estadísticas con cálculo automático de
  varianza

El paquete maneja diseños de encuestas complejos incluyendo
estratificación, conglomerados y ponderadores replicados, con validación
automática de dependencias y optimización de expresiones.

### Características Principales

- **Objetos de encuesta tipo-seguro** que encapsulan datos, ponderadores
  y diseño muestral
- **Evaluación lazy** de transformaciones para mejor rendimiento
- **Rastreo automático de dependencias** previene errores antes de la
  ejecución
- **Workflows reproducibles** mediante recetas compartibles
- **Salida ordenada** con errores estándar y coeficientes de variación

## Instalación

``` r
# Instalar desde GitHub
# devtools::install_github("metaSurveyR/metasurvey")

library(metasurvey)
library(data.table)
```

## Crear un Objeto Survey

Un objeto `Survey` agrupa los microdatos con metadatos sobre
ponderadores, edición y tipo de encuesta. Comenzaremos con un ejemplo
simple usando datos simulados que imitan la estructura de la Encuesta
Continua de Hogares (ECH) de Uruguay.

La ECH es una encuesta de hogares con panel rotativo realizada por el
Instituto Nacional de Estadística (INE) de Uruguay. Variables clave
incluyen:

- **e27**: Edad en años
- **e26**: Sexo (1 = Hombre, 2 = Mujer)
- **e51**: Nivel educativo (escala codificada 1-14)
- **POBPCOAC**: Condición de actividad
  - 2 = Ocupado
  - 3-5 = Desocupado
  - 6-8 = Inactivo
- **ht11**: Ingreso del hogar
- **pesoano**: Factor de expansión anual (ponderador)

``` r
library(metasurvey)
library(data.table)

set.seed(42)
n <- 200

# Simular microdatos tipo ECH
dt <- data.table(
  numero = 1:n, # ID de hogar
  e27 = sample(18:80, n, replace = TRUE), # Edad
  e26 = sample(c(1, 2), n, replace = TRUE), # Sexo
  ht11 = round(runif(n, 5000, 80000)), # Ingreso hogar (pesos)
  POBPCOAC = sample(c(2, 3, 4, 5, 6), n,
    replace = TRUE,
    prob = c(0.55, 0.03, 0.02, 0.03, 0.37)
  ), # Condición actividad
  dpto = sample(1:19, n, replace = TRUE), # Departamento
  pesoano = round(runif(n, 0.5, 3.0), 4) # Ponderador anual
)

# Crear objeto Survey
encuesta <- Survey$new(
  data    = dt,
  edition = "2023",
  type    = "ech",
  psu     = NULL, # Sin PSU para muestra aleatoria simple
  engine  = "data.table", # Manipulación rápida de datos
  weight  = add_weight(annual = "pesoano")
)
```

La función
[`add_weight()`](https://metasurveyr.github.io/metasurvey/reference/add_weight.md)
mapea etiquetas de periodicidad (ej., “annual”, “monthly”) a nombres de
columnas de ponderadores en los datos. Esto permite que la misma receta
funcione en diferentes ediciones de la encuesta.

Puede inspeccionar los datos en cualquier momento:

``` r
head(get_data(encuesta), 3)
#>    numero   e27   e26  ht11 POBPCOAC  dpto pesoano
#>     <int> <int> <num> <num>    <num> <int>   <num>
#> 1:      1    66     2 36408        2     5  1.9904
#> 2:      2    54     1 70945        2    10  1.2100
#> 3:      3    18     1 13099        2    12  0.6380
```

## Trabajando con Pasos

Los pasos son **lazy por defecto**: se registran pero no se aplican
hasta que llama a
[`bake_steps()`](https://metasurveyr.github.io/metasurvey/reference/bake_steps.md).
Esto le permite:

1.  Construir un pipeline completo de transformaciones
2.  Inspeccionar y validar pasos antes de ejecutarlos
3.  Reutilizar secuencias de pasos como recetas
4.  Asegurar que todas las dependencias existan antes del procesamiento

### Calcular Nuevas Variables

Use
[`step_compute()`](https://metasurveyr.github.io/metasurvey/reference/step_compute.md)
para crear variables derivadas. El paquete automáticamente:

- Valida que las variables de entrada existan
- Detecta dependencias entre pasos
- Optimiza expresiones para mejor rendimiento

``` r
encuesta <- step_compute(encuesta,
  # Convertir ingreso a miles para mejor lectura
  ht11_miles = ht11 / 1000,

  # Crear indicador de empleo siguiendo definiciones OIT
  ocupado = ifelse(POBPCOAC == 2, 1, 0),

  # Población en edad de trabajar (14+ años, estándar ECH)
  edad_trabajar = ifelse(e27 >= 14, 1, 0),
  comment = "Indicadores básicos de fuerza laboral"
)
```

Puede agrupar cálculos usando el parámetro `.by` (similar a
`data.table`):

``` r
# Calcular ingreso promedio del hogar por departamento
encuesta <- step_compute(encuesta,
  ingreso_medio_depto = mean(ht11, na.rm = TRUE),
  .by = "dpto",
  comment = "Promedios de ingreso a nivel departamental"
)
```

### Recodificar en Categorías

Use
[`step_recode()`](https://metasurveyr.github.io/metasurvey/reference/step_recode.md)
para crear variables categóricas a partir de condiciones. Las
condiciones se evalúan **de arriba hacia abajo**, y la primera condición
que coincide determina el valor.

``` r
# Recodificar condición de actividad (POBPCOAC) en categorías significativas
encuesta <- step_recode(encuesta, condicion_actividad,
  POBPCOAC == 2 ~ "Ocupado",
  POBPCOAC %in% 3:5 ~ "Desocupado",
  POBPCOAC %in% 6:8 ~ "Inactivo",
  .default = "Sin clasificar",
  comment = "Condición de actividad - estándar OIT"
)

# Crear grupos etarios estándar para estadísticas laborales
encuesta <- step_recode(encuesta, grupo_edad,
  e27 < 25 ~ "Jóvenes (14-24)",
  e27 < 45 ~ "Adultos (25-44)",
  e27 < 65 ~ "Maduros (45-64)",
  .default = "Mayores (65+)",
  .to_factor = TRUE, # Convertir a factor
  ordered = TRUE, # Factor ordenado
  comment = "Grupos etarios para análisis laboral"
)

# Recodificar sexo en etiquetas descriptivas
encuesta <- step_recode(encuesta, genero,
  e26 == 1 ~ "Hombre",
  e26 == 2 ~ "Mujer",
  .default = "Otro",
  comment = "Clasificación por género"
)
```

### Renombrar y Eliminar Variables

Renombrar variables para mayor claridad o consistencia:

``` r
encuesta <- step_rename(encuesta,
  edad = e27, # Renombrar e27 a edad
  codigo_sexo = e26 # Mantener original como codigo_sexo
)
```

Eliminar variables que ya no son necesarias:

``` r
# Eliminar cálculos intermedios
encuesta <- step_remove(encuesta, edad_trabajar, ingreso_medio_depto)
```

### Unir Datos Externos

Use
[`step_join()`](https://metasurveyr.github.io/metasurvey/reference/step_join.md)
para combinar datos de referencia externos. Esto es útil para agregar:

- Nombres geográficos y clasificaciones
- Tipos de cambio o deflactores
- Referencias externas o metas

``` r
# Nombres de departamentos y regiones
datos_departamentos <- data.table(
  dpto = 1:19,
  nombre_dpto = c(
    "Montevideo", "Artigas", "Canelones", "Cerro Largo",
    "Colonia", "Durazno", "Flores", "Florida", "Lavalleja",
    "Maldonado", "Paysandú", "Río Negro", "Rivera", "Rocha",
    "Salto", "San José", "Soriano", "Tacuarembó", "Treinta y Tres"
  ),
  region = c("Montevideo", rep("Interior", 18))
)

encuesta <- step_join(encuesta,
  datos_departamentos,
  by = "dpto",
  type = "left",
  comment = "Agregar nombres de departamentos y regiones"
)
```

## Ejecutando Transformaciones

### Hornear (Bake) los Pasos

Llame a
[`bake_steps()`](https://metasurveyr.github.io/metasurvey/reference/bake_steps.md)
para ejecutar todas las transformaciones pendientes:

``` r
encuesta <- bake_steps(encuesta)
head(get_data(encuesta), 3)
```

El historial de pasos se preserva para documentación y reproducibilidad:

``` r
pasos <- get_steps(encuesta)
length(pasos) # Número de pasos de transformación

# Ver detalles de un paso
cat("Paso 1:", pasos[[1]]$name, "\n")
cat("Comentario:", pasos[[1]]$comments, "\n")
```

### Visualizar el Pipeline

Puede visualizar su pipeline de transformaciones como un grafo dirigido:

``` r
view_graph(encuesta, init_step = "Cargar ECH 2023")
```

Esto crea un grafo interactivo mostrando:

- Fuentes de datos y uniones
- Pasos de transformación
- Dependencias de variables
- Comentarios y metadatos

## Ejecutando Estimaciones Estadísticas

Después de preparar los datos, use
[`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
para calcular estimaciones. La función envuelve los estimadores del
paquete `survey` ([Lumley 2004](#ref-lumley2004)) y devuelve resultados
ordenados con errores estándar y coeficientes de variación.

**Importante:** Pase el objeto de encuesta dentro de un
[`list()`](https://rdrr.io/r/base/list.html).

### Estimaciones Básicas

``` r
# Estimar ingreso promedio del hogar
resultado <- workflow(
  list(encuesta),
  survey::svymean(~ht11, na.rm = TRUE),
  estimation_type = "annual"
)

resultado
#>                     stat    value       se       cv confint_lower confint_upper
#>                   <char>    <num>    <num>    <num>         <num>         <num>
#> 1: survey::svymean: ht11 39774.23 1622.948 0.040804      36593.31      42955.15
```

La salida incluye:

- `estimate`: Estimación puntual
- `se`: Error estándar
- `cv`: Coeficiente de variación
- `var_name`: Nombre de la variable
- `level`: Nivel del factor (para variables categóricas)

### Múltiples Estimaciones

Calcule varias estadísticas en una sola llamada:

``` r
resultados <- workflow(
  list(encuesta),
  survey::svymean(~ht11, na.rm = TRUE), # Ingreso promedio
  survey::svytotal(~ocupado, na.rm = TRUE), # Total ocupados
  survey::svymean(~condicion_actividad, na.rm = TRUE), # Distribución empleo
  estimation_type = "annual"
)

resultados
#>                                              stat        value           se
#>                                            <char>        <num>        <num>
#> 1:                          survey::svymean: ht11 3.977423e+04 1.622948e+03
#> 2:                      survey::svytotal: ocupado 2.020862e+02 1.351243e+01
#> 3: survey::svymean: condicion_actividadDesocupado 5.853947e-02 1.680764e-02
#> 4:   survey::svymean: condicion_actividadInactivo 3.748725e-01 3.721431e-02
#> 5:    survey::svymean: condicion_actividadOcupado 5.665880e-01 3.788473e-02
#>            cv confint_lower confint_upper
#>         <num>         <num>         <num>
#> 1: 0.04080400  3.659331e+04  4.295515e+04
#> 2: 0.06686469  1.756023e+02  2.285701e+02
#> 3: 0.28711634  2.559710e-02  9.148183e-02
#> 4: 0.09927192  3.019338e-01  4.478112e-01
#> 5: 0.06686469  4.923353e-01  6.408407e-01
```

### Estimación por Dominios

Calcule estimaciones para subpoblaciones usando
[`survey::svyby()`](https://rdrr.io/pkg/survey/man/svyby.html):

``` r
# Ingreso promedio por género
ingreso_por_genero <- workflow(
  list(encuesta),
  survey::svyby(~ht11, ~genero, survey::svymean, na.rm = TRUE),
  estimation_type = "annual"
)

ingreso_por_genero
#>      stat     value    se         cv confint_lower confint_upper
#>    <char>     <num> <num>      <num>         <num>         <num>
#> 1: Hombre        NA    NA 0.05677403      37150.72      46453.82
#> 2:  Mujer        NA    NA 0.05715728      33165.86      41534.22
#> 3: Hombre 41802.272    NA 0.05677403            NA            NA
#> 4:  Mujer 37350.039    NA 0.05715728            NA            NA
#> 5: Hombre  2373.283    NA 0.05677403            NA            NA
#> 6:  Mujer  2134.827    NA 0.05715728            NA            NA
```

## Evaluación de Calidad

El **coeficiente de variación (CV)** mide la confiabilidad de la
estimación. Un CV más bajo indica estimaciones más precisas. Siguiendo
las pautas del INE Uruguay ([Instituto Nacional de Estadística (INE)
2023](#ref-ine2023)):

| Rango CV | Categoría de Calidad | Recomendación                           |
|----------|----------------------|-----------------------------------------|
| \< 5%    | Excelente            | Usar sin restricciones                  |
| 5%–10%   | Muy bueno            | Usar con confianza                      |
| 10%–15%  | Bueno                | Usar para la mayoría de propósitos      |
| 15%–25%  | Aceptable            | Usar con precaución, notar limitaciones |
| 25%–35%  | Pobre                | Usar solo para tendencias generales     |
| ≥ 35%    | No confiable         | No publicar                             |

Use
[`evaluate_cv()`](https://metasurveyr.github.io/metasurvey/reference/evaluate_cv.md)
para clasificar la calidad de la estimación:

``` r
# Verificar calidad de la estimación de ingreso promedio
cv_porcentaje <- resultados$cv[1] * 100
calidad <- evaluate_cv(cv_porcentaje)

cat("CV:", round(cv_porcentaje, 2), "%\n")
#> CV: 4.08 %
cat("Calidad:", calidad, "\n")
#> Calidad: Excelente
```

Para estadísticas oficiales, siempre reporte:

1.  Estimación puntual
2.  Error estándar o intervalo de confianza
3.  Coeficiente de variación
4.  Clasificación de calidad
5.  Tamaño de muestra

## Trabajando con Recetas

Las recetas empaquetan pasos de transformación para **reproducibilidad**
y **compartir**. Una vez que haya desarrollado un pipeline funcional,
conviértalo en una receta que pueda ser:

- Aplicada a diferentes ediciones de la encuesta
- Compartida con colaboradores
- Publicada para transparencia
- Versionada y documentada

### Crear una Receta

Cree una receta a partir de sus pasos desarrollados:

``` r
# Convertir pasos actuales a una receta
receta_laboral <- steps_to_recipe(
  name = "Indicadores Laborales ECH",
  user = "Instituto Nacional de Estadística",
  svy = encuesta,
  description = paste(
    "Indicadores laborales estándar siguiendo definiciones OIT.",
    "Crea condición de actividad, grupos etarios y género."
  ),
  steps = get_steps(encuesta),
  topic = "estadisticas_laborales"
)

class(receta_laboral)
#> [1] "Recipe" "R6"
receta_laboral
#> 
#> ── Recipe: Indicadores Laborales ECH ──
#> Author:  Instituto Nacional de Estadística
#> Survey:  ech / 2023
#> Version: 1.0.0
#> Topic:   estadisticas_laborales
#> Description: Indicadores laborales estándar siguiendo definiciones OIT. Crea condición de actividad, grupos etarios y género.
#> Certification: community
#> 
#> ── Requires (5 variables) ──
#>   ht11, POBPCOAC, e27, e26, dpto
#> 
#> ── Pipeline (8 steps) ──
#>   1. [compute] -> ht11_miles, ocupado, edad_trabajar  "Indicadores básicos de fuerza laboral"
#>   2. [compute] -> ingreso_medio_depto  "Promedios de ingreso a nivel departamental"
#>   3. [recode] -> condicion_actividad  "Condición de actividad - estándar OIT"
#>   4. [recode] -> grupo_edad  "Grupos etarios para análisis laboral"
#>   5. [recode] -> genero  "Clasificación por género"
#>   6. [step_rename] -> mapping  "Rename variables"
#>   7. [step_remove] -> (no output)  "Remove variables"
#>   8. [step_join] -> (no output)  "Agregar nombres de departamentos y regiones"
#> 
#> ── Produces (8 variables) ──
#>   condicion_actividad [categorical], grupo_edad [categorical], genero [categorical], mapping [inherited], ht11_miles [numeric], ocupado [numeric], edad_trabajar [numeric], ingreso_medio_depto [numeric]
```

O defina una receta desde cero:

``` r
receta_minima <- recipe(
  name = "Demografía Básica",
  user = "analista",
  svy = survey_empty(type = "ech", edition = "2023"),
  description = "Recodificación demográfica básica",
  topic = "demografia",

  # Definir pasos en línea
  step_recode(
    genero,
    e26 == 1 ~ "Hombre",
    e26 == 2 ~ "Mujer",
    .default = "Otro"
  ),
  step_recode(
    grupo_edad,
    e27 < 18 ~ "Menor",
    e27 < 65 ~ "Adulto",
    .default = "Mayor"
  )
)
```

### Aplicar Recetas a Nuevos Datos

Una vez publicada, cualquier persona puede buscar una receta y
aplicarla:

``` r
# Buscar recetas existentes
encontradas <- search_recipes("labor")

# Aplicar a una nueva encuesta
nueva_encuesta <- add_recipe(nueva_encuesta, encontradas[[1]])
procesada <- bake_recipes(nueva_encuesta)
```

### Documentación de Recetas

Las recetas documentan automáticamente sus transformaciones:

``` r
doc <- receta_laboral$doc()
names(doc)
#> [1] "meta"             "input_variables"  "output_variables" "pipeline"

# Variables de entrada requeridas
doc$input_variables
#> [1] "ht11"     "POBPCOAC" "e27"      "e26"      "dpto"

# Variables de salida creadas
doc$output_variables
#> [1] "ht11_miles"          "ocupado"             "edad_trabajar"      
#> [4] "ingreso_medio_depto" "condicion_actividad" "grupo_edad"         
#> [7] "genero"              "mapping"
```

## Configuración del Paquete

metasurvey proporciona configuraciones globales que puede ajustar:

``` r
# Verificar configuración actual de procesamiento lazy
lazy_default() # TRUE = pasos registrados pero no ejecutados inmediatamente
#> [1] TRUE

# Verificar comportamiento de copia de datos
use_copy_default() # TRUE = operar en copias (más seguro pero más lento)
#> [1] TRUE

# Ver motores de computación disponibles
show_engines() # "data.table", "dplyr", etc.
#> [1] "data.table" "tidyverse"  "dplyr"
```

Cambiar configuraciones para su sesión:

``` r
# Deshabilitar evaluación lazy (ejecutar pasos inmediatamente)
set_lazy(FALSE)

# Modificar in-place (más rápido, pero modifica datos originales)
set_use_copy(FALSE)

# Restablecer a valores predeterminados
set_lazy(TRUE)
set_use_copy(TRUE)
```

## Próximos Pasos

Ahora que comprende los fundamentos, explore estas guías:

- **[Recetas](https://metasurveyr.github.io/metasurvey/articles/recipes.md)**
  – Registro de recetas, certificación y descubrimiento
- **[Workflows de
  Estimación](https://metasurveyr.github.io/metasurvey/articles/workflows-and-estimation.md)**
  –
  [`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md),
  `RecipeWorkflow` y estimaciones publicables
- **[Diseños y
  Validación](https://metasurveyr.github.io/metasurvey/articles/complex-designs.md)**
  – Estratificación, conglomerados, ponderadores replicados, validación
- **[Paneles Rotativos y
  PoolSurvey](https://metasurveyr.github.io/metasurvey/articles/panel-analysis.md)**
  – Análisis longitudinal con `RotativePanelSurvey` y `PoolSurvey`
- **[Estudio de Caso
  ECH](https://metasurveyr.github.io/metasurvey/articles/ech-case-study.md)**
  – Análisis laboral completo con comparación STATA

## Referencias

- Lumley, T. (2004). *Analysis of Complex Survey Samples*. Journal of
  Statistical Software, 9(1), 1-19
- Instituto Nacional de Estadística (INE), Uruguay. (2023). *Encuesta
  Continua de Hogares: Metodología y Documentación*.
  <https://www.ine.gub.uy/encuesta-continua-de-hogares1>
- Organización Internacional del Trabajo (OIT). (2013). *Resolución
  sobre las estadísticas del trabajo, el empleo y la subutilización de
  la fuerza de trabajo*. 19° Conferencia Internacional de Estadísticos
  del Trabajo.
- INE Uruguay. Diccionario de Datos ECH.
  <https://www4.ine.gub.uy/Anda5/index.php/catalog/775/data-dictionary>

Instituto Nacional de Estadística (INE). 2023. *Encuesta Continua de
Hogares: Metodología y Documentación*. Instituto Nacional de Estadística
(INE), Uruguay. <https://www.ine.gub.uy/encuesta-continua-de-hogares1>.

Lumley, Thomas. 2004. “Analysis of Complex Survey Samples.” *Journal of
Statistical Software* 9 (1): 1–19.
<https://doi.org/10.18637/jss.v009.i08>.
