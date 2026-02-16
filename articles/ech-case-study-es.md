# Estudio de caso ECH: De STATA a R (ES)

## El problema de la compatibilización de la ECH

La *Encuesta Continua de Hogares* (ECH) de Uruguay es publicada
anualmente por el Instituto Nacional de Estadística (INE). A lo largo de
los años, el INE ha modificado nombres de variables, definiciones del
libro de códigos y la estructura de módulos entre ediciones. Quienes
investigan con datos de la ECH deben *compatibilizar* variables —es
decir, mapear las distintas convenciones de nombres a un esquema común—
antes de que cualquier análisis inter-anual sea posible.

La compatibilización de la ECH ha sido un trabajo histórico del
Instituto de Economía (IECON) de la Universidad de la República. El
dataset resultante tiene DOI: <http://doi.org/10.47426/ECH.INE>
(Instituto de Economía, 2020). metasurvey busca complementar y facilitar
este tipo de trabajo ofreciendo herramientas reproducibles en R.

En la academia uruguaya, este trabajo se ha realizado en STATA durante
más de 30 años. Un pipeline de compatibilización típico consiste en
aproximadamente **8 archivos .do por año**, cubriendo:

1.  `2_correc_datos.do` – Carga de datos crudos, unión de archivos de
    personas y hogares, corrección de nombres de variables
2.  `3_compatibilizacion_mod_1_4.do` – Armonización de módulos
    demográficos, de salud, educación y trabajo
3.  `4_ingreso_ht11.do` – Construcción del ingreso del hogar (`ht11`)
4.  `5_descomp_fuentes.do` – Descomposición del ingreso por fuente
5.  `6_ingreso_ht11_sss.do` – Ajustes de seguridad social
6.  `7_check_ingr.do` – Validación de variables de ingreso
7.  `8_arregla_base_comp.do` – Preparación final del dataset
8.  `9_labels.do` – Aplicación de etiquetas de valores

Multiplicado por más de 30 años, eso significa más de **240 scripts de
STATA** haciendo esencialmente la misma tarea: mapear variables crudas
de la ECH a un esquema común.

**metasurvey resuelve esto con recipes.** En lugar de mantener cientos
de archivos `.do`, se escribe un único recipe que codifica la lógica de
transformación y puede aplicarse a cualquier edición de la ECH.

## STATA vs metasurvey: Comparación lado a lado

A continuación se presenta un fragmento de un script de
compatibilización típico en STATA para sexo, edad y parentesco:

``` stata
* STATA: Typical ECH compatibility script

* sexo
g bc_pe2 = e26

* edad
g bc_pe3 = e27

* parentesco
g bc_pe4 = -9
  replace bc_pe4 = 1 if e31 == 1
  replace bc_pe4 = 2 if e31 == 2
  replace bc_pe4 = 3 if e31 == 3 | e31 == 4 | e31 == 5
  replace bc_pe4 = 4 if e31 == 7 | e31 == 8
  replace bc_pe4 = 5 if e31 == 6 | e31 == 9 | e31 == 10 | e31 == 11 | e31 == 12
  replace bc_pe4 = 6 if e31 == 13
  replace bc_pe4 = 7 if e31 == 14
```

El equivalente en metasurvey:

``` r
svy <- step_rename(svy, sex = e26, age = e27)

svy <- step_recode(svy, relationship,
  e31 == 1                ~ "Head",
  e31 == 2                ~ "Spouse",
  e31 %in% 3:5            ~ "Child",
  e31 %in% c(7, 8)        ~ "Parent",
  e31 %in% c(6, 9:12)     ~ "Other relative",
  e31 == 13               ~ "Domestic service",
  e31 == 14               ~ "Non-relative",
  .default = NA_character_,
  comment = "Relationship to head of household"
)
```

Las diferencias clave:

| Aspecto          | Archivo `.do` de STATA                 | Recipe de metasurvey               |
|------------------|----------------------------------------|------------------------------------|
| Formato          | Script plano con rutas hardcodeadas    | JSON portable con metadatos        |
| Validación       | Verificaciones manuales con `assert`   | Método automático `validate()`     |
| Documentación    | Comentarios en el código               | Método auto-generado `doc()`       |
| Compartir        | Copiar archivos por email/servidor     | Registry con búsqueda y versionado |
| Reproducibilidad | Depende de rutas de archivos y entorno | Autocontenido, cualquier máquina   |
| Inter-ediciones  | Duplicar script por año                | Un recipe, múltiples ediciones     |

## Simulación de datos similares a la ECH

Para esta viñeta simulamos datos que replican la estructura de un
archivo de microdatos real de la ECH. Las variables simuladas siguen las
mismas convenciones de nombres y rangos de valores utilizados por el
INE.

``` r
library(metasurvey)
library(data.table)

set.seed(2023)
n <- 500

ech_sim <- data.table(
  numero = rep(1:100, each = 5),
  nper = rep(1:5, 100),
  e26 = sample(c(1, 2), n, replace = TRUE),
  e27 = sample(0:90, n, replace = TRUE),
  e31 = sample(1:14, n,
    replace = TRUE,
    prob = c(
      0.20, 0.15, 0.25, 0.05, 0.03,
      0.02, 0.03, 0.02, 0.02, 0.01,
      0.01, 0.01, 0.05, 0.15
    )
  ),
  pobpcoac = sample(c(2, 3, 4, 5, 6, 7), n,
    replace = TRUE,
    prob = c(0.50, 0.03, 0.02, 0.02, 0.30, 0.13)
  ),
  e51 = sample(1:14, n, replace = TRUE),
  ht11 = round(runif(n, 5000, 120000)),
  dpto = sample(1:19, n, replace = TRUE),
  pesoano = round(runif(n, 0.5, 3.0), 4)
)

svy <- Survey$new(
  data    = ech_sim,
  edition = "2023",
  type    = "ech",
  psu     = NULL,
  engine  = "data.table",
  weight  = add_weight(annual = "pesoano")
)

head(get_data(svy), 3)
#>    numero  nper   e26   e27   e31 pobpcoac   e51   ht11  dpto pesoano
#>     <int> <int> <num> <int> <int>    <num> <int>  <num> <int>   <num>
#> 1:      1     1     1    60     1        2    14 116378     5  0.6996
#> 2:      1     2     2     5     1        7    10  48407    11  0.7896
#> 3:      1     3     1     0     1        2     7 103278     4  1.5015
```

## Paso 1: Variables demográficas

Recodificar los códigos crudos del INE a nombres legibles y recodificar
variables categóricas:

``` r
# Recode sex from INE codes (e26: 1=Male, 2=Female)
svy <- step_recode(svy, sex,
  e26 == 1 ~ "Male",
  e26 == 2 ~ "Female",
  .default = NA_character_,
  comment = "Sex: 1=Male, 2=Female (INE e26)"
)

# Recode age groups (standard ECH grouping, e27 = age)
svy <- step_recode(svy, age_group,
  e27 < 14 ~ "Child (0-13)",
  e27 < 25 ~ "Youth (14-24)",
  e27 < 45 ~ "Adult (25-44)",
  e27 < 65 ~ "Mature (45-64)",
  .default = "Senior (65+)",
  .to_factor = TRUE,
  ordered = TRUE,
  comment = "Standard age groups for labor statistics"
)
```

## Paso 2: Clasificación de la fuerza laboral

La variable `POBPCOAC` (Población por condición de actividad) es la
clasificación central del estado laboral en la ECH. Códigos del INE:

- 2 = Ocupado
- 3-5 = Desocupado (diversas subcategorías)
- 6-7 = Inactivo

Esto replica el marco estándar de fuerza laboral de la OIT:

``` r
svy <- step_recode(svy, labor_status,
  pobpcoac == 2 ~ "Employed",
  pobpcoac %in% 3:5 ~ "Unemployed",
  pobpcoac %in% 6:7 ~ "Inactive",
  .default = NA_character_,
  comment = "ILO labor force status from POBPCOAC"
)

# Create binary indicators
svy <- step_compute(svy,
  employed = ifelse(pobpcoac == 2, 1L, 0L),
  unemployed = ifelse(pobpcoac %in% 3:5, 1L, 0L),
  active = ifelse(pobpcoac %in% 2:5, 1L, 0L),
  working_age = ifelse(e27 >= 14, 1L, 0L),
  comment = "Labor force binary indicators"
)
```

## Paso 3: Variables de ingreso

Construir indicadores de ingreso siguiendo la metodología estándar
utilizada con datos de la ECH:

``` r
svy <- step_compute(svy,
  income_pc = ht11 / 5,
  income_thousands = ht11 / 1000,
  log_income = log(ht11 + 1),
  comment = "Income transformations"
)
```

## Paso 4: Clasificación geográfica

``` r
dept_names <- data.table(
  dpto = 1:19,
  department = c(
    "Montevideo", "Artigas", "Canelones", "Cerro Largo",
    "Colonia", "Durazno", "Flores", "Florida", "Lavalleja",
    "Maldonado", "Paysandu", "Rio Negro", "Rivera", "Rocha",
    "Salto", "San Jose", "Soriano", "Tacuarembo",
    "Treinta y Tres"
  ),
  region = c("Montevideo", rep("Interior", 18))
)

svy <- step_join(svy,
  dept_names,
  by = "dpto",
  type = "left",
  comment = "Department names and region classification"
)
```

## Construcción del recipe

Convertir todas las transformaciones en un recipe portable:

``` r
ech_recipe <- steps_to_recipe(
  name = "ECH Labor Market Indicators",
  user = "Research Team",
  svy = svy,
  description = paste(
    "Standard labor market indicators for the ECH.",
    "Includes demographic recoding, ILO labor classification,",
    "income transformations, and geographic joins."
  ),
  steps = get_steps(svy),
  topic = "labor"
)

ech_recipe
#> 
#> ── Recipe: ECH Labor Market Indicators ──
#> Author:  Research Team
#> Survey:  ech / 2023
#> Version: 1.0.0
#> Topic:   labor
#> Description: Standard labor market indicators for the ECH. Includes demographic recoding, ILO labor classification, income transformations, and geographic joins.
#> Certification: community
#> 
#> ── Requires (5 variables) ──
#>   e26, e27, pobpcoac, ht11, dpto
#> 
#> ── Pipeline (6 steps) ──
#>   1. [recode] -> sex  "Sex: 1=Male, 2=Female (INE e26)"
#>   2. [recode] -> age_group  "Standard age groups for labor statistics"
#>   3. [recode] -> labor_status  "ILO labor force status from POBPCOAC"
#>   4. [compute] -> employed, unemployed, active, working_age  "Labor force binary indicators"
#>   5. [compute] -> income_pc, income_thousands, log_income  "Income transformations"
#>   6. [step_join] -> (no output)  "Department names and region classification"
#> 
#> ── Produces (10 variables) ──
#>   sex [categorical], age_group [categorical], labor_status [categorical], employed [numeric], unemployed [numeric], active [numeric], working_age [numeric], income_pc [numeric], income_thousands [numeric], log_income [numeric]
```

### Documentación automática

``` r
doc <- ech_recipe$doc()

# What variables does the recipe need?
doc$input_variables
#> [1] "e26"      "e27"      "pobpcoac" "ht11"     "dpto"

# What variables does it create?
doc$output_variables
#>  [1] "sex"              "age_group"        "labor_status"     "employed"        
#>  [5] "unemployed"       "active"           "working_age"      "income_pc"       
#>  [9] "income_thousands" "log_income"
```

### Publicación en el registry

Publicar el recipe para que otras personas puedan descubrirlo y
reutilizarlo:

``` r
# Set up a local registry
set_backend("local", path = tempfile(fileext = ".json"))
publish_recipe(ech_recipe)

# Ahora cualquiera puede recuperarla por ID
r <- get_recipe("ech_labor")
print(r)
#> NULL
```

## Estimación con workflow()

Ahora calculamos indicadores estándar del mercado laboral:

``` r
# Mean household income
result_income <- workflow(
  list(svy),
  survey::svymean(~ht11, na.rm = TRUE),
  estimation_type = "annual"
)

result_income
#>                     stat    value       se         cv confint_lower
#>                   <char>    <num>    <num>      <num>         <num>
#> 1: survey::svymean: ht11 60742.91 1631.918 0.02686598      57544.41
#>    confint_upper
#>            <num>
#> 1:      63941.41
```

``` r
# Employment rate (proportion employed among total population)
result_employment <- workflow(
  list(svy),
  survey::svymean(~employed, na.rm = TRUE),
  estimation_type = "annual"
)

result_employment
#>                         stat     value         se         cv confint_lower
#>                       <char>     <num>      <num>      <num>         <num>
#> 1: survey::svymean: employed 0.5778099 0.02386869 0.04130891     0.5310281
#>    confint_upper
#>            <num>
#> 1:     0.6245916
```

### Estimación por dominio

Calcular estimaciones por subpoblación:

``` r
# Mean income by region
income_region <- workflow(
  list(svy),
  survey::svyby(~ht11, ~region, survey::svymean, na.rm = TRUE),
  estimation_type = "annual"
)

income_region
#>          stat     value    se        cv confint_lower confint_upper
#>        <char>     <num> <num>     <num>         <num>         <num>
#> 1:   Interior        NA    NA 0.0275355      57568.21      64136.44
#> 2: Montevideo        NA    NA 0.1223059      44632.89      72778.09
#> 3:   Interior 60852.322    NA 0.0275355            NA            NA
#> 4: Montevideo 58705.489    NA 0.1223059            NA            NA
#> 5:   Interior  1675.599    NA 0.0275355            NA            NA
#> 6: Montevideo  7180.028    NA 0.1223059            NA            NA
```

``` r
# Employment by sex
employment_sex <- workflow(
  list(svy),
  survey::svyby(~employed, ~sex, survey::svymean, na.rm = TRUE),
  estimation_type = "annual"
)

employment_sex
#>      stat      value    se         cv confint_lower confint_upper
#>    <char>      <num> <num>      <num>         <num>         <num>
#> 1: Female         NA    NA 0.06019825     0.4912161     0.6226354
#> 2:   Male         NA    NA 0.05644886     0.5343696     0.6673221
#> 3: Female 0.55692572    NA 0.06019825            NA            NA
#> 4:   Male 0.60084583    NA 0.05644886            NA            NA
#> 5: Female 0.03352595    NA 0.06019825            NA            NA
#> 6:   Male 0.03391706    NA 0.05644886            NA            NA
```

### Evaluación de calidad

``` r
results_all <- workflow(
  list(svy),
  survey::svymean(~ht11, na.rm = TRUE),
  survey::svymean(~employed, na.rm = TRUE),
  estimation_type = "annual"
)

for (i in seq_len(nrow(results_all))) {
  cv_pct <- results_all$cv[i] * 100
  cat(
    results_all$stat[i], ":",
    round(cv_pct, 1), "% CV -",
    evaluate_cv(cv_pct), "\n"
  )
}
#> survey::svymean: ht11 : 2.7 % CV - Excelente 
#> survey::svymean: employed : 4.1 % CV - Excelente
```

## Reproducibilidad: mismo recipe, distinta edición

El poder de los recipes radica en aplicarlos sin modificaciones a datos
nuevos. Aquí simulamos una “edición 2024” con la misma estructura:

``` r
set.seed(2024)
ech_2024 <- data.table(
  numero = rep(1:100, each = 5),
  nper = rep(1:5, 100),
  e26 = sample(c(1, 2), n, replace = TRUE),
  e27 = sample(0:90, n, replace = TRUE),
  e31 = sample(1:14, n,
    replace = TRUE,
    prob = c(
      0.20, 0.15, 0.25, 0.05, 0.03,
      0.02, 0.03, 0.02, 0.02, 0.01,
      0.01, 0.01, 0.05, 0.15
    )
  ),
  pobpcoac = sample(c(2, 3, 4, 5, 6, 7), n,
    replace = TRUE,
    prob = c(0.50, 0.03, 0.02, 0.02, 0.30, 0.13)
  ),
  e51 = sample(1:14, n, replace = TRUE),
  ht11 = round(runif(n, 5500, 130000)),
  dpto = sample(1:19, n, replace = TRUE),
  pesoano = round(runif(n, 0.5, 3.0), 4)
)

svy_2024 <- Survey$new(
  data = ech_2024, edition = "2024", type = "ech",
  psu = NULL, engine = "data.table",
  weight = add_weight(annual = "pesoano")
)

# Apply the same recipe
svy_2024 <- add_recipe(svy_2024, ech_recipe)
svy_2024 <- bake_recipes(svy_2024)

# Estimate
result_2024 <- workflow(
  list(svy_2024),
  survey::svymean(~ht11, na.rm = TRUE),
  survey::svymean(~employed, na.rm = TRUE),
  estimation_type = "annual"
)

result_2024
#>                         stat        value           se         cv confint_lower
#>                       <char>        <num>        <num>      <num>         <num>
#> 1:     survey::svymean: ht11 7.177680e+04 1.702000e+03 0.02371240  6.844094e+04
#> 2: survey::svymean: employed 5.234533e-01 2.410369e-02 0.04604745  4.762109e-01
#>    confint_upper
#>            <num>
#> 1:  7.511266e+04
#> 2:  5.706957e-01
```

Mismo recipe, datos diferentes, metodología consistente.

## Para usuarios de STATA: referencia rápida

Si estás haciendo la transición de STATA a R para análisis de encuestas,
aquí hay un mapeo de operaciones comunes:

| STATA                        | metasurvey                                                                                             | Notas                                                                                                                       |
|------------------------------|--------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------|
| `gen var = expr`             | `step_compute(svy, var = expr)`                                                                        | Lazy por defecto; llamar a [`bake_steps()`](https://metasurveyr.github.io/metasurvey/reference/bake_steps.md) para ejecutar |
| `replace var = x if cond`    | `step_compute(svy, var = ifelse(cond, x, var))`                                                        | Asignación condicional                                                                                                      |
| `recode var (old=new)`       | `step_recode(svy, new_var, old == val ~ "label")`                                                      | Crea una nueva variable                                                                                                     |
| `rename old new`             | `step_rename(svy, new = old)`                                                                          |                                                                                                                             |
| `drop var1 var2`             | `step_remove(svy, var1, var2)`                                                                         |                                                                                                                             |
| `merge using file`           | `step_join(svy, data, by = "key")`                                                                     | Left join por defecto                                                                                                       |
| `svy: mean var`              | `workflow(list(svy), svymean(~var))`                                                                   | Devuelve data.table con SE, CV                                                                                              |
| `svy: total var`             | `workflow(list(svy), svytotal(~var))`                                                                  |                                                                                                                             |
| `svy: mean var, over(group)` | `workflow(list(svy), svyby(~var, ~group, svymean))`                                                    |                                                                                                                             |
| `.do` file                   | [`steps_to_recipe()`](https://metasurveyr.github.io/metasurvey/reference/steps_to_recipe.md) + publish | Portable, descubrible, con control de versiones                                                                             |
| `use "data.dta"`             | `load_survey(path = "data.dta")`                                                                       | Lee STATA, CSV, RDS, etc.                                                                                                   |

### Diferencias clave

1.  **Evaluación lazy**: En STATA, los comandos se ejecutan
    inmediatamente. En metasurvey, los steps se registran y se ejecutan
    juntos con
    [`bake_steps()`](https://metasurveyr.github.io/metasurvey/reference/bake_steps.md).
    Esto permite validación y optimización antes de la ejecución.

2.  **Inmutabilidad**: metasurvey crea nuevas variables en lugar de
    modificar las existentes.
    [`step_recode()`](https://metasurveyr.github.io/metasurvey/reference/step_recode.md)
    crea una nueva columna; no sobreescribe la variable fuente.

3.  **Conciencia del diseño**: Los pesos muestrales y el diseño están
    asociados al objeto `Survey`. No es necesario prefijar comandos con
    `svy:` ni recordar configurar el diseño
    —[`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
    lo maneja automáticamente.

4.  **Recipes vs archivos .do**: Los recipes son auto-documentados (vía
    `doc()`), auto-validados (vía `validate()`), y descubribles (vía el
    registry). Un archivo `.do` es simplemente un script; un recipe es
    un objeto estructurado y portable.

## Próximos pasos

- **[Creación y publicación de
  recipes](https://metasurveyr.github.io/metasurvey/articles/recipes.md)**
  – Aprende sobre registries de recipes, certificación y descubrimiento
- **[Flujos de
  estimación](https://metasurveyr.github.io/metasurvey/articles/workflows-and-estimation.md)**
  – Profundización en
  [`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
  y `RecipeWorkflow`
- **[Paneles rotativos y
  PoolSurvey](https://metasurveyr.github.io/metasurvey/articles/panel-analysis.md)**
  – Trabajar con la estructura de panel rotativo de la ECH
- **[Primeros
  pasos](https://metasurveyr.github.io/metasurvey/articles/getting-started.md)**
  – Revisar los conceptos básicos
