# ECH Case Study: From STATA to R

## The ECH harmonization problem

Uruguay’s *Encuesta Continua de Hogares* (ECH) is published annually by
the Instituto Nacional de Estadistica (INE). Over the years, the INE has
changed variable names, codebook definitions, and module structure
across editions. Researchers working with ECH data must *harmonize*
variables —that is, map different naming conventions to a common schema—
before any cross-year analysis is possible.

ECH harmonization has historically been carried out by the Instituto de
Economia (IECON) at the Universidad de la Republica. The resulting
dataset has DOI: <http://doi.org/10.47426/ECH.INE> (Instituto de
Economia, 2020). metasurvey aims to complement and facilitate this kind
of work by providing reproducible tools in R.

In Uruguayan academia, this work has been done in STATA for over 30
years. A typical harmonization pipeline consists of approximately **8
.do files per year**, covering:

1.  `2_correc_datos.do` – Raw data loading, merging person and household
    files, variable name corrections
2.  `3_compatibilizacion_mod_1_4.do` – Harmonization of demographic,
    health, education, and labor modules
3.  `4_ingreso_ht11.do` – Household income construction (`ht11`)
4.  `5_descomp_fuentes.do` – Income decomposition by source
5.  `6_ingreso_ht11_sss.do` – Social security adjustments
6.  `7_check_ingr.do` – Income variable validation
7.  `8_arregla_base_comp.do` – Final dataset preparation
8.  `9_labels.do` – Value label application

Multiplied over 30+ years, that means over **240 STATA scripts** doing
essentially the same task: mapping raw ECH variables to a common schema.

**metasurvey solves this with recipes.** Instead of maintaining hundreds
of `.do` files, you write a single recipe that encodes the
transformation logic and can be applied to any ECH edition.

## STATA vs metasurvey: Side-by-side comparison

Below is a snippet from a typical STATA harmonization script for sex,
age, and relationship to head of household:

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

The metasurvey equivalent:

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

Key differences:

| Aspect          | STATA `.do` file                      | metasurvey recipe                   |
|-----------------|---------------------------------------|-------------------------------------|
| Format          | Flat script with hard-coded paths     | Portable JSON with metadata         |
| Validation      | Manual checks with `assert`           | Automatic `validate()` method       |
| Documentation   | In-code comments                      | Auto-generated `doc()` method       |
| Sharing         | Copy files via email/server           | Registry with search and versioning |
| Reproducibility | Depends on file paths and environment | Self-contained, any machine         |
| Cross-edition   | Duplicate script per year             | One recipe, multiple editions       |

## Simulating ECH-like data

For this vignette we simulate data that replicates the structure of a
real ECH microdata file. The simulated variables follow the same naming
conventions and value ranges used by the INE.

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

## Step 1: Demographic variables

Recode raw INE codes to readable names and recode categorical variables:

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

## Step 2: Labor force classification

The `POBPCOAC` variable (Population by activity status) is the central
labor status classification in the ECH. INE codes:

- 2 = Employed
- 3-5 = Unemployed (various subcategories)
- 6-7 = Inactive

This replicates the standard ILO labor force framework:

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

## Step 3: Income variables

Build income indicators following the standard methodology used with ECH
data:

``` r
svy <- step_compute(svy,
  income_pc = ht11 / 5,
  income_thousands = ht11 / 1000,
  log_income = log(ht11 + 1),
  comment = "Income transformations"
)
```

## Step 4: Geographic classification

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

## Building the recipe

Convert all transformations into a portable recipe:

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

### Automatic documentation

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

### Publishing to the registry

Publish the recipe so others can discover and reuse it:

``` r
# Set up a local registry
set_backend("local", path = tempfile(fileext = ".json"))
publish_recipe(ech_recipe)

# Now anyone can retrieve it by ID
r <- get_recipe("ech_labor")
print(r)
#> NULL
```

## Estimation with workflow()

Now we compute standard labor market indicators:

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

### Domain estimation

Compute estimates by subpopulation:

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

### Quality assessment

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

## Reproducibility: same recipe, different edition

The power of recipes lies in applying them unchanged to new data. Here
we simulate a “2024 edition” with the same structure:

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

Same recipe, different data, consistent methodology.

## For STATA users: quick reference

If you are transitioning from STATA to R for survey analysis, here is a
mapping of common operations:

| STATA                        | metasurvey                                                                                             | Notes                                                                                                               |
|------------------------------|--------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------|
| `gen var = expr`             | `step_compute(svy, var = expr)`                                                                        | Lazy by default; call [`bake_steps()`](https://metasurveyr.github.io/metasurvey/reference/bake_steps.md) to execute |
| `replace var = x if cond`    | `step_compute(svy, var = ifelse(cond, x, var))`                                                        | Conditional assignment                                                                                              |
| `recode var (old=new)`       | `step_recode(svy, new_var, old == val ~ "label")`                                                      | Creates a new variable                                                                                              |
| `rename old new`             | `step_rename(svy, new = old)`                                                                          |                                                                                                                     |
| `drop var1 var2`             | `step_remove(svy, var1, var2)`                                                                         |                                                                                                                     |
| `merge using file`           | `step_join(svy, data, by = "key")`                                                                     | Left join by default                                                                                                |
| `svy: mean var`              | `workflow(list(svy), svymean(~var))`                                                                   | Returns data.table with SE, CV                                                                                      |
| `svy: total var`             | `workflow(list(svy), svytotal(~var))`                                                                  |                                                                                                                     |
| `svy: mean var, over(group)` | `workflow(list(svy), svyby(~var, ~group, svymean))`                                                    |                                                                                                                     |
| `.do` file                   | [`steps_to_recipe()`](https://metasurveyr.github.io/metasurvey/reference/steps_to_recipe.md) + publish | Portable, discoverable, version-controlled                                                                          |
| `use "data.dta"`             | `load_survey(path = "data.dta")`                                                                       | Reads STATA, CSV, RDS, etc.                                                                                         |

### Key differences

1.  **Lazy evaluation**: In STATA, commands execute immediately. In
    metasurvey, steps are recorded and executed together with
    [`bake_steps()`](https://metasurveyr.github.io/metasurvey/reference/bake_steps.md).
    This enables validation and optimization before execution.

2.  **Immutability**: metasurvey creates new variables instead of
    modifying existing ones.
    [`step_recode()`](https://metasurveyr.github.io/metasurvey/reference/step_recode.md)
    creates a new column; it does not overwrite the source variable.

3.  **Design awareness**: Survey weights and design are attached to the
    `Survey` object. There is no need to prefix commands with `svy:` or
    remember to set up the design
    —[`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
    handles it automatically.

4.  **Recipes vs .do files**: Recipes are self-documenting (via
    `doc()`), self-validating (via `validate()`), and discoverable (via
    the registry). A `.do` file is just a script; a recipe is a
    structured, portable object.

## Next steps

- **[Creating and publishing
  recipes](https://metasurveyr.github.io/metasurvey/articles/recipes.md)**
  – Learn about recipe registries, certification, and discovery
- **[Estimation
  workflows](https://metasurveyr.github.io/metasurvey/articles/workflows-and-estimation.md)**
  – Deep dive into
  [`workflow()`](https://metasurveyr.github.io/metasurvey/reference/workflow.md)
  and `RecipeWorkflow`
- **[Rotating panels and
  PoolSurvey](https://metasurveyr.github.io/metasurvey/articles/panel-analysis.md)**
  – Working with the ECH’s rotating panel structure
- **[Getting
  started](https://metasurveyr.github.io/metasurvey/articles/getting-started.md)**
  – Review the basics
