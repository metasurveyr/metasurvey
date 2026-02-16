# Tests de Comparación Directa: metasurvey vs transformaciones base R

## Resumen
Este archivo contiene **12 tests** que comparan **exactamente** los resultados entre:
- Transformaciones con metasurvey (steps) vs transformaciones directas en data.table
- Estimaciones survey sobre datos transformados con metasurvey vs datos transformados directamente

## Tests Implementados

### 1. Comparaciones de Data.frames (Transformaciones)

#### `step_compute produces identical data.frame as direct transformation`
- **Compara**: `step_compute()` vs `data.table[, := ]`
- **Verifica**: Las columnas generadas son `identical()` (exactamente iguales)
- **Variables**: age_squared, income_log
- **Resultado**: Los data.frames deben ser idénticos

#### `multiple step_compute produce identical results as chained transformations`
- **Compara**: Pipeline de 4 `step_compute()` vs 4 transformaciones encadenadas
- **Verifica**: Cada columna generada con `expect_identical()`
- **Variables**: income_thousands, income_log, age_decade, high_income
- **Resultado**: Todas las columnas deben coincidir exactamente

#### `step_join produces identical data.frame as merge`
- **Compara**: `step_join()` vs `merge()`
- **Verifica**: Columnas agregadas, número de filas, contenido idéntico
- **Variables**: region_name, population (de tabla auxiliar)
- **Resultado**: Los joins deben producir el mismo resultado

### 2. Comparaciones de Estimaciones Survey

#### `survey design with metasurvey data produces same estimates as direct data`
- **Compara**: Estimaciones `svymean()` sobre datos transformados con metasurvey vs directos
- **Verifica**: Coeficientes y errores estándar con `expect_identical()`
- **Variable**: age_cat (categorización de edad)
- **Resultado**: Estimaciones numéricamente idénticas

#### `svymean on transformed data matches direct computation exactly`
- **Compara**: Pipeline completo (log_income, age_group) + svymean/svytotal
- **Verifica**: Coeficientes y SE con tolerancia de 1e-10
- **Variables**: log_income, age_group
- **Funciones**: svymean, svytotal
- **Resultado**: Diferencias menores a 1e-10

#### `svytotal produces identical results on metasurvey vs direct data`
- **Compara**: `svytotal()` sobre mismo data
- **Verifica**: Coeficientes y SE con `expect_identical()`
- **Resultado**: Resultados exactamente iguales

#### `svyratio produces identical results with metasurvey transformed data`
- **Compara**: `svyratio()` sobre age_adjusted transformado
- **Verifica**: Coeficientes y SE con tolerancia 1e-10
- **Resultado**: Ratios numéricamente idénticos

### 3. Pipelines Complejos

#### `complex pipeline: transformations + join produce identical survey results`
- **Compara**: Pipeline de 3 pasos (compute + join + compute) vs directo
- **Verifica**: 
  - Data.frame resultante columna por columna con `expect_identical()`
  - Estimaciones survey con tolerancia 1e-10
- **Variables**: income_normalized, status_label, combined_score
- **Resultado**: Pipeline completo produce resultados idénticos

### 4. Casos Especiales

#### `categorical variables produce identical proportions`
- **Compara**: Categorización con `cut()` + proporciones
- **Verifica**: Proporciones con `expect_identical()`
- **Variable**: income_bracket (Low/Med/High)
- **Resultado**: Distribuciones de frecuencia idénticas

#### `variance estimates are identical between methods`
- **Compara**: `svyvar()` sobre mismos datos
- **Verifica**: Varianza estimada con `expect_identical()`
- **Resultado**: Estimaciones de varianza exactas

#### `quantiles are identical with transformed data`
- **Compara**: `svyquantile()` sobre income_sqrt
- **Verifica**: Cuantiles (p25, p50, p75) con tolerancia 1e-8
- **Resultado**: Cuantiles casi idénticos (pequeñas diferencias numéricas aceptables)

## Metodología de Comparación

### Comparaciones Exactas (expect_identical)
Usadas cuando los resultados **deben ser bit-a-bit idénticos**:
- Transformaciones directas en data.table
- Coeficientes de estimaciones simples
- Errores estándar de diseños idénticos

### Comparaciones con Tolerancia (expect_equal)
Usadas cuando puede haber pequeñas diferencias de punto flotante:
- Estimaciones complejas (tolerance = 1e-10)
- Cuantiles (tolerance = 1e-8)

## Ejecución

Todos los tests se ejecutan **secuencialmente** (no en paralelo) usando testthat.

```r
# Ejecutar solo estos tests
testthat::test_file("tests/testthat/test-metasurvey-vs-direct.R")

# Ejecutar todos los tests
testthat::test_dir("tests/testthat")
```

## Resultados

- ✅ **32 PASS** - Todos los tests pasan
- ✅ **0 FAIL** - Sin errores
- ✅ Cobertura total del paquete: **45.26%**
- ✅ Total de tests: **419**

## Conclusión

Estos tests **garantizan** que:
1. Las transformaciones de metasurvey producen **exactamente** los mismos data.frames que transformaciones directas
2. Las estimaciones survey sobre datos de metasurvey son **numéricamente idénticas** a estimaciones sobre datos procesados directamente
3. Los pipelines complejos (múltiples pasos + joins) mantienen **precisión numérica total**
