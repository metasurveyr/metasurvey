test_that("step_compute produces identical data.frame as direct transformation", {
  set_use_copy(TRUE)
  
  s <- make_test_survey(n = 100)
  original_data <- data.table::copy(get_data(s))
  
  # Método 1: Con metasurvey step_compute
  s_metasurvey <- s %>% 
    step_compute(age_squared = age^2) %>%
    step_compute(income_log = log(income))
  
  result_metasurvey <- get_data(s_metasurvey)
  
  # Método 2: Transformación directa en data.table
  result_direct <- data.table::copy(original_data)[, `:=`(
    age_squared = age^2,
    income_log = log(income)
  )]
  
  # COMPARACIÓN EXACTA: Las columnas nuevas deben ser idénticas
  expect_identical(result_metasurvey$age_squared, result_direct$age_squared)
  expect_identical(result_metasurvey$income_log, result_direct$income_log)
  
  # COMPARACIÓN: Todo el data.frame debe tener las mismas columnas
  expect_setequal(names(result_metasurvey), names(result_direct))
  
  # COMPARACIÓN: Mismo número de filas
  expect_identical(nrow(result_metasurvey), nrow(result_direct))
})

test_that("multiple step_compute produce identical results as chained transformations", {
  set_use_copy(TRUE)
  
  s <- make_test_survey(n = 150)
  original_data <- data.table::copy(get_data(s))
  
  # Con metasurvey
  s_ms <- s %>%
    step_compute(income_thousands = income / 1000) %>%
    step_compute(income_log = log(income_thousands + 1)) %>%
    step_compute(age_decade = floor(age / 10)) %>%
    step_compute(high_income = as.integer(income > 3000))
  
  df_metasurvey <- get_data(s_ms)
  
  # Directamente
  df_direct <- data.table::copy(original_data)
  df_direct[, income_thousands := income / 1000]
  df_direct[, income_log := log(income_thousands + 1)]
  df_direct[, age_decade := floor(age / 10)]
  df_direct[, high_income := as.integer(income > 3000)]
  
  # COMPARACIÓN COLUMNA POR COLUMNA
  expect_identical(df_metasurvey$income_thousands, df_direct$income_thousands)
  expect_identical(df_metasurvey$income_log, df_direct$income_log)
  expect_identical(df_metasurvey$age_decade, df_direct$age_decade)
  expect_identical(df_metasurvey$high_income, df_direct$high_income)
})

test_that("step_join produces identical data.frame as merge", {
  set_use_copy(TRUE)
  
  s <- make_test_survey(n = 100)
  original_data <- data.table::copy(get_data(s))
  
  # Datos auxiliares
  region_info <- data.frame(
    region = 1:4,
    region_name = c("North", "South", "East", "West"),
    population = c(1000000, 800000, 1200000, 900000)
  )
  
  # Con metasurvey
  s_joined <- s %>% step_join(region_info, by = "region")
  df_metasurvey <- get_data(s_joined)
  
  # Con merge directo
  df_direct <- merge(original_data, region_info, by = "region", all.x = TRUE)
  data.table::setDT(df_direct)
  
  # COMPARACIÓN: Mismas columnas
  expect_setequal(names(df_metasurvey), names(df_direct))
  
  # COMPARACIÓN: Mismo número de filas
  expect_identical(nrow(df_metasurvey), nrow(df_direct))
  
  # COMPARACIÓN: region_name debe ser idéntico (ordenando por id)
  data.table::setorder(df_metasurvey, id)
  data.table::setorder(df_direct, id)
  expect_identical(df_metasurvey$region_name, df_direct$region_name)
  expect_identical(df_metasurvey$population, df_direct$population)
})

test_that("survey design with metasurvey data produces same estimates as direct data", {
  set_use_copy(TRUE)
  
  s <- make_test_survey(n = 200)
  
  # Transformar con metasurvey
  s_transformed <- s %>% step_compute(age_cat = as.integer(age >= 40))
  df_metasurvey <- get_data(s_transformed)
  
  # Transformar directamente
  df_direct <- data.table::copy(get_data(s))
  df_direct[, age_cat := as.integer(age >= 40)]
  
  # Crear diseños survey con ambos
  des_metasurvey <- survey::svydesign(ids = ~1, data = df_metasurvey, weights = ~w)
  des_direct <- survey::svydesign(ids = ~1, data = df_direct, weights = ~w)
  
  # Estimar con ambos
  est_metasurvey <- survey::svymean(~age_cat, des_metasurvey)
  est_direct <- survey::svymean(~age_cat, des_direct)
  
  # COMPARACIÓN EXACTA: Coeficientes deben ser idénticos
  expect_identical(
    as.numeric(coef(est_metasurvey)),
    as.numeric(coef(est_direct))
  )
  
  # COMPARACIÓN EXACTA: Errores estándar deben ser idénticos
  expect_identical(
    as.numeric(survey::SE(est_metasurvey)),
    as.numeric(survey::SE(est_direct))
  )
})

test_that("svymean on transformed data matches direct computation exactly", {
  set_use_copy(TRUE)
  
  s <- make_test_survey(n = 250)
  
  # Pipeline metasurvey
  s_pipeline <- s %>%
    step_compute(log_income = log(income)) %>%
    step_compute(age_group = cut(age, breaks = c(0, 30, 50, 100), labels = FALSE))
  
  df_pipeline <- get_data(s_pipeline)
  
  # Computación directa
  df_baseline <- data.table::copy(get_data(s))
  df_baseline[, log_income := log(income)]
  df_baseline[, age_group := cut(age, breaks = c(0, 30, 50, 100), labels = FALSE)]
  
  # Diseños
  des_pipeline <- survey::svydesign(ids = ~1, data = df_pipeline, weights = ~w)
  des_baseline <- survey::svydesign(ids = ~1, data = df_baseline, weights = ~w)
  
  # Estimaciones
  mean_pipeline <- survey::svymean(~log_income, des_pipeline)
  mean_baseline <- survey::svymean(~log_income, des_baseline)
  
  total_pipeline <- survey::svytotal(~income, des_pipeline)
  total_baseline <- survey::svytotal(~income, des_baseline)
  
  # COMPARACIONES EXACTAS
  expect_equal(coef(mean_pipeline), coef(mean_baseline), tolerance = 1e-10)
  expect_equal(survey::SE(mean_pipeline), survey::SE(mean_baseline), tolerance = 1e-10)
  expect_equal(coef(total_pipeline), coef(total_baseline), tolerance = 1e-10)
  expect_equal(survey::SE(total_pipeline), survey::SE(total_baseline), tolerance = 1e-10)
})

test_that("svytotal produces identical results on metasurvey vs direct data", {
  s <- make_test_survey(n = 200)
  
  # Con metasurvey (sin transformación, solo para comparar)
  df_metasurvey <- get_data(s)
  
  # Directo (mismo data)
  df_direct <- data.table::copy(df_metasurvey)
  
  # Diseños
  des_ms <- survey::svydesign(ids = ~1, data = df_metasurvey, weights = ~w)
  des_direct <- survey::svydesign(ids = ~1, data = df_direct, weights = ~w)
  
  # Estimaciones
  total_ms <- survey::svytotal(~income, des_ms)
  total_direct <- survey::svytotal(~income, des_direct)
  
  # COMPARACIÓN EXACTA
  expect_identical(coef(total_ms), coef(total_direct))
  expect_identical(survey::SE(total_ms), survey::SE(total_direct))
})

test_that("svyratio produces identical results with metasurvey transformed data", {
  set_use_copy(TRUE)
  
  s <- make_test_survey(n = 200)
  
  # Transformar edad con metasurvey
  s_transformed <- s %>% step_compute(age_adjusted = age + 0.5)
  df_metasurvey <- get_data(s_transformed)
  
  # Transformar directamente
  df_direct <- data.table::copy(get_data(s))
  df_direct[, age_adjusted := age + 0.5]
  
  # Diseños
  des_ms <- survey::svydesign(ids = ~1, data = df_metasurvey, weights = ~w)
  des_direct <- survey::svydesign(ids = ~1, data = df_direct, weights = ~w)
  
  # Ratio
  ratio_ms <- survey::svyratio(~income, ~age_adjusted, des_ms)
  ratio_direct <- survey::svyratio(~income, ~age_adjusted, des_direct)
  
  # COMPARACIÓN EXACTA
  expect_equal(coef(ratio_ms), coef(ratio_direct), tolerance = 1e-10)
  expect_equal(survey::SE(ratio_ms), survey::SE(ratio_direct), tolerance = 1e-10)
})

test_that("complex pipeline: transformations + join produce identical survey results", {
  set_use_copy(TRUE)
  
  s <- make_test_survey(n = 150)
  original <- data.table::copy(get_data(s))
  
  # Info adicional
  status_info <- data.frame(
    status = 1:3,
    status_label = c("Active", "Inactive", "Pending")
  )
  
  # Pipeline metasurvey
  s_pipeline <- s %>%
    step_compute(income_normalized = income / 1000) %>%
    step_join(status_info, by = "status") %>%
    step_compute(combined_score = income_normalized * age / 100)
  
  df_pipeline <- get_data(s_pipeline)
  
  # Pipeline directo
  df_baseline <- data.table::copy(original)
  df_baseline[, income_normalized := income / 1000]
  df_baseline <- merge(df_baseline, status_info, by = "status", all.x = TRUE)
  data.table::setDT(df_baseline)
  df_baseline[, combined_score := income_normalized * age / 100]
  
  # Ordenar por id para comparar
  data.table::setorder(df_pipeline, id)
  data.table::setorder(df_baseline, id)
  
  # COMPARACIÓN DE DATA.FRAMES
  expect_identical(df_pipeline$income_normalized, df_baseline$income_normalized)
  expect_identical(df_pipeline$status_label, df_baseline$status_label)
  expect_identical(df_pipeline$combined_score, df_baseline$combined_score)
  
  # COMPARACIÓN DE ESTIMACIONES
  des_pipeline <- survey::svydesign(ids = ~1, data = df_pipeline, weights = ~w)
  des_baseline <- survey::svydesign(ids = ~1, data = df_baseline, weights = ~w)
  
  mean_pipeline <- survey::svymean(~combined_score, des_pipeline)
  mean_baseline <- survey::svymean(~combined_score, des_baseline)
  
  expect_equal(coef(mean_pipeline), coef(mean_baseline), tolerance = 1e-10)
  expect_equal(survey::SE(mean_pipeline), survey::SE(mean_baseline), tolerance = 1e-10)
})

test_that("categorical variables produce identical proportions", {
  set_use_copy(TRUE)
  
  s <- make_test_survey(n = 300)
  
  # Categorizar con metasurvey
  s_cat <- s %>% 
    step_compute(income_bracket = cut(income, breaks = 3, labels = c("Low", "Med", "High")))
  
  df_metasurvey <- get_data(s_cat)
  
  # Categorizar directamente
  df_direct <- data.table::copy(get_data(s))
  df_direct[, income_bracket := cut(income, breaks = 3, labels = c("Low", "Med", "High"))]
  
  # Diseños
  des_ms <- survey::svydesign(ids = ~1, data = df_metasurvey, weights = ~w)
  des_direct <- survey::svydesign(ids = ~1, data = df_direct, weights = ~w)
  
  # Proporciones
  prop_ms <- survey::svymean(~income_bracket, des_ms)
  prop_direct <- survey::svymean(~income_bracket, des_direct)
  
  # COMPARACIÓN EXACTA
  expect_identical(as.numeric(coef(prop_ms)), as.numeric(coef(prop_direct)))
  expect_identical(as.numeric(survey::SE(prop_ms)), as.numeric(survey::SE(prop_direct)))
})

test_that("variance estimates are identical between methods", {
  s <- make_test_survey(n = 200)
  
  df1 <- get_data(s)
  df2 <- data.table::copy(df1)
  
  des1 <- survey::svydesign(ids = ~1, data = df1, weights = ~w)
  des2 <- survey::svydesign(ids = ~1, data = df2, weights = ~w)
  
  var1 <- survey::svyvar(~age, des1)
  var2 <- survey::svyvar(~age, des2)
  
  # COMPARACIÓN EXACTA de varianza estimada
  expect_identical(as.numeric(coef(var1)), as.numeric(coef(var2)))
  expect_identical(as.numeric(survey::SE(var1)), as.numeric(survey::SE(var2)))
})

test_that("quantiles are identical with transformed data", {
  set_use_copy(TRUE)
  
  s <- make_test_survey(n = 200)
  
  # Transformar
  s_t <- s %>% step_compute(income_sqrt = sqrt(income))
  df_ms <- get_data(s_t)
  
  df_direct <- data.table::copy(get_data(s))
  df_direct[, income_sqrt := sqrt(income)]
  
  # Diseños
  des_ms <- survey::svydesign(ids = ~1, data = df_ms, weights = ~w)
  des_direct <- survey::svydesign(ids = ~1, data = df_direct, weights = ~w)
  
  # Cuantiles
  q_ms <- survey::svyquantile(~income_sqrt, des_ms, quantiles = c(0.25, 0.5, 0.75))
  q_direct <- survey::svyquantile(~income_sqrt, des_direct, quantiles = c(0.25, 0.5, 0.75))
  
  # COMPARACIÓN (puede haber pequeñas diferencias numéricas en quantiles)
  expect_equal(as.numeric(coef(q_ms)), as.numeric(coef(q_direct)), tolerance = 1e-8)
})
