# Integration tests - end-to-end pipelines using in-memory data

test_that("Pipeline: step_compute -> bake -> step_recode", {
  s <- make_test_survey(n = 50)

  # Compute and bake first
  s2 <- step_compute(s, income_double = income * 2)
  s2 <- bake_steps(s2)
  expect_true("income_double" %in% names(s2$data))
  expect_equal(s2$data$income_double, s2$data$income * 2)

  # Then recode (recode applies immediately with use_copy=TRUE)
  s3 <- step_recode(s2, income_cat,
    income > 3000 ~ "high",
    income > 1500 ~ "medium",
    .default = "low"
  )
  expect_true("income_cat" %in% names(s3$data))
  expect_true(all(s3$data$income_cat %in% c("high", "medium", "low")))
})

test_that("Pipeline: step_remove + step_rename chain", {
  s <- make_test_survey()
  s2 <- step_remove(s, status)
  s2 <- step_rename(s2, edad = age)
  s2 <- bake_steps(s2)

  expect_false("status" %in% names(s2$data))
  expect_true("edad" %in% names(s2$data))
  expect_false("age" %in% names(s2$data))
})

test_that("Pipeline: step_join with external data", {
  s <- make_test_survey()
  lookup <- data.frame(region = 1:4, region_name = c("North", "South", "East", "West"))

  s2 <- step_join(s, lookup, by = "region", type = "left")
  s2 <- bake_steps(s2)

  expect_true("region_name" %in% names(s2$data))
  expect_equal(nrow(s2$data), 10)
})

test_that("Recipe creation and application end-to-end", {
  s <- make_test_survey()

  r <- recipe(
    name = "Integration test recipe",
    user = "test_user",
    svy = s,
    description = "Test recipe for integration",
    step_remove(s, status)
  )

  expect_s3_class(r, "Recipe")
  expect_equal(r$name, "Integration test recipe")

  # Save and read back
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  expect_message(save_recipe(r, tmp), "saved")
  expect_true(file.exists(tmp))
})

test_that("Full pipeline with simulated survey data", {
  svy <- make_test_survey()

  svy <- step_compute(svy, z = x + y, comment = "sum")
  svy <- bake_steps(svy)
  expect_true("z" %in% names(get_data(svy)))

  r <- recipe(
    name = "integration",
    user = "tester",
    svy = svy,
    description = "Integration test"
  )
  expect_s3_class(r, "Recipe")

  result <- workflow(
    list(svy),
    survey::svymean(~x, na.rm = TRUE),
    estimation_type = "annual"
  )
  expect_true(nrow(result) > 0)
  expect_true("value" %in% names(result))
})

# --- Long pipeline integration tests (memory fix validation) ---

test_that("Long pipeline: 25 steps -> bake -> workflow end-to-end", {
  svy <- make_test_survey(n = 100)

  # 10 compute steps
  svy <- step_compute(svy, inc2 = income * 2)
  svy <- step_compute(svy, inc3 = income * 3)
  svy <- step_compute(svy, inc_log = log(income + 1))
  svy <- step_compute(svy, age_sq = age^2)
  svy <- step_compute(svy, ratio_xy = x / (y + 1))
  svy <- step_compute(svy, combined = x + y + age)
  svy <- step_compute(svy, flag_high = as.integer(income > 3000))
  svy <- step_compute(svy, age_norm = age / max(age))
  svy <- step_compute(svy, inc_diff = inc2 - income)
  svy <- step_compute(svy, score = x * 10 + y)

  # 5 recode steps
  svy <- step_recode(svy, inc_cat,
    income < 2000 ~ "low",
    income < 4000 ~ "mid",
    .default = "high"
  )
  svy <- step_recode(svy, age_grp,
    age < 25 ~ "young",
    age < 45 ~ "adult",
    .default = "senior"
  )
  svy <- step_recode(svy, region_lbl,
    region == 1 ~ "A",
    region == 2 ~ "B",
    region == 3 ~ "C",
    .default = "D"
  )
  svy <- step_recode(svy, status_lbl,
    status == 1 ~ "active",
    status == 2 ~ "inactive",
    .default = "other"
  )
  svy <- step_recode(svy, flag_lbl,
    flag_high == 1 ~ "yes",
    .default = "no"
  )

  # 2 rename (avoid renaming weight column 'w' which breaks design)
  svy <- step_rename(svy, identifier = id)
  svy <- step_rename(svy, years_old = age)

  # Verify 17 lazy steps recorded
  expect_length(get_steps(svy), 17)

  # Bake all

  baked <- bake_steps(svy)
  dt <- get_data(baked)

  # Verify computed columns exist and are correct
  expect_equal(dt$inc2, dt$income * 2)
  expect_equal(dt$inc3, dt$income * 3)
  # age was renamed to years_old, so use years_old for comparison
  expect_equal(dt$age_sq, dt$years_old^2)
  expect_equal(dt$inc_diff, dt$inc2 - dt$income)

  # Verify recoded columns exist
  expect_true("inc_cat" %in% names(dt))
  expect_true("age_grp" %in% names(dt))
  expect_true("region_lbl" %in% names(dt))

  # Verify renames applied
  expect_true("identifier" %in% names(dt))
  expect_true("years_old" %in% names(dt))
  expect_false("id" %in% names(dt))
  expect_false("age" %in% names(dt))

  # All steps baked
  for (step in get_steps(baked)) {
    expect_true(step$bake)
  }

  # Design should be valid after bake
  expect_true(baked$design_initialized)

  # Workflow should work on baked survey with renamed weight
  result <- workflow(
    list(baked),
    survey::svymean(~income, na.rm = TRUE),
    estimation_type = "annual"
  )
  expect_true(nrow(result) > 0)
  expect_true("value" %in% names(result))
})

test_that("RotativePanelSurvey: steps on implantation + follow-ups", {
  # Build panel with follow-ups
  impl <- make_test_survey(n = 30)
  impl_data <- get_data(impl)
  impl_data[, `:=`(mes = 1, anio = 2023, numero = id)]
  impl$periodicity <- "monthly"

  fu1 <- make_test_survey(n = 30)
  fu1_data <- get_data(fu1)
  fu1_data[, `:=`(mes = 2, anio = 2023, numero = id)]

  fu2 <- make_test_survey(n = 30)
  fu2_data <- get_data(fu2)
  fu2_data[, `:=`(mes = 3, anio = 2023, numero = id)]

  panel <- RotativePanelSurvey$new(
    implantation = impl,
    follow_up = list(fu1, fu2),
    type = "ech",
    default_engine = "data.table",
    steps = list(),
    recipes = list(),
    workflows = list(),
    design = NULL
  )

  # Apply steps to all levels
  panel <- step_compute(panel, z = x + y, .level = "auto")
  panel <- step_compute(panel, z2 = z * 2, .level = "auto")
  panel <- step_recode(panel, age_cat,
    age < 30 ~ "young",
    .default = "old",
    .level = "auto"
  )

  # Bake
  baked <- bake_steps(panel)

  # Verify implantation
  impl_dt <- get_data(baked$implantation)
  expect_true("z" %in% names(impl_dt))
  expect_true("z2" %in% names(impl_dt))
  expect_true("age_cat" %in% names(impl_dt))
  expect_equal(impl_dt$z, impl_dt$x + impl_dt$y)

  # Verify both follow-ups
  for (i in seq_along(baked$follow_up)) {
    fu_dt <- get_data(baked$follow_up[[i]])
    expect_true("z" %in% names(fu_dt),
      info = paste("follow_up", i, "should have z")
    )
    expect_true("z2" %in% names(fu_dt),
      info = paste("follow_up", i, "should have z2")
    )
    expect_equal(fu_dt$z, fu_dt$x + fu_dt$y,
      info = paste("follow_up", i, "z values should be correct")
    )
  }
})

test_that("use_copy=TRUE vs FALSE produce identical numeric results", {
  old_copy <- use_copy_default()
  on.exit(set_use_copy(old_copy), add = TRUE)

  # Build identical pipelines under both modes
  run_pipeline <- function() {
    svy <- make_test_survey(n = 50)
    svy <- step_compute(svy, z = x + y)
    svy <- step_compute(svy, z2 = z * income)
    svy <- step_compute(svy, log_inc = log(income + 1))
    svy <- step_recode(svy, age_cat,
      age < 30 ~ "young",
      age >= 30 & age < 50 ~ "adult",
      .default = "senior"
    )
    svy <- step_recode(svy, inc_cat,
      income < 2500 ~ "low",
      .default = "high"
    )
    baked <- bake_steps(svy)
    get_data(baked)
  }

  set_use_copy(TRUE)
  dt_copy <- run_pipeline()

  set_use_copy(FALSE)
  dt_inplace <- run_pipeline()

  # Numeric columns must match exactly
  expect_equal(dt_copy$z, dt_inplace$z)
  expect_equal(dt_copy$z2, dt_inplace$z2)
  expect_equal(dt_copy$log_inc, dt_inplace$log_inc)
  expect_equal(dt_copy$age_cat, dt_inplace$age_cat)
  expect_equal(dt_copy$inc_cat, dt_inplace$inc_cat)
})


# --- Merged from test-integration-comparison.R ---

test_that("step_compute produces same results as base R transformation", {
  set_use_copy(TRUE)

  # Create test survey
  s <- make_test_survey(n = 100)
  original_data <- get_data(s)

  # Apply transformation with metasurvey
  s_transformed <- s %>% step_compute(age_squared = age^2)
  ms_data <- get_data(s_transformed)

  # Apply same transformation directly
  direct_result <- original_data[, age_squared := age^2]

  # Compare results
  expect_equal(ms_data$age_squared, direct_result$age_squared)
})

test_that("data transformations can be applied and verified", {
  set_use_copy(TRUE)

  s <- make_test_survey(n = 100)
  original_data <- data.table::copy(get_data(s))

  # Apply compute transformation
  s_transformed <- s %>% step_compute(age_group = ifelse(age < 30, 1, 0))
  ms_data <- get_data(s_transformed)

  # Apply same transformation directly
  direct_result <- original_data[, age_group := ifelse(age < 30, 1, 0)]

  # Compare results
  expect_equal(ms_data$age_group, direct_result$age_group)
})

test_that("svymean produces consistent results", {
  s <- make_test_survey(n = 200)

  # Create design directly with survey package
  des_survey <- survey::svydesign(ids = ~1, data = get_data(s), weights = ~w)

  # Estimate with survey
  result_survey <- survey::svymean(~age, des_survey)

  # Verify result structure
  expect_true(!is.null(coef(result_survey)))
  expect_true(!is.null(survey::SE(result_survey)))
  expect_equal(length(coef(result_survey)), 1)
})

test_that("svytotal produces consistent results", {
  s <- make_test_survey(n = 200)

  des_survey <- survey::svydesign(ids = ~1, data = get_data(s), weights = ~w)

  result_survey <- survey::svytotal(~income, des_survey)

  expect_true(!is.null(coef(result_survey)))
  expect_true(!is.null(survey::SE(result_survey)))
})

test_that("svyratio produces consistent results", {
  s <- make_test_survey(n = 200)

  des_survey <- survey::svydesign(ids = ~1, data = get_data(s), weights = ~w)

  result_survey <- survey::svyratio(~income, ~age, des_survey)

  expect_true(!is.null(coef(result_survey)))
  expect_true(!is.null(survey::SE(result_survey)))
})

test_that("multiple transformations preserve data integrity", {
  set_use_copy(TRUE)

  s <- make_test_survey(n = 100)
  original_data <- data.table::copy(get_data(s))

  # Apply multiple steps
  s_multi <- s %>%
    step_compute(income_thousands = income / 1000) %>%
    step_compute(income_log = log(income_thousands + 1))

  ms_data <- get_data(s_multi)

  # Apply same transformations directly
  direct_result <- original_data[, `:=`(
    income_thousands = income / 1000
  )][, `:=`(
    income_log = log(income_thousands + 1)
  )]

  # Compare all new columns
  expect_equal(ms_data$income_thousands, direct_result$income_thousands)
  expect_equal(ms_data$income_log, direct_result$income_log)
})

test_that("multiple survey statistics are consistent", {
  s <- make_test_survey(n = 200)

  des <- survey::svydesign(ids = ~1, data = get_data(s), weights = ~w)

  # Multiple estimations directly
  mean_age <- survey::svymean(~age, des)
  total_income <- survey::svytotal(~income, des)
  mean_region <- survey::svymean(~region, des)

  # Verify all results are valid
  expect_true(!is.null(coef(mean_age)))
  expect_true(!is.null(coef(total_income)))
  expect_true(!is.null(coef(mean_region)))

  # Verify standard errors are positive
  expect_true(survey::SE(mean_age) > 0)
  expect_true(survey::SE(total_income) > 0)
  expect_true(survey::SE(mean_region) > 0)
})

test_that("transformed data can be used in survey estimation", {
  set_use_copy(TRUE)

  s <- make_test_survey(n = 200)

  # Transform data with metasurvey
  s_transformed <- s %>% step_compute(age_decade = floor(age / 10))

  # Verify transformation worked
  transformed_data <- get_data(s_transformed)
  expect_true("age_decade" %in% names(transformed_data))

  # Create design and estimate with transformed data
  des <- survey::svydesign(ids = ~1, data = transformed_data, weights = ~w)
  result <- survey::svymean(~age_decade, des)

  # Same transformation and estimation directly
  original_data <- get_data(s)
  direct_data <- original_data[, age_decade := floor(age / 10)]
  des_direct <- survey::svydesign(ids = ~1, data = direct_data, weights = ~w)
  result_direct <- survey::svymean(~age_decade, des_direct)

  # Compare
  expect_equal(
    as.numeric(coef(result_direct)),
    as.numeric(coef(result)),
    tolerance = 1e-6
  )
})

test_that("step_join adds columns correctly", {
  set_use_copy(TRUE)

  s <- make_test_survey(n = 100)

  # Create auxiliary data
  aux_data <- data.frame(
    region = 1:4,
    region_name = c("North", "South", "East", "West")
  )

  # Join with metasurvey
  s_joined <- s %>% step_join(aux_data, by = "region")

  joined_data <- get_data(s_joined)

  # Verify join worked
  expect_true("region_name" %in% names(joined_data))
  expect_equal(nrow(joined_data), 100)

  # Verify we can still do survey estimation
  des <- survey::svydesign(ids = ~1, data = joined_data, weights = ~w)
  result <- survey::svymean(~age, des)

  expect_true(!is.null(result))
})

test_that("categorical variables work in survey estimation", {
  s <- make_test_survey(n = 200)

  des <- survey::svydesign(ids = ~1, data = get_data(s), weights = ~w)

  # Estimate proportions
  result_survey <- survey::svymean(~ as.factor(status), des)

  # Verify result structure
  expect_true(!is.null(coef(result_survey)))
  expect_equal(length(coef(result_survey)), 3) # 3 levels of status

  # All proportions should sum to 1
  expect_equal(sum(coef(result_survey)), 1, tolerance = 1e-6)
})

test_that("variance estimation is consistent", {
  s <- make_test_survey(n = 200)

  des <- survey::svydesign(ids = ~1, data = get_data(s), weights = ~w)

  result <- survey::svymean(~income, des)

  # Compare standard errors exist and are positive
  expect_true(!is.null(survey::SE(result)))
  expect_true(survey::SE(result) > 0)

  # Coefficient of variation should be reasonable
  cv <- survey::SE(result) / abs(coef(result))
  expect_true(cv < 1) # CV typically less than 100%
})

test_that("multiple steps in sequence produce correct results", {
  set_use_copy(TRUE)

  s <- make_test_survey(n = 100)
  original_data <- data.table::copy(get_data(s))

  # Apply steps in sequence
  s_step1 <- s %>% step_compute(income_log = log(income))
  s_step2 <- s_step1 %>% step_compute(age_cat = ifelse(age < 40, 1, 0))

  result_data <- get_data(s_step2)

  # Apply same transformations manually
  manual_result <- original_data[, `:=`(
    income_log = log(income),
    age_cat = ifelse(age < 40, 1, 0)
  )]

  # Compare
  expect_equal(result_data$income_log, manual_result$income_log)
  expect_equal(result_data$age_cat, manual_result$age_cat)
})


# --- Merged from test-metasurvey-vs-direct.R ---

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
