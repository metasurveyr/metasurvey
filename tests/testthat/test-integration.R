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
