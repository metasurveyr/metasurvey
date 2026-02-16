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
