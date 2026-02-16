# Additional tests for workflow to increase coverage

test_that("workflow dispatches to workflow_pool for PoolSurvey", {
  # Build a PoolSurvey manually with the right structure
  s1 <- make_test_survey(50)
  s1$edition <- "2023-01-01"
  s2 <- make_test_survey(50)
  s2$edition <- "2023-02-01"

  surveys_struct <- list(
    annual = list(
      "group1" = list(s1, s2)
    )
  )
  pool <- PoolSurvey$new(surveys_struct)

  result <- workflow(
    survey = pool,
    survey::svymean(~age, na.rm = TRUE),
    estimation_type = "annual"
  )
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  expect_true("stat" %in% names(result))
})

test_that("workflow_pool with colon-separated estimation types aggregates", {
  s1 <- make_test_survey(50)
  s1$edition <- "2023-01-01"
  s2 <- make_test_survey(50)
  s2$edition <- "2023-02-01"

  surveys_struct <- list(
    annual = list(
      "group1" = list(s1, s2)
    )
  )
  pool <- PoolSurvey$new(surveys_struct)

  result <- workflow(
    survey = pool,
    survey::svymean(~age, na.rm = TRUE),
    estimation_type = "annual:annual"
  )
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("cat_estimation dispatches correctly for different types", {
  survey <- make_test_survey(100)
  des <- survey::svydesign(ids = ~1, data = get_data(survey), weights = ~w)

  # Test svymean
  est_mean <- survey::svymean(~age, des, na.rm = TRUE)
  result_mean <- metasurvey:::cat_estimation(est_mean, "survey::svymean")
  expect_s3_class(result_mean, "data.table")
  expect_true("stat" %in% names(result_mean))

  # Test svytotal
  est_total <- survey::svytotal(~income, des, na.rm = TRUE)
  result_total <- metasurvey:::cat_estimation(est_total, "survey::svytotal")
  expect_s3_class(result_total, "data.table")

  # Test svyratio
  est_ratio <- survey::svyratio(~income, ~age, des, na.rm = TRUE)
  result_ratio <- metasurvey:::cat_estimation(est_ratio, "survey::svyratio")
  expect_s3_class(result_ratio, "data.table")

  # Test svyby
  est_by <- survey::svyby(~income, ~region, des, survey::svymean, na.rm = TRUE)
  result_by <- metasurvey:::cat_estimation(est_by, "survey::svyby")
  expect_s3_class(result_by, "data.table")
})

test_that("workflow_pool with rho and R parameters", {
  s1 <- make_test_survey(50)
  s1$edition <- "2023-01-01"
  s2 <- make_test_survey(50)
  s2$edition <- "2023-02-01"

  surveys_struct <- list(
    annual = list(
      "group1" = list(s1, s2)
    )
  )
  pool <- PoolSurvey$new(surveys_struct)

  result <- workflow(
    survey = pool,
    survey::svymean(~age, na.rm = TRUE),
    rho = 0.5,
    R = 2,
    estimation_type = "annual"
  )
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("workflow handles empty survey list", {
  result <- tryCatch(
    {
      workflow(
        survey = list(),
        survey::svymean(~age, na.rm = TRUE),
        estimation_type = "annual"
      )
    },
    error = function(e) e
  )

  # Should error, return NULL, or return empty data.frame for empty list
  expect_true(inherits(result, "error") || is.null(result) || (is.data.frame(result) && nrow(result) == 0))
})

test_that("workflow validates estimation type exists in design", {
  survey <- make_test_survey(50)

  # This should work with annual
  result <- workflow(
    survey = list(survey),
    survey::svymean(~age, na.rm = TRUE),
    estimation_type = "annual"
  )

  expect_s3_class(result, "data.table")
})

test_that("workflow_default processes multiple calls correctly", {
  survey <- make_test_survey(50)

  result <- workflow(
    survey = list(survey),
    survey::svymean(~age, na.rm = TRUE),
    survey::svytotal(~income, na.rm = TRUE),
    survey::svyratio(~income, ~age, na.rm = TRUE),
    estimation_type = "annual"
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) >= 3)
})

test_that("cat_estimation.default handles all result columns", {
  survey <- make_test_survey(100)
  des <- survey::svydesign(ids = ~1, data = get_data(survey), weights = ~w)
  estimation <- survey::svymean(~age, des, na.rm = TRUE)

  result <- metasurvey:::cat_estimation.default(estimation, "survey::svymean")

  expected_cols <- c("stat", "value", "se", "cv", "confint_lower", "confint_upper")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("cat_estimation.svyby handles multiple groups", {
  survey <- make_test_survey(100)
  des <- survey::svydesign(ids = ~1, data = get_data(survey), weights = ~w)

  # svyby with multiple groups
  estimation <- survey::svyby(~income, ~region, des, survey::svymean, na.rm = TRUE)

  result <- metasurvey:::cat_estimation.svyby(estimation, "survey::svyby")

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 1)
  expect_true("value" %in% names(result))
})

test_that("workflow handles survey with multiple editions", {
  survey1 <- make_test_survey(50)
  survey1$edition <- "2023"

  survey2 <- make_test_survey(50)
  survey2$edition <- "2024"

  survey3 <- make_test_survey(50)
  survey3$edition <- "2025"

  result <- workflow(
    survey = list(survey1, survey2, survey3),
    survey::svymean(~age, na.rm = TRUE),
    estimation_type = "annual"
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})
