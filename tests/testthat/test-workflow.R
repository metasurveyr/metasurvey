test_that("workflow function dispatches correctly", {
  # Create test survey
  survey <- make_test_survey(50)
  
  # Test with regular survey - using survey package functions
  result <- workflow(
    survey = list(survey),
    survey::svymean(~age, na.rm = TRUE),
    estimation_type = "annual"
  )
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  expect_true("stat" %in% names(result))
  expect_true("value" %in% names(result))
  expect_true("se" %in% names(result))
})

test_that("workflow handles multiple estimations", {
  survey <- make_test_survey(50)
  
  result <- workflow(
    survey = list(survey),
    survey::svymean(~age, na.rm = TRUE),
    survey::svytotal(~income, na.rm = TRUE),
    estimation_type = "annual"
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
})

test_that("workflow handles multiple surveys", {
  survey1 <- make_test_survey(50)
  survey2 <- make_test_survey(50)
  survey2$edition <- "2024"
  
  result <- workflow(
    survey = list(survey1, survey2),
    survey::svymean(~age, na.rm = TRUE),
    estimation_type = "annual"
  )
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("workflow handles multiple estimation types", {
  survey <- make_test_survey(50)
  survey$weight <- add_weight(
    annual = "w",
    quarterly = "w",
    monthly = "w"
  )
  survey$design <- list(
    annual = survey::svydesign(ids = ~1, weights = ~w, data = survey$data),
    quarterly = survey::svydesign(ids = ~1, weights = ~w, data = survey$data),
    monthly = survey::svydesign(ids = ~1, weights = ~w, data = survey$data)
  )
  
  result <- workflow(
    survey = list(survey),
    survey::svymean(~age, na.rm = TRUE),
    estimation_type = c("annual", "quarterly")
  )
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) >= 2)
})

test_that("cat_estimation helper formats results correctly", {
  survey <- make_test_survey(100)
  des <- survey::svydesign(ids = ~1, data = get_data(survey), weights = ~w)
  
  estimation <- survey::svymean(~age, des, na.rm = TRUE)
  
  # Verify estimation has required components
  expect_true(!is.null(coef(estimation)))
  expect_true(!is.null(survey::SE(estimation)))
})

test_that("svyratio estimation produces valid results", {
  survey <- make_test_survey(100)
  des <- survey::svydesign(ids = ~1, data = get_data(survey), weights = ~w)
  
  estimation <- survey::svyratio(~income, ~age, des, na.rm = TRUE)
  
  # Verify estimation structure
  expect_true(!is.null(coef(estimation)))
  expect_true(!is.null(survey::SE(estimation)))
})


test_that("workflow validates PoolSurvey dispatch", {
  survey <- make_test_survey(50)
  result <- workflow(
    survey = list(survey),
    survey::svymean(~age, na.rm = TRUE),
    estimation_type = "annual"
  )

  expect_s3_class(result, "data.table")
})

# --- cat_estimation.default tests ---

test_that("cat_estimation.default formats svymean result", {
  survey <- make_test_survey(100)
  des <- survey::svydesign(ids = ~1, data = get_data(survey), weights = ~w)
  estimation <- survey::svymean(~age, des, na.rm = TRUE)

  result <- metasurvey:::cat_estimation.default(estimation, "survey::svymean")
  expect_s3_class(result, "data.table")
  expect_true(all(c("stat", "value", "se", "cv", "confint_lower", "confint_upper") %in% names(result)))
  expect_true(nrow(result) > 0)
})

test_that("cat_estimation.default formats svytotal result", {
  survey <- make_test_survey(100)
  des <- survey::svydesign(ids = ~1, data = get_data(survey), weights = ~w)
  estimation <- survey::svytotal(~income, des, na.rm = TRUE)

  result <- metasurvey:::cat_estimation.default(estimation, "survey::svytotal")
  expect_s3_class(result, "data.table")
  expect_true(all(c("stat", "value", "se") %in% names(result)))
})

# --- cat_estimation.svyratio tests ---

test_that("cat_estimation.svyratio formats ratio result", {
  survey <- make_test_survey(100)
  des <- survey::svydesign(ids = ~1, data = get_data(survey), weights = ~w)
  estimation <- survey::svyratio(~income, ~age, des, na.rm = TRUE)

  result <- metasurvey:::cat_estimation.svyratio(estimation, "survey::svyratio")
  expect_s3_class(result, "data.table")
  expect_true(all(c("stat", "value", "se", "cv", "confint_lower", "confint_upper") %in% names(result)))
})

# --- cat_estimation.svyby tests ---

test_that("cat_estimation.svyby formats by-group result", {
  survey <- make_test_survey(100)
  des <- survey::svydesign(ids = ~1, data = get_data(survey), weights = ~w)
  estimation <- survey::svyby(~income, ~region, des, survey::svymean, na.rm = TRUE)

  result <- metasurvey:::cat_estimation.svyby(estimation, "survey::svyby")
  expect_s3_class(result, "data.table")
  expect_true("stat" %in% names(result))
  expect_true("value" %in% names(result))
})

# --- cat_estimation dispatcher ---

test_that("cat_estimation dispatches to default for svymean", {
  survey <- make_test_survey(100)
  des <- survey::svydesign(ids = ~1, data = get_data(survey), weights = ~w)
  estimation <- survey::svymean(~age, des, na.rm = TRUE)

  result <- metasurvey:::cat_estimation(estimation, "survey::svymean")
  expect_s3_class(result, "data.table")
})

test_that("cat_estimation dispatches to svyratio", {
  survey <- make_test_survey(100)
  des <- survey::svydesign(ids = ~1, data = get_data(survey), weights = ~w)
  estimation <- survey::svyratio(~income, ~age, des, na.rm = TRUE)

  result <- metasurvey:::cat_estimation(estimation, "survey::svyratio")
  expect_s3_class(result, "data.table")
})

test_that("cat_estimation dispatches to svyby", {
  survey <- make_test_survey(100)
  des <- survey::svydesign(ids = ~1, data = get_data(survey), weights = ~w)
  estimation <- survey::svyby(~income, ~region, des, survey::svymean, na.rm = TRUE)

  result <- metasurvey:::cat_estimation(estimation, "survey::svyby")
  expect_s3_class(result, "data.table")
})

# --- workflow with svyratio ---

test_that("workflow handles svyratio estimation", {
  survey <- make_test_survey(50)
  result <- workflow(
    survey = list(survey),
    survey::svyratio(~income, ~age, na.rm = TRUE),
    estimation_type = "annual"
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  expect_true("stat" %in% names(result))
})

# --- workflow with svyby ---

test_that("workflow handles svyby estimation", {
  survey <- make_test_survey(50)
  result <- workflow(
    survey = list(survey),
    survey::svyby(~income, ~region, survey::svymean, na.rm = TRUE),
    estimation_type = "annual"
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

# --- workflow result correctness ---

test_that("workflow svymean result matches direct calculation", {
  survey <- make_test_survey(100)
  des <- survey::svydesign(ids = ~1, data = get_data(survey), weights = ~w)

  direct <- survey::svymean(~age, des, na.rm = TRUE)

  wf_result <- workflow(
    survey = list(survey),
    survey::svymean(~age, na.rm = TRUE),
    estimation_type = "annual"
  )

  expect_equal(wf_result$value[1], unname(coef(direct)), tolerance = 1e-6)
  expect_equal(as.numeric(wf_result$se[1]), as.numeric(survey::SE(direct)), tolerance = 1e-6)
})
