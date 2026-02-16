# Additional tests for PanelSurvey to increase coverage

test_that("RotativePanelSurvey get_recipes returns recipes", {
  implantation <- make_test_survey(20)
  implantation$periodicity <- "annual"

  follow_up1 <- make_test_survey(20)
  follow_up1$periodicity <- "quarterly"

  recipes_list <- list(
    recipe1 = list(name = "Test Recipe 1"),
    recipe2 = list(name = "Test Recipe 2")
  )

  panel <- RotativePanelSurvey$new(
    implantation = implantation,
    follow_up = list(follow_up1),
    type = "rotative",
    default_engine = "data.table",
    steps = list(),
    recipes = recipes_list,
    workflows = list(),
    design = NULL
  )

  result <- panel$get_recipes()
  expect_equal(result, recipes_list)
  expect_length(result, 2)
})

test_that("RotativePanelSurvey get_workflows returns workflows", {
  implantation <- make_test_survey(20)
  implantation$periodicity <- "annual"

  follow_up1 <- make_test_survey(20)
  follow_up1$periodicity <- "quarterly"

  workflows_list <- list(
    workflow1 = list(name = "Test Workflow")
  )

  panel <- RotativePanelSurvey$new(
    implantation = implantation,
    follow_up = list(follow_up1),
    type = "rotative",
    default_engine = "data.table",
    steps = list(),
    recipes = list(),
    workflows = workflows_list,
    design = NULL
  )

  result <- panel$get_workflows()
  expect_equal(result, workflows_list)
})

test_that("RotativePanelSurvey get_design returns design", {
  implantation <- make_test_survey(20)
  implantation$periodicity <- "annual"

  follow_up1 <- make_test_survey(20)
  follow_up1$periodicity <- "quarterly"

  design_obj <- list(annual = "design1")

  panel <- RotativePanelSurvey$new(
    implantation = implantation,
    follow_up = list(follow_up1),
    type = "rotative",
    default_engine = "data.table",
    steps = list(),
    recipes = list(),
    workflows = list(),
    design = design_obj
  )

  result <- panel$get_design()
  expect_equal(result, design_obj)
})

test_that("extract_surveys with monthly parameter works", {
  implantation <- make_test_survey(20)
  implantation$periodicity <- "annual"
  implantation$edition <- as.Date("2023-01-01")

  # Create multiple monthly follow-ups
  follow_ups <- lapply(1:6, function(m) {
    fu <- make_test_survey(20)
    fu$periodicity <- "monthly"
    fu$edition <- as.Date(paste0("2023-", sprintf("%02d", m), "-01"))
    fu
  })

  panel <- RotativePanelSurvey$new(
    implantation = implantation,
    follow_up = follow_ups,
    type = "rotative",
    default_engine = "data.table",
    steps = list(),
    recipes = list(),
    workflows = list(),
    design = NULL
  )

  result <- extract_surveys(panel, monthly = c(1, 3, 6))
  expect_s3_class(result, "PoolSurvey")
  expect_true("monthly" %in% names(result$surveys))
})

test_that("extract_surveys with quarterly parameter works", {
  implantation <- make_test_survey(20)
  implantation$periodicity <- "annual"
  implantation$edition <- as.Date("2023-01-01")

  follow_ups <- lapply(1:12, function(m) {
    fu <- make_test_survey(20)
    fu$periodicity <- "monthly"
    fu$edition <- as.Date(paste0("2023-", sprintf("%02d", m), "-01"))
    fu
  })

  panel <- RotativePanelSurvey$new(
    implantation = implantation,
    follow_up = follow_ups,
    type = "rotative",
    default_engine = "data.table",
    steps = list(),
    recipes = list(),
    workflows = list(),
    design = NULL
  )

  result <- extract_surveys(panel, quarterly = c(1, 4))
  expect_s3_class(result, "PoolSurvey")
  expect_true("quarterly" %in% names(result$surveys))
})

test_that("extract_surveys with annual parameter works", {
  implantation <- make_test_survey(20)
  implantation$periodicity <- "annual"
  implantation$edition <- as.Date("2023-01-01")

  follow_ups <- lapply(1:12, function(m) {
    fu <- make_test_survey(20)
    fu$periodicity <- "monthly"
    fu$edition <- as.Date(paste0("2023-", sprintf("%02d", m), "-01"))
    fu
  })

  panel <- RotativePanelSurvey$new(
    implantation = implantation,
    follow_up = follow_ups,
    type = "rotative",
    default_engine = "data.table",
    steps = list(),
    recipes = list(),
    workflows = list(),
    design = NULL
  )

  result <- extract_surveys(panel, annual = 2023)
  expect_s3_class(result, "PoolSurvey")
  expect_true("annual" %in% names(result$surveys))
})

test_that("extract_surveys with biannual parameter works", {
  implantation <- make_test_survey(20)
  implantation$periodicity <- "annual"
  implantation$edition <- as.Date("2023-01-01")

  follow_ups <- lapply(1:12, function(m) {
    fu <- make_test_survey(20)
    fu$periodicity <- "monthly"
    fu$edition <- as.Date(paste0("2023-", sprintf("%02d", m), "-01"))
    fu
  })

  panel <- RotativePanelSurvey$new(
    implantation = implantation,
    follow_up = follow_ups,
    type = "rotative",
    default_engine = "data.table",
    steps = list(),
    recipes = list(),
    workflows = list(),
    design = NULL
  )

  result <- extract_surveys(panel, biannual = c(1, 2))
  expect_s3_class(result, "PoolSurvey")
  expect_true("biannual" %in% names(result$surveys))
})

test_that("PoolSurvey get_surveys with period works", {
  surveys <- list(
    monthly = list(
      January = list(make_test_survey()),
      February = list(make_test_survey())
    )
  )

  pool <- PoolSurvey$new(surveys)

  result <- pool$get_surveys("January")
  expect_type(result, "list")
})

test_that("PoolSurvey get_surveys without period returns all", {
  surveys <- list(
    monthly = list(January = list(make_test_survey()))
  )

  pool <- PoolSurvey$new(surveys)
  result <- pool$get_surveys()
  expect_equal(result, surveys)
})
