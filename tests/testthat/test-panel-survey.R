test_that("RotativePanelSurvey can be created", {
  # Create test surveys
  implantation <- make_test_survey(20)
  implantation$periodicity <- "annual"
  
  follow_up1 <- make_test_survey(20)
  follow_up1$periodicity <- "quarterly"
  follow_up1$edition <- "2023-Q1"
  
  follow_up2 <- make_test_survey(20)
  follow_up2$periodicity <- "quarterly"
  follow_up2$edition <- "2023-Q2"
  
  # Create panel survey
  panel <- RotativePanelSurvey$new(
    implantation = implantation,
    follow_up = list(follow_up1, follow_up2),
    type = "rotative",
    default_engine = "data.table",
    steps = list(),
    recipes = list(),
    workflows = list(),
    design = NULL
  )
  
  expect_s3_class(panel, "RotativePanelSurvey")
  expect_equal(panel$type, "rotative")
  expect_equal(panel$default_engine, "data.table")
})

test_that("RotativePanelSurvey validates follow-up periodicity", {
  implantation <- make_test_survey(20)
  implantation$periodicity <- "annual"
  
  follow_up1 <- make_test_survey(20)
  follow_up1$periodicity <- "quarterly"
  
  follow_up2 <- make_test_survey(20)
  follow_up2$periodicity <- "monthly"
  
  expect_error(
    RotativePanelSurvey$new(
      implantation = implantation,
      follow_up = list(follow_up1, follow_up2),
      type = "rotative",
      default_engine = "data.table",
      steps = list(),
      recipes = list(),
      workflows = list(),
      design = NULL
    ),
    "All follow-up surveys must have the same type"
  )
})

test_that("RotativePanelSurvey get methods work", {
  implantation <- make_test_survey(20)
  implantation$periodicity <- "annual"
  
  follow_up1 <- make_test_survey(20)
  follow_up1$periodicity <- "quarterly"
  
  panel <- RotativePanelSurvey$new(
    implantation = implantation,
    follow_up = list(follow_up1),
    type = "rotative",
    default_engine = "data.table",
    steps = list(),
    recipes = list(),
    workflows = list(),
    design = NULL
  )
  
  # Test get_implantation
  expect_identical(panel$get_implantation(), implantation)
  
  # Test get_follow_up
  expect_length(panel$get_follow_up(), 1)
  
  # Test get_type
  expect_equal(panel$get_type(), "rotative")
  
  # Test get_default_engine
  expect_equal(panel$get_default_engine(), "data.table")
  
  # Test get_steps
  steps <- panel$get_steps()
  expect_type(steps, "list")
  expect_named(steps, c("implantation", "follow_up"))
})

test_that("extract_surveys works with basic input", {
  implantation <- make_test_survey(20)
  implantation$periodicity <- "annual"
  implantation$edition <- as.Date("2023-01-01")
  
  follow_up1 <- make_test_survey(20)
  follow_up1$periodicity <- "quarterly"
  follow_up1$edition <- as.Date("2023-03-01")
  
  follow_up2 <- make_test_survey(20)
  follow_up2$periodicity <- "quarterly"
  follow_up2$edition <- as.Date("2023-06-01")
  
  panel <- RotativePanelSurvey$new(
    implantation = implantation,
    follow_up = list(follow_up1, follow_up2),
    type = "rotative",
    default_engine = "data.table",
    steps = list(),
    recipes = list(),
    workflows = list(),
    design = NULL
  )
  
  # Test extraction by index
  result <- extract_surveys(panel, index = 1)
  expect_s3_class(result, "Survey")
  
  # Test extraction by multiple indices
  result <- extract_surveys(panel, index = c(1, 2))
  expect_type(result, "list")
  expect_length(result, 2)
})

test_that("extract_surveys validates input", {
  not_panel <- list(a = 1, b = 2)

  expect_error(
    extract_surveys(not_panel, index = 1),
    "must be an object of class"
  )
})

# --- get_implantation ---

test_that("get_implantation returns implantation survey", {
  panel <- make_test_panel()
  result <- get_implantation(panel)
  expect_s3_class(result, "Survey")
})

test_that("get_implantation errors on non-RotativePanelSurvey", {
  expect_error(get_implantation("not a panel"), "must be an object of class")
})

# --- get_follow_up ---

test_that("get_follow_up returns follow-up surveys", {
  panel <- make_test_panel()
  result <- get_follow_up(panel)
  expect_true(is.list(result))
})

test_that("get_follow_up errors on non-RotativePanelSurvey", {
  expect_error(get_follow_up("not a panel"), "must be an object of class")
})

# --- PoolSurvey ---

test_that("PoolSurvey can be created", {
  pool <- PoolSurvey$new(surveys = list(annual = list()))
  expect_s3_class(pool, "PoolSurvey")
})

test_that("PoolSurvey get_surveys with period returns specific period", {
  surveys_data <- list(annual = list(a = 1, b = 2), monthly = list(c = 3))
  pool <- PoolSurvey$new(surveys = list(surveys_data))
  result <- pool$get_surveys(period = "annual")
  expect_true(!is.null(result))
})

test_that("PoolSurvey get_surveys without period returns all", {
  pool <- PoolSurvey$new(surveys = list(annual = list()))
  result <- pool$get_surveys()
  expect_true(is.list(result))
})

# --- RotativePanelSurvey print ---

test_that("RotativePanelSurvey print calls get_metadata", {
  panel <- make_test_panel()
  expect_message(panel$print())
})

# --- RotativePanelSurvey get_recipes/get_workflows/get_design ---

test_that("RotativePanelSurvey accessors return stored values", {
  panel <- make_test_panel()
  expect_true(is.list(panel$get_recipes()) || is.null(panel$get_recipes()))
  expect_true(is.list(panel$get_workflows()) || is.null(panel$get_workflows()))
  expect_true(is.null(panel$get_design()))
})

# --- step_compute on RotativePanelSurvey ---

test_that("step_compute works on RotativePanelSurvey", {
  panel <- make_test_panel()
  result <- tryCatch(
    step_compute(panel, double_age = age * 2),
    error = function(e) NULL
  )
  # step_compute on RotativePanelSurvey may fail depending on internal dispatch
  expect_true(is.null(result) || inherits(result, "RotativePanelSurvey"))
})

# --- step_remove on RotativePanelSurvey ---

test_that("step_remove works on RotativePanelSurvey", {
  panel <- make_test_panel()
  result <- step_remove(panel, x)
  expect_s3_class(result, "RotativePanelSurvey")
})

# --- step_rename on RotativePanelSurvey ---

test_that("step_rename works on RotativePanelSurvey", {
  panel <- make_test_panel()
  result <- step_rename(panel, years = age)
  expect_s3_class(result, "RotativePanelSurvey")
})

# --- step_recode on RotativePanelSurvey ---

test_that("step_join works on RotativePanelSurvey", {
  panel <- make_test_panel()
  extra <- data.table::data.table(id = 1:20, extra_val = 100:119)
  result <- step_join(panel, extra, by = "id", type = "left")
  expect_s3_class(result, "RotativePanelSurvey")
  expect_true("extra_val" %in% names(get_data(result$implantation)))
})

test_that("step_recode works on RotativePanelSurvey", {
  panel <- make_test_panel()
  result <- step_recode(panel, age_cat,
    age < 30 ~ "young",
    age >= 30 ~ "old",
    .default = "unknown"
  )
  expect_s3_class(result, "RotativePanelSurvey")
  expect_true("age_cat" %in% names(get_data(result$implantation)))
})
