# Tests for Survey R6 class and related functions

test_that("Survey$new() creates object with correct fields", {
  df <- data.table::data.table(id = 1:5, x = 10:14, w = 1)
  s <- Survey$new(
    data = df, edition = "2023", type = "ech",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )

  expect_s3_class(s, "Survey")
  expect_equal(nrow(s$data), 5)
  expect_equal(s$type, "ech")
  expect_equal(s$default_engine, "data.table")
  expect_true(length(s$design) > 0)
})

test_that("get_data() returns underlying data", {
  s <- make_test_survey()
  d <- get_data(s)
  expect_true(data.table::is.data.table(d))
  expect_equal(nrow(d), 10)
})

test_that("set_data() replaces data", {
  s <- make_test_survey()
  new_df <- data.table::data.table(id = 1:3, w = 1)
  s$set_data(new_df)
  expect_equal(nrow(get_data(s)), 3)
})

test_that("get/set_edition round-trip", {
  s <- make_test_survey()
  s$set_edition("2024")
  expect_equal(s$get_edition(), "2024")
})

test_that("get/set_type round-trip", {
  s <- make_test_survey()
  s$set_type("eph")
  expect_equal(s$get_type(), "eph")
})

test_that("survey_to_data_frame returns data.frame", {
  s <- make_test_survey()
  df <- survey_to_data_frame(s)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 10)
})

test_that("survey_to_data.table returns data.table", {
  s <- make_test_survey()
  dt <- survey_to_data.table(s)
  expect_true(data.table::is.data.table(dt))
})

test_that("get_metadata does not error on Survey", {
  s <- make_test_survey()
  expect_message(get_metadata(s))
})

test_that("add_step registers steps correctly", {
  s <- make_test_survey()
  step <- Step$new(
    name = "test step", edition = "2023", survey_type = "ech",
    type = "compute", new_var = "z", exprs = list(),
    call = NULL, svy_before = NULL, default_engine = "data.table",
    depends_on = list()
  )
  s$add_step(step)
  expect_equal(length(s$steps), 1)
})

test_that("Survey design is created from weight", {
  s <- make_test_survey()
  expect_true(length(s$design) >= 1)
  expect_true(inherits(s$design[[1]], "survey.design"))
})

test_that("cat_design returns string without error", {
  s <- make_test_survey()
  result <- cat_design(s)
  expect_true(is.character(result) || inherits(result, "glue"))
})

test_that("cat_recipes returns 'None' for survey without recipes", {
  s <- make_test_survey()
  result <- cat_recipes(s)
  expect_equal(result, "None")
})

test_that("get_steps returns empty list for new survey", {
  s <- make_test_survey()
  expect_equal(length(get_steps(s)), 0)
})
