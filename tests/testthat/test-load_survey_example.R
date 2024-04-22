# tests/testthat/test-load_survey_example.R
library(testthat)
library(metaSurvey)

test_that("load_survey_example works correctly", {
  # Test that the function returns the correct files when no path is provided
  testthat::expect_type(load_survey_example(), "character")

  # Test that the function returns the correct file when a path is provided
  testthat::expect_equal(
    load_survey_example("2019-2021.csv"),
    system.file("extdata", "2019-2021.csv", package = "metasurvey", mustWork = TRUE)
  )

  # Test that the function throws an error when a non-existent path is provided
  testthat::expect_error(load_survey_example("non_existent_file.csv"))
})
