# Additional tests for steps to increase coverage

test_that("compute handles lazy evaluation", {
  svy <- make_test_survey()

  result <- metasurvey:::compute(
    svy,
    new_var = x + y,
    lazy = TRUE
  )

  # With lazy=TRUE, should return survey without modification
  expect_s3_class(result, "Survey")
})

test_that("compute handles non-lazy evaluation", {
  svy <- make_test_survey()

  result <- metasurvey:::compute(
    svy,
    new_var = x + y,
    lazy = FALSE,
    use_copy = TRUE
  )

  expect_s3_class(result, "Survey")
  expect_true("new_var" %in% names(get_data(result)))
})

test_that("compute handles grouped computations", {
  svy <- make_test_survey()

  result <- metasurvey:::compute(
    svy,
    mean_income = mean(income),
    .by = "region",
    lazy = FALSE,
    use_copy = TRUE
  )

  expect_s3_class(result, "Survey")
  expect_true("mean_income" %in% names(get_data(result)))
})

test_that("recode handles lazy evaluation", {
  svy <- make_test_survey()

  result <- metasurvey:::recode(
    svy,
    new_var = "test",
    age < 30 ~ "Young",
    lazy = TRUE
  )

  expect_s3_class(result, "Survey")
})

test_that("recode handles factor conversion", {
  svy <- make_test_survey()

  result <- metasurvey:::recode(
    svy,
    new_var = "age_cat",
    age < 30 ~ "Young",
    age >= 30 ~ "Old",
    .to_factor = TRUE,
    lazy = FALSE,
    use_copy = TRUE
  )

  expect_s3_class(result, "Survey")
  expect_true("age_cat" %in% names(get_data(result)))
  expect_true(is.factor(get_data(result)$age_cat))
})

test_that("recode handles ordered factors", {
  svy <- make_test_survey()

  result <- metasurvey:::recode(
    svy,
    new_var = "status_ord",
    status == 1 ~ "Low",
    status == 2 ~ "Medium",
    status == 3 ~ "High",
    .to_factor = TRUE,
    ordered = TRUE,
    lazy = FALSE,
    use_copy = TRUE
  )

  expect_s3_class(result, "Survey")
  expect_true(is.ordered(get_data(result)$status_ord))
})

test_that("recode handles default values", {
  svy <- make_test_survey()

  result <- metasurvey:::recode(
    svy,
    new_var = "test_default",
    age < 25 ~ "Young",
    .default = "Other",
    lazy = FALSE,
    use_copy = TRUE
  )

  expect_s3_class(result, "Survey")
  expect_true("Other" %in% get_data(result)$test_default)
})
