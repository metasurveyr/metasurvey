# Additional tests for steps to increase coverage

test_that("compute_with_ast handles empty ast_expressions", {
  svy <- make_test_survey()
  
  result <- metasurvey:::compute_with_ast(
    svy,
    ast_expressions = NULL,
    .by = NULL,
    use_copy = TRUE
  )
  
  expect_s3_class(result, "Survey")
})

test_that("compute_with_ast handles grouped operations", {
  skip("Grouped AST operations need special handling")
})

test_that("compute_with_ast handles AST evaluation errors", {
  svy <- make_test_survey()
  
  # Invalid AST expression that should fail
  ast_expr <- list(
    bad_var = metasurvey:::parse_ast(quote(nonexistent_column + 1))
  )
  
  expect_error(
    metasurvey:::compute_with_ast(
      svy,
      ast_expressions = ast_expr,
      use_copy = TRUE
    )
  )
})

test_that("recode_with_ast handles multiple conditions", {
  svy <- make_test_survey()
  
  conditions <- list(
    age < 30 ~ "Young",
    age >= 30 & age < 50 ~ "Middle",
    age >= 50 ~ "Senior"
  )
  
  result <- metasurvey:::recode_with_ast(
    svy,
    new_var = "age_group",
    conditions = conditions,
    default_value = "Unknown"
  )
  
  expect_type(result, "character")
  expect_true(all(result %in% c("Young", "Middle", "Senior", "Unknown")))
})

test_that("recode_with_ast handles default values", {
  svy <- make_test_survey()
  
  conditions <- list(
    age < 25 ~ "Young"
  )
  
  result <- metasurvey:::recode_with_ast(
    svy,
    new_var = "category",
    conditions = conditions,
    default_value = "Other"
  )
  
  expect_true("Other" %in% result)
})

test_that("recode_with_ast handles AST optimization", {
  svy <- make_test_survey()
  
  conditions <- list(
    income > 2500 ~ "High",
    income <= 2500 ~ "Low"
  )
  
  result <- metasurvey:::recode_with_ast(
    svy,
    new_var = "income_cat",
    conditions = conditions,
    ast_params = list(optimize_ast = TRUE)
  )
  
  expect_type(result, "character")
})

test_that("recode_with_ast validates dependencies when requested", {
  svy <- make_test_survey()
  
  conditions <- list(
    nonexistent_var > 10 ~ "High"
  )
  
  expect_error(
    metasurvey:::recode_with_ast(
      svy,
      new_var = "test",
      conditions = conditions,
      ast_params = list(validate_deps = TRUE)
    ),
    "Missing variables"
  )
})

test_that("recode_with_ast_survey handles to_factor conversion", {
  svy <- make_test_survey()
  
  conditions <- list(
    region == 1 ~ "North",
    region == 2 ~ "South",
    region == 3 ~ "East",
    region == 4 ~ "West"
  )
  
  result <- metasurvey:::recode_with_ast_survey(
    svy,
    new_var = "region_name",
    conditions = conditions,
    to_factor = TRUE,
    use_copy = TRUE
  )
  
  expect_s3_class(result, "Survey")
  expect_true("region_name" %in% names(get_data(result)))
  expect_true(is.factor(get_data(result)$region_name))
})

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
