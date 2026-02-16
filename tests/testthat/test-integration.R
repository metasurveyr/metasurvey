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
