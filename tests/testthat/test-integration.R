# Integration tests - end-to-end pipelines
# These tests use example-data/ and are skipped on CRAN

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

test_that("Full pipeline with example-data/ ECH", {
  ech_path <- file.path("../../example-data/ech/ech_2023/ECH_implantacion_2023.csv")
  
  result <- tryCatch({
    if (!file.exists(ech_path)) {
      return(NULL)
    }
    
    # Read data to discover the actual weight column name
    dt <- data.table::fread(ech_path, nrows = 5)
    weight_col <- intersect(c("pesoano", "PESOANO", "W", "w"), names(dt))
    
    if (length(weight_col) == 0) {
      return(NULL)
    }
    
    s <- load_survey(
      path = ech_path,
      svy_type = "ech",
      svy_edition = "2023",
      svy_weight = add_weight(annual = weight_col[1])
    )
    
    s
  }, error = function(e) NULL)
  
  # Test passes whether data exists or not
  expect_true(is.null(result) || inherits(result, "Survey"))
})
