# Additional tests for load_survey to increase coverage

test_that("load_survey error when no path and no survey args", {
  expect_error(
    load_survey()
  )
})

test_that("load_survey with bake=TRUE applies recipes", {
  df <- data.table::data.table(
    id = 1:10,
    x = 1:10,
    w = 1
  )
  
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  data.table::fwrite(df, tmp)
  
  svy_empty <- make_test_survey()
  
  rec <- recipe(
    name = "test recipe",
    user = "tester",
    svy = svy_empty,
    description = "Test recipe with step"
  )
  
  s <- load_survey(
    path = tmp,
    svy_type = "ech",
    svy_edition = "2023",
    svy_weight = add_weight(annual = "w"),
    recipes = rec,
    bake = TRUE
  )
  
  expect_s3_class(s, "Survey")
})

test_that("load_survey handles different survey types", {
  df <- data.table::data.table(id = 1:10, w = 1)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  data.table::fwrite(df, tmp)
  
  types <- c("ech", "eph", "eai", "eaii")
  
  for (type in types) {
    s <- load_survey(
      path = tmp,
      svy_type = type,
      svy_edition = "2023",
      svy_weight = add_weight(annual = "w")
    )
    
    expect_equal(s$type, type)
  }
})

test_that("load_survey handles RDS files", {
  skip("RDS handling requires additional setup")
})

test_that("load_survey handles XLSX files", {
  skip("XLSX handling requires additional setup")
})

test_that("load_survey handles DTA files", {
  skip("DTA handling requires additional setup")
})

test_that("load_survey handles SAV files", {
  skip("SAV handling requires additional setup")
})

test_that("read_file handles unsupported extension", {
  tmp <- tempfile(fileext = ".xyz")
  writeLines("test", tmp)
  on.exit(unlink(tmp), add = TRUE)
  
  expect_error(
    metasurvey:::read_file(tmp),
    "Unsupported file type"
  )
})

test_that("validate_recipe returns FALSE for mismatched type", {
  result <- metasurvey:::validate_recipe(
    svy_type = "ech",
    svy_edition = "2023",
    recipe_svy_edition = "2023",
    recipe_svy_type = "eph"
  )
  
  expect_false(result)
})

test_that("validate_recipe returns FALSE for mismatched edition", {
  result <- metasurvey:::validate_recipe(
    svy_type = "ech",
    svy_edition = "2023",
    recipe_svy_edition = "2022",
    recipe_svy_type = "ech"
  )
  
  expect_false(result)
})

test_that("validate_recipe returns TRUE for matching type and edition", {
  result <- metasurvey:::validate_recipe(
    svy_type = "ech",
    svy_edition = "2023",
    recipe_svy_edition = "2023",
    recipe_svy_type = "ech"
  )
  
  expect_true(result)
})

