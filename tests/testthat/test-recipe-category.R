test_that("RecipeCategory creates with name and description", {
  cat <- RecipeCategory$new(name = "labor_market", description = "Labor market indicators")
  expect_s3_class(cat, "RecipeCategory")
  expect_equal(cat$name, "labor_market")
  expect_equal(cat$description, "Labor market indicators")
  expect_null(cat$parent)
})

test_that("RecipeCategory creates with parent hierarchy", {
  economics <- RecipeCategory$new(name = "economics", description = "Economic indicators")
  labor <- RecipeCategory$new(name = "labor_market", description = "Labor market", parent = economics)
  expect_equal(labor$parent$name, "economics")
})

test_that("RecipeCategory validates name is non-empty string", {
  expect_error(RecipeCategory$new(name = "", description = "test"))
  expect_error(RecipeCategory$new(name = NULL, description = "test"))
  expect_error(RecipeCategory$new(name = 123, description = "test"))
})

test_that("RecipeCategory validates parent is RecipeCategory or NULL", {
  expect_error(RecipeCategory$new(name = "test", description = "test", parent = "not_a_category"))
  expect_error(RecipeCategory$new(name = "test", description = "test", parent = list()))
})

test_that("is_subcategory_of checks hierarchy", {
  economics <- RecipeCategory$new(name = "economics", description = "Economic indicators")
  labor <- RecipeCategory$new(name = "labor_market", description = "Labor market", parent = economics)
  employment <- RecipeCategory$new(name = "employment", description = "Employment stats", parent = labor)

  expect_true(labor$is_subcategory_of("economics"))
  expect_true(employment$is_subcategory_of("labor_market"))
  expect_true(employment$is_subcategory_of("economics"))
  expect_false(economics$is_subcategory_of("labor_market"))
  expect_false(labor$is_subcategory_of("employment"))
})

test_that("get_path returns full category path", {
  economics <- RecipeCategory$new(name = "economics", description = "Economic indicators")
  labor <- RecipeCategory$new(name = "labor_market", description = "Labor market", parent = economics)
  employment <- RecipeCategory$new(name = "employment", description = "Employment stats", parent = labor)

  expect_equal(economics$get_path(), "economics")
  expect_equal(labor$get_path(), "economics/labor_market")
  expect_equal(employment$get_path(), "economics/labor_market/employment")
})

test_that("default_categories returns built-in taxonomy", {
  cats <- default_categories()
  expect_type(cats, "list")
  expect_true(length(cats) >= 6)
  names_list <- vapply(cats, function(c) c$name, character(1))
  expect_true("labor_market" %in% names_list)
  expect_true("income" %in% names_list)
  expect_true("education" %in% names_list)
  expect_true("health" %in% names_list)
  expect_true("demographics" %in% names_list)
  expect_true("housing" %in% names_list)
  # All should be RecipeCategory objects
  for (cat in cats) {
    expect_s3_class(cat, "RecipeCategory")
  }
})

test_that("RecipeCategory equality by name", {
  cat1 <- RecipeCategory$new(name = "labor_market", description = "v1")
  cat2 <- RecipeCategory$new(name = "labor_market", description = "v2")
  cat3 <- RecipeCategory$new(name = "income", description = "v1")
  expect_true(cat1$equals(cat2))
  expect_false(cat1$equals(cat3))
})

test_that("to_list serialization", {
  economics <- RecipeCategory$new(name = "economics", description = "Economic indicators")
  labor <- RecipeCategory$new(name = "labor_market", description = "Labor market", parent = economics)

  lst <- labor$to_list()
  expect_type(lst, "list")
  expect_equal(lst$name, "labor_market")
  expect_equal(lst$description, "Labor market")
  expect_equal(lst$parent$name, "economics")
})

test_that("from_list deserialization", {
  lst <- list(
    name = "labor_market",
    description = "Labor market",
    parent = list(name = "economics", description = "Economic indicators", parent = NULL)
  )
  cat <- RecipeCategory$from_list(lst)
  expect_s3_class(cat, "RecipeCategory")
  expect_equal(cat$name, "labor_market")
  expect_equal(cat$parent$name, "economics")
})

test_that("to_list/from_list round-trip", {
  economics <- RecipeCategory$new(name = "economics", description = "Economic indicators")
  labor <- RecipeCategory$new(name = "labor_market", description = "Labor market", parent = economics)
  employment <- RecipeCategory$new(name = "employment", description = "Employment stats", parent = labor)

  restored <- RecipeCategory$from_list(employment$to_list())
  expect_equal(restored$name, "employment")
  expect_equal(restored$get_path(), "economics/labor_market/employment")
})

test_that("print method works", {
  cat <- RecipeCategory$new(name = "labor_market", description = "Labor market indicators")
  expect_output(print(cat), "labor_market")
})

test_that("from_list with NULL returns NULL", {
  expect_null(RecipeCategory$from_list(NULL))
})

test_that("description can be empty string", {
  cat <- RecipeCategory$new(name = "test", description = "")
  expect_equal(cat$description, "")
})

# ── Batch 10: RecipeCategory print with description, from_list edges ──────────

test_that("RecipeCategory print shows description when non-empty", {
  cat <- RecipeCategory$new(name = "labor", description = "Labor indicators")
  expect_output(print(cat), "Labor indicators")
})

test_that("RecipeCategory print shows path when parent exists", {
  parent <- RecipeCategory$new(name = "economics", description = "Econ")
  child <- RecipeCategory$new(name = "labor", description = "Labor", parent = parent)
  expect_output(print(child), "economics/labor")
})

test_that("from_list handles empty data.frame (JSON edge case)", {
  result <- RecipeCategory$from_list(data.frame())
  expect_null(result)
})

test_that("from_list handles empty list (JSON edge case)", {
  result <- RecipeCategory$from_list(list())
  expect_null(result)
})

test_that("from_list with deeply nested parent chain", {
  lst <- list(
    name = "employment",
    description = "Employment",
    parent = list(
      name = "labor",
      description = "Labor",
      parent = list(
        name = "economics",
        description = "Economics",
        parent = NULL
      )
    )
  )
  cat <- RecipeCategory$from_list(lst)
  expect_equal(cat$get_path(), "economics/labor/employment")
})

test_that("equals returns FALSE for non-RecipeCategory", {
  cat <- RecipeCategory$new(name = "test", description = "test")
  expect_false(cat$equals("not_a_category"))
  expect_false(cat$equals(list(name = "test")))
})
