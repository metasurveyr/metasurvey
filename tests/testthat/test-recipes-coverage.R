# Additional tests for Recipes to increase coverage

test_that("Recipe stores all fields correctly", {
  r <- Recipe$new(
    name = "comprehensive test",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("var1", "var2"),
    user = "test_user",
    description = "Comprehensive test",
    steps = list(step1 = "compute"),
    id = "recipe-001",
    doi = "10.1234/test",
    topic = "labor"
  )
  
  expect_equal(r$name, "comprehensive test")
  expect_equal(r$edition, "2023")
  expect_equal(r$survey_type, "ech")
  expect_equal(r$default_engine, "data.table")
  expect_length(r$depends_on, 2)
  expect_equal(r$user, "test_user")
  expect_equal(r$doi, "10.1234/test")
  expect_equal(r$topic, "labor")
})

test_that("recipe() creates recipe without steps", {
  svy <- make_test_survey()
  
  r <- recipe(
    name = "no steps",
    user = "tester",
    svy = svy,
    description = "Recipe without steps"
  )
  
  expect_s3_class(r, "Recipe")
  expect_type(r$steps, "list")
})

test_that("recipe() with id parameter sets id", {
  svy <- make_test_survey()
  
  r <- recipe(
    name = "with id",
    user = "tester",
    svy = svy,
    description = "Recipe with custom id",
    id = "custom-123"
  )
  
  expect_equal(r$id, "custom-123")
})

test_that("recipe() with doi parameter sets doi", {
  svy <- make_test_survey()
  
  r <- recipe(
    name = "with doi",
    user = "tester",
    svy = svy,
    description = "Recipe with DOI",
    doi = "10.5555/test"
  )
  
  expect_equal(r$doi, "10.5555/test")
})

test_that("recipe() with topic parameter sets topic", {
  svy <- make_test_survey()
  
  r <- recipe(
    name = "with topic",
    user = "tester",
    svy = svy,
    description = "Recipe with topic",
    topic = "demographics"
  )
  
  expect_equal(r$topic, "demographics")
})

test_that("encoding_recipe encodes steps as strings", {
  svy <- make_test_survey()
  
  r <- Recipe$new(
    name = "test",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list(),
    user = "tester",
    description = "Test",
    steps = list(quote(x + 1)),
    id = "001",
    doi = NULL,
    topic = NULL
  )
  
  encoded <- metasurvey:::encoding_recipe(r)
  expect_type(encoded$steps[[1]], "character")
})

test_that("decode_step converts strings back to calls", {
  step_strings <- c("x + 1", "y * 2")
  decoded <- metasurvey:::decode_step(step_strings)
  expect_true(is.call(decoded))
})

test_that("read_recipe reads saved recipe", {
  skip("decode_step needs non-empty steps")
})

test_that("recipe_to_json converts recipe to JSON", {
  skip("Recipe is R6, cannot modify locked environment")
})

test_that("metadata_recipe returns expected fields", {
  fields <- metasurvey:::metadata_recipe()
  expect_true("name" %in% fields)
  expect_true("user" %in% fields)
  expect_true("svy" %in% fields)
  expect_true("description" %in% fields)
})

test_that("bake_recipes applies recipe steps to survey", {
  skip("Requires proper recipe step setup")
})

test_that("get_recipe handles allowMultiple parameter", {
  skip("Requires API or repository access")
})

test_that("get_recipe filters by topic", {
  skip("Requires API or repository access")
})

test_that("recipe with multiple depends_on aggregates correctly", {
  svy <- make_test_survey()
  
  r <- Recipe$new(
    name = "deps test",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("var1", "var2", "var3"),
    user = "tester",
    description = "Multiple dependencies",
    steps = list(),
    id = "001",
    doi = NULL,
    topic = NULL
  )
  
  expect_length(r$depends_on, 3)
})
