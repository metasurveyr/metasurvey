# Tests for Recipe system

test_that("Recipe$new() creates object with all fields", {
  r <- Recipe$new(
    name = "test recipe",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list(),
    user = "tester",
    description = "A test recipe",
    steps = list(),
    id = "test-001",
    doi = NULL,
    topic = "testing"
  )

  expect_s3_class(r, "Recipe")
  expect_equal(r$name, "test recipe")
  expect_equal(r$edition, "2023")
  expect_equal(r$survey_type, "ech")
  expect_equal(r$user, "tester")
  expect_equal(r$id, "test-001")
  expect_false(r$bake)
})

test_that("recipe() creates Recipe with required metadata", {
  svy <- Survey$new(
    data = data.table::data.table(id = 1:3, w = 1),
    edition = "2023", type = "ech",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )

  r <- recipe(
    name = "basic recipe",
    user = "tester",
    svy = svy,
    description = "Basic test recipe"
  )

  expect_s3_class(r, "Recipe")
  expect_equal(r$name, "basic recipe")
  expect_equal(r$user, "tester")
})

test_that("recipe() fails without required metadata", {
  expect_error(
    recipe(name = "incomplete")
  )
})

test_that("save_recipe writes JSON file", {
  svy <- Survey$new(
    data = data.table::data.table(id = 1:3, w = 1),
    edition = "2023", type = "ech",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )

  r <- recipe(
    name = "save test",
    user = "tester",
    svy = svy,
    description = "Testing save"
  )

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  expect_message(save_recipe(r, tmp), "saved")
  expect_true(file.exists(tmp))

  content <- jsonlite::read_json(tmp, simplifyVector = TRUE)
  expect_equal(content$name, "save test")
  expect_equal(content$user, "tester")
})

test_that("bake_recipes with empty recipes returns survey unchanged", {
  s <- make_test_survey()
  s$recipes <- list()
  s2 <- bake_recipes(s)
  expect_equal(nrow(get_data(s2)), nrow(get_data(s)))
})

test_that("recipe() with steps captures them", {
  svy <- Survey$new(
    data = data.table::data.table(id = 1:3, x = 1, w = 1),
    edition = "2023", type = "ech",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )

  r <- recipe(
    name = "with steps",
    user = "tester",
    svy = svy,
    description = "Recipe with inline steps",
    step_remove(svy, x)
  )

  expect_true(length(r$steps) > 0)
})

test_that("Recipe clone creates independent copy", {
  r <- Recipe$new(
    name = "original", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "Test", steps = list(),
    id = "r-001", doi = NULL, topic = NULL
  )

  r2 <- r$clone()
  r2$name <- "cloned"
  expect_equal(r$name, "original")
  expect_equal(r2$name, "cloned")
})
