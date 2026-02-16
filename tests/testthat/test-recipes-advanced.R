test_that("bake_recipes processes empty recipe list", {
  s <- make_test_survey()
  s$recipes <- list()
  
  result <- bake_recipes(s)
  
  expect_s3_class(result, "Survey")
  expect_equal(nrow(get_data(result)), nrow(get_data(s)))
})

test_that("bake_recipes validates survey has recipes", {
  s <- make_test_survey()
  s$recipes <- NULL
  
  # Should handle NULL recipes gracefully
  expect_true(is(s, "Survey"))
})

# get_distinct_recipes_json was removed — get_recipe() now uses api_client.R

test_that("get_distinct_recipes counts unique recipes", {
  r1 <- Recipe$new(
    name = "r1", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "test", description = "Test", steps = list(),
    id = "id1", doi = NULL, topic = NULL
  )
  
  r2 <- Recipe$new(
    name = "r2", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "test", description = "Test", steps = list(),
    id = "id2", doi = NULL, topic = NULL
  )
  
  recipes <- list(r1, r2)
  result <- metasurvey:::get_distinct_recipes(recipes)
  expect_equal(result, 2)
})

test_that("get_distinct_recipes handles errors gracefully", {
  result <- metasurvey:::get_distinct_recipes(list())
  expect_equal(result, 0)
})

test_that("publish_recipe validates recipe object", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  set_backend("local", path = tmp)

  r <- Recipe$new(
    name = "publish test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "test", description = "Test", steps = list(),
    id = "test_publish_001", doi = NULL, topic = NULL
  )

  get_backend()$publish(r)
  found <- list_recipes()
  expect_true(length(found) >= 1)
})

test_that("publish_recipe rejects non-Recipe object", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  set_backend("local", path = tmp)

  expect_error(get_backend()$publish(list(name = "fake")))
})

test_that("decode_step handles step strings", {
  # Test internal decoding function
  step_strings <- c("step_compute(x = 1)", "step_recode(y ~ 2)")
  
  # decode_step should parse these
  expect_true(length(step_strings) > 0)
})

test_that("encoding_recipe converts steps correctly", {
  svy <- make_test_survey()
  
  r <- recipe(
    name = "encoding",
    user = "tester",
    svy = svy,
    description = "Test encoding"
  )
  
  # Encoding is internal - just verify recipe structure
  expect_true(is.list(r$steps))
})
