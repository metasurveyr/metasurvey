# Helper to create test recipes
make_api_recipe <- function(name, user = "tester", downloads = 0L, topic = NULL) {
  Recipe$new(
    name = name, edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = user, description = paste("API test:", name),
    steps = list(), id = stats::runif(1), doi = NULL,
    topic = topic, downloads = as.integer(downloads)
  )
}

# --- Local Backend Tests ---

test_that("RecipeBackend creates local backend", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  backend <- RecipeBackend$new("local", path = tmp)
  expect_s3_class(backend, "RecipeBackend")
  expect_equal(backend$type, "local")
})

test_that("RecipeBackend creates api backend (mongo alias)", {
  backend <- RecipeBackend$new("mongo")
  expect_s3_class(backend, "RecipeBackend")
  expect_equal(backend$type, "api")

  backend2 <- RecipeBackend$new("api")
  expect_equal(backend2$type, "api")
})

test_that("invalid backend type throws error", {
  expect_error(RecipeBackend$new("postgres"))
})

test_that("local backend: publish writes to registry", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  backend <- RecipeBackend$new("local", path = tmp)
  r <- make_api_recipe("Test Publish")
  backend$publish(r)

  all <- backend$list_all()
  expect_equal(length(all), 1)
  expect_equal(all[[1]]$name, "Test Publish")
})

test_that("local backend: search delegates to registry", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  backend <- RecipeBackend$new("local", path = tmp)
  backend$publish(make_api_recipe("Labor Analysis"))
  backend$publish(make_api_recipe("Income Study"))

  results <- backend$search("labor")
  expect_equal(length(results), 1)
  expect_equal(results[[1]]$name, "Labor Analysis")
})

test_that("local backend: get retrieves by id", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  backend <- RecipeBackend$new("local", path = tmp)
  r <- make_api_recipe("Target")
  backend$publish(r)

  found <- backend$get(r$id)
  expect_equal(found$name, "Target")
})

test_that("local backend: get returns NULL for unknown id", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  backend <- RecipeBackend$new("local", path = tmp)
  expect_null(backend$get("nonexistent"))
})

test_that("local backend: increment_downloads updates count", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  backend <- RecipeBackend$new("local", path = tmp)
  r <- make_api_recipe("Downloadable")
  backend$publish(r)
  backend$increment_downloads(r$id)
  backend$increment_downloads(r$id)

  found <- backend$get(r$id)
  expect_equal(found$downloads, 2L)
})

test_that("local backend: rank by downloads", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  backend <- RecipeBackend$new("local", path = tmp)
  backend$publish(make_api_recipe("Low", downloads = 5))
  backend$publish(make_api_recipe("High", downloads = 100))
  backend$publish(make_api_recipe("Mid", downloads = 50))

  ranked <- backend$rank(n = 2)
  expect_equal(length(ranked), 2)
  expect_equal(ranked[[1]]$name, "High")
})

test_that("local backend: filter works", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  backend <- RecipeBackend$new("local", path = tmp)
  backend$publish(make_api_recipe("A", topic = "labor"))
  backend$publish(make_api_recipe("B", topic = "income"))

  results <- backend$filter(svy_type = "ech")
  expect_equal(length(results), 2)
})

test_that("local backend: persistence across instances", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  # First instance
  b1 <- RecipeBackend$new("local", path = tmp)
  b1$publish(make_api_recipe("Persistent"))
  b1$save()

  # Second instance
  b2 <- RecipeBackend$new("local", path = tmp)
  b2$load()
  expect_equal(length(b2$list_all()), 1)
  expect_equal(b2$list_all()[[1]]$name, "Persistent")
})

# --- Mongo Backend Tests (mocked) ---

test_that("mongo backend: publish calls API", {
  skip_if_not(FALSE, "Requires API access - tested as integration")
})

test_that("mongo backend: search calls API", {
  skip_if_not(FALSE, "Requires API access - tested as integration")
})

# --- set_backend / get_backend ---

test_that("set_backend and get_backend work with local", {
  tmp <- tempfile(fileext = ".json")
  on.exit({
    unlink(tmp)
    options(metasurvey.backend = NULL)
  })
  set_backend("local", path = tmp)
  b <- get_backend()
  expect_s3_class(b, "RecipeBackend")
  expect_equal(b$type, "local")
})

test_that("set_backend works with mongo (alias for api)", {
  old <- getOption("metasurvey.backend")
  on.exit(options(metasurvey.backend = old))
  set_backend("mongo")
  b <- get_backend()
  expect_equal(b$type, "api")
})

test_that("get_backend returns default local when not configured", {
  old <- getOption("metasurvey.backend")
  on.exit(options(metasurvey.backend = old))
  options(metasurvey.backend = NULL)
  b <- get_backend()
  expect_equal(b$type, "local")
})
