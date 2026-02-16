# Tests for R/run_app.R — Shiny app launcher

test_that("explore_recipes finds app directory", {
  app_dir <- system.file("shiny", package = "metasurvey")
  expect_true(nzchar(app_dir))
  expect_true(dir.exists(app_dir))
})

test_that("explore_recipes calls shiny::runApp with correct args", {
  called_with <- list()
  local_mocked_bindings(
    runApp = function(appDir, port = NULL, host = "127.0.0.1", launch.browser = TRUE, ...) {
      called_with$appDir <<- appDir
      called_with$port <<- port
      called_with$host <<- host
      called_with$launch.browser <<- launch.browser
      invisible(NULL)
    },
    .package = "shiny"
  )

  explore_recipes(port = 4321, host = "0.0.0.0", launch.browser = FALSE)
  expect_true(nzchar(called_with$appDir))
  expect_equal(called_with$port, 4321)
  expect_equal(called_with$host, "0.0.0.0")
  expect_false(called_with$launch.browser)
})
