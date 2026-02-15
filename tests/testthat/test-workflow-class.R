test_that("RecipeWorkflow creation with defaults", {
  wf <- RecipeWorkflow$new(name = "Test Workflow")
  expect_s3_class(wf, "RecipeWorkflow")
  expect_s3_class(wf, "R6")
  expect_equal(wf$name, "Test Workflow")
  expect_equal(wf$user, "Unknown")
  expect_equal(wf$description, "")
  expect_equal(wf$downloads, 0L)
  expect_equal(wf$version, "1.0.0")
  expect_equal(wf$certification$level, "community")
  expect_equal(wf$recipe_ids, character(0))
  expect_equal(wf$estimation_type, character(0))
  expect_true(nzchar(wf$id))
  expect_true(nzchar(wf$created_at))
})

test_that("RecipeWorkflow creation with all fields", {
  wf <- RecipeWorkflow$new(
    id = "wf_001",
    name = "Labor Market Analysis",
    description = "Full labor market estimation",
    user = "Test User",
    survey_type = "ech",
    edition = "2023",
    estimation_type = c("annual", "quarterly"),
    recipe_ids = c("recipe_001", "recipe_002"),
    calls = list("svymean(~employed, design = design, na.rm = TRUE)"),
    call_metadata = list(
      list(type = "svymean", formula = "~employed", description = "Mean employment")
    ),
    downloads = 42L,
    version = "2.0.0",
    doi = "10.1234/test"
  )

  expect_equal(wf$id, "wf_001")
  expect_equal(wf$name, "Labor Market Analysis")
  expect_equal(wf$survey_type, "ech")
  expect_equal(wf$estimation_type, c("annual", "quarterly"))
  expect_equal(wf$recipe_ids, c("recipe_001", "recipe_002"))
  expect_equal(length(wf$calls), 1)
  expect_equal(length(wf$call_metadata), 1)
  expect_equal(wf$downloads, 42L)
  expect_equal(wf$doi, "10.1234/test")
})

test_that("RecipeWorkflow$doc() returns correct structure", {
  wf <- RecipeWorkflow$new(
    name = "Test WF",
    user = "Tester",
    survey_type = "ech",
    edition = "2023",
    estimation_type = "annual",
    recipe_ids = c("r1", "r2"),
    call_metadata = list(
      list(type = "svymean", formula = "~x", description = "Mean of x")
    )
  )

  doc <- wf$doc()
  expect_type(doc, "list")
  expect_named(doc, c("meta", "recipe_ids", "estimations", "estimation_types"))
  expect_equal(doc$meta$name, "Test WF")
  expect_equal(doc$meta$user, "Tester")
  expect_equal(doc$recipe_ids, c("r1", "r2"))
  expect_equal(length(doc$estimations), 1)
  expect_equal(doc$estimation_types, "annual")
})

test_that("RecipeWorkflow$to_list() and workflow_from_list() round-trip", {
  wf <- RecipeWorkflow$new(
    id = "wf_rt",
    name = "Roundtrip WF",
    description = "Test roundtrip",
    user = "Author",
    survey_type = "ech",
    edition = "2023",
    estimation_type = c("annual"),
    recipe_ids = c("recipe_001"),
    calls = list("svymean(~x, design)"),
    call_metadata = list(
      list(type = "svymean", formula = "~x", description = "Mean x")
    ),
    downloads = 10L,
    version = "1.1.0",
    doi = "10.5678/rt"
  )

  lst <- wf$to_list()
  expect_type(lst, "list")
  expect_equal(lst$name, "Roundtrip WF")
  expect_equal(lst$recipe_ids, list("recipe_001"))

  # Reconstruct
  wf2 <- workflow_from_list(lst)
  expect_s3_class(wf2, "RecipeWorkflow")
  expect_equal(wf2$name, wf$name)
  expect_equal(wf2$user, wf$user)
  expect_equal(wf2$recipe_ids, wf$recipe_ids)
  expect_equal(wf2$estimation_type, wf$estimation_type)
  expect_equal(wf2$downloads, wf$downloads)
  expect_equal(wf2$doi, wf$doi)
  expect_equal(wf2$version, wf$version)
})

test_that("save_workflow() and read_workflow() round-trip", {
  wf <- RecipeWorkflow$new(
    id = "wf_save",
    name = "Save Test WF",
    description = "Testing save/read",
    user = "Saver",
    survey_type = "eaii",
    edition = "2021",
    estimation_type = "annual",
    recipe_ids = c("r1", "r2"),
    calls = list("svymean(~y, design, na.rm = TRUE)"),
    call_metadata = list(
      list(type = "svymean", formula = "~y", description = "Mean y")
    ),
    doi = "10.1234/save"
  )

  tmp_file <- tempfile(fileext = ".json")
  save_workflow(wf, tmp_file)
  expect_true(file.exists(tmp_file))

  # Read back
  wf2 <- read_workflow(tmp_file)
  expect_s3_class(wf2, "RecipeWorkflow")
  expect_equal(wf2$name, "Save Test WF")
  expect_equal(wf2$user, "Saver")
  expect_equal(wf2$recipe_ids, c("r1", "r2"))
  expect_equal(wf2$doi, "10.1234/save")
  expect_equal(wf2$estimation_type, "annual")
  expect_equal(length(wf2$call_metadata), 1)

  unlink(tmp_file)
})

test_that("RecipeWorkflow$increment_downloads() works", {
  wf <- RecipeWorkflow$new(name = "DL Test")
  expect_equal(wf$downloads, 0L)
  wf$increment_downloads()
  expect_equal(wf$downloads, 1L)
  wf$increment_downloads()
  expect_equal(wf$downloads, 2L)
})

test_that("RecipeWorkflow$certify() works", {
  wf <- RecipeWorkflow$new(name = "Cert Test")
  expect_equal(wf$certification$level, "community")

  user <- RecipeUser$new(name = "IECON", user_type = "institution")
  wf$certify(user, "official")
  expect_equal(wf$certification$level, "official")
})

test_that("RecipeWorkflow add/remove category works", {
  wf <- RecipeWorkflow$new(name = "Cat Test")
  expect_length(wf$categories, 0)

  cat1 <- RecipeCategory$new(name = "labor", description = "Labor")
  wf$add_category(cat1)
  expect_length(wf$categories, 1)

  # No duplicate

  wf$add_category(cat1)
  expect_length(wf$categories, 1)

  cat2 <- RecipeCategory$new(name = "income", description = "Income")
  wf$add_category(cat2)
  expect_length(wf$categories, 2)

  wf$remove_category("labor")
  expect_length(wf$categories, 1)
  expect_equal(wf$categories[[1]]$name, "income")
})

test_that("print.RecipeWorkflow produces output", {
  wf <- RecipeWorkflow$new(
    name = "Print WF",
    user = "Printer",
    survey_type = "ech",
    edition = "2023",
    estimation_type = "annual",
    recipe_ids = c("r1"),
    call_metadata = list(
      list(type = "svymean", formula = "~x", description = "Mean x")
    )
  )

  output <- capture.output(print(wf))
  expect_true(any(grepl("Print WF", output)))
  expect_true(any(grepl("Printer", output)))
  expect_true(any(grepl("Recipes", output)))
  expect_true(any(grepl("Estimations", output)))
  expect_true(any(grepl("svymean", output)))
})

test_that("save_workflow rejects non-RecipeWorkflow objects", {
  expect_error(save_workflow(list(name = "fake"), tempfile()), "Can only save RecipeWorkflow")
})

test_that("workflow_from_list handles minimal input", {
  wf <- workflow_from_list(list(name = "Minimal"))
  expect_s3_class(wf, "RecipeWorkflow")
  expect_equal(wf$name, "Minimal")
  expect_equal(wf$user, "Unknown")
  expect_equal(wf$recipe_ids, character(0))
})
