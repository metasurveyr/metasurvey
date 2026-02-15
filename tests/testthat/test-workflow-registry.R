test_that("WorkflowRegistry register and list", {
  reg <- WorkflowRegistry$new()
  expect_equal(length(reg$list_all()), 0)

  wf <- RecipeWorkflow$new(id = "wf1", name = "WF 1", survey_type = "ech")
  reg$register(wf)
  expect_equal(length(reg$list_all()), 1)

  wf2 <- RecipeWorkflow$new(id = "wf2", name = "WF 2", survey_type = "eaii")
  reg$register(wf2)
  expect_equal(length(reg$list_all()), 2)
})

test_that("WorkflowRegistry rejects non-RecipeWorkflow", {
  reg <- WorkflowRegistry$new()
  expect_error(reg$register(list(name = "fake")), "Can only register")
})

test_that("WorkflowRegistry unregister", {
  reg <- WorkflowRegistry$new()
  wf <- RecipeWorkflow$new(id = "wf1", name = "WF 1")
  reg$register(wf)
  expect_equal(length(reg$list_all()), 1)
  reg$unregister("wf1")
  expect_equal(length(reg$list_all()), 0)
})

test_that("WorkflowRegistry search", {
  reg <- WorkflowRegistry$new()
  reg$register(RecipeWorkflow$new(id = "1", name = "Labor Analysis"))
  reg$register(RecipeWorkflow$new(id = "2", name = "Income Distribution"))
  reg$register(RecipeWorkflow$new(id = "3", name = "Labor Force Participation"))

  results <- reg$search("labor")
  expect_equal(length(results), 2)

  results <- reg$search("income")
  expect_equal(length(results), 1)

  results <- reg$search("nonexistent")
  expect_equal(length(results), 0)
})

test_that("WorkflowRegistry filter by survey type", {
  reg <- WorkflowRegistry$new()
  reg$register(RecipeWorkflow$new(id = "1", name = "WF1", survey_type = "ech"))
  reg$register(RecipeWorkflow$new(id = "2", name = "WF2", survey_type = "eaii"))
  reg$register(RecipeWorkflow$new(id = "3", name = "WF3", survey_type = "ech"))

  results <- reg$filter(svy_type = "ech")
  expect_equal(length(results), 2)

  results <- reg$filter(svy_type = "eaii")
  expect_equal(length(results), 1)
})

test_that("WorkflowRegistry filter by recipe_id (find_by_recipe)", {
  reg <- WorkflowRegistry$new()
  reg$register(RecipeWorkflow$new(id = "1", name = "WF1", recipe_ids = c("r1", "r2")))
  reg$register(RecipeWorkflow$new(id = "2", name = "WF2", recipe_ids = c("r2", "r3")))
  reg$register(RecipeWorkflow$new(id = "3", name = "WF3", recipe_ids = c("r4")))

  results <- reg$find_by_recipe("r2")
  expect_equal(length(results), 2)

  results <- reg$find_by_recipe("r4")
  expect_equal(length(results), 1)

  results <- reg$find_by_recipe("r999")
  expect_equal(length(results), 0)
})

test_that("WorkflowRegistry rank_by_downloads", {
  reg <- WorkflowRegistry$new()
  reg$register(RecipeWorkflow$new(id = "1", name = "Low", downloads = 5L))
  reg$register(RecipeWorkflow$new(id = "2", name = "High", downloads = 100L))
  reg$register(RecipeWorkflow$new(id = "3", name = "Mid", downloads = 50L))

  ranked <- reg$rank_by_downloads()
  expect_equal(length(ranked), 3)
  expect_equal(ranked[[1]]$name, "High")
  expect_equal(ranked[[3]]$name, "Low")

  top2 <- reg$rank_by_downloads(n = 2)
  expect_equal(length(top2), 2)
})

test_that("WorkflowRegistry get by id", {
  reg <- WorkflowRegistry$new()
  reg$register(RecipeWorkflow$new(id = "wf_x", name = "Target WF"))

  wf <- reg$get("wf_x")
  expect_s3_class(wf, "RecipeWorkflow")
  expect_equal(wf$name, "Target WF")

  expect_null(reg$get("nonexistent"))
})

test_that("WorkflowRegistry save and load round-trip", {
  reg <- WorkflowRegistry$new()
  reg$register(RecipeWorkflow$new(
    id = "wf_sl", name = "Save Load WF", user = "Author",
    recipe_ids = c("r1"), estimation_type = "annual",
    downloads = 7L
  ))

  tmp <- tempfile(fileext = ".json")
  reg$save(tmp)
  expect_true(file.exists(tmp))

  reg2 <- WorkflowRegistry$new()
  reg2$load(tmp)
  expect_equal(length(reg2$list_all()), 1)
  loaded <- reg2$get("wf_sl")
  expect_equal(loaded$name, "Save Load WF")
  expect_equal(loaded$recipe_ids, "r1")
  expect_equal(loaded$downloads, 7L)

  unlink(tmp)
})

test_that("WorkflowRegistry stats", {
  reg <- WorkflowRegistry$new()
  reg$register(RecipeWorkflow$new(id = "1", name = "A", survey_type = "ech"))
  reg$register(RecipeWorkflow$new(id = "2", name = "B", survey_type = "ech"))
  reg$register(RecipeWorkflow$new(id = "3", name = "C", survey_type = "eaii"))

  s <- reg$stats()
  expect_equal(s$total, 3)
  expect_equal(s$by_survey_type[["ech"]], 2L)
  expect_equal(s$by_survey_type[["eaii"]], 1L)
  expect_equal(s$by_certification[["community"]], 3L)
})

test_that("WorkflowRegistry print produces output", {
  reg <- WorkflowRegistry$new()
  reg$register(RecipeWorkflow$new(id = "1", name = "A"))
  output <- capture.output(print(reg))
  expect_true(any(grepl("WorkflowRegistry", output)))
  expect_true(any(grepl("1 workflows", output)))
})

# --- WorkflowBackend tests ---

test_that("WorkflowBackend local publish and search", {
  backend <- WorkflowBackend$new("local")
  wf <- RecipeWorkflow$new(id = "b1", name = "Backend WF", description = "For testing")
  backend$publish(wf)

  results <- backend$search("backend")
  expect_equal(length(results), 1)

  expect_equal(backend$get("b1")$name, "Backend WF")
})

test_that("WorkflowBackend local find_by_recipe", {
  backend <- WorkflowBackend$new("local")
  backend$publish(RecipeWorkflow$new(id = "1", name = "A", recipe_ids = c("r1")))
  backend$publish(RecipeWorkflow$new(id = "2", name = "B", recipe_ids = c("r2")))

  results <- backend$find_by_recipe("r1")
  expect_equal(length(results), 1)
  expect_equal(results[[1]]$name, "A")
})

test_that("WorkflowBackend invalid type", {
  expect_error(WorkflowBackend$new("invalid"), "Backend type must be")
})

# --- Tidy API tests ---

test_that("search_workflows uses active backend", {
  old_opt <- getOption("metasurvey.workflow_backend")
  on.exit(options(metasurvey.workflow_backend = old_opt), add = TRUE)

  set_workflow_backend("local")
  backend <- get_workflow_backend()
  backend$publish(RecipeWorkflow$new(id = "t1", name = "Tidy Search WF"))

  # Need to re-set because publish was on a different ref
  options(metasurvey.workflow_backend = backend)
  results <- search_workflows("tidy")
  expect_equal(length(results), 1)
})

test_that("list_workflows returns all", {
  old_opt <- getOption("metasurvey.workflow_backend")
  on.exit(options(metasurvey.workflow_backend = old_opt), add = TRUE)

  set_workflow_backend("local")
  backend <- get_workflow_backend()
  backend$publish(RecipeWorkflow$new(id = "a", name = "A"))
  backend$publish(RecipeWorkflow$new(id = "b", name = "B"))
  options(metasurvey.workflow_backend = backend)

  all <- list_workflows()
  expect_equal(length(all), 2)
})

test_that("find_workflows_for_recipe cross-ref", {
  old_opt <- getOption("metasurvey.workflow_backend")
  on.exit(options(metasurvey.workflow_backend = old_opt), add = TRUE)

  set_workflow_backend("local")
  backend <- get_workflow_backend()
  backend$publish(RecipeWorkflow$new(id = "1", name = "A", recipe_ids = c("r1", "r2")))
  backend$publish(RecipeWorkflow$new(id = "2", name = "B", recipe_ids = c("r3")))
  options(metasurvey.workflow_backend = backend)

  results <- find_workflows_for_recipe("r1")
  expect_equal(length(results), 1)
  expect_equal(results[[1]]$name, "A")
})

test_that("publish_workflow via tidy API", {
  old_opt <- getOption("metasurvey.workflow_backend")
  on.exit(options(metasurvey.workflow_backend = old_opt), add = TRUE)

  set_workflow_backend("local")
  wf <- RecipeWorkflow$new(id = "pub1", name = "Published WF")
  publish_workflow(wf)

  results <- search_workflows("Published")
  expect_equal(length(results), 1)
})
