# --- topo_sort_recipes tests ---

test_that("topo_sort_recipes: empty list returns empty", {
  expect_equal(topo_sort_recipes(list()), list())
})

test_that("topo_sort_recipes: single recipe returns as-is", {
  r <- make_eco_recipe("A", "u")
  result <- topo_sort_recipes(list(r))
  expect_equal(length(result), 1)
  expect_equal(result[[1]]$name, "A")
})

test_that("topo_sort_recipes: respects dependency order", {
  r1 <- make_eco_recipe("Base", "u")
  r2 <- make_eco_recipe("Derived", "u")
  r2$depends_on_recipes <- list(as.character(r1$id))

  result <- topo_sort_recipes(list(r2, r1))
  expect_equal(result[[1]]$name, "Base")
  expect_equal(result[[2]]$name, "Derived")
})

test_that("topo_sort_recipes: diamond dependency", {
  #    A
  #   / \
  #  B   C
  #   \ /
  #    D
  rA <- make_eco_recipe("A", "u")
  rB <- make_eco_recipe("B", "u")
  rC <- make_eco_recipe("C", "u")
  rD <- make_eco_recipe("D", "u")
  rB$depends_on_recipes <- list(as.character(rA$id))
  rC$depends_on_recipes <- list(as.character(rA$id))
  rD$depends_on_recipes <- list(as.character(rB$id), as.character(rC$id))

  result <- topo_sort_recipes(list(rD, rC, rB, rA))
  names_order <- vapply(result, function(r) r$name, character(1))
  # A must come before B and C; B and C must come before D
  expect_true(which(names_order == "A") < which(names_order == "B"))
  expect_true(which(names_order == "A") < which(names_order == "C"))
  expect_true(which(names_order == "B") < which(names_order == "D"))
  expect_true(which(names_order == "C") < which(names_order == "D"))
})

test_that("topo_sort_recipes: cycle detection", {
  r1 <- make_eco_recipe("X", "u")
  r2 <- make_eco_recipe("Y", "u")
  r1$depends_on_recipes <- list(as.character(r2$id))
  r2$depends_on_recipes <- list(as.character(r1$id))

  expect_error(
    topo_sort_recipes(list(r1, r2)),
    "Cycle detected"
  )
})

test_that("topo_sort_recipes: external deps are ignored", {
  r1 <- make_eco_recipe("A", "u")
  r1$depends_on_recipes <- list("nonexistent_id_123")

  result <- topo_sort_recipes(list(r1))
  expect_equal(length(result), 1)
  expect_equal(result[[1]]$name, "A")
})

# --- harmonize tests ---

test_that("harmonize: rejects non-list input", {
  expect_error(harmonize(NULL), "non-empty list")
  expect_error(harmonize(list()), "non-empty list")
  expect_error(harmonize("not a list"), "non-empty list")
})

test_that("harmonize: rejects non-Survey elements", {
  expect_error(
    harmonize(list("not a survey")),
    "not a Survey object"
  )
})

test_that("harmonize: rejects invalid grouping", {
  svy <- make_test_survey(10)
  expect_error(
    harmonize(list(svy), grouping = "invalid"),
    "grouping"
  )
})

test_that("harmonize: no recipes warns and returns unchanged", {
  svy <- make_test_survey(10)
  old_backend <- getOption("metasurvey.backend")
  on.exit(options(metasurvey.backend = old_backend), add = TRUE)
  set_backend("local", path = tempfile(fileext = ".json"))

  expect_warning(
    pool <- harmonize(list(svy), .verbose = FALSE),
    "No recipes found"
  )

  expect_s3_class(pool, "PoolSurvey")
  surveys_out <- pool$surveys$annual$series
  expect_equal(length(surveys_out), 1)
})

test_that("harmonize: applies recipes and returns PoolSurvey", {
  svy1 <- make_test_survey(10)
  svy2 <- make_test_survey(10)
  svy2$edition <- "2022"

  # Set up local backend with recipes
  old_backend <- getOption("metasurvey.backend")
  on.exit(options(metasurvey.backend = old_backend), add = TRUE)
  tmp <- tempfile(fileext = ".json")
  set_backend("local", path = tmp)

  r1 <- Recipe$new(
    name = "Add flag 2023", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "test", description = "test",
    steps = list("step_compute(., flag = 1L)"),
    id = "rec_2023"
  )
  r2 <- Recipe$new(
    name = "Add flag 2022", edition = "2022", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "test", description = "test",
    steps = list("step_compute(., flag = 1L)"),
    id = "rec_2022"
  )
  publish_recipe(r1)
  publish_recipe(r2)

  pool <- harmonize(list(svy1, svy2), .verbose = FALSE)

  expect_s3_class(pool, "PoolSurvey")
  surveys_out <- pool$surveys$annual$series
  expect_equal(length(surveys_out), 2)
  # Recipes were baked
  expect_true("flag" %in% names(get_data(surveys_out[[1]])))
  expect_true("flag" %in% names(get_data(surveys_out[[2]])))
})

test_that("harmonize: respects dependency order", {
  svy <- make_test_survey(10)

  old_backend <- getOption("metasurvey.backend")
  on.exit(options(metasurvey.backend = old_backend), add = TRUE)
  tmp <- tempfile(fileext = ".json")
  set_backend("local", path = tmp)

  # r_base creates column 'base_val'
  r_base <- Recipe$new(
    name = "Base", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "test", description = "base recipe",
    steps = list("step_compute(., base_val = 42L)"),
    id = "r_base"
  )
  # r_derived depends on r_base, uses 'base_val'
  r_derived <- Recipe$new(
    name = "Derived", edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("base_val"),
    user = "test", description = "derived recipe",
    steps = list("step_compute(., derived_val = base_val * 2L)"),
    id = "r_derived",
    depends_on_recipes = list("r_base")
  )
  publish_recipe(r_derived)
  publish_recipe(r_base)

  suppressWarnings(
    pool <- harmonize(list(svy), .verbose = FALSE)
  )
  result_data <- get_data(pool$surveys$annual$series[[1]])
  expect_true("base_val" %in% names(result_data))
  expect_true("derived_val" %in% names(result_data))
  expect_equal(result_data$derived_val[1], 84L)
})

test_that("harmonize: custom grouping and group_name", {
  svy <- make_test_survey(10)
  old_backend <- getOption("metasurvey.backend")
  on.exit(options(metasurvey.backend = old_backend), add = TRUE)
  tmp <- tempfile(fileext = ".json")
  set_backend("local", path = tmp)

  r <- Recipe$new(
    name = "Simple", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "test", description = "test",
    steps = list("step_compute(., z = 1L)"),
    id = "r_simple"
  )
  publish_recipe(r)

  pool <- harmonize(
    list(svy),
    grouping = "quarterly", group_name = "q1",
    .verbose = FALSE
  )

  expect_true("quarterly" %in% names(pool$surveys))
  expect_true("q1" %in% names(pool$surveys$quarterly))
})

test_that("harmonize: topic filter selects only matching recipes", {
  svy <- make_test_survey(10)
  old_backend <- getOption("metasurvey.backend")
  on.exit(options(metasurvey.backend = old_backend), add = TRUE)
  tmp <- tempfile(fileext = ".json")
  set_backend("local", path = tmp)

  r_compat <- Recipe$new(
    name = "Compat recipe", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "test", description = "test",
    steps = list("step_compute(., compat_var = 1L)"),
    id = "r_compat", topic = "compatibilizada"
  )
  r_other <- Recipe$new(
    name = "Other recipe", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "test", description = "test",
    steps = list("step_compute(., other_var = 1L)"),
    id = "r_other", topic = "labor_market"
  )
  publish_recipe(r_compat)
  publish_recipe(r_other)

  pool <- harmonize(list(svy), topic = "compatibilizada", .verbose = FALSE)
  result_data <- get_data(pool$surveys$annual$series[[1]])
  expect_true("compat_var" %in% names(result_data))
  expect_false("other_var" %in% names(result_data))
})

test_that("harmonize: category filter selects only matching recipes", {
  svy <- make_test_survey(10)
  old_backend <- getOption("metasurvey.backend")
  on.exit(options(metasurvey.backend = old_backend), add = TRUE)
  tmp <- tempfile(fileext = ".json")
  set_backend("local", path = tmp)

  compat_cat <- RecipeCategory$new("compatibilizada", "Compatibilizacion IECON")
  r_compat <- Recipe$new(
    name = "Compat recipe", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "test", description = "test",
    steps = list("step_compute(., cat_var = 1L)"),
    id = "r_cat_compat",
    categories = list(compat_cat)
  )
  other_cat <- RecipeCategory$new("labor_market", "Labor")
  r_other <- Recipe$new(
    name = "Other recipe", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "test", description = "test",
    steps = list("step_compute(., cat_other = 1L)"),
    id = "r_cat_other",
    categories = list(other_cat)
  )
  publish_recipe(r_compat)
  publish_recipe(r_other)

  pool <- harmonize(list(svy), category = "compatibilizada", .verbose = FALSE)
  result_data <- get_data(pool$surveys$annual$series[[1]])
  expect_true("cat_var" %in% names(result_data))
  expect_false("cat_other" %in% names(result_data))
})

test_that("harmonize: survey_type override", {
  svy <- make_test_survey(10)
  svy$type <- "other_type"

  old_backend <- getOption("metasurvey.backend")
  on.exit(options(metasurvey.backend = old_backend), add = TRUE)
  tmp <- tempfile(fileext = ".json")
  set_backend("local", path = tmp)

  r <- Recipe$new(
    name = "ECH recipe", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "test", description = "test",
    steps = list("step_compute(., from_ech = 1L)"),
    id = "r_override"
  )
  publish_recipe(r)

  pool <- harmonize(list(svy), survey_type = "ech", .verbose = FALSE)
  result_data <- get_data(pool$surveys$annual$series[[1]])
  expect_true("from_ech" %in% names(result_data))
})
