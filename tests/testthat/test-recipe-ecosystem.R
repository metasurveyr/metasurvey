# Tests for Recipe extension with categories, downloads, certification, user_info, version

test_that("Recipe$new() backward compat - no new fields", {
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "desc", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  expect_s3_class(r, "Recipe")
  expect_equal(r$downloads, 0L)
  expect_equal(r$version, "1.0.0")
  expect_s3_class(r$certification, "RecipeCertification")
  expect_equal(r$certification$level, "community")
  expect_null(r$user_info)
  expect_true(is.list(r$categories))
  expect_equal(length(r$categories), 0)
})

test_that("Recipe with categories assigned", {
  labor <- RecipeCategory$new("labor_market", "Labor market indicators")
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "desc", steps = list(),
    id = 1, doi = NULL, topic = NULL,
    categories = list(labor)
  )
  expect_equal(length(r$categories), 1)
  expect_equal(r$categories[[1]]$name, "labor_market")
})

test_that("Recipe with RecipeUser assigned to user_info", {
  user <- RecipeUser$new(name = "Juan", user_type = "individual")
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "Juan", description = "desc", steps = list(),
    id = 1, doi = NULL, topic = NULL,
    user_info = user
  )
  expect_s3_class(r$user_info, "RecipeUser")
  expect_equal(r$user_info$name, "Juan")
})

test_that("Recipe with RecipeCertification", {
  inst <- RecipeUser$new(name = "IECON", user_type = "institution")
  cert <- RecipeCertification$new(level = "official", certified_by = inst)
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "desc", steps = list(),
    id = 1, doi = NULL, topic = NULL,
    certification = cert
  )
  expect_equal(r$certification$level, "official")
})

test_that("increment_downloads increases count", {
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "desc", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  expect_equal(r$downloads, 0L)
  r$increment_downloads()
  expect_equal(r$downloads, 1L)
  r$increment_downloads()
  r$increment_downloads()
  expect_equal(r$downloads, 3L)
})

test_that("certify creates/upgrades certification", {
  inst <- RecipeUser$new(name = "IECON", user_type = "institution")
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "desc", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  expect_equal(r$certification$level, "community")
  r$certify(inst, "official")
  expect_equal(r$certification$level, "official")
  expect_equal(r$certification$certified_by$name, "IECON")
})

test_that("certify rejects insufficient trust level", {
  individual <- RecipeUser$new(name = "Juan", user_type = "individual")
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "desc", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  expect_error(r$certify(individual, "official"))
  expect_error(r$certify(individual, "reviewed"))
})

test_that("add_category appends to categories", {
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "desc", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  labor <- RecipeCategory$new("labor_market", "Labor")
  income <- RecipeCategory$new("income", "Income")
  r$add_category(labor)
  expect_equal(length(r$categories), 1)
  r$add_category(income)
  expect_equal(length(r$categories), 2)
})

test_that("add_category does not duplicate", {
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "desc", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  labor <- RecipeCategory$new("labor_market", "Labor")
  r$add_category(labor)
  r$add_category(labor)
  expect_equal(length(r$categories), 1)
})

test_that("remove_category removes by name", {
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "desc", steps = list(),
    id = 1, doi = NULL, topic = NULL
  )
  labor <- RecipeCategory$new("labor_market", "Labor")
  income <- RecipeCategory$new("income", "Income")
  r$add_category(labor)
  r$add_category(income)
  r$remove_category("labor_market")
  expect_equal(length(r$categories), 1)
  expect_equal(r$categories[[1]]$name, "income")
})

test_that("save_recipe includes new fields in JSON", {
  inst <- RecipeUser$new(name = "IECON", user_type = "institution", verified = TRUE)
  cert <- RecipeCertification$new(level = "official", certified_by = inst)
  user <- RecipeUser$new(name = "Maria", user_type = "institutional_member", institution = inst)
  labor <- RecipeCategory$new("labor_market", "Labor")

  r <- Recipe$new(
    name = "test_save", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "Maria", description = "Save test", steps = list(),
    id = 42, doi = NULL, topic = "labor",
    categories = list(labor), certification = cert,
    user_info = user, version = "2.0.0"
  )

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  expect_message(save_recipe(r, tmp))
  json <- jsonlite::read_json(tmp)
  expect_equal(json$version, "2.0.0")
  expect_equal(json$downloads, 0)
  expect_equal(json$certification$level, "official")
  expect_equal(json$user_info$name, "Maria")
  expect_equal(length(json$categories), 1)
  expect_equal(json$categories[[1]]$name, "labor_market")
})

test_that("read_recipe reconstructs new fields from JSON", {
  inst <- RecipeUser$new(name = "IECON", user_type = "institution")
  cert <- RecipeCertification$new(level = "official", certified_by = inst)
  labor <- RecipeCategory$new("labor_market", "Labor")

  r <- Recipe$new(
    name = "test_read", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "Read test", steps = list(),
    id = 43, doi = NULL, topic = "labor",
    categories = list(labor), certification = cert, version = "1.1.0"
  )

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  save_recipe(r, tmp)
  loaded <- read_recipe(tmp)

  expect_s3_class(loaded, "Recipe")
  expect_equal(loaded$version, "1.1.0")
  expect_equal(loaded$downloads, 0)
  expect_s3_class(loaded$certification, "RecipeCertification")
  expect_equal(loaded$certification$level, "official")
  expect_equal(length(loaded$categories), 1)
  expect_equal(loaded$categories[[1]]$name, "labor_market")
})

test_that("doc() includes categories and certification in meta", {
  labor <- RecipeCategory$new("labor_market", "Labor")
  inst <- RecipeUser$new(name = "IECON", user_type = "institution")
  cert <- RecipeCertification$new(level = "official", certified_by = inst)

  r <- Recipe$new(
    name = "doc_test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "Doc test", steps = list(),
    id = 44, doi = NULL, topic = "labor",
    categories = list(labor), certification = cert
  )

  doc <- r$doc()
  expect_true("categories" %in% names(doc$meta))
  expect_true("certification" %in% names(doc$meta))
  expect_equal(doc$meta$certification, "official")
  expect_equal(doc$meta$categories, "labor_market")
})

test_that("print.Recipe shows new fields", {
  labor <- RecipeCategory$new("labor_market", "Labor")
  inst <- RecipeUser$new(name = "IECON", user_type = "institution")
  cert <- RecipeCertification$new(level = "official", certified_by = inst)

  r <- Recipe$new(
    name = "print_test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "Print test", steps = list(),
    id = 45, doi = NULL, topic = "labor",
    categories = list(labor), certification = cert,
    version = "1.2.0", downloads = 150L
  )

  out <- capture.output(print(r))
  out_text <- paste(out, collapse = "\n")
  expect_true(grepl("official", out_text, ignore.case = TRUE))
  expect_true(grepl("labor_market", out_text, ignore.case = TRUE))
  expect_true(grepl("1.2.0", out_text))
  expect_true(grepl("150", out_text))
})

test_that("backward-compatible: old JSON without new fields loads fine", {
  # Simulate old JSON with just basic fields
  old_json <- list(
    name = "old_recipe",
    user = "old_user",
    svy_type = "ech",
    edition = "2020",
    description = "Legacy recipe",
    steps = list()
  )
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  jsonlite::write_json(old_json, tmp, auto_unbox = TRUE)

  loaded <- read_recipe(tmp)
  expect_s3_class(loaded, "Recipe")
  expect_equal(loaded$downloads, 0L)
  expect_equal(loaded$version, "1.0.0")
  expect_s3_class(loaded$certification, "RecipeCertification")
  expect_equal(loaded$certification$level, "community")
  expect_equal(length(loaded$categories), 0)
  expect_null(loaded$user_info)
})

test_that("version field works", {
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "desc", steps = list(),
    id = 1, doi = NULL, topic = NULL,
    version = "3.0.0"
  )
  expect_equal(r$version, "3.0.0")
})

test_that("downloads field can be set at construction", {
  r <- Recipe$new(
    name = "test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "desc", steps = list(),
    id = 1, doi = NULL, topic = NULL,
    downloads = 100L
  )
  expect_equal(r$downloads, 100L)
})
