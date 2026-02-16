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
  svy <- make_test_survey()
  r <- recipe(
    name = "read test",
    user = "tester",
    svy = svy,
    description = "Testing read_recipe"
  )
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  save_recipe(r, tmp)
  r2 <- read_recipe(tmp)
  expect_s3_class(r2, "Recipe")
  expect_equal(r2$name, "read test")
})

test_that("recipe_to_json converts recipe to JSON", {
  svy <- make_test_survey()
  r <- recipe(
    name = "json test",
    user = "tester",
    svy = svy,
    description = "Testing JSON conversion"
  )
  json <- metasurvey:::recipe_to_json(r)
  expect_type(json, "character")
  expect_true(nchar(json) > 0)
})

test_that("metadata_recipe returns expected fields", {
  fields <- metasurvey:::metadata_recipe()
  expect_true("name" %in% fields)
  expect_true("user" %in% fields)
  expect_true("svy" %in% fields)
  expect_true("description" %in% fields)
})

test_that("bake_recipes applies recipe steps to survey", {
  svy <- make_test_survey()
  r <- Recipe$new(
    name = "bake test",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list(),
    user = "tester",
    description = "Test bake",
    steps = list(quote(step_compute(., z = x + y))),
    id = "bake_test_001",
    doi = NULL,
    topic = NULL
  )
  svy <- add_recipe(svy, r)
  svy <- bake_recipes(svy)
  expect_true("z" %in% names(get_data(svy)))
})

test_that("get_recipe retrieves from local backend", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  old <- tryCatch(get_backend(), error = function(e) NULL)
  set_backend("local", path = tmp)

  svy <- make_test_survey()
  r <- recipe(name = "local get", user = "t", svy = svy, description = "d")
  get_backend()$publish(r)

  found <- list_recipes()
  expect_true(length(found) >= 1)

  if (!is.null(old)) set_backend("local", path = tempfile(fileext = ".json"))
})

test_that("search_recipes filters by keyword", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  set_backend("local", path = tmp)

  svy <- make_test_survey()
  r <- recipe(name = "labor market test", user = "t", svy = svy, description = "employment stats")
  get_backend()$publish(r)

  found <- search_recipes("labor")
  expect_true(length(found) >= 1)
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

# --- Recipe$doc() with step_objects ---

test_that("Recipe$doc generates documentation from step_objects", {
  step1 <- Step$new(
    name = "compute_age_group", edition = "2023", survey_type = "ech",
    type = "compute", new_var = "age_group",
    exprs = list(age_group = quote(ifelse(age > 30, "adult", "young"))),
    call = NULL, svy_before = NULL, default_engine = "data.table",
    depends_on = list("age"), comments = "Compute age group"
  )

  step2 <- Step$new(
    name = "recode_sex", edition = "2023", survey_type = "ech",
    type = "recode", new_var = "sex_label",
    exprs = list(sex_label = quote(ifelse(sex == 1, "M", "F"))),
    call = NULL, svy_before = NULL, default_engine = "data.table",
    depends_on = list("sex"), comments = "Recode sex"
  )

  r <- Recipe$new(
    name = "doc test",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("age", "sex"),
    user = "tester",
    description = "Testing doc generation",
    steps = list(),
    id = "doc_test_001",
    step_objects = list(step1, step2)
  )

  doc <- r$doc()
  expect_true(is.list(doc))
  expect_true("meta" %in% names(doc))
  expect_true("input_variables" %in% names(doc))
  expect_true("output_variables" %in% names(doc))
  expect_true("pipeline" %in% names(doc))
  expect_true("age" %in% doc$input_variables)
  expect_true("age_group" %in% doc$output_variables)
  expect_length(doc$pipeline, 2)
  expect_equal(doc$pipeline[[1]]$type, "compute")
  expect_equal(doc$pipeline[[2]]$type, "recode")
})

test_that("Recipe$doc with cached_doc returns cached values", {
  cached <- list(
    input_variables = c("var1", "var2"),
    output_variables = c("var3"),
    pipeline = list(list(index = 1, type = "compute"))
  )

  r <- Recipe$new(
    name = "cached doc test",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list(),
    user = "tester",
    description = "Test cached doc",
    steps = list(),
    id = "cached_doc_001",
    cached_doc = cached
  )

  doc <- r$doc()
  expect_equal(doc$input_variables, c("var1", "var2"))
  expect_equal(doc$output_variables, c("var3"))
  expect_length(doc$pipeline, 1)
})

test_that("Recipe$doc without step_objects or cached_doc returns empty", {
  r <- Recipe$new(
    name = "empty doc",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list(),
    user = "tester",
    description = "Test empty doc",
    steps = list(),
    id = "empty_doc_001"
  )

  doc <- r$doc()
  expect_length(doc$input_variables, 0)
  expect_length(doc$output_variables, 0)
  expect_length(doc$pipeline, 0)
})

# --- Recipe$validate ---

test_that("Recipe$validate returns TRUE when all vars present", {
  svy <- make_test_survey()

  step1 <- Step$new(
    name = "test", edition = "2023", survey_type = "ech",
    type = "compute", new_var = "z",
    exprs = list(z = quote(x + y)),
    call = NULL, svy_before = NULL, default_engine = "data.table",
    depends_on = list("x", "y")
  )

  r <- Recipe$new(
    name = "validate pass",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("x", "y"),
    user = "tester",
    description = "Test validate",
    steps = list(),
    id = "validate_pass_001",
    step_objects = list(step1)
  )

  expect_true(r$validate(svy))
})

test_that("Recipe$validate errors when vars missing", {
  svy <- make_test_survey()

  step1 <- Step$new(
    name = "test", edition = "2023", survey_type = "ech",
    type = "compute", new_var = "z",
    exprs = list(z = quote(missing_var + 1)),
    call = NULL, svy_before = NULL, default_engine = "data.table",
    depends_on = list("missing_var", "another_missing")
  )

  r <- Recipe$new(
    name = "validate fail",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("missing_var"),
    user = "tester",
    description = "Test validate fail",
    steps = list(),
    id = "validate_fail_001",
    step_objects = list(step1)
  )

  expect_error(r$validate(svy), "missing_var")
})

test_that("Recipe$validate is case-insensitive", {
  # Create survey with uppercase column names (like ECH real data)
  df <- data.table::data.table(
    POBPCOAC = c(1, 2, 3), W_ANO = c(1, 1, 1)
  )
  svy <- Survey$new(
    data = df, edition = "2024", type = "ech",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "W_ANO")
  )

  step1 <- Step$new(
    name = "test", edition = "2024", survey_type = "ech",
    type = "compute", new_var = "pea",
    exprs = list(pea = quote(pobpcoac == 2)),
    call = NULL, svy_before = NULL, default_engine = "data.table",
    depends_on = list("pobpcoac")
  )

  r <- Recipe$new(
    name = "case insensitive",
    edition = "2024",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("pobpcoac"),
    user = "tester",
    description = "Test case insensitive validate",
    steps = list(),
    id = "ci_001",
    step_objects = list(step1)
  )

  # Should NOT error even though data has POBPCOAC and recipe has pobpcoac
  expect_true(r$validate(svy))
})

# --- Recipe$to_list ---

test_that("Recipe$to_list serializes all fields", {
  cat1 <- RecipeCategory$new(name = "labor", description = "Labor market")
  inst <- RecipeUser$new(name = "INE", user_type = "institution", email = "ine@test.com")
  reviewer <- RecipeUser$new(
    name = "Reviewer", user_type = "institutional_member",
    email = "rev@test.com", institution = inst
  )
  cert <- RecipeCertification$new(level = "reviewed", certified_by = reviewer)
  user_info <- RecipeUser$new(name = "Test User", user_type = "individual", email = "test@test.com")

  r <- Recipe$new(
    name = "to_list test",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("age"),
    user = "tester",
    description = "Serialization test",
    steps = list(quote(step_compute(z = x + 1))),
    id = "to_list_001",
    doi = "10.1234/test",
    topic = "labor",
    categories = list(cat1),
    certification = cert,
    user_info = user_info,
    version = "2.0.0"
  )

  lst <- r$to_list()
  expect_equal(lst$name, "to_list test")
  expect_equal(lst$survey_type, "ech")
  expect_equal(lst$edition, "2023")
  expect_equal(lst$doi, "10.1234/test")
  expect_equal(lst$version, "2.0.0")
  expect_true(is.list(lst$categories))
  expect_true(is.list(lst$certification))
  expect_true(is.list(lst$user_info))
  expect_true(is.character(lst$steps[[1]]))
})

# --- Recipe$increment_downloads ---

test_that("Recipe$increment_downloads increments count", {
  r <- Recipe$new(
    name = "dl test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "Test", steps = list(),
    id = "dl_001", downloads = 5L
  )

  expect_equal(r$downloads, 5L)
  r$increment_downloads()
  expect_equal(r$downloads, 6L)
  r$increment_downloads()
  expect_equal(r$downloads, 7L)
})

# --- Recipe$certify ---

test_that("Recipe$certify updates certification", {
  r <- Recipe$new(
    name = "cert test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "Test", steps = list(),
    id = "cert_001"
  )

  expect_equal(r$certification$level, "community")

  inst <- RecipeUser$new(name = "INE", user_type = "institution", email = "ine@test.com")
  certifier <- RecipeUser$new(
    name = "Reviewer", user_type = "institutional_member",
    email = "rev@test.com", institution = inst
  )
  r$certify(certifier, "reviewed")
  expect_equal(r$certification$level, "reviewed")
})

# --- Recipe$add_category / remove_category ---

test_that("Recipe$add_category and remove_category work", {
  r <- Recipe$new(
    name = "cat test", edition = "2023", survey_type = "ech",
    default_engine = "data.table", depends_on = list(),
    user = "tester", description = "Test", steps = list(),
    id = "cat_001"
  )

  cat1 <- RecipeCategory$new(name = "labor", description = "Labor market")
  cat2 <- RecipeCategory$new(name = "income", description = "Income analysis")

  r$add_category(cat1)
  expect_length(r$categories, 1)

  r$add_category(cat2)
  expect_length(r$categories, 2)

  # Adding duplicate should not increase count
  r$add_category(cat1)
  expect_length(r$categories, 2)

  r$remove_category("labor")
  expect_length(r$categories, 1)
  expect_equal(r$categories[[1]]$name, "income")
})

# --- read_recipe full roundtrip with metadata ---

test_that("read_recipe restores full metadata", {
  svy <- make_test_survey()
  cat1 <- RecipeCategory$new(name = "labor", description = "Labor market")
  user_info <- RecipeUser$new(name = "Test User", user_type = "individual", email = "test@test.com")

  r <- Recipe$new(
    name = "full roundtrip",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("age"),
    user = "tester",
    description = "Full metadata test",
    steps = list(quote(step_compute(z = age + 1))),
    id = "rt_001",
    doi = "10.1234/rt",
    topic = "labor",
    categories = list(cat1),
    user_info = user_info,
    version = "1.2.0"
  )

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  suppressMessages(save_recipe(r, tmp))

  r2 <- read_recipe(tmp)
  expect_s3_class(r2, "Recipe")
  expect_equal(r2$name, "full roundtrip")
  expect_equal(r2$survey_type, "ech")
  expect_equal(r2$edition, "2023")
  expect_equal(r2$doi, "10.1234/rt")
  expect_equal(r2$version, "1.2.0")
  expect_length(r2$categories, 1)
  expect_equal(r2$categories[[1]]$name, "labor")
})

# --- Recipe$doc with step_objects containing rename and remove ---

test_that("Recipe$doc handles rename and remove step types", {
  step_rename <- Step$new(
    name = "rename_step", edition = "2023", survey_type = "ech",
    type = "step_rename", new_var = NULL,
    exprs = list(new_name = "old_name"),
    call = NULL, svy_before = NULL, default_engine = "data.table",
    depends_on = list("old_name")
  )

  step_remove <- Step$new(
    name = "remove_step", edition = "2023", survey_type = "ech",
    type = "step_remove", new_var = NULL,
    exprs = list(),
    call = NULL, svy_before = NULL, default_engine = "data.table",
    depends_on = list()
  )

  r <- Recipe$new(
    name = "rename remove doc",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list(),
    user = "tester",
    description = "Test",
    steps = list(),
    id = "rr_001",
    step_objects = list(step_rename, step_remove)
  )

  doc <- r$doc()
  expect_length(doc$pipeline, 2)
  expect_equal(doc$pipeline[[1]]$inferred_type, "inherited")
  expect_true(is.na(doc$pipeline[[2]]$inferred_type))
})

# --- Recipe with depends_on_recipes ---

test_that("Recipe stores depends_on_recipes", {
  r <- Recipe$new(
    name = "dep recipes test",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list(),
    user = "tester",
    description = "Test",
    steps = list(),
    id = "dep_001",
    depends_on_recipes = list("recipe_A", "recipe_B")
  )

  expect_length(r$depends_on_recipes, 2)
  expect_equal(r$depends_on_recipes[[1]], "recipe_A")
})

# --- Recipe with data_source ---

test_that("Recipe stores data_source", {
  r <- Recipe$new(
    name = "data source test",
    edition = "2023",
    survey_type = "ech",
    default_engine = "data.table",
    depends_on = list(),
    user = "tester",
    description = "Test",
    steps = list(),
    id = "ds_001",
    data_source = list(s3_bucket = "my-bucket", provider = "aws")
  )

  expect_equal(r$data_source$s3_bucket, "my-bucket")
  expect_equal(r$data_source$provider, "aws")
})
