test_that("Recipe$doc() generates documentation from steps", {
  dt <- data.table::data.table(
    age = c(25, 30, 45, 60, 70),
    income = c(1000, 2000, 3000, 4000, 5000),
    w = rep(1, 5)
  )

  svy <- Survey$new(
    data = dt,
    edition = "2023",
    type = "test_survey",
    psu = NULL,
    engine = "data.table",
    weight = add_weight(annual = "w")
  )

  # Apply steps to the survey
  svy <- step_compute(svy,
    high_income = ifelse(income > 2500, 1L, 0L),
    income_log = log(income),
    comment = "Income indicators"
  )
  svy <- bake_steps(svy)

  # Create recipe from steps
  rec <- steps_to_recipe(
    name = "Test Recipe",
    user = "Test User",
    svy = svy,
    description = "A test recipe for documentation",
    steps = get_steps(svy),
    topic = "test_topic",
    doi = "10.1234/test"
  )

  doc <- rec$doc()

  # Check structure
  expect_type(doc, "list")
  expect_named(doc, c("meta", "input_variables", "output_variables", "pipeline"))

  # Check metadata
  expect_equal(doc$meta$name, "Test Recipe")
  expect_equal(doc$meta$user, "Test User")
  expect_equal(doc$meta$topic, "test_topic")
  expect_equal(doc$meta$doi, "10.1234/test")

  # Check input variables (income is used, not created)
  expect_true("income" %in% doc$input_variables)

  # Check output variables
  expect_true("high_income" %in% doc$output_variables)
  expect_true("income_log" %in% doc$output_variables)

  # Check pipeline
  expect_length(doc$pipeline, 1)
  expect_equal(doc$pipeline[[1]]$type, "compute")
  expect_true(all(c("high_income", "income_log") %in% doc$pipeline[[1]]$outputs))
})

test_that("Recipe$doc() handles recode steps", {
  dt <- data.table::data.table(
    status = c(1, 2, 3, 1, 2),
    w = rep(1, 5)
  )

  svy <- Survey$new(
    data = dt,
    edition = "2023",
    type = "test",
    psu = NULL,
    engine = "data.table",
    weight = add_weight(annual = "w")
  )

  svy <- step_compute(svy,
    active = ifelse(status %in% c(1, 2), 1L, 0L),
    comment = "Active population"
  )
  svy <- bake_steps(svy)

  rec <- steps_to_recipe(
    name = "Recode Test",
    user = "Test",
    svy = svy,
    description = "Test recode doc",
    steps = get_steps(svy)
  )

  doc <- rec$doc()
  expect_true("status" %in% doc$input_variables)
  expect_true("active" %in% doc$output_variables)
})

test_that("Recipe$validate() checks for required variables", {
  dt <- data.table::data.table(
    var_a = 1:3,
    w = rep(1, 3)
  )

  svy <- Survey$new(
    data = dt,
    edition = "2023",
    type = "test",
    psu = NULL,
    engine = "data.table",
    weight = add_weight(annual = "w")
  )

  svy <- step_compute(svy, var_b = var_a * 2, comment = "Double var_a")
  svy <- bake_steps(svy)

  rec <- steps_to_recipe(
    name = "Valid Recipe",
    user = "Test",
    svy = svy,
    description = "Test",
    steps = get_steps(svy)
  )

  # Should pass - svy has var_a
  expect_true(rec$validate(svy))

  # Should fail - new survey without var_a
  dt2 <- data.table::data.table(other_var = 1:3, w = rep(1, 3))
  svy2 <- Survey$new(
    data = dt2, edition = "2023", type = "test",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )
  expect_error(rec$validate(svy2), "requires variables not present")
})

test_that("save_recipe() includes all metadata and doc", {
  dt <- data.table::data.table(
    var_old = c(10, 20, 30),
    w = rep(1, 3)
  )

  svy <- Survey$new(
    data = dt, edition = "2023", type = "test",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )

  svy <- step_compute(svy, var_new = var_old * 2, comment = "Double the variable")
  svy <- bake_steps(svy)

  rec <- steps_to_recipe(
    name = "Save Test Recipe",
    user = "Test User",
    svy = svy,
    description = "Testing save functionality",
    steps = get_steps(svy),
    topic = "testing",
    doi = "10.1234/save_test"
  )

  tmp_file <- tempfile(fileext = ".json")
  save_recipe(rec, tmp_file)

  json_data <- jsonlite::read_json(tmp_file, simplifyVector = TRUE)

  # Check all metadata is present
  expect_equal(json_data$name, "Save Test Recipe")
  expect_equal(json_data$user, "Test User")
  expect_equal(json_data$description, "Testing save functionality")
  expect_equal(json_data$topic, "testing")
  expect_equal(json_data$doi, "10.1234/save_test")
  expect_false(is.null(json_data$id))

  # Check doc is auto-generated
  expect_true("doc" %in% names(json_data))
  expect_true("input_variables" %in% names(json_data$doc))
  expect_true("output_variables" %in% names(json_data$doc))
  expect_true("pipeline" %in% names(json_data$doc))

  unlink(tmp_file)
})

test_that("read_recipe() returns full Recipe object with new format", {
  dt <- data.table::data.table(
    old_var = c(1, 2, 3),
    w = rep(1, 3)
  )

  svy <- Survey$new(
    data = dt, edition = "2023", type = "test",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )

  svy <- step_compute(svy, new_var = old_var + 1, comment = "Add one")
  svy <- bake_steps(svy)

  rec <- steps_to_recipe(
    name = "Read Test Recipe",
    user = "Test User",
    svy = svy,
    description = "Testing read functionality",
    steps = get_steps(svy),
    topic = "reading"
  )

  tmp_file <- tempfile(fileext = ".json")
  save_recipe(rec, tmp_file)

  rec_loaded <- read_recipe(tmp_file)

  # Should return a Recipe object
  expect_s3_class(rec_loaded, "Recipe")
  expect_s3_class(rec_loaded, "R6")

  # Check metadata is preserved
  expect_equal(rec_loaded$name, "Read Test Recipe")
  expect_equal(rec_loaded$user, "Test User")
  expect_equal(rec_loaded$description, "Testing read functionality")
  expect_equal(rec_loaded$topic, "reading")

  unlink(tmp_file)
})

test_that("read_recipe() handles old format (backward compatibility)", {
  old_format <- list(
    name = "Old Format Recipe",
    user = "Old User",
    svy_type = "test",
    edition = "2022",
    description = "Old format",
    steps = c("step_compute(svy, new_var = old_var * 2)")
  )

  tmp_file <- tempfile(fileext = ".json")
  jsonlite::write_json(old_format, tmp_file, simplifyVector = TRUE)

  result <- read_recipe(tmp_file)

  expect_s3_class(result, "Recipe")
  expect_equal(result$name, "Old Format Recipe")

  unlink(tmp_file)
})

test_that("print.Recipe displays formatted output", {
  dt <- data.table::data.table(
    value = c(1, 2, 3),
    w = rep(1, 3)
  )

  svy <- Survey$new(
    data = dt, edition = "2023", type = "test",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )

  svy <- step_compute(svy, doubled = value * 2, comment = "Double the value")
  svy <- bake_steps(svy)

  rec <- steps_to_recipe(
    name = "Print Test Recipe",
    user = "Test User",
    svy = svy,
    description = "Testing print functionality",
    steps = get_steps(svy),
    topic = "printing"
  )

  output <- capture.output(print(rec))

  expect_true(any(grepl("Print Test Recipe", output)))
  expect_true(any(grepl("Test User", output)))
  expect_true(any(grepl("printing", output)))
  expect_true(any(grepl("Requires", output)))
  expect_true(any(grepl("Pipeline", output)))
  expect_true(any(grepl("Produces", output)))
})

test_that("Recipe round-trip preserves all information", {
  dt <- data.table::data.table(
    status = c(1, 2, 1, 2, 3),
    total = c(100, 200, 150, 250, 300),
    w = rep(1, 5)
  )

  svy <- Survey$new(
    data = dt, edition = "2023", type = "ech",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )

  svy <- step_compute(svy,
    employed = ifelse(status == 1, 1L, 0L),
    comment = "Employment status"
  )
  svy <- step_compute(svy,
    employment_rate = employed / total * 100,
    comment = "Calculate rate"
  )
  svy <- bake_steps(svy)

  rec_original <- steps_to_recipe(
    name = "Roundtrip Recipe",
    user = "Roundtrip User",
    svy = svy,
    description = "Full round-trip test",
    steps = get_steps(svy),
    topic = "labor",
    doi = "10.1234/roundtrip"
  )

  tmp_file <- tempfile(fileext = ".json")
  save_recipe(rec_original, tmp_file)
  rec_loaded <- read_recipe(tmp_file)

  # Compare key properties
  expect_equal(rec_loaded$name, rec_original$name)
  expect_equal(rec_loaded$user, rec_original$user)
  expect_equal(rec_loaded$description, rec_original$description)
  expect_equal(rec_loaded$topic, rec_original$topic)
  expect_equal(rec_loaded$doi, rec_original$doi)
  expect_equal(rec_loaded$survey_type, rec_original$survey_type)
  expect_equal(rec_loaded$edition, rec_original$edition)

  # Compare doc() - loaded version uses cached doc
  doc_original <- rec_original$doc()
  doc_loaded <- rec_loaded$doc()

  expect_equal(sort(doc_loaded$input_variables), sort(doc_original$input_variables))
  expect_equal(sort(doc_loaded$output_variables), sort(doc_original$output_variables))
  expect_equal(length(doc_loaded$pipeline), length(doc_original$pipeline))

  unlink(tmp_file)
})

test_that("doc() returns empty lists when no step objects available", {
  # Recipe without step_objects or cached doc
  rec <- Recipe$new(
    name = "Empty Recipe",
    edition = "2023",
    survey_type = "test",
    default_engine = "data.table",
    depends_on = list(),
    user = "Test",
    description = "Test",
    steps = list(),
    id = 1,
    doi = NULL,
    topic = NULL
  )

  doc <- rec$doc()
  expect_equal(doc$input_variables, character(0))
  expect_equal(doc$output_variables, character(0))
  expect_equal(doc$pipeline, list())
  expect_equal(doc$meta$name, "Empty Recipe")
})
