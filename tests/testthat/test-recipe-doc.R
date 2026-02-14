test_that("Recipe$doc() generates documentation from steps", {
  # Create a simple survey with some data
  df <- data.frame(
    id = 1:5,
    age = c(25, 30, 45, 60, 70),
    income = c(1000, 2000, 3000, 4000, 5000),
    status = c(1, 2, 2, 3, 3),
    w = 1
  )
  
  svy <- Survey$new(
    data = data.table::data.table(df),
    edition = "2023",
    type = "test_survey",
    psu = NULL,
    engine = "data.table",
    weight = add_weight(annual = "w")
  )
  
  # Create a recipe with some steps
  rec <- recipe(
    name = "Test Recipe",
    user = "Test User",
    svy = survey_empty(type = "test_survey", edition = "2023"),
    description = "A test recipe for documentation",
    topic = "test_topic",
    doi = "10.1234/test",
    
    step_recode(
      age_group,
      age < 30 ~ "Young",
      age >= 30 & age < 60 ~ "Adult",
      age >= 60 ~ "Senior",
      .default = "Unknown",
      comment = "Age categorization"
    ),
    
    step_compute(
      high_income = income > 2500,
      income_log = log(income),
      comment = "Income indicators"
    )
  )
  
  # Generate documentation
  doc <- rec$doc()
  
  # Check structure
  expect_type(doc, "list")
  expect_named(doc, c("meta", "input_variables", "output_variables", "pipeline"))
  
  # Check metadata
  expect_equal(doc$meta$name, "Test Recipe")
  expect_equal(doc$meta$user, "Test User")
  expect_equal(doc$meta$topic, "test_topic")
  expect_equal(doc$meta$doi, "10.1234/test")
  
  # Check input variables (age and income are used, not created)
  expect_true("age" %in% doc$input_variables)
  expect_true("income" %in% doc$input_variables)
  
  # Check output variables
  expect_true("age_group" %in% doc$output_variables)
  expect_true("high_income" %in% doc$output_variables)
  expect_true("income_log" %in% doc$output_variables)
  
  # Check pipeline
  expect_length(doc$pipeline, 2)
  expect_equal(doc$pipeline[[1]]$type, "recode")
  expect_equal(doc$pipeline[[1]]$outputs, "age_group")
  expect_equal(doc$pipeline[[1]]$comment, "Age categorization")
  expect_equal(doc$pipeline[[2]]$type, "compute")
  expect_true(all(c("high_income", "income_log") %in% doc$pipeline[[2]]$outputs))
})

test_that("Recipe$validate() checks for required variables", {
  # Create survey with limited variables
  df <- data.frame(
    id = 1:3,
    var_a = 1:3,
    w = 1
  )
  
  svy <- Survey$new(
    data = data.table::data.table(df),
    edition = "2023",
    type = "test",
    psu = NULL,
    engine = "data.table",
    weight = add_weight(annual = "w")
  )
  
  # Create recipe that requires var_a (which exists)
  rec_valid <- recipe(
    name = "Valid Recipe",
    user = "Test",
    svy = survey_empty(type = "test", edition = "2023"),
    description = "Test",
    
    step_compute(
      var_b = var_a * 2,
      comment = "Double var_a"
    )
  )
  
  # Should pass validation
  expect_true(rec_valid$validate(svy))
  
  # Create recipe that requires var_x (which doesn't exist)
  rec_invalid <- recipe(
    name = "Invalid Recipe",
    user = "Test",
    svy = survey_empty(type = "test", edition = "2023"),
    description = "Test",
    
    step_compute(
      var_c = var_x * 2,
      comment = "Double var_x"
    )
  )
  
  # Should fail validation
  expect_error(
    rec_invalid$validate(svy),
    "requires variables not present"
  )
})

test_that("save_recipe() includes all metadata and doc", {
  # Create a recipe
  rec <- recipe(
    name = "Save Test Recipe",
    user = "Test User",
    svy = survey_empty(type = "test", edition = "2023"),
    description = "Testing save functionality",
    topic = "testing",
    doi = "10.1234/save_test",
    
    step_compute(
      var_new = var_old * 2,
      comment = "Double the variable"
    )
  )
  
  # Save to temp file
  tmp_file <- tempfile(fileext = ".json")
  save_recipe(rec, tmp_file)
  
  # Read the JSON directly to check structure
  json_data <- jsonlite::read_json(tmp_file, simplifyVector = TRUE)
  
  # Check all metadata is present
  expect_equal(json_data$name, "Save Test Recipe")
  expect_equal(json_data$user, "Test User")
  expect_equal(json_data$description, "Testing save functionality")
  expect_equal(json_data$topic, "testing")
  expect_equal(json_data$doi, "10.1234/save_test")
  expect_true(!is.null(json_data$id))
  
  # Check doc is auto-generated
  expect_true("doc" %in% names(json_data))
  expect_true("input_variables" %in% names(json_data$doc))
  expect_true("output_variables" %in% names(json_data$doc))
  expect_true("pipeline" %in% names(json_data$doc))
  
  # Clean up
  unlink(tmp_file)
})

test_that("read_recipe() returns full Recipe object with new format", {
  # Create and save a recipe
  rec <- recipe(
    name = "Read Test Recipe",
    user = "Test User",
    svy = survey_empty(type = "test", edition = "2023"),
    description = "Testing read functionality",
    topic = "reading",
    
    step_compute(
      new_var = old_var + 1,
      comment = "Add one"
    )
  )
  
  tmp_file <- tempfile(fileext = ".json")
  save_recipe(rec, tmp_file)
  
  # Read it back
  rec_loaded <- read_recipe(tmp_file)
  
  # Should return a Recipe object
  expect_s3_class(rec_loaded, "Recipe")
  expect_s3_class(rec_loaded, "R6")
  
  # Check metadata is preserved
  expect_equal(rec_loaded$name, "Read Test Recipe")
  expect_equal(rec_loaded$user, "Test User")
  expect_equal(rec_loaded$description, "Testing read functionality")
  expect_equal(rec_loaded$topic, "reading")
  
  # Check steps are preserved
  expect_length(rec_loaded$steps, 1)
  
  # Clean up
  unlink(tmp_file)
})

test_that("read_recipe() handles old format (backward compatibility)", {
  # Create old format JSON manually
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
  
  # Read it
  result <- read_recipe(tmp_file)
  
  # In new code, should return Recipe object if metadata is present
  expect_s3_class(result, "Recipe")
  expect_equal(result$name, "Old Format Recipe")
  
  # Clean up
  unlink(tmp_file)
})

test_that("print.Recipe displays formatted output", {
  # Create a recipe
  rec <- recipe(
    name = "Print Test Recipe",
    user = "Test User",
    svy = survey_empty(type = "test", edition = "2023"),
    description = "Testing print functionality",
    topic = "printing",
    
    step_recode(
      category,
      value == 1 ~ "A",
      value == 2 ~ "B",
      .default = "C",
      comment = "Categorize values"
    ),
    
    step_compute(
      doubled = value * 2,
      comment = "Double the value"
    )
  )
  
  # Capture print output
  output <- capture.output(print(rec))
  
  # Check that key elements are present in output
  expect_true(any(grepl("Print Test Recipe", output)))
  expect_true(any(grepl("Test User", output)))
  expect_true(any(grepl("printing", output)))
  expect_true(any(grepl("Requires", output)))
  expect_true(any(grepl("Pipeline", output)))
  expect_true(any(grepl("Produces", output)))
})

test_that("Recipe round-trip preserves all information", {
  # Create a comprehensive recipe
  rec_original <- recipe(
    name = "Roundtrip Recipe",
    user = "Roundtrip User",
    svy = survey_empty(type = "ech", edition = "2023"),
    description = "Full round-trip test",
    topic = "labor",
    doi = "10.1234/roundtrip",
    
    step_recode(
      employed,
      status == 1 ~ 1,
      status == 2 ~ 0,
      .default = NA,
      comment = "Employment status"
    ),
    
    step_compute(
      employment_rate = employed / total * 100,
      comment = "Calculate rate"
    )
  )
  
  # Save
  tmp_file <- tempfile(fileext = ".json")
  save_recipe(rec_original, tmp_file)
  
  # Load
  rec_loaded <- read_recipe(tmp_file)
  
  # Compare key properties
  expect_equal(rec_loaded$name, rec_original$name)
  expect_equal(rec_loaded$user, rec_original$user)
  expect_equal(rec_loaded$description, rec_original$description)
  expect_equal(rec_loaded$topic, rec_original$topic)
  expect_equal(rec_loaded$doi, rec_original$doi)
  expect_equal(rec_loaded$survey_type, rec_original$survey_type)
  expect_equal(rec_loaded$edition, rec_original$edition)
  expect_length(rec_loaded$steps, length(rec_original$steps))
  
  # Compare doc()
  doc_original <- rec_original$doc()
  doc_loaded <- rec_loaded$doc()
  
  expect_equal(doc_loaded$input_variables, doc_original$input_variables)
  expect_equal(doc_loaded$output_variables, doc_original$output_variables)
  expect_equal(length(doc_loaded$pipeline), length(doc_original$pipeline))
  
  # Clean up
  unlink(tmp_file)
})
