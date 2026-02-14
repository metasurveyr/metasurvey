test_that("load_panel_survey validates inputs", {
  # Test that it requires valid paths
  expect_error(
    load_panel_survey(
      path_implantation = "nonexistent_path",
      path_follow_up = "nonexistent_follow",
      svy_type = "ech",
      svy_weight_implantation = add_weight(annual = "w"),
      svy_weight_follow_up = add_weight(monthly = "w")
    )
  )
})

test_that("load_panel_survey validates weight time pattern", {
  # Create temporary directories
  temp_impl <- tempfile()
  temp_follow <- tempfile()
  dir.create(temp_impl)
  dir.create(temp_follow)
  on.exit({
    unlink(temp_impl, recursive = TRUE)
    unlink(temp_follow, recursive = TRUE)
  })
  
  # Create test CSV files
  df_impl <- data.table::data.table(id = 1:10, w = 1)
  df_follow <- data.table::data.table(id = 1:10, w = 1)
  
  data.table::fwrite(df_impl, file.path(temp_impl, "implantation.csv"))
  data.table::fwrite(df_follow, file.path(temp_follow, "follow1.csv"))
  
  # Test with multiple weight patterns (should error)
  expect_error(
    load_panel_survey(
      path_implantation = file.path(temp_impl, "implantation.csv"),
      path_follow_up = temp_follow,
      svy_type = "ech",
      svy_weight_implantation = add_weight(annual = "w"),
      svy_weight_follow_up = add_weight(monthly = "w", quarterly = "w")
    ),
    "must have a single weight time pattern"
  )
})

test_that("load_panel_survey basic structure validation", {
  # Just test the function signature exists
  expect_true(exists("load_panel_survey"))
  expect_type(load_panel_survey, "closure")
})
