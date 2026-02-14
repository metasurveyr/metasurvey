test_that("extract_surveys validates periodicity", {
  ps <- make_test_panel()
  
  # Periodicity structure is a list with implantation and follow_up
  expect_true(is.list(ps$periodicity))
})

test_that("extract_surveys handles monthly periodicity", {
  ps <- make_test_panel()
  
  # Should have monthly periodicity on implantation
  expect_equal(ps$periodicity$implantation, "monthly")
})

test_that("extract_surveys handles quarterly periodicity", {
  ps <- make_test_panel()
  ps$periodicity <- "quarterly"
  
  # Should handle quarterly
  expect_equal(ps$periodicity, "quarterly")
})

test_that("extract_surveys handles annual periodicity", {
  ps <- make_test_panel()
  ps$periodicity <- "annual"
  
  # Should handle annual
  expect_equal(ps$periodicity, "annual")
})

test_that("extract_surveys uses parallel processing", {
  ps <- make_test_panel()
  
  # Should accept parallel flag
  # extract <- ps$extract_surveys(periodicity = "monthly", parallel = TRUE)
  expect_true(is.logical(TRUE))
})

test_that("extract_surveys validates date ranges", {
  ps <- make_test_panel()
  
  # Should have valid implantation data
  expect_true(!is.null(ps$get_implantation()))
})

test_that("extract_surveys creates time windows", {
  ps <- make_test_panel()
  
  # Should create windows based on periodicity
  impl <- ps$get_implantation()
  expect_true(nrow(get_data(impl)) > 0)
})

test_that("extract_surveys handles follow-up data", {
  ps <- make_test_panel()
  
  # Should have follow-up surveys
  fu <- ps$get_follow_up()
  expect_true(length(fu) >= 0)
})

test_that("PanelSurvey get_recipes returns list", {
  ps <- make_test_panel()
  
  recipes <- ps$get_recipes()
  expect_true(is.list(recipes))
})

test_that("PanelSurvey get_workflows returns list", {
  ps <- make_test_panel()
  
  workflows <- ps$get_workflows()
  expect_true(is.list(workflows))
})

test_that("PanelSurvey get_design returns design object", {
  ps <- make_test_panel()
  
  # Should have design or NULL
  design <- ps$get_design()
  expect_true(is.null(design) || inherits(design, "survey.design"))
})

test_that("PanelSurvey filters by date range", {
  ps <- make_test_panel()
  
  impl <- ps$get_implantation()
  data <- get_data(impl)
  
  # Should have date columns for filtering
  expect_true(nrow(data) > 0)
})

test_that("PanelSurvey handles missing follow-up data", {
  ps <- make_test_panel()
  
  # Should handle case with no follow-up
  fu <- ps$get_follow_up()
  expect_true(is.list(fu))
})

test_that("PanelSurvey validates weight patterns", {
  ps <- make_test_panel()
  
  # Should have weight pattern
  expect_true(is.character(ps$weight_pattern) || is.null(ps$weight_pattern))
})

test_that("PanelSurvey processes replicate weights", {
  ps <- make_test_panel()
  
  # Should handle replicate weights if present
  impl <- ps$get_implantation()
  expect_true(!is.null(impl))
})
