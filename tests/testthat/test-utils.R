# Tests for utility functions

test_that("add_weight() with annual returns named list", {
  w <- add_weight(annual = "pesoano")
  expect_true(is.list(w))
  expect_equal(names(w), "annual")
  expect_equal(w$annual, "pesoano")
})

test_that("add_weight() with monthly returns named list", {
  w <- add_weight(monthly = "pesomes")
  expect_true(is.list(w))
  expect_equal(names(w), "monthly")
})

test_that("add_weight() with multiple periods", {
  w <- add_weight(monthly = "pesomes", annual = "pesoano", quarterly = "pesotri")
  expect_equal(length(w), 3)
  expect_true(all(c("monthly", "annual", "quarterly") %in% names(w)))
})

test_that("add_weight() with no args returns empty list", {
  w <- add_weight()
  expect_equal(length(w), 0)
})

test_that("extract_time_pattern annual", {
  result <- metasurvey:::extract_time_pattern("ECH_2023")
  expect_equal(result$periodicity, "Annual")
  expect_equal(result$year, 2023)
})

test_that("extract_time_pattern monthly YYYYMM", {
  result <- metasurvey:::extract_time_pattern("ECH_202301")
  expect_equal(result$periodicity, "Monthly")
  expect_equal(result$year, 2023)
  expect_equal(result$month, 1)
})

test_that("extract_time_pattern multianual", {
  result <- metasurvey:::extract_time_pattern("ECH_2019_2022")
  expect_equal(result$periodicity, "Multianual")
  expect_equal(result$year_start, 2019)
  expect_equal(result$year_end, 2022)
})

test_that("extract_time_pattern trianual", {
  result <- metasurvey:::extract_time_pattern("EAII_2019_2021")
  expect_equal(result$periodicity, "Trianual")
})

test_that("validate_time_pattern returns correct structure", {
  result <- validate_time_pattern(svy_type = "ech", svy_edition = "2023")
  expect_true(is.list(result))
  expect_equal(result$svy_type, "ech")
  expect_equal(result$svy_periodicity, "Annual")
})

test_that("validate_time_pattern monthly returns Date edition", {
  result <- validate_time_pattern(svy_type = "ech", svy_edition = "202301")
  expect_true(inherits(result$svy_edition, "Date"))
})

test_that("group_dates monthly groups correctly", {
  dates <- as.Date(c("2023-01-15", "2023-03-10", "2023-06-20"))
  groups <- group_dates(dates, type = "monthly")
  expect_equal(as.integer(groups), c(1, 3, 6))
})

test_that("group_dates quarterly groups correctly", {
  dates <- as.Date(c("2023-01-15", "2023-04-10", "2023-07-20", "2023-10-01"))
  groups <- group_dates(dates, type = "quarterly")
  expect_equal(as.integer(groups), c(1, 2, 3, 4))
})

test_that("use_copy_default returns logical", {
  result <- use_copy_default()
  expect_true(is.logical(result))
})

test_that("set_use_copy changes the option", {
  old <- use_copy_default()
  set_use_copy(FALSE)
  expect_false(use_copy_default())
  set_use_copy(old)
})

test_that("set_use_copy rejects non-logical", {
  expect_error(set_use_copy("yes"), "logical")
})

test_that("lazy_default returns logical", {
  result <- lazy_default()
  expect_true(is.logical(result))
})

test_that("set_lazy_processing changes the option", {
  old <- lazy_default()
  set_lazy_processing(FALSE)
  expect_false(lazy_default())
  set_lazy_processing(old)
})

test_that("set_lazy_processing rejects non-logical", {
  expect_error(set_lazy_processing(42), "logical")
})
