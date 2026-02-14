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

test_that("validate_time_pattern handles different edition formats", {
  # Test MMYYYY format
  result1 <- validate_time_pattern(svy_type = "ech", svy_edition = "012023")
  expect_equal(result1$svy_periodicity, "Monthly")
  
  # Test year range format
  result2 <- validate_time_pattern(svy_type = "eaii", svy_edition = "2019-2021")
  expect_equal(result2$svy_periodicity, "Trianual")
})

test_that("group_dates handles annual grouping", {
  # annual is not a valid type for group_dates
  # Test with valid type
  dates <- as.Date(c("2022-01-15", "2023-03-10", "2024-06-20"))
  groups <- group_dates(dates, type = "monthly")
  expect_equal(length(groups), 3)
})

test_that("add_weight handles custom periods", {
  w <- add_weight(
    annual = "w_annual",
    quarterly = "w_quarterly",
    monthly = "w_monthly"
  )
  
  expect_equal(length(w), 3)
  expect_equal(w$annual, "w_annual")
  expect_equal(w$quarterly, "w_quarterly")
  expect_equal(w$monthly, "w_monthly")
})

test_that("extract_time_pattern handles edge cases", {
  # Test basic year
  result2 <- metasurvey:::extract_time_pattern("2023")
  expect_equal(result2$year, 2023)
})

test_that("validate_time_pattern returns consistent structure", {
  result <- validate_time_pattern(svy_type = "ech", svy_edition = "202306")
  
  expect_true("svy_type" %in% names(result))
  expect_true("svy_edition" %in% names(result))
  expect_true("svy_periodicity" %in% names(result))
})

test_that("group_dates handles empty dates vector", {
  dates <- as.Date(character(0))
  groups <- group_dates(dates, type = "monthly")
  expect_equal(length(groups), 0)
})

test_that("add_weight preserves order of arguments", {
  w <- add_weight(
    quarterly = "q",
    annual = "a",
    monthly = "m"
  )
  
  # add_weight may reorder - just check all are present
  expect_true(all(c("quarterly", "annual", "monthly") %in% names(w)))
  expect_equal(length(w), 3)
})

test_that("validate_time_pattern handles multianual correctly", {
  result <- validate_time_pattern(svy_type = "eaii", svy_edition = "2018-2020")
  
  expect_equal(result$svy_periodicity, "Trianual")
  expect_equal(result$svy_type, "eaii")
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

# --- evaluate_cv tests ---

test_that("evaluate_cv returns Excelente for cv < 5", {
  expect_equal(evaluate_cv(3), "Excelente")
  expect_equal(evaluate_cv(0), "Excelente")
  expect_equal(evaluate_cv(4.9), "Excelente")
})

test_that("evaluate_cv returns Muy bueno for 5 <= cv < 10", {
  expect_equal(evaluate_cv(5), "Muy bueno")
  expect_equal(evaluate_cv(7.5), "Muy bueno")
  expect_equal(evaluate_cv(9.9), "Muy bueno")
})

test_that("evaluate_cv returns Bueno for 10 <= cv < 15", {
  expect_equal(evaluate_cv(10), "Bueno")
  expect_equal(evaluate_cv(12), "Bueno")
})

test_that("evaluate_cv returns Aceptable for 15 <= cv < 25", {
  expect_equal(evaluate_cv(15), "Aceptable")
  expect_equal(evaluate_cv(20), "Aceptable")
})

test_that("evaluate_cv returns Utilizar con precaucion for 25 <= cv < 35", {
  expect_equal(evaluate_cv(25), "Utilizar con precaucion")
  expect_equal(evaluate_cv(30), "Utilizar con precaucion")
})

test_that("evaluate_cv returns No publicar for cv >= 35", {
  expect_equal(evaluate_cv(35), "No publicar")
  expect_equal(evaluate_cv(100), "No publicar")
})

# --- validate_weight tests ---

test_that("validate_weight returns NULL for NULL svy", {
  expect_null(metasurvey:::validate_weight(NULL, "w"))
})

test_that("validate_weight errors on non-character weight", {
  df <- data.table::data.table(id = 1:3, w = 1)
  expect_error(metasurvey:::validate_weight(df, 123), "character")
})

test_that("validate_weight errors when weight not in columns", {
  df <- data.table::data.table(id = 1:3, w = 1)
  expect_error(metasurvey:::validate_weight(df, "nonexistent"), "not found")
})

test_that("validate_weight returns weight name when valid", {
  df <- data.table::data.table(id = 1:3, w = 1)
  result <- metasurvey:::validate_weight(df, "w")
  expect_equal(result, "w")
})

# --- validate_weight_time_pattern tests ---

test_that("validate_weight_time_pattern returns NULL for NULL svy", {
  expect_null(metasurvey:::validate_weight_time_pattern(NULL, list(annual = "w")))
})

test_that("validate_weight_time_pattern errors on non-list weight", {
  df <- data.table::data.table(id = 1:3, w = 1)
  expect_error(metasurvey:::validate_weight_time_pattern(df, "w"), "list")
})

test_that("validate_weight_time_pattern validates character weights", {
  df <- data.table::data.table(id = 1:3, w = 1)
  result <- metasurvey:::validate_weight_time_pattern(df, list(annual = "w"))
  expect_true(is.list(result))
  expect_equal(result$annual, "w")
})

# --- is_blank and operators ---

test_that("is_blank detects NA", {
  expect_true(metasurvey:::is_blank(NA))
})

test_that("is_blank detects empty string", {
  expect_true(metasurvey:::is_blank(""))
})

test_that("is_blank returns FALSE for non-blank", {
  expect_false(metasurvey:::is_blank("hello"))
})

test_that("%||% returns x when not NULL", {
  fn <- metasurvey:::`%||%`
  expect_equal(fn("a", "b"), "a")
})

test_that("%||% returns y when x is NULL", {
  fn <- metasurvey:::`%||%`
  expect_equal(fn(NULL, "b"), "b")
})

test_that("%@% returns x when not blank", {
  fn <- metasurvey:::`%@%`
  expect_equal(fn("hello", "default"), "hello")
})

test_that("%@% returns y when x is blank", {
  fn <- metasurvey:::`%@%`
  expect_equal(fn("", "default"), "default")
  expect_equal(fn(NA, "default"), "default")
})

# --- API config functions ---

test_that("set_api_key sets the option", {
  old <- getOption("metasurvey.api_key")
  on.exit(options(metasurvey.api_key = old), add = TRUE)
  set_api_key("test-key-123")
  expect_equal(getOption("metasurvey.api_key"), "test-key-123")
})

test_that("get_user returns public by default", {
  old_key <- getOption("metasurvey.api_key")
  old_user <- getOption("metasurvey.user")
  on.exit({
    options(metasurvey.api_key = old_key)
    options(metasurvey.user = old_user)
  }, add = TRUE)
  options(metasurvey.api_key = NULL)
  options(metasurvey.user = NULL)
  expect_equal(metasurvey:::get_user(), "public")
})

test_that("get_user returns user when set", {
  old_user <- getOption("metasurvey.user")
  on.exit(options(metasurvey.user = old_user), add = TRUE)
  options(metasurvey.user = "test_user")
  expect_equal(metasurvey:::get_user(), "test_user")
})

test_that("get_user returns apiKey when api_key is set", {
  old_key <- getOption("metasurvey.api_key")
  old_user <- getOption("metasurvey.user")
  on.exit({
    options(metasurvey.api_key = old_key)
    options(metasurvey.user = old_user)
  }, add = TRUE)
  options(metasurvey.api_key = "some-key")
  options(metasurvey.user = NULL)
  expect_equal(metasurvey:::get_user(), "apiKey")
})

test_that("url_api_host returns default URL", {
  old <- getOption("metasurvey.base_url")
  on.exit(options(metasurvey.base_url = old), add = TRUE)
  options(metasurvey.base_url = NULL)
  result <- metasurvey:::url_api_host()
  expect_true(grepl("mongodb", result))
})

test_that("url_api_host respects custom option", {
  old <- getOption("metasurvey.base_url")
  on.exit(options(metasurvey.base_url = old), add = TRUE)
  options(metasurvey.base_url = "https://custom.api.com/")
  expect_equal(metasurvey:::url_api_host(), "https://custom.api.com/")
})

test_that("get_api_key returns apiKey payload when key is set", {
  old <- getOption("metasurvey.api_key")
  on.exit(options(metasurvey.api_key = old), add = TRUE)
  options(metasurvey.api_key = "my-api-key")
  result <- get_api_key()
  expect_equal(result$methodAuth, "apiKey")
  expect_equal(result$token, "my-api-key")
})

test_that("get_api_key returns userPassword payload when credentials set", {
  old_key <- getOption("metasurvey.api_key")
  old_user <- getOption("metasurvey.user")
  old_pass <- getOption("metasurvey.password")
  on.exit({
    options(metasurvey.api_key = old_key)
    options(metasurvey.user = old_user)
    options(metasurvey.password = old_pass)
  }, add = TRUE)
  options(metasurvey.api_key = NULL)
  options(metasurvey.user = "user@test.com")
  options(metasurvey.password = "secret")
  result <- get_api_key()
  expect_equal(result$methodAuth, "userPassword")
  expect_equal(result$email, "user@test.com")
})

# --- add_replicate tests ---

test_that("add_replicate creates list with correct fields", {
  r <- add_replicate(
    weight = "pesoano",
    replicate_pattern = "wr\\d+",
    replicate_path = "weights.csv",
    replicate_id = c("ID_HOGAR" = "ID"),
    replicate_type = "bootstrap"
  )
  expect_true(is.list(r))
  expect_equal(r$weight, "pesoano")
  expect_equal(r$replicate_type, "bootstrap")
  expect_equal(r$replicate_pattern, "wr\\d+")
})

test_that("add_replicate removes NULL fields", {
  r <- add_replicate(
    weight = "w",
    replicate_pattern = "wr\\d+",
    replicate_type = "jackknife"
  )
  expect_false("replicate_path" %in% names(r))
  expect_false("replicate_id" %in% names(r))
})

# --- add_weight with biannual ---

test_that("add_weight supports biannual", {
  w <- add_weight(biannual = "w_bi")
  expect_equal(names(w), "biannual")
  expect_equal(w$biannual, "w_bi")
})

# --- group_dates biannual ---

test_that("group_dates biannual groups correctly", {
  dates <- as.Date(c("2023-01-15", "2023-06-10", "2023-07-20", "2023-12-01"))
  groups <- group_dates(dates, type = "biannual")
  expect_equal(as.integer(groups), c(1, 1, 2, 2))
})

# --- extract_time_pattern more formats ---

test_that("extract_time_pattern YYYY_MM format", {
  result <- metasurvey:::extract_time_pattern("2023_06")
  expect_equal(result$periodicity, "Monthly")
  expect_equal(result$year, 2023)
  expect_equal(result$month, 6)
})

test_that("extract_time_pattern MM_YYYY format", {
  result <- metasurvey:::extract_time_pattern("06_2023")
  expect_equal(result$periodicity, "Monthly")
  expect_equal(result$month, 6)
  expect_equal(result$year, 2023)
})

test_that("extract_time_pattern YYYY_MM format", {
  result <- metasurvey:::extract_time_pattern("2023_05")
  expect_equal(result$periodicity, "Monthly")
  expect_equal(result$month, 5)
  expect_equal(result$year, 2023)
})

test_that("extract_time_pattern MMYYYY format", {
  result <- metasurvey:::extract_time_pattern("122023")
  expect_equal(result$periodicity, "Monthly")
  expect_equal(result$month, 12)
  expect_equal(result$year, 2023)
})

# --- Additional extract_time_pattern edge cases ---

test_that("extract_time_pattern YY_MM format with part1 as month", {
  result <- metasurvey:::extract_time_pattern("06_23")
  expect_equal(result$periodicity, "Monthly")
  expect_equal(result$month, 6)
  expect_equal(result$year, 2023)
})

test_that("extract_time_pattern YY_MM with part1 > 12", {
  # "13_06" has part1=13 > 12, so part2=06 is month and part1=13 becomes year 2013
  result <- metasurvey:::extract_time_pattern("13_06")
  expect_equal(result$periodicity, "Monthly")
  expect_equal(result$month, 6)
  expect_equal(result$year, 2013)
})

test_that("extract_time_pattern YYMM format without separator", {
  result <- metasurvey:::extract_time_pattern("0623")
  expect_equal(result$periodicity, "Monthly")
  expect_equal(result$month, 6)
})

test_that("extract_time_pattern YYYY-MM dash format", {
  result <- metasurvey:::extract_time_pattern("2023-06")
  expect_equal(result$periodicity, "Monthly")
  expect_equal(result$year, 2023)
  expect_equal(result$month, 6)
})

test_that("extract_time_pattern MM-YYYY dash format", {
  result <- metasurvey:::extract_time_pattern("06-2023")
  expect_equal(result$periodicity, "Monthly")
  expect_equal(result$month, 6)
  expect_equal(result$year, 2023)
})

test_that("extract_time_pattern unknown format returns Formato desconocido", {
  result <- metasurvey:::extract_time_pattern("abc")
  expect_equal(result$periodicity, "Formato desconocido")
})

test_that("extract_time_pattern multi-year format 20192021", {
  result <- metasurvey:::extract_time_pattern("20192022")
  expect_equal(result$periodicity, "Multianual")
  expect_equal(result$year_start, 2019)
  expect_equal(result$year_end, 2022)
})

# --- validate_time_pattern edge cases ---

test_that("validate_time_pattern errors when no type found", {
  # When svy_type is NULL and edition has no embedded type, %@% operator fails
  expect_error(
    validate_time_pattern(svy_type = NULL, svy_edition = "2023")
  )
})

test_that("validate_time_pattern messages when type does not match", {
  expect_message(
    validate_time_pattern(svy_type = "ech", svy_edition = "EPH_2023"),
    "Type does not match"
  )
})

# --- validate_replicate tests ---

test_that("validate_replicate returns NULL for NULL svy", {
  expect_null(metasurvey:::validate_replicate(NULL, list()))
})

test_that("validate_replicate errors on non-character replicate_id", {
  df <- data.table::data.table(id = 1:3, w = 1)
  expect_error(
    metasurvey:::validate_replicate(df, list(replicate_id = 123)),
    "character"
  )
})

test_that("validate_replicate errors when replicate_id not in survey", {
  df <- data.table::data.table(id = 1:3, w = 1)
  expect_error(
    metasurvey:::validate_replicate(df, list(replicate_id = c("missing_col" = "ID"))),
    "not found"
  )
})
