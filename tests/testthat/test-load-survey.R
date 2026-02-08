# Tests for load_survey

test_that("load_survey creates Survey from CSV file", {
  df <- data.table::data.table(id = 1:5, income = c(100, 200, 300, 400, 500), w = 1)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  data.table::fwrite(df, tmp)

  s <- load_survey(
    path = tmp,
    svy_type = "ech",
    svy_edition = "2023",
    svy_weight = add_weight(annual = "w")
  )

  expect_s3_class(s, "Survey")
  expect_equal(nrow(get_data(s)), 5)
  expect_true("income" %in% names(get_data(s)))
})

test_that("load_survey with weight creates survey design", {
  df <- data.table::data.table(id = 1:5, val = rnorm(5), w = runif(5, 0.5, 2))
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  data.table::fwrite(df, tmp)

  s <- load_survey(
    path = tmp,
    svy_type = "ech",
    svy_edition = "2023",
    svy_weight = add_weight(annual = "w")
  )

  expect_true(length(s$design) >= 1)
  expect_true(inherits(s$design[[1]], "survey.design"))
})

test_that("load_survey preserves all columns", {
  df <- data.table::data.table(
    id = 1:3, a = c("x", "y", "z"), b = c(10, 20, 30), w = 1
  )
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  data.table::fwrite(df, tmp)

  s <- load_survey(
    path = tmp,
    svy_type = "ech",
    svy_edition = "2023",
    svy_weight = add_weight(annual = "w")
  )

  expect_true(all(c("id", "a", "b", "w") %in% names(get_data(s))))
})
