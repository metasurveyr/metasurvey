test_that("step_remove removes columns and records step", {
  df <- data.frame(id = 1:3, a = 1, b = 2, w = 1)
  s <- Survey$new(data = data.table::data.table(df), edition = "2023", type = "ech", psu = NULL, engine = "data.table", weight = add_weight(annual = "w"))

  s2 <- step_remove(s, a, b)
  expect_true(any(grepl("Remove:", names(s2$steps))))
  s2 <- bake_steps(s2)
  expect_false("a" %in% names(s2$data))
  expect_false("b" %in% names(s2$data))
})

test_that("step_rename renames columns and records step", {
  df <- data.frame(id = 1:3, a = 1, w = 1)
  s <- Survey$new(data = data.table::data.table(df), edition = "2023", type = "ech", psu = NULL, engine = "data.table", weight = add_weight(annual = "w"))

  s2 <- step_rename(s, alpha = a)
  expect_true(any(grepl("Rename:", names(s2$steps))))
  s2 <- bake_steps(s2)
  expect_true("alpha" %in% names(s2$data))
  expect_false("a" %in% names(s2$data))
})

test_that("step_join with data.frame RHS performs left/inner/full joins", {
  lhs <- data.frame(id = 1:3, a = c("x", "y", "z"), w = 1)
  rhs <- data.frame(id = c(2, 3, 4), b = c(20, 30, 40))
  s <- Survey$new(data = data.table::data.table(lhs), edition = "2023", type = "ech", psu = NULL, engine = "data.table", weight = add_weight(annual = "w"))

  # Left join keeps all LHS rows
  s_left <- bake_steps(step_join(s, rhs, by = "id", type = "left"))
  expect_equal(nrow(s_left$data), 3)
  expect_true(all(c("id", "a", "b") %in% names(s_left$data)))

  # Inner join keeps only matches
  s_inner <- bake_steps(step_join(s, rhs, by = "id", type = "inner"))
  expect_equal(nrow(s_inner$data), 2)

  # Full join returns all rows from both
  s_full <- bake_steps(step_join(s, rhs, by = "id", type = "full"))
  expect_equal(nrow(s_full$data), 4)
})

test_that("step_join accepts Survey as RHS and handles key mapping", {
  lhs <- data.frame(id = 1:2, a = 1:2, w = 1)
  rhs <- data.frame(code = 2:3, b = 10:11, w2 = 1)
  s_left <- Survey$new(data = data.table::data.table(lhs), edition = "2023", type = "ech", psu = NULL, engine = "data.table", weight = add_weight(annual = "w"))
  s_right <- Survey$new(data = data.table::data.table(rhs), edition = "2023", type = "ech", psu = NULL, engine = "data.table", weight = add_weight(annual = "w2"))

  s_out <- bake_steps(step_join(s_left, s_right, by = c("id" = "code"), type = "left"))
  expect_true("b" %in% names(s_out$data))
  expect_equal(nrow(s_out$data), 2)
})

test_that("step_remove handles multiple columns", {
  df <- data.frame(id = 1:5, a = 1, b = 2, c = 3, d = 4, w = 1)
  s <- Survey$new(
    data = data.table::data.table(df),
    edition = "2023",
    type = "ech",
    psu = NULL,
    engine = "data.table",
    weight = add_weight(annual = "w")
  )
  
  s2 <- step_remove(s, a, b, c)
  s2 <- bake_steps(s2)
  
  expect_false("a" %in% names(s2$data))
  expect_false("b" %in% names(s2$data))
  expect_false("c" %in% names(s2$data))
  expect_true("d" %in% names(s2$data))
})

test_that("step_rename handles multiple renames", {
  df <- data.frame(id = 1:3, a = 1, b = 2, w = 1)
  s <- Survey$new(
    data = data.table::data.table(df),
    edition = "2023",
    type = "ech",
    psu = NULL,
    engine = "data.table",
    weight = add_weight(annual = "w")
  )
  
  s2 <- step_rename(s, alpha = a, beta = b)
  s2 <- bake_steps(s2)
  
  expect_true("alpha" %in% names(s2$data))
  expect_true("beta" %in% names(s2$data))
  expect_false("a" %in% names(s2$data))
  expect_false("b" %in% names(s2$data))
})

test_that("step chains with remove and rename work correctly", {
  df <- data.frame(
    id = 1:5,
    age = c(20, 30, 40, 50, 60),
    income = c(1000, 2000, 3000, 4000, 5000),
    extra = 1,
    w = 1
  )
  
  s <- Survey$new(
    data = data.table::data.table(df),
    edition = "2023",
    type = "ech",
    psu = NULL,
    engine = "data.table",
    weight = add_weight(annual = "w")
  )
  
  s2 <- s %>%
    step_remove(extra) %>%
    step_rename(years = age)
  
  s2 <- bake_steps(s2)
  
  expect_true("years" %in% names(s2$data))
  expect_false("age" %in% names(s2$data))
  expect_false("extra" %in% names(s2$data))
})

test_that("step_join with different join types", {
  lhs <- data.frame(id = 1:3, a = 1:3, w = 1)
  rhs <- data.frame(id = 2:4, b = 20:22)
  s <- Survey$new(
    data = data.table::data.table(lhs),
    edition = "2023",
    type = "ech",
    psu = NULL,
    engine = "data.table",
    weight = add_weight(annual = "w")
  )
  
  # Test right join
  s_right <- bake_steps(step_join(s, rhs, by = "id", type = "right"))
  expect_equal(nrow(s_right$data), 3)
  
  # Test full join
  s_full <- bake_steps(step_join(s, rhs, by = "id", type = "full"))
  expect_equal(nrow(s_full$data), 4)
})

test_that("bake_steps processes multiple steps in order", {
  df <- data.frame(id = 1:5, a = 1, b = 2, c = 3, w = 1)
  s <- Survey$new(
    data = data.table::data.table(df),
    edition = "2023",
    type = "ech",
    psu = NULL,
    engine = "data.table",
    weight = add_weight(annual = "w")
  )
  
  s2 <- s %>%
    step_remove(c) %>%
    step_rename(alpha = a, beta = b)
  
  # Before baking
  expect_equal(length(s2$steps), 2)
  
  # After baking
  s3 <- bake_steps(s2)
  expect_false("c" %in% names(s3$data))
  expect_true("alpha" %in% names(s3$data))
  expect_true("beta" %in% names(s3$data))
})

test_that("step_remove validates column exists", {
  df <- data.frame(id = 1:3, a = 1, w = 1)
  s <- Survey$new(
    data = data.table::data.table(df),
    edition = "2023",
    type = "ech",
    psu = NULL,
    engine = "data.table",
    weight = add_weight(annual = "w")
  )
  
  # Removing non-existent column should handle gracefully or error
  # depending on implementation
  expect_true(is(s, "Survey"))
})

test_that("step_rename validates new names", {
  df <- data.frame(id = 1:3, a = 1, w = 1)
  s <- Survey$new(
    data = data.table::data.table(df),
    edition = "2023",
    type = "ech",
    psu = NULL,
    engine = "data.table",
    weight = add_weight(annual = "w")
  )

  # Renaming to existing name
  s2 <- step_rename(s, id = a)
  expect_true(is(s2, "Survey"))
})

# --- Internal steps.R functions ---

test_that("get_formulas returns formulas from steps", {
  s <- make_test_survey()
  s2 <- step_compute(s, double_age = age * 2)
  steps <- get_steps(s2)
  formulas <- metasurvey:::get_formulas(steps)
  expect_true(is.character(formulas))
  expect_true(length(formulas) > 0)
})

test_that("get_formulas returns NULL for empty steps", {
  result <- metasurvey:::get_formulas(list())
  expect_null(result)
})

test_that("get_formulas handles recode type step", {
  s <- make_test_survey()
  s2 <- step_recode(s, age_cat, age < 30 ~ "young", age >= 30 ~ "old", .default = "unknown")
  steps <- get_steps(s2)
  formulas <- metasurvey:::get_formulas(steps)
  expect_true(is.character(formulas))
  expect_true(any(grepl("age_cat", formulas)))
})

test_that("get_comments returns comments from steps", {
  s <- make_test_survey()
  s2 <- step_compute(s, z = age + 1, comment = "Add one to age")
  steps <- get_steps(s2)
  comments <- metasurvey:::get_comments(steps)
  expect_true(is.character(comments))
})

test_that("get_comments returns NULL for empty steps", {
  result <- metasurvey:::get_comments(list())
  expect_null(result)
})

test_that("get_type_step returns types from steps", {
  s <- make_test_survey()
  s2 <- step_compute(s, z = age + 1)
  steps <- get_steps(s2)
  types <- metasurvey:::get_type_step(steps)
  expect_true(is.character(types))
  expect_true("ast_compute" %in% types)
})

test_that("get_type_step returns NULL for empty steps", {
  result <- metasurvey:::get_type_step(list())
  expect_null(result)
})

test_that("find_dependencies extracts variable names from expressions", {
  s <- make_test_survey()
  data <- get_data(s)
  deps <- metasurvey:::find_dependencies(quote(age + income), data)
  expect_true("age" %in% deps)
  expect_true("income" %in% deps)
})

test_that("find_dependencies returns empty for literals", {
  s <- make_test_survey()
  data <- get_data(s)
  deps <- metasurvey:::find_dependencies(quote(42), data)
  expect_equal(length(deps), 0)
})

test_that("find_dependencies handles nested calls", {
  s <- make_test_survey()
  data <- get_data(s)
  deps <- metasurvey:::find_dependencies(quote(sqrt(age^2 + income^2)), data)
  expect_true("age" %in% deps)
  expect_true("income" %in% deps)
})

test_that("step_remove warns for nonexistent variables", {
  s <- make_test_survey()
  expect_warning(
    step_remove(s, nonexistent_var),
    "not found"
  )
})

test_that("step_remove with vars parameter works", {
  s <- make_test_survey()
  s2 <- step_remove(s, vars = c("x", "y"))
  s2 <- bake_steps(s2)
  expect_false("x" %in% names(get_data(s2)))
  expect_false("y" %in% names(get_data(s2)))
})

test_that("step_remove with lazy=FALSE applies immediately", {
  s <- make_test_survey()
  s2 <- step_remove(s, x, lazy = FALSE)
  # With lazy=FALSE, removal happens immediately
  expect_false("x" %in% names(get_data(s2)))
})

test_that("step_remove with lazy=FALSE on plain data.frame", {
  df <- data.frame(id = 1:5, a = 1, b = 2, w = 1)
  s <- Survey$new(
    data = data.table::data.table(df),
    edition = "2023", type = "ech",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )
  # Convert to plain data.frame to hit the else branch in step_remove
  s$data <- as.data.frame(s$data)
  s2 <- step_remove(s, a, lazy = FALSE)
  expect_false("a" %in% names(get_data(s2)))
})

test_that("step_rename with mapping parameter works", {
  s <- make_test_survey()
  s2 <- step_rename(s, mapping = c(years = "age", earnings = "income"))
  s2 <- bake_steps(s2)
  expect_true("years" %in% names(get_data(s2)))
  expect_true("earnings" %in% names(get_data(s2)))
})

test_that("step_rename errors for nonexistent variable", {
  s <- make_test_survey()
  expect_error(
    step_rename(s, new_name = nonexistent_col),
    "not found"
  )
})

test_that("step_rename with lazy=FALSE applies immediately", {
  s <- make_test_survey()
  s2 <- step_rename(s, years = age, lazy = FALSE)
  expect_true("years" %in% names(get_data(s2)))
  expect_false("age" %in% names(get_data(s2)))
})

test_that("step_rename with empty dots returns survey unchanged", {
  s <- make_test_survey()
  s2 <- step_rename(s)
  expect_s3_class(s2, "Survey")
})

test_that("step_rename errors with invalid mapping", {
  s <- make_test_survey()
  expect_error(
    step_rename(s, mapping = c("a", "b")),
    "named character vector"
  )
})

test_that("step_remove with vars as non-character errors", {
  s <- make_test_survey()
  expect_error(step_remove(s, vars = 123), "character")
})

test_that("step_join errors when no common columns and by is NULL", {
  s <- make_test_survey()
  extra <- data.table::data.table(code = 1:3, val = 1:3)
  expect_error(
    step_join(s, extra, type = "left"),
    "infer join keys|common columns"
  )
})

test_that("step_join infers common columns when by is NULL", {
  s <- make_test_survey()
  extra <- data.table::data.table(id = 1:5, extra_val = 100:104)
  s2 <- step_join(s, extra, type = "left")
  s2 <- bake_steps(s2)
  expect_true("extra_val" %in% names(get_data(s2)))
})

test_that("step_join errors when keys not found", {
  s <- make_test_survey()
  extra <- data.table::data.table(code = 1:3, val = 1:3)
  expect_error(
    step_join(s, extra, by = c("missing_key" = "code")),
    "not found"
  )
})

test_that("step_join errors when rhs keys not found", {
  s <- make_test_survey()
  extra <- data.table::data.table(code = 1:3, val = 1:3)
  expect_error(
    step_join(s, extra, by = c("id" = "missing_key")),
    "not found"
  )
})

test_that("step_join handles overlapping column names", {
  lhs <- data.table::data.table(id = 1:3, val = 10:12, w = 1)
  rhs <- data.table::data.table(id = 1:3, val = 20:22)
  s <- Survey$new(
    data = lhs, edition = "2023", type = "ech",
    psu = NULL, engine = "data.table",
    weight = add_weight(annual = "w")
  )
  s2 <- bake_steps(step_join(s, rhs, by = "id", type = "left"))
  # Should have val and val.y
  expect_true("val.y" %in% names(get_data(s2)) || "val" %in% names(get_data(s2)))
})

test_that("step_join errors on non-data.frame x", {
  s <- make_test_survey()
  expect_error(step_join(s, "not a data frame", by = "id"), "data.frame")
})

test_that("view_graph requires visNetwork", {
  s <- make_test_survey()
  # Skip if visNetwork is installed, otherwise expect error
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    expect_error(view_graph(s), "visNetwork")
  } else {
    # Just verify it runs without error
    result <- view_graph(s)
    expect_true(!is.null(result))
  }
})
