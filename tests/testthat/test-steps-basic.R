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
