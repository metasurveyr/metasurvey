# Tests for AST (Abstract Syntax Tree) evaluation engine

test_that("parse_ast handles literal numbers", {
  ast <- metasurvey:::parse_ast(quote(42))
  expect_equal(ast$type, "literal")
  expect_equal(ast$value, 42)
  expect_equal(ast$dependencies, character(0))
})

test_that("parse_ast handles literal strings", {
  ast <- metasurvey:::parse_ast(quote("hello"))
  expect_equal(ast$type, "literal")
  expect_equal(ast$value, "hello")
})

test_that("parse_ast handles logical literals", {
  ast <- metasurvey:::parse_ast(quote(TRUE))
  expect_equal(ast$type, "literal")
  expect_equal(ast$value, TRUE)
})

test_that("parse_ast handles symbols", {
  ast <- metasurvey:::parse_ast(quote(x))
  expect_equal(ast$type, "symbol")
  expect_equal(ast$value, "x")
  expect_equal(ast$dependencies, "x")
})

test_that("parse_ast handles function calls", {
  ast <- metasurvey:::parse_ast(quote(x + y))
  expect_equal(ast$type, "call")
  expect_equal(ast$value, "+")
  expect_true("x" %in% ast$dependencies)
  expect_true("y" %in% ast$dependencies)
})

test_that("parse_ast handles nested expressions", {
  ast <- metasurvey:::parse_ast(quote(ifelse(x > 1, a, b)))
  expect_equal(ast$type, "call")
  expect_equal(ast$value, "ifelse")
  expect_true(all(c("x", "a", "b") %in% ast$dependencies))
})

test_that("parse_ast errors on missing expression", {
  expect_error(metasurvey:::parse_ast())
})

test_that("get_ast_dependencies extracts correct variables", {
  ast <- metasurvey:::parse_ast(quote(income * tax_rate + bonus))
  deps <- metasurvey:::get_ast_dependencies(ast)
  expect_true(all(c("income", "tax_rate", "bonus") %in% deps))
})

test_that("get_ast_dependencies returns empty for literals", {
  ast <- metasurvey:::parse_ast(quote(42))
  deps <- metasurvey:::get_ast_dependencies(ast)
  expect_equal(length(deps), 0)
})

test_that("evaluate_ast computes x + y correctly", {
  ast <- metasurvey:::parse_ast(quote(x + y))
  dt <- data.table::data.table(x = c(1, 2, 3), y = c(10, 20, 30))
  result <- metasurvey:::evaluate_ast(ast, dt)
  expect_equal(result, c(11, 22, 33))
})

test_that("evaluate_ast errors on missing variables", {
  ast <- metasurvey:::parse_ast(quote(nonexistent_var + 1))
  dt <- data.table::data.table(x = 1:3)
  expect_error(
    metasurvey:::evaluate_ast(ast, dt),
    "Missing variables"
  )
})

test_that("evaluate_ast errors on non-data.frame input", {
  ast <- metasurvey:::parse_ast(quote(x + 1))
  expect_error(
    metasurvey:::evaluate_ast(ast, "not a dataframe"),
    "data.frame"
  )
})

test_that("evaluate_ast works with ifelse", {
  ast <- metasurvey:::parse_ast(quote(ifelse(x > 2, "high", "low")))
  dt <- data.table::data.table(x = c(1, 3, 5))
  result <- metasurvey:::evaluate_ast(ast, dt)
  expect_equal(result, c("low", "high", "high"))
})

test_that("optimize_ast folds constants", {
  ast <- metasurvey:::parse_ast(quote(5 * 2))
  optimized <- metasurvey:::optimize_ast(ast)
  expect_equal(optimized$type, "literal")
  expect_equal(optimized$value, 10)
})

test_that("optimize_ast preserves non-constant expressions", {
  ast <- metasurvey:::parse_ast(quote(x + 1))
  optimized <- metasurvey:::optimize_ast(ast)
  expect_equal(optimized$type, "call")
  expect_true("x" %in% optimized$dependencies)
})

test_that("optimize_ast handles addition constants", {
  ast <- metasurvey:::parse_ast(quote(3 + 7))
  optimized <- metasurvey:::optimize_ast(ast)
  expect_equal(optimized$type, "literal")
  expect_equal(optimized$value, 10)
})

test_that("evaluate_ast works with data.frame (not just data.table)", {
  ast <- metasurvey:::parse_ast(quote(a * 2))
  df <- data.frame(a = c(5, 10))
  result <- metasurvey:::evaluate_ast(ast, df)
  expect_equal(result, c(10, 20))
})
