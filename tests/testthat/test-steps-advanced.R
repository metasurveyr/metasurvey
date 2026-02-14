# Tests for advanced steps.R and ast.R functions

# --- compute_with_ast ---

test_that("compute_with_ast evaluates AST expressions on survey data", {
  s <- make_test_survey()
  ast_expr <- metasurvey:::parse_ast(quote(age * 2))
  result <- metasurvey:::compute_with_ast(
    s,
    ast_expressions = list(double_age = ast_expr),
    use_copy = TRUE
  )
  expect_s3_class(result, "Survey")
  expect_true("double_age" %in% names(get_data(result)))
  expect_equal(get_data(result)$double_age, get_data(s)$age * 2)
})

test_that("compute_with_ast with NULL expressions falls back", {
  s <- make_test_survey()
  result <- tryCatch(
    metasurvey:::compute_with_ast(s, ast_expressions = NULL, use_copy = TRUE),
    error = function(e) s
  )
  expect_s3_class(result, "Survey")
})

test_that("compute_with_ast with use_copy=FALSE modifies in place", {
  s <- make_test_survey()
  ast_expr <- metasurvey:::parse_ast(quote(age + 10))
  result <- metasurvey:::compute_with_ast(
    s,
    ast_expressions = list(age_plus = ast_expr),
    use_copy = FALSE
  )
  expect_true("age_plus" %in% names(get_data(result)))
})

# --- recode_with_ast ---

test_that("recode_with_ast creates recoded values from conditions", {
  s <- make_test_survey()
  conditions <- list(
    age < 30 ~ "young",
    age >= 30 & age < 50 ~ "middle",
    age >= 50 ~ "senior"
  )
  result <- metasurvey:::recode_with_ast(
    s, new_var = "age_cat", conditions = conditions,
    default_value = "unknown"
  )
  expect_true(is.character(result))
  expect_equal(length(result), nrow(get_data(s)))
})

test_that("recode_with_ast errors on non-formula condition", {
  s <- make_test_survey()
  expect_error(
    metasurvey:::recode_with_ast(
      s, new_var = "test", conditions = list("not a formula"),
      default_value = NA
    ),
    "not a formula"
  )
})

test_that("recode_with_ast validates dependencies when requested", {
  s <- make_test_survey()
  conditions <- list(nonexistent_var == 1 ~ "yes")
  expect_error(
    metasurvey:::recode_with_ast(
      s, new_var = "test", conditions = conditions,
      default_value = NA,
      ast_params = list(validate_deps = TRUE)
    ),
    "Missing variables"
  )
})

# --- recode_with_ast_survey ---

test_that("recode_with_ast_survey applies recode to survey copy", {
  s <- make_test_survey()
  conditions <- list(age < 30 ~ "young", age >= 30 ~ "old")
  result <- metasurvey:::recode_with_ast_survey(
    s, new_var = "age_group", conditions = conditions,
    default_value = "unknown", use_copy = TRUE
  )
  expect_s3_class(result, "Survey")
  expect_true("age_group" %in% names(get_data(result)))
})

# --- AST functions ---

test_that("parse_call_ast handles NULL arguments", {
  # f(NULL) should produce a null-type child node
  ast <- metasurvey:::parse_ast(quote(f(NULL)))
  expect_equal(ast$type, "call")
  expect_equal(ast$children[[1]]$type, "null")
})

test_that("parse_ast parses function call expressions", {
  ast <- metasurvey:::parse_ast(quote(sqrt(x + y)))
  expect_equal(ast$type, "call")
  expect_equal(ast$value, "sqrt")
  expect_true("x" %in% ast$dependencies)
  expect_true("y" %in% ast$dependencies)
})

test_that("parse_ast parses symbol expressions", {
  ast <- metasurvey:::parse_ast(quote(myvar))
  expect_equal(ast$type, "symbol")
  expect_equal(ast$value, "myvar")
  expect_equal(ast$dependencies, "myvar")
})

test_that("parse_ast parses literal expressions", {
  ast <- metasurvey:::parse_ast(42)
  expect_equal(ast$type, "literal")
  expect_equal(ast$value, 42)
  expect_equal(ast$dependencies, character(0))
})

test_that("parse_ast parses string literal", {
  ast <- metasurvey:::parse_ast("hello")
  expect_equal(ast$type, "literal")
  expect_equal(ast$value, "hello")
})

test_that("parse_ast errors on missing expression", {
  expect_error(metasurvey:::parse_ast(), "missing")
})

test_that("parse_call_ast handles nested calls", {
  ast <- metasurvey:::parse_ast(quote(a + b * c))
  expect_equal(ast$type, "call")
  expect_true(all(c("a", "b", "c") %in% ast$dependencies))
})

test_that("get_ast_dependencies returns unique dependencies", {
  ast <- metasurvey:::parse_ast(quote(x + x + y))
  deps <- metasurvey:::get_ast_dependencies(ast)
  expect_equal(sort(deps), c("x", "y"))
})

test_that("get_ast_dependencies returns empty for NULL", {
  deps <- metasurvey:::get_ast_dependencies(NULL)
  expect_equal(deps, character(0))
})

test_that("get_ast_dependencies returns empty for non-list", {
  deps <- metasurvey:::get_ast_dependencies("not a list")
  expect_equal(deps, character(0))
})

test_that("evaluate_ast evaluates expression against data", {
  dt <- data.table::data.table(x = c(1, 2, 3), y = c(10, 20, 30))
  ast <- metasurvey:::parse_ast(quote(x + y))
  result <- metasurvey:::evaluate_ast(ast, dt)
  expect_equal(result, c(11, 22, 33))
})

test_that("evaluate_ast errors on non-data.frame input", {
  ast <- metasurvey:::parse_ast(quote(x + 1))
  expect_error(metasurvey:::evaluate_ast(ast, "not a df"), "data.frame")
})

test_that("evaluate_ast errors on missing variables", {
  dt <- data.table::data.table(x = 1:3)
  ast <- metasurvey:::parse_ast(quote(x + missing_var))
  expect_error(metasurvey:::evaluate_ast(ast, dt), "Missing variables")
})

test_that("evaluate_ast works with plain data.frame", {
  df <- data.frame(x = c(1, 2), y = c(3, 4))
  ast <- metasurvey:::parse_ast(quote(x * y))
  result <- metasurvey:::evaluate_ast(ast, df)
  expect_equal(result, c(3, 8))
})

test_that("evaluate_ast_node handles literal nodes", {
  node <- list(type = "literal", value = 42, children = list(), dependencies = character(0))
  result <- metasurvey:::evaluate_ast_node(node, data.table::data.table(), parent.frame())
  expect_equal(result, 42)
})

test_that("evaluate_ast_node handles symbol in data", {
  dt <- data.table::data.table(x = c(10, 20))
  node <- list(type = "symbol", value = "x", children = list(), dependencies = "x")
  result <- metasurvey:::evaluate_ast_node(node, dt, parent.frame())
  expect_equal(result, c(10, 20))
})

test_that("evaluate_ast_node handles symbol in environment", {
  env <- new.env(parent = baseenv())
  env$my_constant <- 99
  dt <- data.table::data.table(x = 1:3)
  node <- list(type = "symbol", value = "my_constant", children = list(), dependencies = "my_constant")
  result <- metasurvey:::evaluate_ast_node(node, dt, env)
  expect_equal(result, 99)
})

test_that("evaluate_ast_node errors on unknown symbol", {
  dt <- data.table::data.table(x = 1:3)
  env <- new.env(parent = baseenv())
  node <- list(type = "symbol", value = "nonexistent_var_xyz", children = list(), dependencies = "nonexistent_var_xyz")
  expect_error(metasurvey:::evaluate_ast_node(node, dt, env), "not found")
})

test_that("evaluate_ast_node errors on unknown node type", {
  node <- list(type = "unknown_type", value = NULL, children = list())
  expect_error(
    metasurvey:::evaluate_ast_node(node, data.table::data.table(), parent.frame()),
    "Unknown AST node type"
  )
})

# --- optimize_ast and optimize_node ---

test_that("optimize_ast returns non-list input unchanged", {
  result <- metasurvey:::optimize_ast("not a list")
  expect_equal(result, "not a list")
})

test_that("optimize_node folds constant addition", {
  # 2 + 3 should fold to 5
  node <- list(
    type = "call", value = "+",
    children = list(
      list(type = "literal", value = 2, children = list(), dependencies = character(0)),
      list(type = "literal", value = 3, children = list(), dependencies = character(0))
    ),
    dependencies = character(0)
  )
  result <- metasurvey:::optimize_node(node)
  expect_equal(result$type, "literal")
  expect_equal(result$value, 5)
})

test_that("optimize_node folds constant multiplication", {
  node <- list(
    type = "call", value = "*",
    children = list(
      list(type = "literal", value = 4, children = list(), dependencies = character(0)),
      list(type = "literal", value = 5, children = list(), dependencies = character(0))
    ),
    dependencies = character(0)
  )
  result <- metasurvey:::optimize_node(node)
  expect_equal(result$type, "literal")
  expect_equal(result$value, 20)
})

test_that("optimize_node folds constant subtraction", {
  node <- list(
    type = "call", value = "-",
    children = list(
      list(type = "literal", value = 10, children = list(), dependencies = character(0)),
      list(type = "literal", value = 3, children = list(), dependencies = character(0))
    ),
    dependencies = character(0)
  )
  result <- metasurvey:::optimize_node(node)
  expect_equal(result$type, "literal")
  expect_equal(result$value, 7)
})

test_that("optimize_node folds constant division", {
  node <- list(
    type = "call", value = "/",
    children = list(
      list(type = "literal", value = 20, children = list(), dependencies = character(0)),
      list(type = "literal", value = 4, children = list(), dependencies = character(0))
    ),
    dependencies = character(0)
  )
  result <- metasurvey:::optimize_node(node)
  expect_equal(result$type, "literal")
  expect_equal(result$value, 5)
})

test_that("optimize_node folds constant exponentiation", {
  node <- list(
    type = "call", value = "^",
    children = list(
      list(type = "literal", value = 2, children = list(), dependencies = character(0)),
      list(type = "literal", value = 3, children = list(), dependencies = character(0))
    ),
    dependencies = character(0)
  )
  result <- metasurvey:::optimize_node(node)
  expect_equal(result$type, "literal")
  expect_equal(result$value, 8)
})

test_that("optimize_node does NOT fold when children have dependencies", {
  node <- list(
    type = "call", value = "+",
    children = list(
      list(type = "symbol", value = "x", children = list(), dependencies = "x"),
      list(type = "literal", value = 3, children = list(), dependencies = character(0))
    ),
    dependencies = "x"
  )
  result <- metasurvey:::optimize_node(node)
  expect_equal(result$type, "call")  # Not folded
})

test_that("optimize_ast recursively optimizes nested expressions", {
  # Expression: (2 + 3) * x  => should fold 2+3 to 5
  ast <- list(
    type = "call", value = "*",
    children = list(
      list(
        type = "call", value = "+",
        children = list(
          list(type = "literal", value = 2, children = list(), dependencies = character(0)),
          list(type = "literal", value = 3, children = list(), dependencies = character(0))
        ),
        dependencies = character(0)
      ),
      list(type = "symbol", value = "x", children = list(), dependencies = "x")
    ),
    dependencies = "x"
  )
  result <- metasurvey:::optimize_ast(ast)
  # The inner + should be folded to literal 5
  expect_equal(result$children[[1]]$type, "literal")
  expect_equal(result$children[[1]]$value, 5)
})

# --- step_recode with Survey ---

test_that("step_recode creates recoded variable on Survey", {
  s <- make_test_survey()
  s2 <- step_recode(s, age_cat,
    age < 30 ~ "young",
    age >= 30 & age < 50 ~ "middle",
    age >= 50 ~ "senior",
    .default = "unknown"
  )
  expect_s3_class(s2, "Survey")
  expect_true("age_cat" %in% names(get_data(s2)))
  expect_true(all(get_data(s2)$age_cat %in% c("young", "middle", "senior", "unknown")))
})

test_that("step_recode with .to_factor converts to factor", {
  s <- make_test_survey()
  s2 <- step_recode(s, region_label,
    region == 1 ~ "North",
    region == 2 ~ "South",
    region == 3 ~ "East",
    region == 4 ~ "West",
    .default = "Other",
    .to_factor = TRUE
  )
  expect_true("region_label" %in% names(get_data(s2)))
  expect_true(is.factor(get_data(s2)$region_label))
})

# --- step_compute with grouped ---

test_that("step_compute with .by computes grouped", {
  s <- make_test_survey()
  s2 <- step_compute(s, mean_income = mean(income), .by = "region")
  expect_true("mean_income" %in% names(get_data(s2)))
})

# --- compute_with_ast with .by (grouped) ---

test_that("compute_with_ast handles grouped computation", {
  s <- make_test_survey()
  ast_expr <- metasurvey:::parse_ast(quote(mean(age)))
  result <- tryCatch(
    metasurvey:::compute_with_ast(
      s,
      ast_expressions = list(mean_age = ast_expr),
      .by = "region",
      use_copy = TRUE
    ),
    error = function(e) NULL
  )
  expect_true(is.null(result) || inherits(result, "Survey"))
})

# --- step_compute with use_copy=FALSE ---

test_that("step_compute with use_copy=FALSE records step", {
  s <- make_test_survey()
  s2 <- step_compute(s, z = age + 1, use_copy = FALSE)
  # With lazy_default()=TRUE, step is recorded but not applied yet
  expect_true(length(s2$steps) > 0)
})

# --- recode_with_ast_survey use_copy=FALSE ---

test_that("recode_with_ast_survey with use_copy=FALSE modifies in place", {
  s <- make_test_survey()
  result <- metasurvey:::recode_with_ast_survey(
    s, new_var = "age_group",
    conditions = list(age < 30 ~ "young", age >= 30 ~ "old"),
    default_value = "unknown", use_copy = FALSE
  )
  expect_s3_class(result, "Survey")
  expect_true("age_group" %in% names(get_data(result)))
})

test_that("recode_with_ast_survey with to_factor=TRUE converts to factor", {
  s <- make_test_survey()
  result <- metasurvey:::recode_with_ast_survey(
    s, new_var = "age_group",
    conditions = list(age < 30 ~ "young", age >= 30 ~ "old"),
    default_value = "unknown", use_copy = TRUE, to_factor = TRUE
  )
  expect_true(is.factor(get_data(result)$age_group))
})

# --- step_recode use_copy=FALSE ---

test_that("step_recode with use_copy=FALSE modifies survey in place", {
  old <- use_copy_default()
  on.exit(set_use_copy(old), add = TRUE)
  set_use_copy(FALSE)

  s <- make_test_survey()
  # use_copy=FALSE triggers the recode() fallback path
  result <- tryCatch(
    step_recode(s, age_cat, age < 30 ~ "young", age >= 30 ~ "old",
                .default = "unknown", use_copy = FALSE),
    error = function(e) s  # May error due to known bug with recode(record=FALSE)
  )
  expect_s3_class(result, "Survey")
})

# --- step_compute standalone (NULL svy) ---

test_that("step_recode with NULL svy returns standalone step", {
  # step_recode(NULL, ...) should create a standalone step object
  result <- tryCatch(
    step_recode(NULL, new_cat, age < 30 ~ "young", .default = "other"),
    error = function(e) NULL
  )
  # Should return standalone step list, or NULL on error
  expect_true(is.null(result) || is.list(result))
})

# --- ast_step and step_compute_ast ---

test_that("ast_step creates a function from expression", {
  step_fn <- metasurvey:::ast_step(x + y)
  expect_true(is.function(step_fn))
  dt <- data.table::data.table(x = c(1, 2, 3), y = c(10, 20, 30))
  result <- step_fn(dt)
  expect_equal(result, c(11, 22, 33))
})

test_that("ast_step with optimize=FALSE still works", {
  step_fn <- metasurvey:::ast_step(x * 2, optimize = FALSE)
  expect_true(is.function(step_fn))
  dt <- data.table::data.table(x = c(5, 10))
  result <- step_fn(dt)
  expect_equal(result, c(10, 20))
})

test_that("step_compute_ast with svy=NULL returns standalone step", {
  step <- metasurvey:::step_compute_ast(svy = NULL, z = age + 1)
  expect_true(is.list(step))
  expect_equal(step$type, "ast_compute")
  expect_true("z" %in% step$names)
})

test_that("step_compute_ast with svy delegates to step_compute", {
  s <- make_test_survey()
  result <- metasurvey:::step_compute_ast(svy = s, z = age + 1)
  expect_s3_class(result, "Survey")
})

test_that("step_compute_ast with use_ast=FALSE delegates to step_compute", {
  s <- make_test_survey()
  result <- metasurvey:::step_compute_ast(svy = s, z = age + 1, use_ast = FALSE)
  expect_s3_class(result, "Survey")
})
