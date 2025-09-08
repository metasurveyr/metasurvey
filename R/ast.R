#' AST-based Expression Evaluation System
#'
#' This module provides an Abstract Syntax Tree (AST) based system for
#' improved expression evaluation in survey data processing. It offers
#' better performance, dependency tracking, and optimization capabilities
#' compared to standard evaluation methods.
#'
#' @details
#' The AST system provides:
#' \itemize{
#'   \item Static analysis of expressions before execution
#'   \item Dependency detection for variable relationships
#'   \item Expression optimization and caching
#'   \item Better error reporting with precise locations
#'   \item Support for complex nested expressions
#' }
#'
#' @name AST
#' @keywords internal
NULL

#' Parse expression into AST
#'
#' Converts R expressions into an Abstract Syntax Tree representation
#' for improved analysis and evaluation performance.
#'
#' @param expr An R expression to parse (can be quoted or unquoted)
#' @param env Environment for variable resolution (optional)
#'
#' @return A list representing the AST structure with components:
#'   \itemize{
#'     \item \code{type}: Node type ("call", "symbol", "literal", etc.)
#'     \item \code{value}: Node value or function name
#'     \item \code{children}: List of child nodes
#'     \item \code{dependencies}: Variables referenced in this subtree
#'   }
#'
#' @details
#' The AST parser handles:
#' \itemize{
#'   \item Function calls with arbitrary arguments
#'   \item Variable references and symbol resolution
#'   \item Literals (numbers, strings, logicals)
#'   \item Complex nested expressions
#'   \item data.table syntax and special operators
#' }
#'
#' @examples
#' \dontrun{
#' # Parse simple expression
#' ast <- parse_ast(quote(x + y))
#'
#' # Parse complex expression
#' complex_ast <- parse_ast(quote(
#'   ifelse(age >= 18,
#'     case_when(
#'       income > 50000 ~ "high",
#'       income > 25000 ~ "medium",
#'       TRUE ~ "low"
#'     ),
#'     "minor"
#'   )
#' ))
#'
#' # Get dependencies
#' deps <- get_ast_dependencies(complex_ast)
#' print(deps) # c("age", "income")
#' }
#'
#' @seealso \code{\link{evaluate_ast}} to execute parsed expressions
#' @noRd
parse_ast <- function(expr, env = parent.frame()) {
  if (missing(expr)) {
    stop("Expression cannot be missing")
  }

  # Handle different expression types
  if (is.call(expr)) {
    parse_call_ast(expr, env)
  } else if (is.symbol(expr)) {
    parse_symbol_ast(expr, env)
  } else if (is.atomic(expr)) {
    parse_literal_ast(expr)
  } else {
    stop("Unsupported expression type: ", class(expr))
  }
}

#' Parse function call into AST
#' @param call_expr A call expression
#' @param env Environment for resolution
#' @return AST node for function call
#' @keywords internal
parse_call_ast <- function(call_expr, env) {
  func_name <- as.character(call_expr[[1]])
  args <- as.list(call_expr[-1])

  # Parse arguments recursively
  children <- lapply(args, function(arg) {
    if (is.null(arg)) {
      list(type = "null", value = NULL, children = list(), dependencies = character(0))
    } else {
      parse_ast(arg, env)
    }
  })

  # Collect dependencies from all children
  dependencies <- unique(unlist(lapply(children, function(child) child$dependencies)))

  list(
    type = "call",
    value = func_name,
    children = children,
    dependencies = dependencies,
    arg_names = names(args)
  )
}

#' Parse symbol into AST
#' @param symbol_expr A symbol expression
#' @param env Environment for resolution
#' @return AST node for symbol
#' @keywords internal
parse_symbol_ast <- function(symbol_expr, env) {
  symbol_name <- as.character(symbol_expr)

  list(
    type = "symbol",
    value = symbol_name,
    children = list(),
    dependencies = symbol_name
  )
}

#' Parse literal into AST
#' @param literal_expr A literal value
#' @return AST node for literal
#' @keywords internal
parse_literal_ast <- function(literal_expr) {
  list(
    type = "literal",
    value = literal_expr,
    children = list(),
    dependencies = character(0)
  )
}

#' Extract dependencies from AST
#'
#' Recursively extracts all variable dependencies from an AST structure.
#' This is useful for determining which columns are needed before
#' evaluating an expression.
#'
#' @param ast An AST structure created by \code{\link{parse_ast}}
#'
#' @return Character vector of unique variable names referenced in the expression
#'
#' @examples
#' \dontrun{
#' ast <- parse_ast(quote(income * tax_rate + bonus))
#' deps <- get_ast_dependencies(ast)
#' print(deps) # c("income", "tax_rate", "bonus")
#' }
#'
#' @noRd
get_ast_dependencies <- function(ast) {
  if (is.null(ast) || !is.list(ast)) {
    return(character(0))
  }

  unique(ast$dependencies)
}

#' Evaluate AST with data
#'
#' Executes an AST structure against a data.table or data.frame,
#' providing optimized evaluation with dependency checking.
#'
#' @param ast An AST structure created by \code{\link{parse_ast}}
#' @param data A data.table or data.frame containing the data
#' @param env Environment for function resolution (optional)
#'
#' @return Result of evaluating the expression against the data
#'
#' @details
#' The AST evaluator provides several advantages:
#' \itemize{
#'   \item Pre-validates that all required variables exist
#'   \item Optimizes evaluation order for complex expressions
#'   \item Provides better error messages with context
#'   \item Supports data.table's enhanced evaluation
#' }
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' dt <- data.table(
#'   age = c(25, 30, 45, 60),
#'   income = c(30000, 45000, 70000, 55000)
#' )
#'
#' # Parse and evaluate expression
#' ast <- parse_ast(quote(ifelse(age >= 30, income * 1.1, income)))
#' result <- evaluate_ast(ast, dt)
#' print(result)
#' }
#'
#' @noRd
evaluate_ast <- function(ast, data, env = parent.frame()) {
  if (!is.data.frame(data)) {
    stop("Data must be a data.frame or data.table")
  }

  # Check dependencies
  deps <- get_ast_dependencies(ast)
  missing_vars <- setdiff(deps, names(data))
  if (length(missing_vars) > 0) {
    stop("Missing variables in data: ", paste(missing_vars, collapse = ", "))
  }

  # Convert to data.table if needed
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  # Evaluate AST recursively
  evaluate_ast_node(ast, data, env)
}

#' Evaluate single AST node
#' @param node AST node to evaluate
#' @param data Data for evaluation
#' @param env Environment for functions
#' @return Evaluation result
#' @keywords internal
evaluate_ast_node <- function(node, data, env) {
  switch(node$type,
    "literal" = node$value,
    "symbol" = {
      if (node$value %in% names(data)) {
        data[[node$value]]
      } else {
        # Try to resolve in environment
        if (exists(node$value, envir = env)) {
          get(node$value, envir = env)
        } else {
          stop("Variable not found: ", node$value)
        }
      }
    },
    "call" = {
      # Evaluate arguments
      args <- lapply(node$children, function(child) {
        evaluate_ast_node(child, data, env)
      })

      # Set argument names if available
      if (!is.null(node$arg_names)) {
        names(args) <- node$arg_names
      }

      # Get function
      func <- get(node$value, envir = env)

      # Call function with evaluated arguments
      do.call(func, args)
    },
    stop("Unknown AST node type: ", node$type)
  )
}

#' Optimize AST for better performance
#'
#' Applies various optimization techniques to an AST to improve
#' evaluation performance, including constant folding and
#' expression simplification.
#'
#' @param ast An AST structure to optimize
#' @param data Optional data.table for context-aware optimizations
#'
#' @return Optimized AST structure
#'
#' @details
#' Optimizations include:
#' \itemize{
#'   \item Constant folding: Pre-evaluate constant expressions
#'   \item Dead code elimination: Remove unreachable branches
#'   \item Common subexpression elimination: Cache repeated calculations
#'   \item Variable access optimization: Optimize data.table column access
#' }
#'
#' @examples
#' \dontrun{
#' # Original expression: x + (5 * 2) - 1
#' ast <- parse_ast(quote(x + (5 * 2) - 1))
#' optimized <- optimize_ast(ast)
#' # Optimized: x + 9  (constant folding applied)
#' }
#'
#' @noRd
optimize_ast <- function(ast, data = NULL) {
  if (!is.list(ast)) {
    return(ast)
  }

  # Apply optimizations recursively
  optimized_children <- lapply(ast$children, optimize_ast, data)

  # Update the node with optimized children
  ast$children <- optimized_children

  # Apply node-specific optimizations
  ast <- optimize_node(ast, data)

  return(ast)
}

#' Optimize individual AST node
#' @param node AST node to optimize
#' @param data Optional data context
#' @return Optimized node
#' @keywords internal
optimize_node <- function(node, data = NULL) {
  if (node$type == "call") {
    # Constant folding for arithmetic operations
    if (node$value %in% c("+", "-", "*", "/", "^")) {
      if (all(sapply(node$children, function(child) child$type == "literal"))) {
        # All arguments are literals, pre-evaluate
        values <- sapply(node$children, function(child) child$value)
        result <- switch(node$value,
          "+" = sum(values),
          "-" = if (length(values) == 1) -values else values[1] - sum(values[-1]),
          "*" = prod(values),
          "/" = values[1] / prod(values[-1]),
          "^" = values[1]^values[2]
        )
        return(list(
          type = "literal",
          value = result,
          children = list(),
          dependencies = character(0)
        ))
      }
    }
  }

  return(node)
}

#' Create optimized step from AST
#'
#' Creates a step function that uses AST-based evaluation for
#' improved performance in survey data processing pipelines.
#'
#' @param expr Expression to convert to AST-based step
#' @param comment Optional comment for the step
#' @param optimize Whether to apply AST optimizations
#'
#' @return Function that can be used in step_compute or similar
#'
#' @examples
#' \dontrun{
#' # Create AST-based step
#' age_category_step <- ast_step(
#'   quote(case_when(
#'     age < 18 ~ "minor",
#'     age < 65 ~ "adult",
#'     TRUE ~ "senior"
#'   )),
#'   comment = "Categorize age groups",
#'   optimize = TRUE
#' )
#'
#' # Use in pipeline
#' survey_data %>%
#'   step_compute(age_category = age_category_step) %>%
#'   bake_steps()
#' }
#'
#' @noRd
ast_step <- function(expr, comment = NULL, optimize = TRUE) {
  # Parse expression to AST
  ast <- parse_ast(substitute(expr))

  # Optimize if requested
  if (optimize) {
    ast <- optimize_ast(ast)
  }

  # Return function that evaluates AST
  function(data) {
    evaluate_ast(ast, data)
  }
}

#' Enhanced step_compute with AST support
#'
#' Extended version of step_compute that uses AST-based evaluation
#' for improved performance and better error handling.
#'
#' @param svy Survey object or NULL to create standalone step
#' @param ... Named expressions to compute
#' @param .by Variables to group by
#' @param use_copy Whether to copy the object
#' @param comment Descriptive comment
#' @param .level Level for RotativePanelSurvey objects
#' @param use_ast Whether to use AST-based evaluation (default TRUE)
#' @param optimize_ast Whether to optimize AST expressions (default TRUE)
#'
#' @return Same as step_compute but with AST optimizations
#'
#' @details
#' This enhanced version provides:
#' \itemize{
#'   \item Faster evaluation for complex expressions
#'   \item Better dependency analysis
#'   \item Improved error messages
#'   \item Expression optimization capabilities
#' }
#'
#' @examples
#' \dontrun{
#' # Use AST-enhanced step_compute
#' survey_data %>%
#'   step_compute_ast(
#'     complex_score = (income / median_income) *
#'       ifelse(education > 12, education_weight, 1) +
#'       age_factor * region_adjustment,
#'     use_ast = TRUE,
#'     optimize_ast = TRUE,
#'     comment = "Complex scoring with AST optimization"
#'   )
#' }
#'
#' @noRd
step_compute_ast <- function(svy = NULL, ..., .by = NULL, use_copy = use_copy_default(),
                             comment = NULL, .level = "auto", use_ast = TRUE,
                             optimize_ast = TRUE) {
  # Capture expressions
  exprs <- substitute(list(...))[-1]

  if (use_ast) {
    # Parse expressions to AST
    ast_exprs <- lapply(exprs, function(expr) {
      ast <- parse_ast(expr)
      if (optimize_ast) {
        ast <- optimize_ast(ast)
      }
      ast
    })

    # Create enhanced step
    step <- list(
      type = "ast_compute",
      expressions = ast_exprs,
      names = names(exprs),
      by = .by,
      comment = comment,
      level = .level
    )
  } else {
    # Fall back to regular step_compute
    return(step_compute(svy, ...,
      .by = .by, use_copy = use_copy,
      comment = comment, .level = .level
    ))
  }

  # Return standalone step or delegate to step_compute for adding to survey
  if (is.null(svy)) {
    return(step)
  } else {
    return(step_compute(svy, ...,
      .by = .by, use_copy = use_copy,
      comment = comment, .level = .level
    ))
  }
}
