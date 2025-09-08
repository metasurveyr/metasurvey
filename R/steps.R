#' @importFrom data.table copy
#' @importFrom methods is
NULL

#' Core AST computation engine
#'
#' This is the fundamental computation engine that uses Abstract Syntax Tree
#' evaluation as the core mechanism for all metasurvey computations.
#'
#' @noRd
#' @param svy Survey object
#' @param ast_expressions List of parsed AST expressions
#' @param .by Variables to group by
#' @param use_copy Whether to copy the object
#' @param cache_ast Whether to cache compiled AST
#' @return Modified survey object
#' @keywords internal
compute_with_ast <- function(svy, ast_expressions = NULL, .by = NULL,
                             use_copy = use_copy_default(), cache_ast = TRUE) {
  # If no AST expressions provided, fall back to traditional compute
  if (is.null(ast_expressions) || length(ast_expressions) == 0) {
    return(compute(svy, .by = .by, use_copy = use_copy, lazy = lazy_default()))
  }

  # Get data for evaluation
  if (!use_copy) {
    .data <- get_data(svy)
  } else {
    .clone <- svy$clone(deep = TRUE)
    .data <- copy(get_data(.clone))
  }

  ast_results <- list()
  for (var_name in names(ast_expressions)) {
    ast_obj <- ast_expressions[[var_name]]

    tryCatch(
      {
        # Use AST evaluation as core engine
        result <- evaluate_ast(ast_obj, data = .data)
        ast_results[[var_name]] <- result
      },
      error = function(e) {
        # Fallback with context
        warning(sprintf(
          "AST evaluation failed for '%s': %s. Check expression syntax.",
          var_name, e$message
        ))
        # Could implement fallback to traditional evaluation here if needed
        stop(sprintf("AST evaluation error in variable '%s': %s", var_name, e$message))
      }
    )
  }

  # Apply results to data using data.table assignment
  if (length(ast_results) > 0) {
    if (!is.null(.by)) {
      # Grouped assignment
      for (var_name in names(ast_results)) {
        .data[, (var_name) := ast_results[[var_name]], by = .by]
      }
    } else {
      # Direct assignment
      for (var_name in names(ast_results)) {
        .data[, (var_name) := ast_results[[var_name]]]
      }
    }
  }

  # Return modified survey object
  if (!use_copy) {
    return(set_data(svy, .data))
  } else {
    return(set_data(.clone, .data))
  }
}

#' Core AST recoding engine
#'
#' This is the fundamental recoding engine that uses Abstract Syntax Tree
#' evaluation for all conditional logic in recoding operations.
#'
#' @param svy Survey object
#' @param new_var Name of new variable to create
#' @param conditions List of condition formulas (LHS ~ RHS)
#' @param default_value Default value when no conditions match
#' @param ast_params AST configuration parameters
#' @return Modified survey object with new recoded variable
#' @keywords internal
recode_with_ast <- function(svy, new_var, conditions, default_value = NA_character_,
                            ast_params = list()) {
  # Get data for evaluation
  .data <- get_data(svy)
  n_rows <- nrow(.data)

  # Initialize result vector with default values
  result <- rep(default_value, n_rows)


  ast_conditions <- list()
  condition_values <- list()
  all_dependencies <- character()

  for (i in seq_along(conditions)) {
    condition_formula <- conditions[[i]]

    # Extract LHS (condition) and RHS (value)
    if (!inherits(condition_formula, "formula")) {
      stop(sprintf("Condition %d is not a formula. Use format: condition ~ value", i))
    }

    lhs_expr <- condition_formula[[2]] # Left side: condition
    rhs_value <- condition_formula[[3]] # Right side: value

    tryCatch(
      {
        # Parse condition as AST
        ast_condition <- parse_ast(lhs_expr)
        ast_conditions[[i]] <- ast_condition

        # Get dependencies from AST
        deps <- get_ast_dependencies(ast_condition)
        all_dependencies <- unique(c(all_dependencies, deps))

        # Apply optimization if requested
        if (isTRUE(ast_params$optimize_ast)) {
          ast_condition <- optimize_ast(ast_condition, data = .data)
          ast_conditions[[i]] <- ast_condition
        }

        # Store the value to assign (evaluate if it's an expression)
        if (is.call(rhs_value) || is.name(rhs_value)) {
          condition_values[[i]] <- eval(rhs_value)
        } else {
          condition_values[[i]] <- rhs_value
        }
      },
      error = function(e) {
        warning(sprintf(
          "AST parsing failed for condition %d: %s. Using traditional evaluation.",
          i, e$message
        ))
        # Store original expression for fallback
        ast_conditions[[i]] <- lhs_expr
        condition_values[[i]] <- eval(rhs_value)
      }
    )
  }

  # Validate dependencies if requested
  if (isTRUE(ast_params$validate_deps) && length(all_dependencies) > 0) {
    data_vars <- names(.data)
    missing_vars <- setdiff(all_dependencies, data_vars)
    if (length(missing_vars) > 0) {
      stop(sprintf(
        "AST Dependency validation failed in recode conditions. Missing variables: %s",
        paste(missing_vars, collapse = ", ")
      ))
    }
  }

  for (i in seq_along(ast_conditions)) {
    ast_condition <- ast_conditions[[i]]
    condition_value <- condition_values[[i]]

    tryCatch(
      {
        # Use AST evaluation for condition
        if (inherits(ast_condition, "metasurvey_ast")) {
          condition_result <- evaluate_ast(ast_condition, data = .data)
        } else {
          # Fallback to traditional evaluation
          condition_result <- eval(ast_condition, .data)
        }

        # Apply condition (first matching wins)
        if (is.logical(condition_result)) {
          result[condition_result & !is.na(condition_result)] <- condition_value
        }
      },
      error = function(e) {
        warning(sprintf("AST evaluation failed for condition %d: %s", i, e$message))
      }
    )
  }

  return(result)
}

#' AST survey recoding wrapper
#'
#' Wrapper function that integrates AST recoding with survey object management
#'
#' @param svy Survey object
#' @param new_var Name of new variable
#' @param conditions List of recoding conditions
#' @param default_value Default value for unmatched cases
#' @param use_copy Whether to copy survey object
#' @param to_factor Whether to convert result to factor
#' @param ast_params AST configuration parameters
#' @return Modified survey object
#' @keywords internal
recode_with_ast_survey <- function(svy, new_var, conditions, default_value = NA_character_,
                                   use_copy = TRUE, to_factor = FALSE, ast_params = list()) {
  if (use_copy) {
    .svy_clone <- svy$clone(deep = TRUE)
    .data <- copy(get_data(.svy_clone))
  } else {
    .data <- get_data(svy)
  }

  # Use core AST recoding engine
  result_values <- recode_with_ast(svy, new_var, conditions, default_value, ast_params)

  # Convert to factor if requested
  if (to_factor) {
    result_values <- as.factor(result_values)
  }

  # Assign new variable using data.table
  .data[, (new_var) := result_values]

  # Return modified survey object
  if (use_copy) {
    return(set_data(.svy_clone, .data))
  } else {
    return(set_data(svy, .data))
  }
}

compute <- function(svy, ..., .by = NULL, use_copy = use_copy_default(), lazy = lazy_default()) {
  .dots <- substitute(...)


  if (!lazy) {
    if (!use_copy) {
      .data <- get_data(svy)
    } else {
      .clone <- svy$clone(deep = TRUE)
      .data <- copy(get_data(.clone))
    }

    if (!is(.dots, "call") & !is(.dots, "name") & !is(.dots, "numeric") & !is(.dots, "logical")) {
      .exprs <- list()
      for (i in 2:length(.dots)) {
        .exprs <- c(.exprs, .dots[[i]])
      }
    } else {
      .exprs <- substitute(list(...))
    }

    if (!is.null(.by)) {
      .agg <- .data[, j, by = .by, env = list(j = .exprs)]

      .data <- merge(.data, .agg, by = .by, all.x = TRUE)
    } else {
      .exprs <- eval(.exprs, .data)
      .data[
        ,
        (names(.exprs)) := .exprs,
        by = .by
      ]
    }


    if (!use_copy) {
      return(set_data(svy, .data))
    } else {
      return(set_data(.clone, .data))
    }
  } else {
    if (!use_copy) {
      return(svy)
    } else {
      return(svy$clone())
    }
  }
}


#' @importFrom data.table copy

recode <- function(svy, new_var, ..., .default = NA_character_, ordered = FALSE, use_copy = use_copy_default(), .to_factor = FALSE, lazy = lazy_default()) {
  if (!lazy) {
    if (!use_copy) {
      .data <- svy$get_data()
    } else {
      .clone <- svy$clone()
      .data <- copy(get_data(.clone))
    }

    .exprs <- substitute(list(...))
    .exprs <- eval(.exprs, .data, parent.frame())

    if (!is(.exprs[[1]], "formula")) {
      .exprs <- .exprs[[1]]
    }

    .labels <- c(
      .default,
      unique(
        sapply(
          X = seq_along(.exprs),
          FUN = function(x) {
            .exprs[[x]][[3]]
          }
        )
      )
    )





    if (.to_factor) {
      .data[
        ,
        (new_var) := factor(
          .default,
          levels = .labels,
          ordered = ordered
        )
      ]
    } else {
      .data[
        ,
        (new_var) := .default
      ]
    }

    lapply(
      FUN = function(.expr) {
        .filter <- .exprs[[.expr]][[2]]
        .label <- .exprs[[.expr]][[3]]

        .data[
          eval(
            .filter,
            .data,
            parent.frame()
          ),
          (new_var) := .label
        ]
        invisible(NULL)
      },
      X = seq_along(.exprs)
    )

    if (!use_copy) {
      return(set_data(svy, .data))
    } else {
      return(set_data(.clone, .data))
    }
  } else {
    if (!use_copy) {
      return(svy)
    } else {
      return(svy$clone())
    }
  }
}

#' Create AST computation steps for survey variables
#'
#' **CORE AST SYSTEM**: This function now uses Abstract Syntax Tree (AST)
#' evaluation as its fundamental engine. All computations are parsed into AST form,
#' optimized, and evaluated with advanced dependency detection and error prevention.
#'
#' @param svy A `Survey` or `RotativePanelSurvey` object. If NULL, creates a step
#'   that can be applied later using the pipe operator (%>%)
#' @param ... Computation expressions that are **automatically parsed as AST**.
#'   Each expression is converted to Abstract Syntax Tree for optimized evaluation.
#'   Names are assigned using `new_var = expression`
#' @param .by Vector of variables to group computations by. AST automatically
#'   validates these variables exist before execution
#' @param use_copy Logical indicating whether to create a copy of the object before
#'   applying transformations. Defaults to `use_copy_default()`
#' @param comment Descriptive text for the step for documentation and traceability.
#'   Compatible with Markdown syntax. Defaults to "AST Compute step"
#' @param .level For RotativePanelSurvey objects, specifies the level where
#'   computations are applied: "implantation", "follow_up", "quarter", "month", or "auto"
#' @param optimize_ast Whether to apply AST optimizations (constant folding,
#'   expression simplification). Default: TRUE
#' @param cache_ast Whether to cache compiled AST expressions for reuse. Default: TRUE
#' @param validate_deps Whether to validate all variable dependencies exist
#'   before execution. Default: TRUE
#'
#' @return Same type of input object (`Survey` or `RotativePanelSurvey`)
#'   with new computed variables and the step added to the history
#'
#' @details
#' **AST CORE ENGINE FEATURES**:
#'
#' \strong{1. Automatic AST Parsing:}
#' - All expressions converted to Abstract Syntax Trees
#' - Static analysis performed before execution
#' - Dependency detection prevents runtime errors
#'
#' \strong{2. AST Optimization:}
#' - Constant expressions pre-calculated
#' - Dead code elimination
#' - Expression simplification
#' - Optimized evaluation paths
#'
#' \strong{3. Enhanced Error Prevention:}
#' - Missing variables detected before execution
#' - Type checking when possible
#' - Precise error locations with context
#' - Dependency graphs for debugging
#'
#' \strong{4. Performance Benefits:}
#' - Optimized evaluation reduces computation time
#' - Cached AST compilation for repeated operations
#' - Minimal overhead with significant gains
#'
#' For RotativePanelSurvey objects, AST validation ensures computations
#' are compatible with the specified hierarchical level:
#' - "implantation": Household/dwelling level computations
#' - "follow_up": Individual/person level computations
#' - "quarter": Quarterly aggregated computations
#' - "month": Monthly aggregated computations
#' - "auto": AST automatically detects appropriate level
#'
#' @examples
#' \dontrun{
#' # Basic AST computation (automatic optimization)
#' ech <- ech |>
#'   step_compute(
#'     unemployed = ifelse(POBPCOAC %in% 3:5, 1, 0),
#'     comment = "Unemployment indicator - AST optimized"
#'   )
#'
#' # Grouped calculation
#' ech <- ech |>
#'   step_compute(
#'     mean_household_income = mean(ht11, na.rm = TRUE),
#'     .by = "numero",
#'     comment = "Mean income by household"
#'   )
#'
#' # Rotative panel specifying level
#' panel <- panel |>
#'   step_compute(
#'     activity_rate = active_population / total_population * 100,
#'     .level = "quarter",
#'     comment = "Quarterly activity rate"
#'   )
#' }
#' @seealso
#' \code{\link{step_recode}} for categorical recodings
#' \code{\link{bake_steps}} to execute all pending steps
#'
#' @keywords Steps AST
#' @export

step_compute <- function(svy = NULL, ..., .by = NULL, use_copy = use_copy_default(),
                         comment = "AST Compute step", .level = "auto",
                         optimize_ast = TRUE, cache_ast = TRUE, validate_deps = TRUE) {
  .call <- match.call()

  # Prepare AST parameters to pass to implementation functions
  ast_params <- list(
    optimize_ast = optimize_ast,
    cache_ast = cache_ast,
    validate_deps = validate_deps
  )

  # Capture and prepare expressions
  exprs <- as.list(substitute(list(...))[-1])
  expr_names <- names(exprs)

  # Parse expressions into ASTs
  ast_expressions <- list()
  for (i in seq_along(exprs)) {
    ast_expressions[[expr_names[i]]] <- parse_ast(exprs[[i]])
  }

  # Optionally optimize ASTs
  optimized_asts <- if (isTRUE(ast_params$optimize_ast)) {
    lapply(ast_expressions, function(ast) optimize_ast(ast, data = get_data(svy)))
  } else {
    ast_expressions
  }

  # Collect dependencies
  ast_dependencies <- unique(unlist(lapply(ast_expressions, get_ast_dependencies)))

  if (is(svy, "RotativePanelSurvey")) {
    return(step_compute_rotative(svy, ...,
      .by = .by, use_copy = use_copy,
      comment = comment, .level = .level, .call = .call,
      ast_params = ast_params
    ))
  }


  # Validate dependencies if requested
  if (isTRUE(ast_params$validate_deps) && length(ast_dependencies) > 0 && !is.null(svy)) {
    data_vars <- names(get_data(svy))
    missing_vars <- setdiff(ast_dependencies, data_vars)
    if (length(missing_vars) > 0) {
      stop(sprintf(
        "AST Dependency validation failed. Missing variables: %s",
        paste(missing_vars, collapse = ", ")
      ))
    }
  }

  # Store dependencies (use AST-detected ones or fallback)
  depends_on <- if (length(ast_dependencies) > 0) ast_dependencies else NULL

  if (use_copy) {
    tryCatch(
      {
        .svy_before <- svy$clone()
      },
      error = function(e) {
        stop("Error in clone. Please run set_use_copy(TRUE) and instance a new survey object and try again")
      }
    )

    # **CORE AST EVALUATION** - Use AST system for computation
    .svy_after <- compute_with_ast(svy,
      ast_expressions = optimized_asts,
      .by = .by, use_copy = use_copy,
      cache_ast = ast_params$cache_ast %||% TRUE
    )

    .new_vars <- expr_names

    if (length(.new_vars) > 0) {
      step <- Step$new(
        name = paste("AST Compute:", paste(.new_vars, collapse = ", ")),
        edition = get_edition(.svy_after),
        survey_type = get_type(.svy_after),
        type = "ast_compute",
        new_var = paste(.new_vars, collapse = ", "),
        exprs = substitute(list(...)),
        # attach AST payload for baking
        expressions = optimized_asts,
        names = .new_vars,
        ast_info = list(
          expressions = ast_expressions,
          optimized = optimized_asts,
          dependencies = ast_dependencies,
          optimization_enabled = ast_params$optimize_ast %||% TRUE,
          cache_enabled = ast_params$cache_ast %||% TRUE
        ),
        call = .call,
        svy_before = svy,
        default_engine = "ast",
        depends_on = depends_on,
        comment = comment
      )

      if (validate_step(svy, step)) {
        .svy_after$add_step(step)
      } else {
        stop("Error in step")
      }
      return(.svy_after)
    } else {
      message("No new variable created: ", substitute(list(...)))
      return(svy)
    }
  } else {
    names_vars <- names(copy(get_data(svy)))
    compute(svy, ..., .by = .by, use_copy = use_copy, lazy = lazy_default())
    .new_vars <- names(substitute(list(...)))[-1]
    not_in_data <- !(.new_vars %in% names_vars)
    .new_vars <- .new_vars[not_in_data]

    step <- Step$new(
      name = paste("New variable:", paste(.new_vars, collapse = ", ")),
      edition = get_edition(svy),
      survey_type = get_type(svy),
      type = "compute",
      new_var = paste(.new_vars, collapse = ", "),
      exprs = substitute(list(...)),
      call = .call,
      svy_before = NULL,
      default_engine = get_engine(),
      comment = comment,
      depends_on = depends_on
    )

    svy$add_step(step)
    invisible(svy)
  }
}

#' Step compute rotative
#' @param svy Survey object
#' @param ... Expressions to compute
#' @param use_copy Use copy
#' @param .by By
#' @param comment Comment
#' @return Survey object
#' @keywords Steps
#' @noRd
#' @keywords internal


step_compute_rotative <- function(svy, ..., .by = NULL, use_copy = use_copy_default(), comment = "Compute step", .level = "auto", .call) {
  follow_up_processed <- svy$follow_up
  implantation_processed <- svy$implantation


  if (.level == "auto" || .level == "follow_up") {
    follow_up_processed <- lapply(svy$follow_up, function(sub_svy) {
      step_compute(sub_svy, ..., .by = .by, use_copy = use_copy, comment = comment)
    })
  }

  if (.level == "auto" || .level == "implantation") {
    implantation_processed <- step_compute(svy$implantation, ..., .by = .by, use_copy = use_copy, comment = comment)
  }



  if (length(implantation_processed$steps) > 0) {
    steps <- implantation_processed$steps
  } else {
    steps <- follow_up_processed[[1]]$steps
  }




  result <- RotativePanelSurvey$new(
    implantation = implantation_processed,
    follow_up = follow_up_processed,
    type = svy$type,
    default_engine = "data.table",
    steps = NULL,
    recipes = NULL,
    workflows = NULL,
    design = NULL
  )

  result$steps <- steps

  if (use_copy) {
    return(result)
  } else {
    svy$implantation <- implantation_processed
    svy$follow_up <- follow_up_processed
    svy$steps <- steps
    return(svy)
  }
}

#' Create AST recoding steps for categorical variables

#' **CORE AST SYSTEM**: This function now uses Abstract Syntax Tree (AST)
#' evaluation as its fundamental engine for all recoding conditions. All conditional
#' expressions are parsed as AST, optimized, and evaluated with dependency validation.
#'
#' @param svy A `Survey` or `RotativePanelSurvey` object. If NULL, creates a step
#'   that can be applied later using the pipe operator (%>%)
#' @param new_var Name of the new variable to create (unquoted)
#' @param ... Sequence of two-sided formulas defining recoding rules with **AST parsing**.
#'   Left-hand side (LHS) parsed as AST conditional expression, right-hand side (RHS)
#'   defines the replacement value. Format: `ast_condition ~ value`
#' @param .default Default value assigned when no AST condition is met.
#'   Defaults to `NA_character_`
#' @param .name_step Custom name for the step to identify it in the history.
#'   If not provided, generated automatically with "AST Recode" prefix
#' @param ordered Logical indicating whether the new variable should be an
#'   ordered factor. Defaults to FALSE
#' @param use_copy Logical indicating whether to create a copy of the object before
#'   applying transformations. Defaults to `use_copy_default()`
#' @param comment Descriptive text for the step for documentation and traceability.
#'   Compatible with Markdown syntax. Defaults to "AST Recode step"
#' @param .to_factor Logical indicating whether the new variable should be
#'   converted to a factor. Defaults to FALSE
#' @param .level For RotativePanelSurvey objects, specifies the level where
#'   recoding is applied: "implantation", "follow_up", "quarter", "month", or "auto"
#' @param optimize_ast Whether to apply AST optimizations to conditions. Default: TRUE
#' @param validate_deps Whether to validate condition dependencies exist. Default: TRUE
#' @param cache_ast Whether to cache compiled AST conditions for reuse. Default: TRUE
#'
#' @return Same type of input object (`Survey` or `RotativePanelSurvey`)
#'   with the new recoded variable and the step added to the history
#'
#' @details
#' **AST CORE ENGINE FOR RECODING**:
#'
#' \strong{1. AST Condition Parsing:}
#' - All LHS conditions converted to Abstract Syntax Trees
#' - Static analysis of logical expressions
#' - Dependency detection for all referenced variables
#' - Optimization of conditional logic
#'
#' \strong{2. Enhanced Condition Evaluation:}
#' - Conditions evaluated in order using AST engine
#' - First matching AST condition determines assignment
#' - Optimized short-circuit evaluation
#' - Better error reporting with expression context
#'
#' \strong{3. AST Optimization Features:}
#' - Constant folding in conditions (e.g., `5 + 3 > 7` → `8 > 7` → `TRUE`)
#' - Dead code elimination for unreachable conditions
#' - Expression simplification for faster evaluation
#' - Dependency pre-validation prevents runtime errors
#'
#' AST condition examples:
#' - Simple: `variable == 1` (parsed as AST, dependencies: `variable`)
#' - Complex: `age >= 18 & income > 1000 * 12` (optimized: `income > 12000`)
#' - Vectorized: `variable %in% c(1,2,3)` (AST validates `variable` exists)
#' - Logical: `!is.na(education) & education > mean(education, na.rm = TRUE)`
#'
#' @examples
#' \dontrun{
#' # Create labor force status variable
#' ech <- ech |>
#'   step_recode(
#'     labor_status,
#'     POBPCOAC == 2 ~ "Employed",
#'     POBPCOAC %in% 3:5 ~ "Unemployed",
#'     POBPCOAC %in% 6:8 ~ "Inactive",
#'     .default = "Missing",
#'     comment = "Labor force status from ECH"
#'   )
#'
#' # Create age groups
#' ech <- ech |>
#'   step_recode(
#'     age_group,
#'     e27 < 18 ~ "Under 18",
#'     e27 >= 18 & e27 < 65 ~ "Working age",
#'     e27 >= 65 ~ "Senior",
#'     .default = "Missing",
#'     .to_factor = TRUE,
#'     ordered = TRUE,
#'     comment = "Standard age groups"
#'   )
#'
#' # Dummy variable
#' ech <- ech |>
#'   step_recode(
#'     household_head,
#'     e30 == 1 ~ 1,
#'     .default = 0,
#'     comment = "Household head indicator"
#'   )
#'
#' # For rotative panel
#' panel <- panel |>
#'   step_recode(
#'     region_simple,
#'     REGION_4 == 1 ~ "Montevideo",
#'     REGION_4 != 1 ~ "Interior",
#'     .level = "implantation",
#'     comment = "Simplified region"
#'   )
#' }
#'
#' @seealso
#' \code{\link{step_compute}} for more complex calculations
#' \code{\link{bake_steps}} to execute all pending steps
#' \code{\link{get_steps}} to view step history
#'
#' @keywords Steps
#' @export

step_recode <- function(svy = survey_empty(), new_var, ..., .default = NA_character_,
                        .name_step = NULL, ordered = FALSE, use_copy = use_copy_default(),
                        comment = "AST Recode step", .to_factor = FALSE, .level = "auto",
                        optimize_ast = TRUE, validate_deps = TRUE, cache_ast = TRUE) {
  .call <- match.call()

  # Prepare AST parameters
  ast_params <- list(
    optimize_ast = optimize_ast,
    validate_deps = validate_deps,
    cache_ast = cache_ast
  )

  if (is(svy, "RotativePanelSurvey")) {
    return(step_recode_rotative(svy, as.character(substitute(new_var)), ...,
      .default = .default, .name_step = .name_step,
      ordered = ordered, use_copy = use_copy,
      comment = comment, .to_factor = .to_factor,
      .level = .level, .call = .call, ast_params = ast_params
    ))
  }

  if (is(svy, "Survey")) {
    return(step_recode_survey(svy, as.character(substitute(new_var)), ...,
      .default = .default, .name_step = .name_step,
      ordered = ordered, use_copy = use_copy,
      comment = comment, .to_factor = .to_factor,
      .call = .call, ast_params = ast_params
    ))
  }

  # Create standalone step with AST configuration
  standalone_step <- list(
    type = "recode",
    new_var = as.character(substitute(new_var)),
    conditions = substitute(...),
    default = .default,
    name_step = .name_step,
    comment = comment,
    ast_params = ast_params,
    call = .call
  )
  class(standalone_step) <- c("metasurvey_step", "list")

  return(standalone_step)
}

#' Step recode survey
#' @param svy Survey object
#' @param new_var New variable
#' @param ... Expressions to recode
#' @param .default Default value
#' @param .name_step Name of the step
#' @param ordered Ordered
#' @param use_copy Use copy
#' @param comment Comment
#' @keywords Steps
#' @noRd
#' @keywords internal

step_recode_survey <- function(svy, new_var, ..., .default = NA_character_, .name_step = NULL,
                               ordered = FALSE, use_copy = use_copy_default(),
                               comment = "AST Recode step", .to_factor = FALSE,
                               .call = .call, ast_params = list()) {
  new_var <- as.character(new_var)
  check_svy <- is.null(get_data(svy))
  if (check_svy) {
    return(.call)
  }

  if (is.null(.name_step)) {
    .name_step <- paste0("AST Recode: ", new_var)
  }

  # **CORE AST PROCESSING** - Parse all recoding conditions as AST
  conditions <- list(...)
  ast_dependencies <- character()

  # Extract dependencies using AST analysis from each condition
  for (i in seq_along(conditions)) {
    condition_formula <- conditions[[i]]
    if (inherits(condition_formula, "formula")) {
      lhs_expr <- condition_formula[[2]]

      tryCatch(
        {
          # Parse condition as AST and extract dependencies
          ast_condition <- parse_ast(lhs_expr)
          deps <- get_ast_dependencies(ast_condition)
          ast_dependencies <- unique(c(ast_dependencies, deps))
        },
        error = function(e) {
          # Fallback to traditional dependency detection
          traditional_deps <- find_dependencies(call_expr = lhs_expr, survey = get_data(svy))
          ast_dependencies <- unique(c(ast_dependencies, traditional_deps))
        }
      )
    }
  }

  depends_on <- if (length(ast_dependencies) > 0) ast_dependencies else NULL

  if (use_copy) {
    # **CORE AST EVALUATION** - Use AST system for recoding
    .svy_after <- recode_with_ast_survey(
      svy = svy, new_var = new_var,
      conditions = conditions,
      default_value = .default,
      use_copy = use_copy,
      to_factor = .to_factor,
      ast_params = ast_params
    )

    step <- Step$new(
      name = .name_step,
      edition = get_edition(.svy_after),
      survey_type = get_type(.svy_after),
      type = "recode",
      new_var = new_var,
      exprs = list(...),
      call = .call,
      svy_before = svy,
      default_engine = get_engine(),
      depends_on = depends_on,
      comments = comment
    )
    .svy_after$add_step(step)
    return(.svy_after)
  } else {
    recode(svy = svy, new_var = new_var, ..., .default = .default, use_copy = use_copy)
    step <- Step$new(
      name = .name_step,
      edition = get_edition(svy),
      survey_type = get_type(svy),
      type = "recode",
      new_var = new_var,
      exprs = list(...),
      call = .call,
      svy_before = NULL,
      default_engine = get_engine(),
      depends_on = depends_on,
      comments = comment
    )
    svy$add_step(step)
    invisible(svy)
  }
}

#' Step recode rotative
#' @param svy Survey object
#' @param new_var New variable
#' @param ... Expressions to recode
#' @param .default Default value
#' @param .name_step Name of the step
#' @param ordered Ordered
#' @param use_copy Use copy
#' @param comment Comment
#' @param .to_factor To factor
#' @return Survey object
#' @keywords Steps
#' @noRd
#' @keywords internal

step_recode_rotative <- function(svy, new_var, ..., .default = NA_character_, .name_step = NULL, ordered = FALSE, use_copy = use_copy_default(), comment = "Recode step", .to_factor = FALSE, .level = "auto", .call) {
  follow_up_processed <- svy$follow_up
  implantation_processed <- svy$implantation

  if (.level == "auto" || .level == "follow_up") {
    follow_up_processed <- lapply(svy$follow_up, function(sub_svy) {
      step_recode_survey(sub_svy, new_var, ..., .default = .default, .name_step = .name_step, ordered = ordered, use_copy = use_copy, comment = comment, .to_factor = .to_factor, .call = .call)
    })
  }

  if (.level == "auto" || .level == "implantation") {
    implantation_processed <- step_recode_survey(svy$implantation, new_var, ..., .default = .default, .name_step = .name_step, ordered = ordered, use_copy = use_copy, comment = comment, .to_factor = .to_factor, .call = .call)
  }

  result <- RotativePanelSurvey$new(
    implantation = implantation_processed,
    follow_up = follow_up_processed,
    type = svy$type,
    default_engine = "data.table",
    steps = NULL,
    recipes = NULL,
    workflows = NULL,
    design = NULL
  )

  if (use_copy) {
    return(result)
  } else {
    svy$implantation <- implantation_processed
    svy$follow_up <- follow_up_processed
    return(svy)
  }
}


#' Get formulas
#' @param steps List of steps
#' @return List of formulas
#' @noRd

get_formulas <- function(steps) {
  if (length(steps) > 0) {
    sapply(
      X = seq_along(steps),
      FUN = function(x) {
        step <- steps[[x]]
        exprs <- step$exprs
        if (step$type == "recode") {
          paste0(
            step$new_var,
            ": ",
            paste(
              deparse1(
                step$exprs
              ),
              collapse = "\n"
            )
          )
        } else {
          deparse1(exprs)
        }
      }
    )
  } else {
    NULL
  }
}

#' Get comments
#' @param steps List of steps
#' @return List of comments
#' @noRd

get_comments <- function(steps) {
  if (length(steps) > 0) {
    sapply(
      X = seq_along(steps),
      FUN = function(x) {
        step <- steps[[x]]
        step$comments
      }
    )
  } else {
    NULL
  }
}

#' Join external data into survey (step)
#'
#' Creates a step that joins additional data into a Survey or RotativePanelSurvey.
#' Works with a data.frame/data.table or another Survey as the right-hand side.
#'
#' - Supports left, inner, right, and full joins
#' - Allows named `by` mapping (e.g., c("id" = "code")) or simple vector
#' - Avoids extra dependencies; resolves name conflicts by suffixing RHS columns
#'
#' @param svy A Survey or RotativePanelSurvey object. If NULL, returns a step call
#' @param x A data.frame/data.table or a Survey to join into `svy`
#' @param by Character vector of join keys. Named vector for different names
#'   between `svy` and `x` (names are keys in `svy`, values are keys in `x`).
#'   If NULL, tries to infer common column names
#' @param type Join type: "left" (default), "inner", "right", or "full"
#' @param suffixes Length-2 character vector of suffixes for conflicting columns
#'   from `svy` and `x` respectively. Defaults to c("", ".y")
#' @param use_copy Whether to operate on a copy (default: use_copy_default())
#' @param comment Optional description for the step
#'
#' @return Modified survey object with the join recorded as a step (and applied
#'   immediately when baked). For RotativePanelSurvey, the join is applied to
#'   implantation and every follow_up survey.
#'
#' @examples
#' \dontrun{
#' # With data.frame
#' s <- Survey$new(
#'   data = data.table::data.table(id = 1:3, w = 1, a = c("x", "y", "z")),
#'   edition = "2023", type = "ech", psu = NULL, engine = "data.table",
#'   weight = add_weight(annual = "w")
#' )
#' info <- data.frame(id = c(1, 2), b = c(10, 20))
#' s2 <- step_join(s, info, by = "id", type = "left")
#' s2 <- bake_steps(s2)
#'
#' # With another Survey
#' s_right <- Survey$new(
#'   data = data.table::data.table(id = c(2, 3), b = c(200, 300), w2 = 1),
#'   edition = "2023", type = "ech", psu = NULL, engine = "data.table",
#'   weight = add_weight(annual = "w2")
#' )
#' s3 <- step_join(s, s_right, by = c("id" = "id"), type = "inner")
#' s3 <- bake_steps(s3)
#' }
#'
#' @param lazy Logical, whether to delay execution.
#' @param record Logical, whether to record the step.
#' @keywords Steps
#' @export
step_join <- function(
    svy = survey_empty(),
    x,
    by = NULL,
    type = c("left", "inner", "right", "full"),
    suffixes = c("", ".y"),
    use_copy = use_copy_default(),
    comment = "Join step",
    lazy = lazy_default(),
    record = TRUE) {
  .call <- match.call()
  type <- match.arg(type)

  # Normalize RHS data source
  rhs_data <- if (methods::is(x, "Survey")) get_data(x) else x
  if (!is.data.frame(rhs_data)) stop("x must be a data.frame/data.table or a Survey")

  # RotativePanelSurvey: apply to implantation and each follow_up
  if (methods::is(svy, "RotativePanelSurvey")) {
    svy$implantation <- step_join(
      svy = svy$implantation, x = x, by = by, type = type,
      suffixes = suffixes, use_copy = use_copy, comment = comment
    )
    svy$follow_up <- lapply(
      svy$follow_up,
      function(fu) step_join(fu, x = x, by = by, type = type, suffixes = suffixes, use_copy = use_copy, comment = comment)
    )
    return(svy)
  }

  # If no data yet, return the call (pipeline build)
  if (is.null(get_data(svy))) {
    return(.call)
  }

  lhs_data <- get_data(svy)

  # Derive by mapping
  if (is.null(by)) {
    common <- intersect(names(lhs_data), names(rhs_data))
    if (length(common) == 0) stop("Cannot infer join keys: no common columns")
    by.x <- by.y <- common
  } else {
    if (is.null(names(by))) {
      by.x <- by
      by.y <- by
    } else {
      by.x <- names(by)
      by.y <- unname(by)
    }
  }

  # Check keys exist
  miss_x <- setdiff(by.x, names(lhs_data))
  miss_y <- setdiff(by.y, names(rhs_data))
  if (length(miss_x) > 0) stop(sprintf("Join keys not found in survey: %s", paste(miss_x, collapse = ", ")))
  if (length(miss_y) > 0) stop(sprintf("Join keys not found in x: %s", paste(miss_y, collapse = ", ")))

  # Prepare RHS: resolve name conflicts (excluding join keys)
  overlap <- intersect(setdiff(names(lhs_data), by.x), setdiff(names(rhs_data), by.y))
  if (length(overlap) > 0 && (suffixes[2] %in% c("", NA))) {
    # Ensure RHS conflicts are suffixed to avoid overwriting LHS
    suffixes[2] <- ".y"
  }
  if (length(overlap) > 0) {
    new_rhs_names <- names(rhs_data)
    idx <- match(overlap, names(rhs_data))
    # only rename true overlaps not part of by.y
    for (nm in overlap) {
      if (!nm %in% by.y) {
        new_rhs_names[names(rhs_data) == nm] <- paste0(nm, suffixes[2])
      }
    }
    names(rhs_data) <- new_rhs_names
  }

  # Compute merge flags
  all.x <- (type %in% c("left", "full"))
  all.y <- (type %in% c("right", "full"))

  # Perform merge using base merge then coerce to data.table
  merged <- base::merge(
    x = lhs_data,
    y = rhs_data,
    by.x = by.x,
    by.y = by.y,
    all.x = all.x,
    all.y = all.y,
    sort = FALSE
  )

  # Ensure data.table for downstream ops
  merged <- data.table::data.table(merged)

  # Assign data to copy or in-place
  if (use_copy) {
    out <- svy$clone(deep = TRUE)
    out$data <- merged
  } else {
    svy$data <- merged
    out <- svy
  }

  # Build depends_on from join keys
  if (isTRUE(record)) {
    depends_on <- unique(by.x)
    step <- Step$new(
      name = paste0("Join (", type, "): ", paste(by.x, collapse = ", ")),
      edition = get_edition(out),
      survey_type = get_type(out),
      type = "step_join",
      new_var = NULL,
      exprs = list(x = x, by = by, type = type, suffixes = suffixes),
      call = .call,
      svy_before = NULL,
      default_engine = get_engine(),
      depends_on = depends_on,
      comments = comment,
      bake = FALSE
    )
    out$add_step(step)
  }
  out
}

#' Remove variables from survey data (step)
#'
#' Creates a step that removes one or more variables from the survey data when baked.
#'
#' @param svy A Survey or RotativePanelSurvey object
#' @param ... Unquoted variable names to remove, or a character vector
#' @param use_copy Whether to operate on a copy (default: use_copy_default())
#' @param comment Optional description for the step
#' @param vars Character vector of variable names to remove.
#' @param lazy Logical, whether to delay execution.
#' @param record Logical, whether to record the step.
#' @export
step_remove <- function(svy = survey_empty(), ..., vars = NULL, use_copy = use_copy_default(), comment = "Remove variables", lazy = lazy_default(), record = TRUE) {
  .call <- match.call()
  var_names <- NULL
  # Prefer explicit vars argument when provided
  if (!is.null(vars)) {
    if (is.character(vars)) {
      var_names <- vars
    } else {
      stop("'vars' must be a character vector of variable names")
    }
  } else {
    dots_list <- as.list(substitute(list(...)))[-1]
    # Drop injected 'lazy' and 'record' arguments if present
    if (length(dots_list) > 0) {
      is_arg <- vapply(dots_list, function(x) !is.null(names(x)) && names(x) %in% c("lazy", "record"), logical(1))
      dots_list <- dots_list[!is_arg]
    }
    # Allow character vector passed via ...
    if (length(dots_list) == 1) {
      evald <- try(eval(dots_list[[1]], parent.frame()), silent = TRUE)
      if (!inherits(evald, "try-error") && is.character(evald)) {
        var_names <- as.character(evald)
      }
    }
    if (is.null(var_names)) {
      var_names <- vapply(dots_list, deparse1, character(1))
    }
  }

  if (is(svy, "RotativePanelSurvey")) {
    svy$implantation <- step_remove(svy$implantation, vars = var_names, use_copy = use_copy, comment = comment, record = record, lazy = lazy)
    svy$follow_up <- lapply(svy$follow_up, function(x) step_remove(x, vars = var_names, use_copy = use_copy, comment = comment, record = record, lazy = lazy))
    return(svy)
  }

  if (is.null(get_data(svy))) {
    return(.call)
  }

  out <- if (use_copy) svy$clone(deep = TRUE) else svy

  data <- get_data(svy) # Check against original data
  missing <- setdiff(var_names, names(data))
  if (length(missing) > 0) {
    warning(sprintf("Variables not found and cannot be removed: %s", paste(missing, collapse = ", ")))
  }

  # Apply change only if not lazy
  if (!lazy) {
    cols_to_remove <- intersect(var_names, names(out$data))
    if (length(cols_to_remove) > 0) {
      if (data.table::is.data.table(out$data)) {
        data.table::set(out$data, j = cols_to_remove, value = NULL)
      } else {
        out$data <- out$data[, !names(out$data) %in% cols_to_remove, drop = FALSE]
      }
    }
  }

  if (isTRUE(record)) {
    step <- Step$new(
      name = paste0("Remove: ", paste(var_names, collapse = ", ")),
      edition = get_edition(out),
      survey_type = get_type(out),
      type = "step_remove",
      new_var = NULL,
      exprs = list(vars = var_names),
      call = .call,
      svy_before = NULL,
      default_engine = get_engine(),
      depends_on = NULL,
      comments = comment,
      bake = FALSE
    )
    out$add_step(step)
  }
  out
}

#' Rename variables in survey data (step)
#'
#' Creates a step that renames variables in the survey data when baked.
#'
#' @param svy A Survey or RotativePanelSurvey object
#' @param ... Pairs in the form new_name = old_name (unquoted or character)
#' @param use_copy Whether to operate on a copy (default: use_copy_default())
#' @param comment Optional description for the step
#' @param mapping A named character vector of the form `c(new_name = "old_name")`.
#' @param lazy Logical, whether to delay execution.
#' @param record Logical, whether to record the step.
#' @export
step_rename <- function(svy = survey_empty(), ..., mapping = NULL, use_copy = use_copy_default(), comment = "Rename variables", lazy = lazy_default(), record = TRUE) {
  .call <- match.call()
  # Build mapping new -> old
  if (!is.null(mapping)) {
    if (is.null(names(mapping)) || !is.character(mapping)) {
      stop("'mapping' must be a named character vector: new_name = old_name")
    }
    map <- mapping
  } else {
    pairs <- as.list(substitute(list(...)))[-1]
    if (length(pairs) == 0) {
      return(svy)
    }
    # Drop injected 'lazy' and 'record' arguments if present
    if (length(pairs) > 0) {
      is_arg <- vapply(pairs, function(x) !is.null(names(x)) && names(x) %in% c("lazy", "record"), logical(1))
      pairs <- pairs[!is_arg]
    }
    new_names <- names(pairs)
    old_names <- vapply(pairs, function(x) if (is.symbol(x)) deparse1(x) else as.character(x), character(1))
    map <- stats::setNames(old_names, new_names)
  }

  if (is(svy, "RotativePanelSurvey")) {
    # Propagate as explicit mapping to avoid ambiguity
    svy$implantation <- step_rename(svy = svy$implantation, mapping = map, use_copy = use_copy, comment = comment, lazy = lazy, record = record)
    svy$follow_up <- lapply(svy$follow_up, function(x) step_rename(svy = x, mapping = map, use_copy = use_copy, comment = comment, lazy = lazy, record = record))
    return(svy)
  }

  if (is.null(get_data(svy))) {
    return(.call)
  }

  out <- if (use_copy) svy$clone(deep = TRUE) else svy

  data <- get_data(svy) # Check against original data
  missing <- setdiff(unname(map), names(data))
  if (length(missing) > 0) {
    stop(sprintf("Variables to rename not found: %s", paste(missing, collapse = ", ")))
  }

  # Apply change only if not lazy
  if (!lazy) {
    data.table::setnames(out$data, old = unname(map), new = names(map))
  }

  if (isTRUE(record)) {
    step <- Step$new(
      name = paste0("Rename: ", paste(sprintf("%s=%s", names(map), unname(map)), collapse = ", ")),
      edition = get_edition(out),
      survey_type = get_type(out),
      type = "step_rename",
      new_var = NULL,
      exprs = list(mapping = map),
      call = .call,
      svy_before = NULL,
      default_engine = get_engine(),
      depends_on = NULL,
      comments = comment,
      bake = FALSE
    )
    out$add_step(step)
  }
  out
}

#' Get type of step
#' @param steps List of steps
#' @return List of types
#' @noRd

get_type_step <- function(steps) {
  if (length(steps) > 0) {
    sapply(
      X = seq_along(steps),
      FUN = function(x) {
        step <- steps[[x]]
        step$type
      }
    )
  } else {
    NULL
  }
}

#' View graph
#' @param svy Survey object
#' @param init_step Initial step
#' @return Graph
#' @keywords Survey methods
#' @keywords Steps
#' @export
view_graph <- function(svy, init_step = "Load survey") {
  steps <- get_steps(svy)
  steps_type <- get_type_step(steps)
  formulas <- get_formulas(steps)
  comments <- get_comments(steps)

  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Package 'visNetwork' is required for this function. Please install it.")
  }

  # Helper to create the title tooltip
  create_title <- function(comment, formula) {
    paste(
      paste("<h2>", comment, "</h2>", sep = "\n"),
      paste("<h5>", formula, "</h5>", sep = "\n"),
      sep = "\n"
    )
  }

  # Initial node for the main survey
  if (init_step == "Load survey") {
    init_step_label <- "Load survey"
    init_step_title <- glue::glue_col(
      "
            Type: {type}
            Edition: {edition}
            Weight {weight}
            ",
      type = get_type(svy),
      edition = get_edition(svy),
      weight = get_info_weight(svy)
    )
  } else {
    init_step_label <- init_step
    init_step_title <- init_step
  }

  # Main graph components
  nodes <- data.frame(
    id = 1,
    label = init_step_label,
    title = init_step_title,
    group = "Load survey",
    stringsAsFactors = FALSE
  )
  edges <- data.frame(from = integer(), to = integer(), stringsAsFactors = FALSE)

  if (length(steps) > 0) {
    node_ids <- 2:(length(steps) + 1)
    nodes <- rbind(
      nodes,
      data.frame(
        id = node_ids,
        label = names(steps),
        title = create_title(comments, formulas),
        group = steps_type,
        stringsAsFactors = FALSE
      )
    )
    edges <- rbind(
      edges,
      data.frame(from = 1:length(steps), to = 2:(length(steps) + 1))
    )
  }

  # --- Process joins to add extra nodes and edges ---
  extra_nodes_list <- list()
  extra_edges_list <- list()
  node_id_counter <- nrow(nodes)

  if (length(steps) > 0) {
    for (i in 1:length(steps)) {
      step <- steps[[i]]
      current_step_node_id <- i + 1

      if (step$type == "step_join") {
        rhs <- step$exprs$x

        if (methods::is(rhs, "Survey")) {
          rhs_steps <- get_steps(rhs)
          rhs_steps_type <- get_type_step(rhs_steps)
          rhs_formulas <- get_formulas(rhs_steps)
          rhs_comments <- get_comments(rhs_steps)

          node_id_counter <- node_id_counter + 1
          rhs_init_node_id <- node_id_counter

          rhs_init_title <- glue::glue_col(
            "Type: {type}\nEdition: {edition}\nWeight: {weight}",
            type = get_type(rhs), edition = get_edition(rhs), weight = get_info_weight(rhs)
          )

          extra_nodes_list[[length(extra_nodes_list) + 1]] <- data.frame(
            id = rhs_init_node_id, label = "Load survey (join)", title = rhs_init_title, group = "Load survey", stringsAsFactors = FALSE
          )

          prev_rhs_node_id <- rhs_init_node_id

          if (length(rhs_steps) > 0) {
            for (j in 1:length(rhs_steps)) {
              node_id_counter <- node_id_counter + 1

              extra_nodes_list[[length(extra_nodes_list) + 1]] <- data.frame(
                id = node_id_counter,
                label = names(rhs_steps)[j],
                title = create_title(rhs_comments[j], rhs_formulas[j]),
                group = rhs_steps_type[j],
                stringsAsFactors = FALSE
              )

              extra_edges_list[[length(extra_edges_list) + 1]] <- data.frame(from = prev_rhs_node_id, to = node_id_counter)
              prev_rhs_node_id <- node_id_counter
            }
          }

          extra_edges_list[[length(extra_edges_list) + 1]] <- data.frame(from = prev_rhs_node_id, to = current_step_node_id)
        } else if (is.data.frame(rhs)) {
          node_id_counter <- node_id_counter + 1
          df_node_id <- node_id_counter

          df_name <- deparse(step$call$x)

          extra_nodes_list[[length(extra_nodes_list) + 1]] <- data.frame(
            id = df_node_id,
            label = paste("Data:", df_name),
            title = paste("External data.frame:", df_name),
            group = "dataframe",
            stringsAsFactors = FALSE
          )

          extra_edges_list[[length(extra_edges_list) + 1]] <- data.frame(from = df_node_id, to = current_step_node_id)
        }
      }
    }
  }

  if (length(extra_nodes_list) > 0) {
    nodes <- rbind(nodes, do.call(rbind, extra_nodes_list))
  }
  if (length(extra_edges_list) > 0) {
    edges <- rbind(edges, do.call(rbind, extra_edges_list))
  }

  edges <- edges[!is.na(edges$to), ]

  visNetwork::visNetwork(
    nodes = nodes,
    edges = edges,
    height = "500px", width = "100%"
  ) |>
    visNetwork::visGroups(
      groupname = "Load survey",
      shape = "icon",
      icon = list(
        code = "f1c0", # fa-database
        color = "#440154"
      ),
      shadow = list(enabled = TRUE)
    ) |>
    visNetwork::visGroups(
      groupname = "compute",
      shape = "icon",
      icon = list(
        code = "f1ec", # fa-calculator
        color = "#31688e"
      ),
      shadow = list(enabled = TRUE)
    ) |>
    visNetwork::visGroups(
      groupname = "recode",
      shape = "icon",
      icon = list(
        code = "f0e8", # fa-sitemap
        color = "#21918c"
      ),
      shadow = list(enabled = TRUE)
    ) |>
    visNetwork::visGroups(
      groupname = "step_join",
      shape = "icon",
      icon = list(
        code = "f0c1", # fa-link
        color = "#5ec962"
      ),
      shadow = list(enabled = TRUE)
    ) |>
    visNetwork::visGroups(
      groupname = "step_remove",
      shape = "icon",
      icon = list(
        code = "f1f8", # fa-trash
        color = "#fde725"
      ),
      shadow = list(enabled = TRUE)
    ) |>
    visNetwork::visGroups(
      groupname = "step_rename",
      shape = "icon",
      icon = list(
        code = "f044", # fa-edit
        color = "#fde725"
      ),
      shadow = list(enabled = TRUE)
    ) |>
    visNetwork::visGroups(
      groupname = "dataframe",
      shape = "icon",
      icon = list(
        code = "f1b3", # fa-table
        color = "#fde725"
      ),
      shadow = list(enabled = TRUE)
    ) |>
    visNetwork::addFontAwesome() |>
    visNetwork::visEdges(arrows = "to") |>
    visNetwork::visHierarchicalLayout(
      direction = "LR",
      levelSeparation = 200
    ) |>
    visNetwork::visOptions(
      nodesIdSelection = TRUE,
      clickToUse = TRUE,
      manipulation = FALSE
    ) |>
    visNetwork::visLegend(
      width = 0.2,
      position = "left",
      main = "Type",
      zoom = FALSE
    )
}


new_step <- function(id = 1, name, description, depends = NULL, type, new_var = NULL, ...) {
  if (type == "recode") {
    if (is.null(new_var)) {
      stop("new_var is required for recode")
    }
  }

  call <- do.call(
    paste0(
      "step_",
      type
    ),
    args = list(
      svy = survey_empty(),
      new_var = new_var,
      ...
    )
  )

  list(
    id = id,
    name = name,
    description = description,
    depends = depends,
    type = type,
    new_var = new_var,
    call = call
  )
}

#' @title Find dependencies
#' @description Find dependencies
#' @param call_expr Call expression
#' @param survey Survey
#' @keywords internal
#' @return List of dependencies
#' @noRd
#'
find_dependencies <- function(call_expr, survey) {
  dependencies <- character()

  if (is.call(call_expr)) {
    for (i in seq_along(call_expr)) {
      result <- find_dependencies(call_expr[[i]], survey)
      if (!is.null(result)) {
        dependencies <- unique(c(dependencies, result))
      }
    }
  } else if (is.name(call_expr) && as.character(call_expr) %in% names(survey)) {
    dependencies <- unique(c(dependencies, as.character(call_expr)))
  }

  return(unique(dependencies))
}
