#' Step Class
#' Represents a step in a survey workflow.
#' @description The `Step` class is used to define and manage
#' individual steps in a survey workflow. Each step can
#' include operations such as recoding variables, computing
#' new variables, or validating dependencies.
#' @field name The name of the step.
#' @field edition The edition of the survey associated with the step.
#' @field survey_type The type of survey associated with the step.
#' @field type The type of operation performed by the step
#'   (e.g., "compute", "recode").
#' @field new_var The name of the new variable created by
#'   the step, if applicable.
#' @field exprs A list of expressions defining the step's
#'   operations.
#' @field call The function call associated with the step.
#' @field svy_before The survey object before the step is
#'   applied.
#' @field default_engine The default engine used for
#'   processing the step.
#' @field depends_on A list of variables that the step
#'   depends on.
#' @field comments Comments or notes about the step.
#' @field bake A logical value indicating whether the step
#'   has been executed.
#' @details The `Step` class is part of the survey workflow
#' system and is designed to encapsulate all the information
#' and operations required for a single step in the
#' workflow. Steps can be chained together to form a
#' complete workflow.
#' @examples
#' # Step objects are created internally by step_compute(), step_recode(), etc.
#' # Use the tidy API:
#' dt <- data.table::data.table(id = 1:3, age = c(25, 30, 45), w = 1)
#' svy <- Survey$new(
#'   data = dt, edition = "2023", type = "test",
#'   psu = NULL, engine = "data.table", weight = add_weight(annual = "w")
#' )
#' svy <- step_compute(svy, age2 = age * 2)
#' get_steps(svy)
#' @keywords survey
#' @keywords step
#' @keywords internal
#' @param name The name of the step.
#' @param edition The edition of the survey associated with the step.
#' @param survey_type The type of survey associated with the step.
#' @param type The type of operation performed by the step
#'   (e.g., "compute", "recode").
#' @param new_var The name of the new variable created by
#'   the step, if applicable.
#' @param exprs A list of expressions defining the step's
#'   operations.
#' @param call The function call associated with the step.
#' @param svy_before The survey object before the step is
#'   applied.
#' @param default_engine The default engine used for
#'   processing the step.
#' @param depends_on A list of variables that the step
#'   depends on.
#' @param comments Comments or notes about the step.
#' @param comment Optional alias of `comments` for
#'   backwards compatibility.
#' @param bake A logical value indicating whether the step
#'   has been executed.
Step <- R6Class("Step",
  public = list(
    name = NULL,
    edition = NULL,
    survey_type = NULL,
    type = NULL,
    new_var = NULL,
    exprs = NULL,
    call = NULL,
    svy_before = NULL,
    default_engine = NULL,
    depends_on = list(),
    comments = NULL,
    bake = NULL,
    #' @description Create a new Step object
    #' @param name The name of the step.
    #' @param edition The edition of the survey associated with the step.
    #' @param survey_type The type of survey associated with the step.
    #' @param type The type of operation performed by the
    #'   step (e.g., "compute" or "recode").
    #' @param new_var The name of the new variable created
    #'   by the step, if applicable.
    #' @param exprs A list of expressions defining the
    #'   step's operations.
    #' @param call The function call associated with the
    #'   step.
    #' @param svy_before The survey object before the step
    #'   is applied.
    #' @param default_engine The default engine used for
    #'   processing the step.
    #' @param depends_on A list of variables that the step
    #'   depends on.
    #' @param comments Comments or notes about the step.
    #' @param comment Optional alias of `comments` for
    #'   backwards compatibility.
    #' @param bake A logical value indicating whether the
    #'   step has been executed.

    initialize = function(name, edition, survey_type,
                          type, new_var, exprs, call,
                          svy_before, default_engine,
                          depends_on, comments = NULL,
                          bake = !lazy_default(),
                          comment = NULL) {
      self$name <- name
      self$edition <- edition
      self$survey_type <- survey_type
      self$type <- type
      self$new_var <- new_var
      self$exprs <- exprs
      self$call <- call
      self$svy_before <- svy_before
      self$default_engine <- default_engine
      self$depends_on <- depends_on
      # accept both 'comments' and legacy 'comment'
      self$comments <- comments %||% comment
      self$bake <- bake
    }
  )
)

#' Validate step
#' @param svy A Survey object
#' @param step A Step object
#' @keywords survey
#' @keywords step
#' @keywords Validate
#' @noRd
#' @keywords internal

validate_step <- function(svy, step) {
  names_svy <- names(svy$data)
  depends_on <- step$depends_on


  missing_vars <- depends_on[!depends_on %in% names_svy]

  # If there are missing variables, throw an error with their names
  if (length(missing_vars) > 0) {
    stop(
      paste0(
        "The following variables are not in the survey: ",
        paste(missing_vars, collapse = ", ")
      )
    )
  } else {
    return(TRUE)
  }
}


#' Bake step
#' @param svy A Survey object
#' @param step A Step object
#' @return A Survey object
#' @keywords survey
#' @keywords step
#' @keywords Bake
#' @keywords Survey
#' @noRd
#' @keywords internal

bake_step <- function(svy, step) {
  if (step$bake) {
    return(svy)
  }

  validate_step(svy, step)

  # Prepare the list of arguments for do.call
  args <- list()

  # Add mandatory arguments for baking
  args$svy <- svy
  args$lazy <- FALSE
  args$use_copy <- use_copy_default()

  # Add step-specific arguments from exprs
  # step$exprs may be a call (e.g., list(var = expr)) from substitute(list(...))
  # or a regular list — handle both cases
  if (is.call(step$exprs) &&
      identical(step$exprs[[1]], as.name("list"))) {
    args <- c(args, as.list(step$exprs)[-1])
  } else if (is.list(step$exprs)) {
    args <- c(args, step$exprs)
  }

  # Special case for step_recode, which has `new_var` as a separate argument
  if (step$type == "recode") {
    args$new_var <- step$new_var
  }

  # Validate step type before dispatch

  valid_types <- c(
    "compute", "recode", "step_join",
    "step_remove", "step_rename"
  )
  if (!step$type %in% valid_types) {
    stop("Invalid step type: '", step$type, "'. Must be one of: ",
      paste(valid_types, collapse = ", "),
      call. = FALSE
    )
  }

  # Execute the step function
  updated_svy <- do.call(step$type, args)

  return(updated_svy)
}

#' Execute all pending steps
#'
#' Iterates over all pending (lazy) steps attached to a Survey or
#' RotativePanelSurvey and executes them sequentially, mutating
#' the underlying data.table. Each step is validated before execution
#' (checks that required variables exist).
#'
#' @param svy A `Survey` or `RotativePanelSurvey` object with pending steps
#'
#' @return The same object with all steps materialized in the data
#'   and each step marked as `bake = TRUE`.
#'
#' @details
#' Steps are executed in the order they were added. Each step's expressions
#' can reference variables created by previous steps.
#'
#' For RotativePanelSurvey objects, steps are applied to both the
#' implantation and all follow-up surveys.
#'
#' @examples
#' dt <- data.table::data.table(id = 1:5, age = c(15, 30, 45, 50, 70), w = 1)
#' svy <- Survey$new(
#'   data = dt, edition = "2023", type = "test",
#'   psu = NULL, engine = "data.table", weight = add_weight(annual = "w")
#' )
#' svy <- step_compute(svy, age2 = age * 2)
#' svy <- bake_steps(svy)
#' get_data(svy)
#' @family steps
#' @export
bake_steps <- function(svy) {
  if (is(svy, "Survey")) {
    return(bake_steps_survey(svy))
  }

  if (is(svy, "RotativePanelSurvey")) {
    return(bake_steps_rotative(svy))
  }

  stop("The object is not a Survey or RotativePanelSurvey object")
}

#' Bake steps survey rotative
#' @param svy A Survey object
#' @return A Survey object
#' @keywords survey
#' @keywords step
#' @keywords Bake
#' @keywords Survey
#' @noRd
#' @keywords internal


bake_steps_rotative <- function(svy) {
  if (use_copy_default()) {
    svy_copy <- svy$clone(deep = TRUE)
    svy_copy$implantation <- bake_steps_survey(svy_copy$implantation)
    for (i in seq_along(svy$follow_up)) {
      svy_copy$follow_up[[i]] <- bake_steps_survey(svy_copy$follow_up[[i]])
    }
    return(svy_copy)
  } else {
    svy$implantation <- bake_steps_survey(svy$implantation)
    for (i in seq_along(svy$follow_up)) {
      svy$follow_up[[i]] <- bake_steps_survey(svy$follow_up[[i]])
    }
    return(svy)
  }
}

#' Bake steps survey
#' @param svy A Survey object
#' @return A Survey object
#' @keywords survey
#' @keywords step
#' @keywords Bake
#' @keywords Survey
#' @noRd
#' @keywords internal

bake_steps_survey <- function(svy) {
  if (use_copy_default()) {
    svy_copy <- svy$clone(deep = TRUE)
    for (i in seq_along(svy_copy$steps)) {
      svy_copy <- bake_step(svy_copy, svy_copy$steps[[i]])
      svy_copy$steps[[i]]$bake <- TRUE
    }
    svy_copy$update_design()
    return(svy_copy)
  } else {
    for (i in seq_along(svy$steps)) {
      bake_step(svy, svy$steps[[i]])
      svy$steps[[i]]$bake <- TRUE
    }
    svy$update_design()
    return(svy)
  }
}
