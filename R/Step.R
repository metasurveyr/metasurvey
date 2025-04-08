#' Step Class
#' Represents a step in a survey workflow.
#' @description The `Step` class is used to define and manage individual steps in a survey workflow. Each step can include operations such as recoding variables, computing new variables, or validating dependencies.
#' @field name The name of the step.
#' @field edition The edition of the survey associated with the step.
#' @field survey_type The type of survey associated with the step.
#' @field type The type of operation performed by the step (e.g., "compute", "recode").
#' @field new_var The name of the new variable created by the step, if applicable.
#' @field exprs A list of expressions defining the step's operations.
#' @field call The function call associated with the step.
#' @field svy_before The survey object before the step is applied.
#' @field default_engine The default engine used for processing the step.
#' @field depends_on A list of variables that the step depends on.
#' @field comments Comments or notes about the step.
#' @field bake A logical value indicating whether the step has been executed.
#' @details The `Step` class is part of the survey workflow system and is designed to encapsulate all the information and operations required for a single step in the workflow. Steps can be chained together to form a complete workflow.
#' @examples
#' step <- Step$new(
#'   name = "example_step",
#'   edition = "2023",
#'   survey_type = "example_survey",
#'   type = "compute",
#'   new_var = "example_var",
#'   exprs = list(a = 1, b = 2),
#'   call = NULL,
#'   svy_before = NULL,
#'   default_engine = NULL,
#'   depends_on = list("var1", "var2"),
#'   comments = "Example step",
#'   bake = FALSE
#' )
#' print(step)
#' @keywords Surveymethods
#' @keywords Steps
#' @keywords Workflow
#' @export
#' @param name The name of the step.
#' @param edition The edition of the survey associated with the step.
#' @param survey_type The type of survey associated with the step.
#' @param type The type of operation performed by the step (e.g., "compute", "recode").
#' @param new_var The name of the new variable created by the step, if applicable.
#' @param exprs A list of expressions defining the step's operations.
#' @param call The function call associated with the step.
#' @param svy_before The survey object before the step is applied.
#' @param default_engine The default engine used for processing the step.
#' @param depends_on A list of variables that the step depends on.
#' @param comments Comments or notes about the step.
#' @param bake A logical value indicating whether the step has been executed.
Step <- R6Class("Step",
  public = list(
    #' @method initialize method
    #' @description Initializes a new Step object with the provided attributes.
    #' @param name The name of the step.
    #' @param edition The edition of the survey associated with the step.
    #' @param survey_type The type of survey associated with the step.
    #' @param type The type of operation performed by the step (e.g., "compute", "recode").
    #' @param new_var The name of the new variable created by the step, if applicable.
    #' @param exprs A list of expressions defining the step's operations.
    #' @param call The function call associated with the step.
    #' @param svy_before The survey object before the step is applied.
    #' @param default_engine The default engine used for processing the step.
    #' @param depends_on A list of variables that the step depends on.
    #' @param comments Comments or notes about the step.
    #' @param bake A logical value indicating whether the step has been executed.
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
    initialize = function(name, edition, survey_type, type, new_var, exprs, call, svy_before, default_engine, depends_on, comments, bake = !lazy_default()) {
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
      self$comments <- comments
      self$bake <- bake
    }
  )
)

#' Step to environment
#' Converts a Step object into an environment for further processing.
#' @param step A Step object containing the step details.
#' @return An environment containing the step's arguments and expressions.
#' @details This function extracts the arguments of the step's type function and maps them to the step's attributes. It also substitutes the step's expressions into the environment for evaluation.
#' @examples
#' step <- Step$new(name = "example_step", edition = "2023", survey_type = "example_survey", type = "compute", new_var = "example_var", exprs = list(a = 1, b = 2), call = NULL, svy_before = NULL, default_engine = NULL, depends_on = list("var1", "var2"), comments = "Example step", bake = FALSE)
#' env <- step_to_env(step)
#' print(env)
#' @keywords Surveymethods
#' @noRd
#' @keywords internal

step_to_env <- function(step) {
  args_function_step <- names(formals(step$type))
  env <- list()
  for (i in seq_along(args_function_step)) {
    env[[args_function_step[i]]] <- step[[args_function_step[i]]]
  }
  env <- substitute(step$exprs)
  return(env)
}

#' Validate step
#' Validates that all dependencies of a Step object are present in the Survey object.
#' @param svy A Survey object containing the survey data.
#' @param step A Step object containing the step details.
#' @return TRUE if all dependencies are present, otherwise an error is raised.
#' @details This function checks if the variables listed in the `depends_on` attribute of the Step object exist in the Survey object's data. If any variables are missing, an error is thrown with the names of the missing variables.
#' @examples
#' svy <- Survey$new(data = data.frame(var1 = 1:10, var2 = 11:20))
#' step <- Step$new(name = "example_step", edition = "2023", survey_type = "example_survey", type = "compute", new_var = "example_var", exprs = list(a = 1, b = 2), call = NULL, svy_before = NULL, default_engine = NULL, depends_on = list("var1", "var2"), comments = "Example step", bake = FALSE)
#' validate_step(svy, step)
#' @keywords Surveymethods
#' @keywords Steps
#' @keywords Validate
#' @noRd
#' @keywords internal

validate_step <- function(svy, step) {
  names_svy <- names(svy$data)
  depends_on <- step$depends_on

  missing_vars <- depends_on[!depends_on %in% names_svy]

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
#' @keywords Surveymethods
#' @keywords Steps
#' @keywords Bake
#' @keywords Survey
#' @noRd
#' @keywords internal

bake_step <- function(svy, step) {
  if (!step$bake) {
    step_valid <- validate_step(svy, step)

    if (!step_valid) {
      return(svy)
    }

    args_function_step <- names(formals(step$type))

    exprs_combined <- lapply(step$exprs, identity)

    env <- list()

    for (i in 1:length(exprs_combined)) {
      if (exprs_combined[[i]] != "list") {
        env <- c(env, exprs_combined[[i]])
      }
    }

    if (step$type == "compute") {
      names(env) <- names(exprs_combined)[-1]
    }

    if (step$type == "recode") {
      env[["new_var"]] <- step$new_var
    }

    names_arg <- names(step$call)

    names_arg <- names_arg[names_arg %in% args_function_step]

    names_not_env <- names_arg[!names_arg %in% names(env)]

    for (i in seq_along(names_not_env)) {
      env[[names_not_env[i]]] <- step$call[[names_not_env[i]]]
    }

    env[["lazy"]] <- FALSE
    env[["svy"]] <- svy

    if (use_copy_default()) {
      env[["use_copy"]] <- TRUE
    }



    .svy_after <- do.call(
      what = step$type,
      args = env
    )

    return(.svy_after)
  }
}

#' Bake steps
#' Applies all steps in a survey workflow to a Survey or RotativePanelSurvey object.
#' @param svy A Survey or RotativePanelSurvey object containing the survey data and steps to be applied.
#' @return A Survey or RotativePanelSurvey object with all steps applied.
#' @details This function iterates through all the steps defined in the survey object and applies them sequentially. If the object is a RotativePanelSurvey, it processes both the implantation and follow-up surveys.
#' @examples
#' svy <- Survey$new(data = data.frame(var1 = 1:10, var2 = 11:20))
#' step1 <- Step$new(name = "step1", edition = "2023", survey_type = "example_survey", type = "compute", new_var = "var3", exprs = list(var3 = var1 + var2), call = NULL, svy_before = NULL, default_engine = NULL, depends_on = list("var1", "var2"), comments = "Compute var3", bake = FALSE)
#' svy$steps <- list(step1)
#' svy <- bake_steps(svy)
#' print(svy$data)
#' @keywords Surveymethods
#' @keywords Steps
#' @keywords Bake
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
#' @keywords Surveymethods
#' @keywords Steps
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
      svy$follow_up <- bake_steps_survey(svy$follow_up[[i]])
    }
    return(svy)
  }
}

#' Bake steps survey
#' @param svy A Survey object
#' @return A Survey object
#' @keywords Surveymethods
#' @keywords Steps
#' @keywords Bake
#' @keywords Survey
#' @noRd
#' @keywords internal

bake_steps_survey <- function(svy) {
  if (use_copy_default()) {
    svy_copy <- svy$clone(deep = TRUE)
    for (i in seq_along(svy$steps)) {
      svy_copy <- bake_step(svy_copy, svy$steps[[i]])
      svy_copy$steps[[i]] <- svy_copy$steps[[i]]$clone(deep = TRUE)
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
