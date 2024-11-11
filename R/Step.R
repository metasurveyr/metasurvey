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
#' @param step A Step object
#' @return An environment
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
#' @param svy A Survey object
#' @param step A Step object
#' @keywords Surveymethods
#' @keywords Steps
#' @keywords Validate
#' @noRd
#' @keywords internal

validate_step <- function(svy, step) {
  names_svy <- names(svy$data)
  depends_on <- step$depends_on


  missing_vars <- depends_on[!depends_on %in% names_svy]

  # Si hay variables faltantes, lanza un error con los nombres
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
#' @export
#' @keywords Surveymethods
#' @keywords Steps
#' @keywords Bake
#' @param svy A Survey object
#'

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
      svy$follow_up[[i]] <- bake_steps_survey(svy$follow_up[[i]])
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
