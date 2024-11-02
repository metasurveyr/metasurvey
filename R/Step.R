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

    names(env) <- names(exprs_combined)[-1]

    for (i in seq_along(args_function_step)) {
      env[[args_function_step[i]]] <- step[[args_function_step[i]]]
    }

    env[["lazy"]] <- FALSE
    env[["svy"]] <- svy

    .svy_after <- do.call(
      what = step$type,
      args = env
    )

    .svy_after$steps[[step$name]]$bake <- TRUE

    return(.svy_after)
  }
}

#' Bake steps
#' @export
#' @keywords Surveymethods
#' @keywords Steps
#' @keywords Bake
#' @param svy A Survey object

bake_steps <- function(svy) {
  for (i in seq_along(svy$steps)) {
    svy <- bake_step(svy, svy$steps[[i]])
  }
  return(svy)
}
