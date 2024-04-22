is_blank <- function(x) {
  return(
    is.na(x) || x == ""
  )
}


"%||%" <- function(x, y) if (is.null(x)) y else x

"%@%" <- function(x, y) {
  if (is_blank(x)) {
    y
  } else {
    x
  }
}


validate_weight <- function(svy, weight) {
  if (!is.character(weight)) {
    stop("Weight must be a character")
  }

  if (!weight %in% colnames(svy)) {
    stop(glue_col(
      "{emoji::emoji('prohibited')} {red Weight {weight} not found in survey}",
      .literal = TRUE
    ))
  } else {
    weight
  }
}

#' Load survey example
#' @param path Path to the survey file
#' @export

load_survey_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "metaSurvey"))
  } else {
    system.file("extdata", path, package = "metaSurvey", mustWork = TRUE)
  }
}

#' Get use_copy option
#' @return Use copy
#' @export

use_copy_default <- function() {
  getOption("use_copy", default = TRUE)
}

#' Set use_copy option
#' @param use_copy Use copy
#' @export
#' @examples
#' set_use_copy(FALSE)
#' use_copy_default()
#' set_use_copy(TRUE)
#' use_copy_default()
set_use_copy <- function(use_copy) {
  if (!is.logical(use_copy)) {
    stop("use_copy must be a logical")
  }

  options(use_copy = use_copy)
}
