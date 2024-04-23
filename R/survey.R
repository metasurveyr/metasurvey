Survey <- R6Class("Survey",
  public = list(
    data = NULL,
    edition = NULL,
    type = NULL,
    default_engine = NULL,
    weight = NULL,
    steps = list(),
    initialize = function(data, edition, type, engine, weight) {
      self$data <- data
      self$edition <- edition
      self$type <- type
      self$default_engine <- engine
      self$weight <- validate_weight(data, weight)
      self$steps <- list()
    },
    get_data = function() {
      return(self$data)
    },
    get_edition = function() {
      return(self$edition)
    },
    get_type = function() {
      return(self$type)
    },
    set_data = function(data) {
      self$data <- data
    },
    set_edition = function(edition) {
      self$edition <- edition
    },
    set_type = function(type) {
      self$type <- type
    },
    set_weight = function(weight) {
      data <- self$data
      self$weight <- validate_weight(data, weight)
    },
    print = function() {
      get_metadata(self)
    },
    add_step = function(step) {
      self$steps[[step$name]] <- step
    },
    head = function() {
      head(self$data)
    },
    str = function() {
      str(self$data)
    }
  )
)


#' @title survey_to_data_frame
#' @description Convert survey to data.frame
#' @param svy Survey object
#' @export
#' @return data.frame
survey_to_data_frame <- function(svy) {
  data.frame(svy$get_data())
}

#' @title survey_to_tibble
#' @description Convert survey to tibble
#' @param svy Survey object
#' @export
#' @return tibble

survey_to_tibble <- function(svy) {
  tibble::as_tibble(svy$get_data())
}

#' @title survey_to_data.table
#' @description Convert survey to data.table
#' @param svy Survey object
#' @export
#' @importFrom data.table data.table
#' @return data.table
#' 

survey_to_data.table <- function(svy) {
  data.table::data.table(svy$get_data())
}

#' @title get_data
#' @description Get data from survey
#' @param svy Survey object
#' @export
#' @return Data
#' 

get_data <- function(svy) {
  svy$get_data()
}

get_edition <- function(svy) {
  svy$get_edition()
}

get_weight <- function(svy) {
  svy$weight
}

get_type <- function(svy) {
  svy$get_type()
}

set_data <- function(svy, data, .copy = TRUE) {
  if (.copy) {
    clone <- svy$clone()
    clone$set_data(data)
    return(clone)
  } else {
    svy$set_data(data)
  }
}

set_edition <- function(svy, new_edition) {
  clone <- svy$clone()
  clone$set_edition(new_edition)
  return(clone)
}

set_type <- function(svy, new_type) {
  clone <- svy$clone()
  clone$set_type(new_type)
  return(clone)
}

set_weight <- function(svy, new_weight) {
  clone <- svy$clone()
  clone$set_weight(new_weight)
  return(clone)
}

#' @title get_metadata
#' @description Get metadata from survey
#' @importFrom glue glue glue_col
#' @importFrom emoji emoji
#' @param self Object of class Survey
#' @export

get_metadata <- function(self) {
  message(
    glue::glue_col(
      "
            {emoji('information')}  {blue Type:} {type}
            {emoji('graph')} {blue Edition:} {edition}
            {emoji('desktop_computer')}  {blue Engine:} {default_engine}
            {emoji('abacus')} {blue Weight:} {weight}
            {emoji('mag')} {blue Steps:} {steps}
            ",
      type = self$type,
      edition = self$edition,
      default_engine = self$default_engine,
      weight = self$weight,
      steps = ifelse(
        length(self$steps) == 0,
        "None",
        paste0(
          "\n  - ",
          paste0(names(self$steps), collapse = "\n  - ")
        )
      ),
      .literal = TRUE
    )
  )

  invisible(
    list(
      type = self$type,
      edition = self$edition,
      default_engine = self$default_engine,
      weight = self$weight,
      steps = names(self$steps)
    )
  )
}

#' @title get_steps
#' @description Get steps from survey
#' @param svy Survey object
#' @export
#' @return List

get_steps <- function(svy) {
  svy$steps
}
