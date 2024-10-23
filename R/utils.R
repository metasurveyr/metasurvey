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
  if (is.null(svy)) {
    return(NULL)
  }

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
#' @param svy_type Survey type
#' @param svy_edition Survey edition
#' @keywords utils
#' @export

load_survey_example <- function(svy_type, svy_edition) {

  baseUrl <- "https://raw.githubusercontent.com/metasurveyr/metasurvey_data/main/"

  f <- tempfile(fileext = ".csv")
  if (file.exists(f)) {
    return(f)
  } else {
    utils::download.file(
      paste0(
        baseUrl, 
        glue::glue(
          '{svy_type}/{svy_edition}.csv'
        )
      ),
      f,
      method = "auto"
    )
    return(f)
  }

  
  return(file.path(tempdir(), paste0(svy_type, paste0(svy_edition, ".csv"), sep = "/")))
}

#' Get use_copy option
#' @return Use copy
#' @keywords utils
#' @export

use_copy_default <- function() {
  getOption("use_copy", default = TRUE)
}

#' Set use_copy option
#' @param use_copy Use copy
#' @export
#' @keywords utils
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


#' Get User
#' @return User
#' @keywords utils
#' @keywords internal
#' @noRd

get_user <- function() {
  getOption("metasurvey.user", default = NULL) %||% "public"
}


#' URL API Host
#' @return URL API Host
#' @keywords utils
#' @keywords internal
#' @noRd

url_api_host <- function() {
  default_host <- "https://sa-east-1.aws.data.mongodb-api.com/app/data-vonssxi/endpoint/data/v1/action/"

  getOption("metasurvey.base_url") %||% default_host
}

#' Get API Key
#' @return API Key
#' @keywords utils
#' @export

get_api_key <- function() {
  public_key <- public_key()
  public_key
}

#' Public Key
#' @return Public Key
#' @keywords utils
#' @keywords internal
#' @noRd

public_key <- function() {
  url <- "https://services.cloud.mongodb.com/api/client/v2.0/app/data-vonssxi/auth/providers/anon-user/login"
  response <- POST(url)
  content <- content(response)
  return(content$access_token)
}


#' Set API Key
#' @param api_key API Key
#' @keywords utils
#' @export

set_api_key <- function(api_key) {
  options(metasurvey.api_key = api_key)
}


#' Lazy processing
#' @return Value
#' @keywords utils
#' @export

lazy_default <- function() {
  getOption("lazy_processing", default = TRUE)
}

#' Set lazy processing
#' @param lazy Lazy processing
#' @keywords utils
#' @export

set_lazy_processing <- function(lazy) {
  if (!is.logical(lazy)) {
    stop("lazy must be a logical")
  }

  options(lazy_processing = lazy)
}