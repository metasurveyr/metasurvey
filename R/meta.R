#' Metadata arguments for survey objects
#' @return Character vector
#' @noRd
#' @keywords internal

metadata_args <- function() {
  c(
    "svy_type", "svy_edition", ".engine_name",
    "svy_weight", "recipes", "steps", "svy_psu", "svy_strata", "bake"
  )
}

#' @importFrom glue glue
NULL

#' User-Agent string for all HTTP requests
#' @return Character string like "metasurvey/0.0.19"
#' @noRd
metasurvey_user_agent <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("metasurvey")),
    error = function(e) "dev"
  )
  paste0("metasurvey/", version, " (https://github.com/metasurveyr/metasurvey)")
}

#' Internal message helper with verbose opt-out
#'
#' Wraps \code{message()} behind the \code{metasurvey.verbose} option.
#' Set \code{options(metasurvey.verbose = FALSE)} to suppress all
#' informational messages from metasurvey.
#' @param ... Arguments passed to \code{message()}.
#' @noRd
metasurvey_msg <- function(...) {
  if (isTRUE(getOption("metasurvey.verbose", TRUE))) {
    message(...)
  }
}

.onLoad <- function(libname, pkgname) {
  default_engine()

  set_use_copy(use_copy_default())

  if (is.null(getOption("metasurvey.verbose"))) {
    options(metasurvey.verbose = TRUE)
  }

  # Auto-configure API from environment variables
  env_url <- Sys.getenv("METASURVEY_API_URL", "")
  if (nzchar(env_url)) {
    options(metasurvey.api_url = sub("/$", "", env_url))
  }
  env_token <- Sys.getenv("METASURVEY_TOKEN", "")
  if (nzchar(env_token)) {
    options(metasurvey.api_token = env_token)
  }
}
