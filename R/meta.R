#' Metadata arguments for survey objects
#' @return Character vector
#' @noRd
#' @keywords internal

metadata_args <- function() {
  c(
    "svy_type", "svy_edition", ".engine_name",
    "svy_weight", "recipes", "steps", "svy_psu", "bake"
  )
}

#' @importFrom glue glue glue_col
#' @importFrom crayon green
#' @importFrom crayon red

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    sprintf("metasurvey %s", utils::packageVersion(pkgname))
  )
}

#' @importFrom glue glue glue_col
#' @importFrom crayon green
#' @importFrom crayon red

.onLoad <- function(libname, pkgname) {
  default_engine()

  set_use_copy(use_copy_default())

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
