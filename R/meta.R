#' Metadata arguments for survey objects
#' @return Character vector
#' @noRd
#' @keywords internal

metadata_args <- function() {
  c("svy_type", "svy_edition", ".engine_name", "svy_weight", "recipes", "steps", "svy_psu", "bake")
}

#' @importFrom glue glue glue_col
#' @importFrom crayon green
#' @importFrom crayon red

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(glue_col("{green Welcome to: {.PACKAGE} version {utils::packageVersion(.PACKAGE)}}", .PACKAGE = pkgname))
  opts_default <- list(
    use_copy = use_copy_default(),
    metasurvey.engine = get_engine(),
    metasurvey.user = get_user(),
    metasurvey.lazy_processing = lazy_default()
  )

  invisible(vapply(
    X = seq_along(opts_default),
    FUN = function(x) {
      hidden_opts <- names(opts_default)[x] %in% c("metasurvey.api.key")

      if (hidden_opts) {
        opts_default[[x]] <- "********"
      }

      packageStartupMessage(glue_col("{red {names(opts_default)[x]}}: {opts_default[[x]]}"))
      ""
    },
    FUN.VALUE = character(1)
  ))
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
  } else {
    options(metasurvey.api_url = "https://metasurvey-api-production.up.railway.app")
  }
  env_token <- Sys.getenv("METASURVEY_TOKEN", "")
  if (nzchar(env_token)) {
    options(metasurvey.api_token = env_token)
  }
}
