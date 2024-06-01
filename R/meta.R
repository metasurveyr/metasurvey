#' Metadata arguments for survey objects
#' @return Character vector
#' @noRd
#' @keywords internal

metadata_args <- function() {
  c("svy_type", "svy_edition", ".engine_name", "svy_weight", "recipes", "steps")
}

#' @importFrom glue glue glue_col
#' @importFrom crayon green
#' @importFrom crayon red

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(glue_col("{green Welcome to: {.PACKAGE} version {utils::packageVersion(.PACKAGE)}}", .PACKAGE = pkgname))
  opts_default <- list(
    use_copy = use_copy_default(),
    metasurvey.engine = get_engine(),
    metasurvey.api.key = get_api_key(),
    metasurvey.user = get_user()
  )

  sapply(
    X = 1:length(opts_default),
    FUN = function(x) {
      hidden_opts <- names(opts_default)[x] %in% c("metasurvey.api.key")

      if (hidden_opts) {
        opts_default[[x]] <- "********"
      }

      packageStartupMessage(glue_col("{red {names(opts_default)[x]}}: {opts_default[[x]]}"))
    }
  )
}

#' @importFrom glue glue glue_col
#' @importFrom crayon green
#' @importFrom crayon red

.onLoad <- function(libname, pkgname) {
  default_engine()

  set_use_copy(use_copy_default())
}
