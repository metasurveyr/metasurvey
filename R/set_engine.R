#' @title set_engine
#' @keywords engine
#' @description This function configures the engine to be used for loading surveys. It checks if the provided engine is supported, sets the default engine if none is specified, and generates a message indicating the configured engine. If the engine is not supported, it throws an error.
#' @param .engine Character vector with the name of the engine to configure. By default, the engine returned by the `show_engines()` function is used.
#' @return Invisibly, the configured engine name.
#' @examples
#' \dontrun{
#' set_engine("data.table")
#' }
#' @importFrom glue glue glue_col identity_transformer
#' @export

set_engine <- function(.engine = show_engines()) {
  .support_engine <- show_engines()

  if (
    identical(
      .engine,
      .support_engine
    )
  ) {
    default_engine()
  } else {
    if (
      .engine %in% .support_engine
    ) {
      options(metasurvey.engine = .engine)
    } else {
      stop(
        message(
          glue_col(
            "{red Upss... engine: {.engine} no soportado :(}",
            .engine = .engine
          )
        )
      )
    }
  }

  message(
    glue_col(
      "{green Engine: {.engine}}",
      .engine = get_engine()
    )
  )

  metacode <- glue(
    "if (!require({.engine},quietly = TRUE, warn.conflicts = TRUE)) install.packages('{.engine}', repos = 'http://cran.us.r-project.org')",
    .engine = get_engine()
  )

  identity_transformer(
    text = metacode,
    envir = parent.frame()
  )
}

#' @title show_engines
#' @description This function returns a list of available engines that can be used for loading surveys. The available engines are "data.table", "tidyverse", and "dplyr".
#' @importFrom glue glue glue_col
#' @export
#' @return Character vector with the names of the available engines.
#' @examples
#' show_engines()

show_engines <- function() {
  c(
    "data.table",
    "tidyverse",
    "dplyr"
  )
}

#' @title get_engine
#' @description This function retrieves the currently configured engine for loading surveys. It returns the engine configured in the system options or environment variables.
#' @export
#' @return Character vector with the name of the configured engine.
#' @keywords engine
#' @examples
#' get_engine()

get_engine <- function() {
  Sys.getenv("metasurvey.engine") %@% getOption("metasurvey.engine")
}

#' @title default_engine
#' @description This function sets a default engine for loading surveys. If an engine is already configured, it keeps it; otherwise, it sets "data.table" as the default engine.
#' @param .engine Character vector with the name of the default engine. By default, "data.table" is used.
#' @keywords internal

default_engine <- function(.engine = "data.table") {
  engine_env <- get_engine()

  options(metasurvey.engine = engine_env %||% .engine)
  engine_env %||% .engine
}
