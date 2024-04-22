#' @title set_engine
#' @description Set engine for load survey
#' @param .engine Character vector with the name of the engine
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
      Sys.setenv(
        "metasurvey.engine" = .engine
      )
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
      .engine = Sys.getenv("metasurvey.engine")
    )
  )

  metacode <- glue(
    "if (!require({.engine},quietly = TRUE, warn.conflicts = TRUE)) install.packages('{.engine}')",
    .engine = get_engine()
  )

  identity_transformer(
    text = metacode,
    envir = .GlobalEnv
  )
}

#' @title show_engines
#' @description Show engines available
#' @importFrom glue glue glue_col
#' @export
#' @return Character vector

show_engines <- function() {
  c(
    "data.table",
    "tidyverse",
    "dplyr"
  )
}

#' @title get_engine
#' @description Get engine
#' @export
#' @return Character vector of the engine
#' @keywords engine

get_engine <- function() {
  Sys.getenv("metasurvey.engine")
}

#' @title default_engine
#' @description Set default engine
#' @param .engine Character vector with the name of the engine
#' @export
#' @keywords engine

default_engine <- function(.engine = "data.table") {
  engine_env <- get_engine()

  Sys.setenv(
    "metasurvey.engine" = engine_env %@% .engine
  )
}
