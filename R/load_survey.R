#' Load survey
#'
#' @param path Path to the survey file
#' @param svy_type Type of survey
#' @param svy_edition Edition of the survey
#' @param svy_weight Weight of the survey
#' @param ... Additional arguments
#' @return Survey object
#' @keywords preprocessing
#' @export
#' @examples
#' set_engine("data.table")
#' svy_example <- load_survey(
#'   svy_type = "eaii",
#'   svy_edition = "2019-2021",
#'   svy_weight = "w_trans",
#'   input = load_survey_example("2019-2021.csv"),
#'   dec = ","
#' )
#' svy_example
load_survey <- function(
    path = NULL,
    svy_type = NULL,
    svy_edition = NULL,
    svy_weight = NULL,
    ...) {
  path_null <- missing(path)

  svy_args_null <- missing(svy_type) || missing(svy_edition) || missing(svy_weight)


  if (
    path_null && svy_args_null
  ) {
    stop(
      message(
        "Se debe indicar la ruta del archivo o una encuesta con su respectiva edicion"
      )
    )
  }

  .engine <- Sys.getenv("metasurvey.engine")

  .namespace <- ls(
    envir = asNamespace("metasurvey"),
    pattern = "load_survey"
  )

  .args <- list(
    file = path,
    svy_type = svy_type,
    svy_edition = svy_edition,
    svy_weight = svy_weight,
    .engine_name = .engine,
    ...
  )

  .call_engine <- paste0(
    "load_survey.",
    .engine
  )


  do.call(
    .call_engine,
    args = .args
  )
}

#' Load survey with data.table
#' @param ... Additional arguments
#' @inheritDotParams  load_survey
#' @seealso data.table::fread
#' @return Survey object
#' @importFrom data.table fread
#' @noRd
#' @keywords internal

load_survey.data.table <- function(...) {
  .engine_name <- "data.table"

  .args <- list(...)

  .names_args <- names(.args)

  .metadata_args <- metadata_args()

  .names_args <- .names_args[!.names_args %in% .metadata_args]

  args <- .args[.names_args]

  svy <- do.call(
    fread,
    args = args
  )

  Survey$new(
    data = svy,
    edition = .args$svy_edition,
    type = .args$svy_type,
    engine = .engine_name,
    weight = .args$svy_weight
  )
}



#' Config survey
#' @inheritDotParams load_survey
#' @noRd
#' @keywords internal

config_survey <- function(...) {
  match.call()[[1]]
}
