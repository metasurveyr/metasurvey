 #' @title Load survey
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
#'   svy_weight = add_weight(annual = "w_trans"),
#'   input = "https://raw.githubusercontent.com/metasurveyr/metasurvey_data/main/eaii/2019-2021.csv",
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

  .engine <- getOption("metasurvey.engine")

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

#' Read file with data.table
#' @param file
#' @param args Additional arguments
#' @return data.table

read_file <- function(file, args) {
  .extension <- gsub(".*\\.", "", file)
  .read_function <- switch(.extension,
    sav = list(package = "foreign", read_function = "read.spss"),
    dta = list(package = "foreign", read_function = "read.dta"),
    csv = list(package = "data.table", read_function = "fread"),
    xlsx = list(package = "openxlsx", read_function = "loadWorkbook"),
    stop("Unsupported file type: ", .extension) 
  )

  require(.read_function$package, character.only = TRUE)

  do.call(.read_function$read_function, args = args)
}


#' Load survey with data.table
#' @param ... Additional arguments
#' @inheritDotParams  load_survey
#' @seealso data.table::fread foreign::read.spss openxlsx::loadWorkbook
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

  svy <- read_file(.args$file, .args[.names_args])

  if (!is.null(.args$recipes)) {
    if (get_distinct_recipes(.args$recipes) > 1) {
      index_valid_recipes <- sapply(
        X = 1:get_distinct_recipes(.args$recipes),
        FUN = function(x) {
          validate_recipe(
            svy_type = .args$svy_type,
            svy_edition = .args$svy_edition,
            recipe_svy_edition = .args$recipes[[x]]$edition,
            recipe_svy_type = .args$recipes[[x]]$svy_type
          )
        }
      )
    } else {
      index_valid_recipes <- validate_recipe(
        svy_type = .args$svy_type,
        svy_edition = .args$svy_edition,
        recipe_svy_edition = .args$recipes$edition,
        recipe_svy_type = .args$recipes$survey_type
      )
      if (!index_valid_recipes) {
        message(
          "Invalid Recipe: \n",
          .args$recipes$name
        )



        .args$recipes <- NULL
      }
    }
  }




  Survey$new(
    data = svy,
    edition = .args$svy_edition,
    type = .args$svy_type,
    engine = .engine_name,
    weight = .args$svy_weight,
    recipes = .args$recipes %||% NULL
  )
}



#' Config survey
#' @inheritDotParams load_survey
#' @noRd
#' @keywords internal

config_survey <- function(...) {
  match.call()[[1]]
}


#' Check valid recipe
#' @param svy_type Type of survey
#' @param svy_edition Edition of the survey
#' @param recipe_svy_edition Edition of the recipe
#' @param recipe_svy_type Type of the recipe
#' @return Logical
#' @keywords internal

validate_recipe <- function(svy_type, svy_edition, recipe_svy_edition, recipe_svy_type) {
  equal_type <- svy_type == recipe_svy_type

  equal_edition <- svy_edition == recipe_svy_edition



  return(equal_type & equal_edition)
}
