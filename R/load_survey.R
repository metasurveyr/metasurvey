#' @title Read survey files from different formats and create survey objects 
#' @param path Path to the survey file
#' @param svy_type Type of survey
#' @param svy_edition Edition of the survey
#' @param svy_weight Weight of the survey
#' @param svy_psu Primary sampling unit
#' @param ... Additional arguments
#' @param bake Logical
#' @return Survey object
#' @keywords preprocessing
#' @export
load_survey <- function(
    path = NULL,
    svy_type = NULL,
    svy_edition = NULL,
    svy_weight = NULL,
    svy_psu = NULL,
    ..., bake = FALSE) {
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
    svy_psu = svy_psu,
    .engine_name = .engine,
    bake = bake,
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


#' Load panel survey
#' @param path_implantation Path to the implantation survey
#' @param path_follow_up Path to the follow-up survey
#' @param svy_type Type of survey
#' @param svy_weight_implantation Weight of the implantation survey
#' @param svy_weight_follow_up Weight of the follow-up survey
#' @keywords preprocessing
#' @return RotativePanelSurvey object
#' @export

load_panel_survey <- function(
    path_implantation,
    path_follow_up,
    svy_type,
    svy_weight_implantation,
    svy_weight_follow_up) {
  names_survey <- gsub(
    "\\..*",
    "",
    list.files(path_follow_up, full.names = FALSE, pattern = ".csv")
  )

  if (length(names(svy_weight_follow_up)) > 1) {
    stop(
      "The follow-up survey must have a single weight time pattern"
    )
  }

  time_pattern_follow_up <- names(svy_weight_follow_up)

  if (is(svy_weight_follow_up[[1]], "list")) {
    svy_weight_follow_up <- svy_weight_follow_up[[1]]
  }

  path_survey <- list.files(path_follow_up, full.names = TRUE, pattern = ".csv")

  names(path_survey) <- names_survey




  implantation <- load_survey(
    path_implantation,
    svy_type = svy_type,
    svy_edition = basename(path_implantation),
    svy_weight = svy_weight_implantation
  )

  if (!is.null(svy_weight_follow_up$replicate_path)) {
    path_file <- svy_weight_follow_up$replicate_path
    path_file_final <- c()

    for (i in path_file) {
      if (file.info(i)$isdir) {
        path_file_final <- c(path_file_final, list.files(i, full.names = TRUE, pattern = ".csv"))
      } else {
        path_file_final <- c(path_file_final, i)
      }
    }


    names_year_month <- sapply(
      X = basename(path_file_final),
      FUN = function(x) {
        time_pattern <- extract_time_pattern(x)
        if (time_pattern$periodicity != "Monthly") {
          stop(
            message(
              "The periodicity of the file is not monthly"
            )
          )
        } else {
          return(
            time_pattern$year * 100 + time_pattern$month
          )
        }
      },
      USE.NAMES = FALSE
    )

    names(path_file_final) <- names_year_month


    svy_weight_follow_up <- lapply(
      X = as.character(names_year_month),
      FUN = function(x) {
        replicate <- list(
          add_replicate(
            "W",
            replicate_path = unname(path_file_final[x]),
            replicate_id = c("ID" = "ID"),
            replicate_pattern = "wr[0-9]+",
            replicate_type = "bootstrap"
          )
        )

        names(replicate) <- time_pattern_follow_up

        return(replicate)
      }
    )

    names(svy_weight_follow_up) <- names_year_month

    names_path_survey_year_month <- sapply(
      X = names(path_survey),
      FUN = function(x) {
        time_pattern <- extract_time_pattern(x)
        if (time_pattern$periodicity != "Monthly") {
          stop(
            message(
              "The periodicity of the file is not monthly"
            )
          )
        } else {
          return(
            time_pattern$year * 100 + time_pattern$month
          )
        }
      },
      USE.NAMES = FALSE
    )

    names(path_survey) <- names_path_survey_year_month

    follow_up <- lapply(
      X = 1:length(path_survey),
      FUN = function(x) {
        y <- path_survey[[x]]
        z <- names(path_survey)[x]
        svy_weight <- unname(svy_weight_follow_up[z])[[1]]

        load_survey(
          y,
          svy_type = svy_type,
          svy_edition = basename(y),
          svy_weight = svy_weight
        )
      }
    )

    names(follow_up) <- names_survey
  } else {
    follow_up <- lapply(
      X = names(path_survey),
      FUN = function(x) {
        load_survey(
          path_survey[[x]],
          svy_type = svy_type,
          svy_edition = x,
          svy_weight = svy_weight_follow_up
        )
      }
    )

    names(follow_up) <- names_survey
  }

  return(
    RotativePanelSurvey$new(
      implantation = implantation,
      follow_up = follow_up,
      type = svy_type,
      default_engine = "data.table",
      steps = NULL,
      recipes = NULL,
      workflows = NULL,
      design = NULL
    )
  )
}

#' Read file with data.table
#' @param file Path to the file
#' @param .args Additional arguments
#' @keywords internal
#' @noRd
#' @return data.table

read_file <- function(file, .args = NULL, convert = FALSE) {
  .extension <- gsub(".*\\.", "", file)
  .file_name <- basename(file)

  .path_without_extension <- gsub("\\..*", "", .file_name)
  .output_file <- paste0(.path_without_extension, ".csv")
  .output_file <- file.path(
    dirname(file),
    .output_file
  )


  if (convert) {
    if (.extension != ".csv" && !file.exists(.output_file)) {
      requireNamespace("rio", quietly = TRUE)
      rio::convert(
        in_file = file,
        out_file = .output_file
      )
      .extension <- "csv"
    } else {
      .extension <- "csv"
    }
  }

  .read_function <- switch(.extension,
    sav = list(package = "foreign", read_function = "read.spss"),
    dta = list(package = "foreign", read_function = "read.dta"),
    csv = list(package = "data.table", read_function = "fread"),
    xlsx = list(package = "openxlsx", read_function = "read.xlsx"),
    rds = list(package = "base", read_function = "readRDS"),
    stop("Unsupported file type: ", .extension)
  )

  require(.read_function$package, character.only = TRUE)

  if (is.null(.args)) {
    .args <- list(.output_file)
    names(.args) <- names(formals(.read_function$read_function)[1])
  } else {
    .args$file <- .output_file
  }

  .names_args <- names(.args)

  .metadata_args <- metadata_args()

  .names_args <- .names_args[!.names_args %in% .metadata_args]



  df <- do.call(.read_function$read_function, args = .args[.names_args])
  return(data.table::data.table(df))
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
            recipe_svy_type = .args$recipes[[x]]$survey_type
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


  Survey <- Survey$new(
    data = svy,
    edition = .args$svy_edition,
    type = .args$svy_type,
    psu = .args$svy_psu,
    engine = .engine_name,
    weight = .args$svy_weight,
    recipes = .args$recipes %||% NULL
  )

  if (.args$bake %||% FALSE) {
    return(bake_recipes(Survey))
  } else {
    return(Survey)
  }
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
