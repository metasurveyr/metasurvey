Survey <- R6Class(
  "Survey",
  public = list(
    data = NULL,
    edition = NULL,
    type = NULL,
    periodicity = NULL,
    default_engine = NULL,
    weight = NULL,
    steps = list(),
    recipes = NULL,
    workflows = list(),
    design = NULL,
    initialize = function(data, edition, type, psu, engine, weight, design = NULL, steps = NULL, recipes = NULL) {
      self$data <- data

      time_pattern <- validate_time_pattern(
        svy_type = type,
        svy_edition = edition
      )

      weight_list <- validate_weight_time_pattern(data, weight)

      design_list <- lapply(
        weight_list,
        function(x) {
          if (is.character(x)) {
            if (is.null(psu)) {
              psu <- ~1
            } else {
              psu <- as.formula(paste("~", psu))
            }

            survey::svydesign(
              id = psu,
              weights = as.formula(paste("~", x)),
              data = data,
              calibrate.formula = ~1
            )
          } else {
            survey::svrepdesign(
              id = psu,
              weights = as.formula(paste("~", x$weight)),
              data = merge(data, x$replicate_file, by.x = names(x$replicate_id), by.y = x$replicate_id),
              repweights = x$replicate_pattern,
              type = x$replicate_type
            )
          }
        }
      )
      names(design_list) <- names(weight_list)

      self$edition <- time_pattern$svy_edition
      self$type <- time_pattern$svy_type
      self$default_engine <- engine
      self$weight <- weight_list
      self$design <- design_list
      self$recipes <- recipes
      self$workflows <- list()
      self$periodicity <- time_pattern$svy_periodicity
    },
    get_data = function() {
      return(self$data)
    },
    get_edition = function() {
      return(self$edition)
    },
    get_type = function() {
      return(self$type)
    },
    set_data = function(data) {
      self$data <- data
    },
    set_edition = function(edition) {
      self$edition <- edition
    },
    set_type = function(type) {
      self$type <- type
    },
    set_weight = function(weight) {
      message("Setting weight")
      data <- self$data
      weight_list <- validate_weight_time_pattern(data, weight)
      self$weight <- weight_list
    },
    print = function() {
      get_metadata(self)
    },
    add_step = function(step) {
      self$steps[[step$name]] <- step
      self$update_design()
    },
    add_recipe = function(recipe, bake = lazy_default()) {
      self$recipes <- recipe
    },
    add_workflow = function(workflow) {
      self$workflows[[workflow$name]] <- workflow
    },
    bake = function() {
      bake_recipes(self, self$recipes)
    },
    head = function() {
      head(self$data)
    },
    str = function() {
      str(self$data)
    },
    set_design = function(design) {
      self$design <- design
    },
    update_design = function() {
      weight_list <- self$weight

      design_list <- lapply(
        weight_list,
        function(x) {
          if (is.character(x)) {
            self$design[[1]]$variables <- self$data
          } else {
            self$design[[1]]$variables <- merge(
              self$data,
              x$replicate_file,
              by.x = names(x$replicate_id),
              by.y = x$replicate_id
            )
          }
        }
      )
    },
    active = list(
      design = function() {
        weight_list <- self$weight

        design_list <- lapply(
          weight_list,
          function(x) {
            if (is.character(x)) {
              survey::svydesign(
                id = ~1,
                weights = as.formula(paste("~", x)),
                data = self$data,
                calibrate.formula = ~1
              )
            } else {
              survey::svrepdesign(
                id = ~1,
                weights = as.formula(paste("~", x$weight)),
                data = merge(self$data, x$replicate_file, by.x = names(x$replicate_id), by.y = x$replicate_id),
                repweights = x$replicate_pattern,
                type = x$replicate_type
              )
            }
          }
        )

        names(design_list) <- names(weight_list)

        return(design_list)
      }
    )
  )
)


#' @title survey_to_data_frame
#' @description Convert survey to data.frame
#' @keywords Surveymethods
#' @param svy Survey object

#' @export
#' @return data.frame
survey_to_data_frame <- function(svy) {
  data.frame(svy$get_data())
}

#' @title survey_to_tibble
#' @keywords Surveymethods
#' @description Convert survey to tibble
#' @param svy Survey object
#' @export
#' @return tibble


survey_to_tibble <- function(svy) {
  tibble::as_tibble(svy$get_data())
}

#' @title survey_to_data.table
#' @keywords Surveymethods
#' @description Convert survey to data.table
#' @param svy Survey object
#' @export

#' @importFrom data.table data.table
#' @return data.table
#'

survey_to_data.table <- function(svy) {
  data.table::data.table(svy$get_data())
}

#' @title get_data
#' @description Get data from survey
#' @param svy Survey object
#' @keywords Surveymethods
#' @export
#' @return Data
#'

get_data <- function(svy) {
  svy$get_data()
}

get_edition <- function(svy) {
  svy$get_edition()
}

get_weight <- function(svy) {
  svy$weight
}

get_type <- function(svy) {
  svy$get_type()
}

get_design <- function(self) {
  self$active$design()
}

set_data <- function(svy, data, .copy = use_copy_default()) {
  if (.copy) {
    clone <- svy$clone()
    clone$set_data(data)
    return(clone)
  } else {
    svy$set_data(data)
    return(svy)
  }
}

set_edition <- function(svy, new_edition, .copy = use_copy_default()) {
  if (.copy) {
    clone <- svy$clone()
    clone$set_edition(new_edition)
    return(clone)
  } else {
    svy$set_edition(new_edition)
    return(svy)
  }
}

set_type <- function(svy, new_type, .copy = use_copy_default()) {
  if (.copy) {
    clone <- svy$clone()
    clone$set_type(new_type)
    return(clone)
  } else {
    svy$set_type(new_type)
    return(svy)
  }
}

set_weight <- function(svy, new_weight, .copy = use_copy_default()) {
  if (.copy) {
    clone <- svy$clone()
    clone$set_weight(new_weight)
    return(clone)
  } else {
    if (svy$weight == new_weight) {
      return(svy)
    }

    svy$set_weight(new_weight)
    return(svy)
  }
}

#' @title get_metadata
#' @description Get metadata from survey
#' @keywords Surveymethods
#' @importFrom glue glue glue_col
#' @param self Object of class Survey
#' @export

get_metadata <- function(self) {
  
  
  if (is(self, "Survey")) {
    message(
      glue::glue_col(
        "
            {blue Type:} {type}
            {blue Edition:} {edition}
            {blue Periodicity:} {periodicity}
            {blue Engine:} {default_engine}
            {blue Design:} {design}
            {blue Steps:} {steps}
            {blue Recipes:} {names_recipes}
            ",
        type = toupper(self$type),
        edition = ifelse(
          is(self$edition, "character") || is(self$edition, "numeric"),
          self$edition,
          format(self$edition, "%Y-%m")
        ),
        default_engine = self$default_engine,
        design = cat_design(self),
        steps = ifelse(
          length(self$steps) == 0,
          "None",
          paste0(
            "\n  - ",
            paste0(names(self$steps), collapse = "\n  - ")
          )
        ),
        periodicity = self$periodicity,
        names_recipes = cat_recipes(self),
        .literal = TRUE
      )
    )

    invisible(
      list(
        type = self$type,
        edition = self$edition,
        default_engine = self$default_engine,
        weight = self$weight,
        steps = names(self$steps)
      )
    )
  }

  if (is(self, "RotativePanelSurvey")) {
    message(
      glue::glue_col(
        "
            {blue Type:} {type} (Rotative Panel)
            {blue Edition:} {edition}
            {blue Periodicity:} Implantation: {implantationPeriodicity}, Follow-up: {follow_upPeriodicity}
            {blue Engine:} {default_engine}
            {blue Design:} {design}
            {blue Steps:} {steps}
            {blue Recipes:} {names_recipes}
            ",
        type = toupper(self$type),
        edition = ifelse(
          is(self$implantation$edition, "character") || is(self$implantation$edition, "numeric"),
          self$implantation$edition,
          format(self$implantation$edition, "%Y-%m")
        ),
        default_engine = self$default_engine,
        design = cat_design(self),
        steps = ifelse(
          length(self$steps) == 0,
          "None",
          paste0(
            "\n  - ",
            paste0(names(self$steps), collapse = "\n  - ")
          )
        ),
        implantationPeriodicity = self$periodicity$implantation,
        follow_upPeriodicity = self$periodicity$follow_up,
        names_recipes = cat_recipes(self),
        .literal = TRUE
      )
    )

    invisible(
      list(
        type = self$type,
        edition = self$edition,
        default_engine = self$default_engine,
        weight = self$weight,
        steps = names(self$steps)
      )
    )
  }

  
  
}


#' @title cat_design
#' @description Cast design from survey
#' @keywords Surveymethods
#' @param self Object of class Survey
#' @export
#'
#'

cat_design <- function(self) {
  design_list <- self$design


  green <- "\033[32m"
  reset <- "\033[39m"
  red <- "\033[31m"

  output_list <- sapply(
    names(design_list),
    function(x) {
      call <- design_list[[x]]$call
      cluster <- deparse(call$id) %||% "None"
      strata <- deparse(call$strata) %||% "None"
      weight <- ifelse(
        is.character(self$weight[[x]]),
        self$weight[[x]] %||% "None",
        self$weight[[x]]$weight %||% "None"
      )
      fpc <- deparse(call$fpc) %||% "None"
      calibrate.formula <- deparse(call$calibrate.formula) %||% "None"
      design_type <- cat_design_type(self, x)

      text <- paste0(
        "\n* ", red, paste(toupper(x), "ESTIMATION"), reset, "\n",
        "        ", design_type, "\n",
        "  * ", green, "PSU:", reset, " ", cluster, "\n",
        "  * ", green, "Strata:", reset, " ", strata, "\n",
        "  * ", green, "Weight:", reset, " ", weight, "\n",
        "  * ", green, "FPC:", reset, " ", fpc, "\n",
        "  * ", green, "Calibrate formula:", reset, " ", calibrate.formula
      )

      return(paste("\n  ", text))
    }
  )

  output <- glue::glue_col(paste0(output_list, collapse = ""))

  return(output)
}







#' @title cat_design_type
#' @description Cast design type from survey
#' @keywords Surveymethods
#' @param self Object of class Survey
#' @param design_name Name of design
#' @export
#'
#'

cat_design_type <- function(self, design_name) {
  design_engine <- list(
    "survey.design2" = list(
      package = "survey",
      type = "Weighted design",
      variance_estimation = "Ultimate cluster"
    ),
    "svyrep.design" = list(
      package = "survey",
      type = "Replicated design",
      variance_estimation = list(
        "Jkn" = "Jackknife",
        "bootstrap" = "Bootstrap"
      )
    )
  )

  # Obtener la clase del diseño específico
  design_class <- class(self$design[[design_name]])[1]
  design_details <- design_engine[[design_class]]

  if (is.null(design_details)) {
    return("None")
  } else {
    # Determinar el método de estimación de varianza
    variance_estimation <- ifelse(
      is.list(design_details$variance_estimation),
      design_details$variance_estimation[[self$design[[design_name]]$type]],
      design_details$variance_estimation
    )
    return(
      glue::glue_col(
        "\n
        * {green Package:} {package}
        * {green Variance estimation:} {variance_estimation}",
        package = design_details$package,
        variance_estimation = variance_estimation
      )
    )
  }
}


#' @title cat_recipes
#' @description Cast recipes from survey
#' @keywords Surveymethods
#' @param self Object of class Survey
#' @export
#'

cat_recipes <- function(self) {
  if (is.null(self$recipes)) {
    return("None")
  }

  n_recipes <- get_distinct_recipes(self$recipes)

  if (n_recipes > 1) {
    string_print <- Reduce(
      f = function(x, y) {
        paste0(x, "\n  - ", y)
      },
      x = sapply(
        X = 1:n_recipes,
        FUN = function(x) {
          glue::glue_col(
            " {green Name:} {self$recipes[[x]]$name}
                    * {green User:} {self$recipes[[x]]$user}
                    * {green Id:} {self$recipes[[x]]$id}
                    * {green DOI:} {doi}
                ",
            doi = ifelse(
              is.null(self$recipes[[x]]$DOI),
              "None",
              self$recipes[[x]]$DOI
            )
          )
        }
      ),
      init = crayon::bgRed("Without bake")
    )
  } else {
    string_print <- glue::glue_col(
      " {green Name:} {self$recipes$name}
                    * {green User:} {self$recipes$user}
                    * {green Id:} {self$recipes$id}
                    * {green DOI:} {doi}
                ",
      doi = ifelse(
        is.null(self$recipes$DOI),
        "None",
        self$recipes$DOI
      )
    )
  }

  return(string_print)
}

#' @title get_steps
#' @description Get steps from survey
#' @param svy Survey object
#' @keywords Survey methods
#' @keywords Steps
#' @export
#' @return List

get_steps <- function(svy) {
  svy$steps
}

#' @title survey_empty
#' @description Create an empty survey
#' @keywords Surveymethods
#' @param edition Edition of survey
#' @param type Type of survey
#' @param weight Weight of survey
#' @param engine Engine of survey
#' @export

#' @return Survey object
#'

survey_empty <- function(edition = NULL, type = NULL, weight = NULL, engine = NULL) {
  Survey$new(
    data = NULL,
    edition = edition,
    type = type,
    weight = weight,
    engine = engine
  )
}


#' Bake recipes
#' @param svy Survey object
#' @param recipes List of recipes
#' @keywords Surveymethods
#' @export
#' @return Survey object
bake_recipes <- function(svy, recipes) {
  if (length(recipes) == 0) {
    return(svy)
  }

  for (i in seq_along(recipes)) {
    svy <- recipes[[i]]$bake(svy)
  }

  return(svy)
}
