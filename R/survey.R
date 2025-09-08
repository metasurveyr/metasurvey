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
    recipes = list(),
    workflows = list(),
    design = NULL,
    initialize = function(data, edition, type, psu, engine, weight, design = NULL, steps = NULL, recipes = list()) {
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
            aux_vars <- c(x$weight, x$replicate_id)
            data_aux <- data[, aux_vars, with = FALSE]
            data_aux <- merge(
              data_aux,
              x$replicate_file[, 1:11],
              by.x = names(x$replicate_id),
              by.y = x$replicate_id
            )

            design <- survey::svrepdesign(
              id = psu,
              weights = as.formula(paste("~", x$weight)),
              data = data_aux,
              repweights = x$replicate_pattern,
              type = x$replicate_type
            )


            data <- merge(data, x$replicate_file, by.x = names(x$replicate_id), by.y = x$replicate_id)
            design$variables <- data
            vars <- grepl(x$replicate_pattern, names(data))
            rep_weights <- data[, vars, with = FALSE]
            design$repweights <- data.table(rep_weights)
            return(design)
          }
        }
      )

      names(design_list) <- names(weight_list)

      if (length(recipes) == 1) {
        recipes <- list(recipes)
      }

      if (length(recipes) == 1) {
        recipes <- list(recipes)
      }

      self$edition <- time_pattern$svy_edition
      self$type <- time_pattern$svy_type
      self$default_engine <- engine
      self$weight <- weight_list
      self$design <- design_list
      self$recipes <- if (is.null(recipes)) list() else recipes
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
      name_index <- length(self$steps) + 1
      name_index <- paste0("step_", name_index, " ", step$name)
      step$name <- name_index
      self$steps[[name_index]] <- step
      self$update_design()
    },
    add_recipe = function(recipe, bake = lazy_default()) {
      if ((self$edition != recipe$edition)) {
        stop("Invalid Recipe: \n", recipe$name, "\nEdition of survey: ", self$edition, "\nEdition of recipe: ", recipe$edition)
      }
      index_recipe <- length(self$recipes) + 1
      self$recipes[[index_recipe]] <- recipe
    },
    add_workflow = function(workflow) {
      self$workflows[[workflow$name]] <- workflow
    },
    bake = function() {
      bake_recipes(self)
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

      data_now <- self$get_data()

      for (i in seq_along(weight_list)) {
        if (is.character(weight_list[[i]])) {
          self$design[[i]]$variables <- self$data
        } else {
          temp <- self$get_data()
          self$design[[i]]$variables <- merge(
            data_now,
            weight_list[[i]]$replicate_file,
            by.x = names(weight_list[[i]]$replicate_id),
            by.y = weight_list[[i]]$replicate_id
          )
        }
      }
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

get_weight <- function(svy, estimation_type = 1:length(svy$weight)) {
  svy$weight[[estimation_type]]
}

get_info_weight <- function(svy) {
  info_weight <- c("")

  for (i in seq_along(svy$weight)) {
    if (is.character(svy$weight[[i]]) == 1) {
      info_weight[i] <- glue::glue(
        "Weight {names(svy$weight)[[i]]}: {svy$weight[[i]]} (Simple design)"
      )
    } else {
      info_weight[i] <- glue::glue(
        "

         {names(svy$weight)[[i]]}: {svy$weight[[i]]$weight} (Replicate design)
         Type: {svy$weight[[i]]$replicate_type}
         Pattern: {svy$weight[[i]]$replicate_pattern}
         Replicate file: {basename(svy$weight[[i]]$replicate_path)} with {ncol(svy$weight[[i]]$replicate_file) - 1} replicates"
      )
    }
  }

  glue::glue(
    Reduce(
      f = function(x, y) {
        paste0(x, "\n", y)
      },
      x = info_weight
    )
  )
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
        steps = sub(
          "\n$",
          "",
          paste0(
            if (length(self$get_steps()$implantation) > 0) {
              paste0("implantation: (", paste(names(self$get_steps()$implantation), collapse = ", "), ")\n")
            } else {
              ""
            },
            if (length(self$get_steps()$follow_up) > 0 && !is.null(row.names(self$get_steps()$follow_up))) {
              paste0("follow_up: (", paste(row.names(self$get_steps()$follow_up), collapse = ", "), ")\n")
            } else {
              ""
            }
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

  if (is(self, "PoolSurvey")) {
    message(
      glue::glue_col(
        "
            {blue Type:} {type}
            {blue Periodicity:} {red Periodicity of pool} {periodicity} {red each survey} {periodicity_each}
            {blue Steps:} {steps}
            {blue Recipes:} {names_recipes}
            {blue Group:} {groups}
            ",
        type = toupper(
          unique(
            sapply(
              self$surveys[[1]],
              function(x) x[[1]]$type
            )
          )
        ),
        steps = unique(
          sapply(
            self$surveys[[1]],
            function(x) {
              ifelse(
                length(x[[1]]$steps) == 0,
                "None",
                paste0(
                  "\n  - ",
                  paste0(names(x[[1]]$steps), collapse = "\n  - ")
                )
              )
            }
          )
        ),
        periodicity = names(self$surveys),
        periodicity_each = tolower(
          unique(sapply(
            self$surveys[[1]],
            function(x) x[[1]]$periodicity
          ))
        ),
        names_recipes = unique(
          sapply(
            self$surveys[[1]],
            function(x) cat_recipes(x[[1]])
          )
        ),
        groups = Reduce(
          f = function(x, y) {
            paste0(x, ", ", y)
          },
          names(self$surveys[[1]])
        ),
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

#' Display survey design information
#'
#' Pretty-prints the sampling design configuration for each estimation type
#' in a Survey object, showing PSU, strata, weights, and other design elements
#' in a color-coded, readable format.
#'
#' @param self Survey object containing design information
#'
#' @return Invisibly returns NULL; called for side effect of printing design info
#'
#' @details
#' This function displays design information including:
#' \itemize{
#'   \item Primary Sampling Units (PSU/clusters)
#'   \item Stratification variables
#'   \item Weight variables for each estimation type
#'   \item Finite Population Correction (FPC) if used
#'   \item Calibration formulas if applied
#'   \item Overall design type classification
#' }
#'
#' Output is color-coded for better readability in supporting terminals.
#'
#' @examples
#' \dontrun{
#' # Display design for survey with multiple estimation types
#' ech_survey <- load_survey("ech_2023.dta",
#'   svy_type = "ech",
#'   svy_edition = "2023"
#' )
#' cat_design(ech_survey)
#' }
#'
#' @seealso \code{\link{cat_design_type}} for design type classification
#' @keywords Surveymethods
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
  if (is.null(self$recipes) || length(self$recipes) == 0) {
    return("None")
  }

  n_recipes <- get_distinct_recipes(self$recipes)

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
                    * {green Bake:} {self$recipes[[x]]$bake}
                ",
          doi = ifelse(
            is.null(self$recipes[[x]]$DOI),
            "None",
            self$recipes[[x]]$DOI
          )
        )
      }
    ),
    init = ""
  )

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
#' @keywords Surveymethods
#' @export
#' @return Survey object
bake_recipes <- function(svy) {
  recipes <- svy$recipes

  if (length(recipes) == 0) {
    return(svy)
  }

  if (is(recipes, "Recipe")) {
    recipes <- list(recipes)
  }

  for (i in seq_along(recipes)) {
    recipe <- recipes[[i]]
    expr <- as.name("svy")

    for (step in seq_along(recipe$steps)) {
      step_call <- recipe$steps[[step]]
      expr <- call("%>%", expr, step_call)
    }
    svy <- eval(expr)
  }

  svy_after <- svy$clone(deep = TRUE)
  svy_after$recipes <- lapply(
    X = seq_along(recipes),
    FUN = function(x) {
      recipes[[x]]$clone()
    }
  )

  for (i in seq_along(recipes)) {
    svy_after$recipes[[i]]$bake <- TRUE
  }

  return(svy_after)
}
