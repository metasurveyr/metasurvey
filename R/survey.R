#' @title Survey Class
#' @description This class represents a survey object with various attributes and methods to manage survey data, design, steps, recipes, and workflows.
#' @field data The survey data.
#' @field edition The edition of the survey.
#' @field type The type´ of the survey.
#' @field periodicity The periodicity of the survey.
#' @field default_engine The default engine used for the survey.
#' @field weight The weight(s) associated with the survey.
#' @field steps A list of steps applied to the survey.
#' @field recipes A list of recipes associated with the survey.
#' @field workflows A list of workflows associated with the survey.
#' @field design The survey design object(s).
#' @field active A list of active bindings for dynamic properties of the survey.
#' @keywords Survey
#' @export
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

    #' @description Initialize a new Survey object.
    #' @param data The survey data.
    #' @param edition The edition of the survey.
    #' @param type The type of the survey.
    #' @param psu Primary sampling unit (PSU).
    #' @param engine The engine used for the survey.
    #' @param weight The weight(s) for the survey.
    #' @param design Optional survey design.
    #' @param steps Optional list of steps.
    #' @param recipes Optional list of recipes.
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
            data_merged <- merge(
              data,
              x$replicate_file,
              by.x = names(x$replicate_id),
              by.y = names(x$replicate_id),
              sort = TRUE # Conserva el orden original, si es necesario
            )

            all_rep_cols <- names(data_merged)[grepl(x$replicate_pattern, names(data_merged))]

            initial_rep_cols <- all_rep_cols[1:10]

            setDT(data_merged)

            design <- survey::svrepdesign(
              weights = as.formula(paste("~", x$weight)),
              data = data_merged[, c(x$weight, names(x$replicate_id), initial_rep_cols), with = FALSE],
              repweights = data_merged[, ..initial_rep_cols],
              type = x$replicate_type
            )

            design_full <- update(design, repweights = data_merged[, ..all_rep_cols])

            design_full$degf <- length(all_rep_cols) - 1
            design_full$repweights <- data_merged[, ..all_rep_cols]
            # design_full$scale <- 0.001001001
            design_full$scale <- 1 / length(all_rep_cols)
            design_full$rscales <- rep(1, length(all_rep_cols))

            return(design_full)
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

    #' @description Get the survey data.
    #' @return The survey data.
    get_data = function() {
      return(self$data)
    },

    #' @description Get the edition of the survey.
    #' @return The survey edition.
    get_edition = function() {
      return(self$edition)
    },

    #' @description Get the type of the survey.
    #' @return The survey type.
    get_type = function() {
      return(self$type)
    },

    #' @description Set the survey data.
    #' @param data The new survey data.
    set_data = function(data) {
      self$data <- data
    },

    #' @description Set the edition of the survey.
    #' @param edition The new survey edition.
    set_edition = function(edition) {
      self$edition <- edition
    },

    #' @description Set the type of the survey.
    #' @param type The new survey type.
    set_type = function(type) {
      self$type <- type
    },

    #' @description Set the weight(s) for the survey.
    #' @param weight The new weight(s).
    set_weight = function(weight) {
      message("Setting weight")
      data <- self$data
      weight_list <- validate_weight_time_pattern(data, weight)
      self$weight <- weight_list
    },

    #' @description Print metadata of the survey.
    print = function() {
      get_metadata(self)
    },

    #' @description Add a step to the survey.
    #' @param step The step to add.
    add_step = function(step) {
      name_index <- length(self$steps) + 1
      name_index <- paste0("step_", name_index, " ", step$name)
      step$name <- name_index
      self$steps[[name_index]] <- step
      self$update_design()
    },

    #' @description Add a recipe to the survey.
    #' @param recipe The recipe to add.
    #' @param bake Whether to bake the recipe immediately.
    add_recipe = function(recipe, bake = lazy_default()) {
      if ((self$edition != recipe$edition)) {
        stop("Invalid Recipe: \n", recipe$name, "\nEdition of survey: ", self$edition, "\nEdition of recipe: ", recipe$edition)
      }
      index_recipe <- length(self$recipes) + 1
      self$recipes[[index_recipe]] <- recipe
    },

    #' @description Add a workflow to the survey.
    #' @param workflow The workflow to add.
    add_workflow = function(workflow) {
      self$workflows[[workflow$name]] <- workflow
    },

    #' @description Bake all recipes in the survey.
    bake = function() {
      bake_recipes(self)
    },

    #' @description Display the first few rows of the survey data.
    head = function() {
      head(self$data)
    },

    #' @description Display the structure of the survey data.
    str = function() {
      str(self$data)
    },

    #' @description Set the survey design.
    #' @param design The new survey design.
    set_design = function(design) {
      self$design <- design
    },

    #' @description Update the survey design based on the current data and weights.
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
#' @description Convert a Survey object to a data.frame.
#' @param svy A Survey object.
#' @keywords Surveymethods
#' @export
#' @return A data.frame containing the survey data.
survey_to_data_frame <- function(svy) {
  data.frame(svy$get_data())
}

#' @title survey_to_tibble
#' @description Convert a Survey object to a tibble.
#' @param svy A Survey object.
#' @keywords Surveymethods
#' @export
#' @return A tibble containing the survey data.
survey_to_tibble <- function(svy) {
  tibble::as_tibble(svy$get_data())
}

#' @title survey_to_data.table
#' @description Convert a Survey object to a data.table.
#' @param svy A Survey object.
#' @keywords Surveymethods
#' @export
#' @importFrom data.table data.table
#' @return A data.table containing the survey data.
survey_to_data.table <- function(svy) {
  data.table::data.table(svy$get_data())
}

#' @title get_data
#' @description Retrieve the data from a Survey object.
#' @param svy A Survey object.
#' @keywords Surveymethods
#' @export
#' @return The survey data.
get_data <- function(svy) {
  svy$get_data()
}

#' @title get_edition
#' @description Retrieve the edition of a Survey object.
#' @param svy A Survey object.
#' @keywords Surveymethods
#' @export
#' @return The survey edition.
get_edition <- function(svy) {
  svy$get_edition()
}

#' @title get_weight
#' @description Retrieve the weight(s) of a Survey object.
#' @param svy A Survey object.
#' @keywords Surveymethods
#' @param estimation_type The type of estimation (default: all weights).
#' @export
#' @return The weight(s) of the survey.
get_weight <- function(svy, estimation_type = 1:length(svy$weight)) {
  svy$weight[[estimation_type]]
}

#' @title get_info_weight
#' @description Retrieve detailed information about the weights of a Survey object.
#' @param svy A Survey object.
#' @keywords Surveymethods
#' @export
#' @return A string with detailed weight information.
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

#' @title get_type
#' @description Retrieve the type of a Survey object.
#' @param svy A Survey object.
#' @keywords Surveymethods
#' @export
#' @return The survey type.
get_type <- function(svy) {
  svy$get_type()
}

#' @title set_data
#' @description Set new data for a Survey object.
#' @param svy A Survey object.
#' @param data The new data.
#' @param .copy Whether to return a copy of the Survey object (default: TRUE).
#' @keywords Surveymethods
#' @export
#' @return The updated Survey object.
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

#' @title set_edition
#' @description Set a new edition for a Survey object.
#' @param svy A Survey object.
#' @param new_edition The new edition.
#' @param .copy Whether to return a copy of the Survey object (default: TRUE).
#' @keywords Surveymethods
#' @export
#' @return The updated Survey object.
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

#' @title set_type
#' @description Set a new type for a Survey object.
#' @param svy A Survey object.
#' @param new_type The new type.
#' @param .copy Whether to return a copy of the Survey object (default: TRUE).
#' @keywords Surveymethods
#' @export
#' @return The updated Survey object.
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

#' @title set_weight
#' @description Set new weight(s) for a Survey object.
#' @param svy A Survey object.
#' @param new_weight The new weight(s).
#' @param .copy Whether to return a copy of the Survey object (default: TRUE).
#' @keywords Surveymethods
#' @export
#' @return The updated Survey object.
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
#' @description Retrieve metadata from a Survey object.
#' @param self A Survey object.
#' @keywords Surveymethods
#' @export
#' @importFrom glue glue glue_col
#' @return Metadata as a list.
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
#' @description Create an empty Survey object.
#' @param edition The edition of the survey.
#' @param type The type of the survey.
#' @param weight The weight(s) of the survey.
#' @param engine The engine of the survey.
#' @keywords Surveymethods
#' @export
#' @return An empty Survey object.
survey_empty <- function(edition = NULL, type = NULL, weight = NULL, engine = NULL) {
  Survey$new(
    data = NULL,
    edition = edition,
    type = type,
    weight = weight,
    engine = engine
  )
}

#' @title bake_recipes
#' @description Bake all recipes in a Survey object.
#' @param svy A Survey object with recipes to bake.
#' @keywords Surveymethods
#' @export
#' @return A Survey object with baked recipes.
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
