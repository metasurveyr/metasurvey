Survey <- R6Class("Survey",
  public = list(
    data = NULL,
    edition = NULL,
    type = NULL,
    default_engine = NULL,
    weight = NULL,
    steps = list(),
    recipes = NULL,
    workflows = list(),
    design = NULL,
    initialize = function(data, edition, type, engine, weight, design = NULL, steps = NULL, recipes = NULL) {
      self$data <- data
      self$edition <- edition
      self$type <- type
      self$default_engine <- engine
      self$weight <- validate_weight(data, weight)
      self$design <- survey::svydesign(
        id = ~1,
        weights = as.formula(paste("~", validate_weight(data, weight))),
        data = data,
        calibrate.formula = ~1
      )
      self$recipes <- recipes
      self$workflows <- list()
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
      data <- self$data
      self$weight <- validate_weight(data, weight)
    },
    print = function() {
      get_metadata(self)
    },
    add_step = function(step) {
      self$steps[[step$name]] <- step
      self$update_design()
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
      self$design <- survey::svydesign(
        id = ~1,
        weights = as.formula(paste("~", validate_weight(self$data, self$weight))),
        data = self$data
      )
    },
    bake_recipes = function() {
      if (length(self$recipes) > 0) {
        for (i in seq_along(self$recipes)) {
          self <- 1
        }
      }
    },
    active = list(
      design = function() {
        survey::svydesign(
          id = ~1,
          weights = as.formula(paste("~", self$weight)),
          data = self$data
        )
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

set_data <- function(svy, data, .copy = TRUE) {
  if (.copy) {
    clone <- svy$clone()
    clone$set_data(data)
    return(clone)
  } else {
    svy$set_data(data)
  }
}

set_edition <- function(svy, new_edition) {
  clone <- svy$clone()
  clone$set_edition(new_edition)
  return(clone)
}

set_type <- function(svy, new_type) {
  clone <- svy$clone()
  clone$set_type(new_type)
  return(clone)
}

set_weight <- function(svy, new_weight) {
  clone <- svy$clone()
  clone$set_weight(new_weight)
  return(clone)
}

#' @title get_metadata
#' @description Get metadata from survey
#' @keywords Surveymethods
#' @importFrom glue glue glue_col
#' @importFrom emoji emoji
#' @param self Object of class Survey
#' @export

get_metadata <- function(self) {
  message(
    glue::glue_col(
      "
            {emoji('information')}  {blue Type:} {type}
            {emoji('graph')} {blue Edition:} {edition}
            {emoji('desktop_computer')}  {blue Engine:} {default_engine}
            {emoji('abacus')} {blue Design:} {design}
            {emoji('mag')} {blue Steps:} {steps}
            {emoji('cupcake')} {blue Recipes:} {names_recipes}
            ",
      type = self$type,
      edition = self$edition,
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


#' @title cat_design
#' @description Cast design from survey
#' @keywords Surveymethods
#' @param self Object of class Survey
#' @export
#'
#'

cat_design <- function(self) {
  call <- self$design$call
  cluster <- deparse(call$id) %||% "None"
  strata <- deparse(call$strata) %||% "None"
  weight <- self$weight %||% "None"
  fpc <- deparse(call$fpc) %||% "None"
  calibrate.formula <- deparse(call$calibrate.formula) %||% "None"
  return(
    glue::glue_col(
      "\n
          * {green Type:} {design_type}
          * {green PSU:} {cluster}
          * {green Strata:} {strata}
          * {green Weight:} {weight}
          * {green FPC:} {fpc}
          * {green Calibrate formula:} {calibrate.formula}
      ",
      cluster = cluster,
      strata = strata,
      weight = weight,
      fpc = fpc,
      design_type = cat_design_type(self)
    )
  )
}


#' @title cat_design_type
#' @description Cast design type from survey
#' @keywords Surveymethods
#' @param self Object of class Survey
#' @export
#'
#'

cat_design_type <- function(self) {
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

  design_engine <- design_engine[[class(self$design)[1]]]

  if (is.null(design_engine)) {
    return("None")
  } else {
    return(
      glue::glue_col(
        "\n
              * {green Package:} {package}
              * {green Variance estimation:} {variance_estimation}
        ",
        package = design_engine$package,
        type = design_engine$type,
        variance_estimation = ifelse(
          is.list(design_engine$variance_estimation),
          design_engine$variance_estimation[self$design$type],
          design_engine$variance_estimation
        )
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
