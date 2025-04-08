#' @title RotativePanelSurvey Class
#' @description This class represents a rotative panel survey, which includes implantation and follow-up surveys.
#' It provides methods to access and manipulate survey data, steps, recipes, workflows, and designs.
#' @field implantation A survey object representing the implantation survey.
#' @field follow_up A list of survey objects representing the follow-up surveys.
#' @field type A string indicating the type of the survey.
#' @field default_engine A string specifying the default engine used for processing.
#' @field steps A list of steps applied to the survey.
#' @field recipes A list of recipes associated with the survey.
#' @field workflows A list of workflows associated with the survey.
#' @field design A design object for the survey.
#' @field periodicity A list containing the periodicity of the implantation and follow-up surveys.
#' @keywords Surveymethods, RotativePanelSurvey
#' @export
RotativePanelSurvey <- R6Class(
  "RotativePanelSurvey",
  public = list(
    implantation = NULL,
    follow_up = NULL,
    type = NULL,
    default_engine = NULL,
    steps = NULL,
    recipes = NULL,
    workflows = NULL,
    design = NULL,
    periodicity = NULL,

    #' @description Initializes a new instance of the RotativePanelSurvey class.
    #' @param implantation A survey object representing the implantation survey.
    #' @param follow_up A list of survey objects representing the follow-up surveys.
    #' @param type A string indicating the type of the survey.
    #' @param default_engine A string specifying the default engine used for processing.
    #' @param steps A list of steps applied to the survey.
    #' @param recipes A list of recipes associated with the survey.
    #' @param workflows A list of workflows associated with the survey.
    #' @param design A design object for the survey.
    initialize = function(implantation, follow_up, type, default_engine, steps, recipes, workflows, design) {
      self$implantation <- implantation
      self$follow_up <- follow_up
      self$type <- type
      self$default_engine <- default_engine
      self$steps <- steps
      self$recipes <- recipes
      self$workflows <- workflows
      self$design <- design

      follow_up_types <- sapply(self$follow_up, function(f) f$periodicity)

      if (length(unique(follow_up_types)) > 1) {
        stop("All follow-up surveys must have the same type")
      }

      self$periodicity <- list(
        implantation = self$implantation$periodicity,
        follow_up = unique(follow_up_types)
      )
    },

    #' @description Retrieves the implantation survey.
    #' @return A survey object representing the implantation survey.
    get_implantation = function() {
      return(self$implantation)
    },

    #' @description Retrieves the follow-up surveys.
    #' @param index An integer specifying the index of the follow-up survey to retrieve.
    #' @param monthly A vector of integers specifying monthly intervals.
    #' @param quarterly A vector of integers specifying quarterly intervals.
    #' @param semiannual A vector of integers specifying semiannual intervals.
    #' @param annual A vector of integers specifying annual intervals.
    #' @return A list of follow-up surveys matching the specified criteria.
    get_follow_up = function(index = length(self$follow_up), monthly = NULL, quarterly = NULL, semiannual = NULL, annual = NULL) {
      return(self$follow_up[index])
    },

    #' @description Retrieves the type of the survey.
    #' @return A string indicating the type of the survey.
    get_type = function() {
      return(self$type)
    },

    #' @description Retrieves the default engine used for processing.
    #' @return A string specifying the default engine.
    get_default_engine = function() {
      return(self$default_engine)
    },

    #' @description Retrieves the steps applied to the survey.
    #' @return A list containing the steps for the implantation and follow-up surveys.
    get_steps = function() {
      steps_implantation <- self$implantation$steps
      steps_follow_up <- sapply(self$follow_up, function(f) f$steps)

      return(list(
        implantation = steps_implantation,
        follow_up = steps_follow_up
      ))
    },

    #' @description Retrieves the recipes associated with the survey.
    #' @return A list of recipes.
    get_recipes = function() {
      return(self$recipes)
    },

    #' @description Retrieves the workflows associated with the survey.
    #' @return A list of workflows.
    get_workflows = function() {
      return(self$workflows)
    },

    #' @description Retrieves the design object for the survey.
    #' @return A design object.
    get_design = function() {
      return(self$design)
    },

    #' @description Prints metadata about the RotativePanelSurvey object.
    print = function() {
      get_metadata(self = self)
    }
  )
)

#' Extract surveys
#' @param RotativePanelSurvey A RotativePanelSurvey object
#' @param index An integer
#' @param monthly A vector of integers
#' @param annual A vector of integers
#' @param quarterly A vector of integers
#' @param biannual A vector of integers
#' @param use.parallel A logical
#' @return A list of surveys
#' @keywords Surveymethods
#' @keywords RotativePanelSurvey
#' @export

extract_surveys <- function(RotativePanelSurvey, index = NULL, monthly = NULL, annual = NULL, quarterly = NULL, biannual = NULL, use.parallel = FALSE) {
  if (is.null(monthly) && is.null(annual) && is.null(quarterly) && is.null(biannual) && is.null(index)) {
    warning("At least one interval argument must be different from NULL. Returning the implantation survey.")
    annual <- 1
  }

  if (!inherits(RotativePanelSurvey, "RotativePanelSurvey")) {
    stop("The `RotativeSurvey` argument must be an object of class `RotativePanelSurvey`")
  }

  follow_up <- RotativePanelSurvey$follow_up

  if (!is.null(index)) {
    if (length(index) > 1) {
      return(follow_up[index])
    }
    return(follow_up[[index]])
  }

  dates <- as.Date(sapply(unname(follow_up), function(x) x$edition))

  ts_series <- stats::ts(1:length(follow_up), start = c(as.numeric(format(min(dates), "%Y")), as.numeric(format(min(dates), "%m"))), frequency = 12)

  apply_interval <- function(ts_series, start_year, start_month, end_year, end_month) {
    as.vector(stats::window(ts_series, start = c(start_year, start_month), end = c(end_year, end_month)))
  }

  apply_func <- if (use.parallel) {
    requireNamespace("parallel")
    parallel::mclapply
  } else {
    base::lapply
  }

  results <- list()

  # Crear solo los intervalos especificados y no dejar listas vacías
  month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  if (!is.null(monthly)) {
    results$monthly <- list()
    for (month in monthly) {
      indices <- apply_interval(ts_series, as.numeric(format(min(dates), "%Y")), month, as.numeric(format(max(dates), "%Y")), month)
      results$monthly[[month_names[month]]] <- follow_up[indices]
    }
  }

  if (!is.null(annual)) {
    results$annual <- list()
    for (year in annual) {
      indices <- apply_interval(ts_series, year, 1, year, 12)
      results$annual[[as.character(year)]] <- follow_up[indices]
    }
  }

  quarter_names <- c("Q1", "Q2", "Q3", "Q4")
  if (!is.null(quarterly)) {
    results$quarterly <- list()
    for (quarter in quarterly) {
      start_month <- (quarter - 1) * 3 + 1
      end_month <- start_month + 2
      indices <- apply_interval(ts_series, as.numeric(format(min(dates), "%Y")), start_month, as.numeric(format(max(dates), "%Y")), end_month)
      results$quarterly[[quarter_names[quarter]]] <- follow_up[indices]
    }
  }

  biannual_names <- c("H1", "H2")
  if (!is.null(biannual)) {
    results$biannual <- list()
    for (semester in biannual) {
      start_month <- ifelse(semester == 1, 1, 7)
      end_month <- ifelse(semester == 1, 6, 12)
      indices <- apply_interval(ts_series, as.numeric(format(min(dates), "%Y")), start_month, as.numeric(format(max(dates), "%Y")), end_month)
      results$biannual[[biannual_names[semester]]] <- follow_up[indices]
    }
  }

  return(PoolSurvey$new(results))
}

#' @title PoolSurvey Class
#' @description This class represents a collection of surveys grouped by specific periods (e.g., monthly, quarterly, annual).
#' It provides methods to access and manipulate the grouped surveys.
#' @field surveys A list containing the grouped surveys.
#' @keywords Surveymethods, PoolSurvey
#' @export
PoolSurvey <- R6Class(
  "PoolSurvey",
  public = list(
    surveys = NULL,

    #' @description Initializes a new instance of the PoolSurvey class.
    #' @param surveys A list containing the grouped surveys.
    initialize = function(surveys) {
      self$surveys <- surveys
    },

    #' @description Retrieves surveys for a specific period.
    #' @param period A string specifying the period to retrieve (e.g., "monthly", "quarterly").
    #' @return A list of surveys for the specified period.
    get_surveys = function(period = NULL) {
      if (!is.null(period)) {
        return(self$surveys[[1]][[period]])
      } else {
        return(self$surveys)
      }
    },

    #' @description Prints metadata about the PoolSurvey object.
    print = function() {
      get_metadata(self = self)
    }
  )
)

#' get_implantation
#' @param RotativePanelSurvey A RotativePanelSurvey object
#' @return A list
#' @keywords Surveymethods
#' @keywords RotativePanelSurvey
#' @export

get_implantation <- function(RotativePanelSurvey) {
  if (!inherits(RotativePanelSurvey, "RotativePanelSurvey")) {
    stop("The `RotativeSurvey` argument must be an object of class `RotativePanelSurvey`")
  }

  return(RotativePanelSurvey$implantation)
}

#' get_follow_up
#' @param RotativePanelSurvey A RotativePanelSurvey object
#' @param index An integer
#' @return A list
#' @keywords Surveymethods
#' @keywords RotativePanelSurvey
#' @export


get_follow_up <- function(RotativePanelSurvey, index = 1:length(RotativePanelSurvey$follow_up)) {
  if (!inherits(RotativePanelSurvey, "RotativePanelSurvey")) {
    stop("The `RotativeSurvey` argument must be an object of class `RotativePanelSurvey`")
  }

  return(RotativePanelSurvey$follow_up[index])
}
