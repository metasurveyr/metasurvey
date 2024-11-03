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
    get_implantation = function() {
      return(self$implantation)
    },
    get_follow_up = function(index = length(self$follow_up), monthly = NULL, quarterly = NULL, semiannual = NULL, annual = NULL) {
      return(self$follow_up[index])
    },
    get_type = function() {
      return(self$type)
    },
    get_default_engine = function() {
      return(self$default_engine)
    },
    get_steps = function() {
      return(self$steps)
    },
    get_recipes = function() {
      return(self$recipes)
    },
    get_workflows = function() {
      return(self$workflows)
    },
    get_design = function() {
      return(self$design)
    },
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

  if (is(RotativePanelSurvey, "RotativePanelSurvey")) {
    follow_up <- RotativePanelSurvey$follow_up
  } else {
    stop("The `RotativeSurvey` argument must be an object of class `RotativePanelSurvey`")
  }

  args <- c("index", "Monthly", "Annual", "Quarterly", "Biannual")
  arg_not_null_index <- c(!is.null(index), !is.null(monthly), !is.null(annual), !is.null(quarterly), !is.null(biannual))
  arg_not_null <- args[arg_not_null_index]

  if (!is.null(index)) {
    return(RotativePanelSurvey$get_follow_up(index))
  }


  if (arg_not_null == RotativePanelSurvey$periodicity$implantation) {
    return(RotativePanelSurvey$get_implantation())
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


  month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  if (!is.null(monthly)) {
    monthly_results <- apply_func(monthly, function(month) {
      indices <- apply_interval(ts_series, as.numeric(format(min(dates), "%Y")), month, as.numeric(format(max(dates), "%Y")), month)
      stats::setNames(list(follow_up[indices]), paste0("monthly_", month_names[month]))
    })
    results <- c(results, unlist(monthly_results, recursive = FALSE))
  }

  if (!is.null(annual)) {
    if (RotativePanelSurvey$implantation$periodicity != "Annual") {
      annual_results <- apply_func(annual, function(year) {
        indices <- apply_interval(ts_series, year, 1, year, 12)
        stats::setNames(list(follow_up[indices]), paste0("annual_", year))
      })
      results <- c(results, unlist(annual_results, recursive = FALSE))
    } else {
      return(RotativePanelSurvey$get_implantation())
    }
  }


  quarter_names <- c("Q1", "Q2", "Q3", "Q4")
  if (!is.null(quarterly)) {
    quarterly_results <- apply_func(quarterly, function(quarter) {
      start_month <- (quarter - 1) * 3 + 1
      end_month <- start_month + 2
      indices <- apply_interval(ts_series, as.numeric(format(min(dates), "%Y")), start_month, as.numeric(format(max(dates), "%Y")), end_month)
      stats::setNames(list(follow_up[indices]), paste0("quarterly_", quarter_names[quarter]))
    })
    results <- c(results, unlist(quarterly_results, recursive = FALSE))
  }


  biannual_names <- c("H1", "H2")
  if (!is.null(biannual)) {
    biannual_results <- apply_func(biannual, function(semester) {
      start_month <- ifelse(semester == 1, 1, 7)
      end_month <- ifelse(semester == 1, 6, 12)
      indices <- apply_interval(ts_series, as.numeric(format(min(dates), "%Y")), start_month, as.numeric(format(max(dates), "%Y")), end_month)
      stats::setNames(list(follow_up[indices]), paste0("biannual_", biannual_names[semester]))
    })
    results <- c(results, unlist(biannual_results, recursive = FALSE))
  }

  return(results)
}


PoolSurvey <- R6Class(
  "PoolSurvey",
  public = list(
    svy_type = NULL,
    svy_edition = NULL,
    surveys = NULL,
    initialize = function(surveys) {
      self$surveys <- surveys
      self$svy_type <- unique(sapply(surveys, function(s) s$type))
      self$svy_edition <- unique(sapply(surveys, function(s) s$edition))
    },
    get_surveys = function(index = length(self$surveys)) {
      return(self$surveys[index])
    },
    print = function() {
      get_metadata(self = self)
    },
    summary = function() {
      get_metadata(self = self)
    }
  )
)