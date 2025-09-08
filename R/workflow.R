#' Execute estimation workflow for surveys
#'
#' This function executes a sequence of statistical estimations on Survey
#' objects, applying functions from the R survey package with appropriate
#' metadata. Automatically handles different survey types and periodicities.
#'
#' @param survey Survey object, list of Survey objects, or PoolSurvey. Must
#'   contain properly configured sample design
#' @param ... Calls to survey package functions (such as \code{svymean},
#'   \code{svytotal}, \code{svyratio}, etc.) that will be executed sequentially
#' @param estimation_type Type of estimation that determines which weight to use.
#'   Options: "monthly", "quarterly", "annual", or vector with multiple types
#'
#' @return \code{data.table} with results from all estimations, including columns:
#'   \itemize{
#'     \item \code{stat}: Name of estimated statistic
#'     \item \code{value}: Estimation value
#'     \item \code{se}: Standard error
#'     \item \code{cv}: Coefficient of variation
#'     \item \code{estimation_type}: Type of estimation used
#'     \item \code{survey_edition}: Survey edition
#'     \item Other columns depending on estimation type
#'   }
#'
#' @details
#' The function automatically selects the appropriate sample design according
#' to the specified \code{estimation_type}. For each Survey in the input list,
#' it executes all functions specified in \code{...} and combines the results.
#'
#' Supported estimation types:
#' \itemize{
#'   \item "monthly": Monthly estimations
#'   \item "quarterly": Quarterly estimations
#'   \item "annual": Annual estimations
#' }
#'
#' For PoolSurvey objects, it uses a specialized methodology that handles
#' pooling of multiple surveys.
#'
#' @examples
#' \dontrun{
#' # Basic estimations
#' result <- workflow(
#'   survey = list(ech_2023),
#'   svymean(~unemployed, na.rm = TRUE),
#'   svytotal(~active_population, na.rm = TRUE),
#'   estimation_type = "annual"
#' )
#'
#' # Multiple periods
#' result_multiple <- workflow(
#'   survey = list(ech_jan, ech_feb, ech_mar),
#'   svymean(~labor_income, na.rm = TRUE),
#'   svyratio(~total_income, ~persons, na.rm = TRUE),
#'   estimation_type = "monthly"
#' )
#'
#' # Domain estimations
#' result_domains <- workflow(
#'   survey = list(ech_2023),
#'   svyby(~unemployed, ~region_4, svymean, na.rm = TRUE),
#'   estimation_type = "annual"
#' )
#'
#' # Multiple estimation types
#' result_complete <- workflow(
#'   survey = list(ech_quarter),
#'   svymean(~activity_rate, na.rm = TRUE),
#'   estimation_type = c("monthly", "quarterly")
#' )
#' }
#'
#' @seealso
#' \code{\link[survey]{svymean}} for population means
#' \code{\link[survey]{svytotal}} for population totals
#' \code{\link[survey]{svyratio}} for ratios
#' \code{\link[survey]{svyby}} for domain estimations
#' \code{\link{PoolSurvey}} for survey pooling
#'
#' @keywords Surveymethods
#' @export
#' @return Data

workflow <- function(survey, ..., estimation_type = "monthly") {
  if (is(survey, "PoolSurvey")) {
    return(workflow_pool(survey, ..., estimation_type = estimation_type))
  } else {
    return(workflow_default(survey, ..., estimation_type = estimation_type))
  }
}


#' @title Workflow default
#' @description Workflow default
#' @keywords Surveymethods
#' @param survey Survey object
#' @param ... Calls
#' @param estimation_type Estimation type
#' @importFrom data.table rbindlist
#' @keywords internal
#' @noRd

workflow_default <- function(survey, ..., estimation_type = "monthly") {
  .calls <- substitute(list(...))

  result <- rbindlist(
    lapply(
      estimation_type,
      function(x) {
        rbindlist(
          lapply(
            X = seq_along(survey),
            function(i) {
              survey <- survey[[i]]

              partial_result <- rbindlist(
                lapply(
                  2:length(.calls),
                  function(i) {
                    call <- as.list(.calls[[i]])
                    name_function <- deparse(call[[1]])
                    call[["design"]] <- substitute(design)
                    call <- as.call(call)
                    estimation <- eval(call, envir = list(design = survey$design[[x]]))

                    return(cat_estimation(estimation, name_function))
                  }
                )
              )
              return(partial_result)
            }
          )
        )
      }
    )
  )

  return(result)
}

#' @title Workflow pool
#' @description Workflow pool
#' @keywords Surveymethods
#' @param survey Pool Survey object
#' @param ... Calls
#' @param estimation_type Estimation type
#' @importFrom data.table rbindlist
#' @keywords internal
#' @noRd


workflow_pool <- function(survey, ..., estimation_type = "monthly") {
  if (grepl(":", estimation_type)) {
    estimation_type_first <- strsplit(estimation_type, ":")[[1]][1]
    estimation_type <- strsplit(estimation_type, ":")[[1]][2]
  } else {
    estimation_type <- estimation_type
    estimation_type_first <- estimation_type
  }

  .calls <- substitute(list(...))

  if (all(c("rho", "R") %in% names(.calls))) {
    rho <- eval(.calls[["rho"]])
    R <- eval(.calls[["R"]])
    .calls <- .calls[-which(names(.calls) %in% c("rho", "R"))]
  } else {
    rho <- 1
    R <- 1
  }

  survey <- survey$surveys[[estimation_type_first]]
  estimation_type_vector <- names(survey)

  result <- rbindlist(
    lapply(
      estimation_type_vector,
      function(x) {
        partial_result <- rbindlist(
          lapply(
            seq_along(survey[[x]]),
            function(i) {
              survey_item <- survey[[x]][[i]]

              result <- rbindlist(
                lapply(
                  2:length(.calls),
                  function(j) {
                    call <- as.list(.calls[[j]])
                    name_function <- deparse(call[[1]])
                    call[["design"]] <- substitute(design)
                    call <- as.call(call)
                    estimation <- eval(call, envir = list(design = survey_item$design[[estimation_type]]))
                    return(cat_estimation(estimation, name_function))
                  }
                )
              )
              result[, period := survey_item$edition]
              return(result)
            }
          )
        )
        partial_result[, type := x]
        return(partial_result)
      }
    )
  )

  adj_se <- function(variance, rho, R) {
    sqrt(1 + rho * R) * sqrt(variance)
  }

  result <- result[
    ,
    variance := se**2
  ]

  if (estimation_type_first == estimation_type) {
    return(data.table(result))
  } else {
    numeric_vars <- names(result)[sapply(result, is.numeric)]
    agg <- result[, lapply(.SD, mean), by = list(stat, type), .SDcols = numeric_vars]
    agg[, se := sapply(variance, adj_se, rho = rho, R = R)]
    agg[, cv := se / value]
    agg[, evaluate := sapply(cv, evaluate_cv)]
    return(data.table(agg[order(stat), ]))
  }
}


cat_estimation <- function(estimation, call) {
  class_estimation <- class(estimation)[1]

  if (class_estimation != "svyby" & class_estimation != "svyratio") {
    class_estimation <- "default"
  }

  do.call(
    paste0(
      "cat_estimation.",
      class_estimation
    ),
    list(
      estimation,
      call
    )
  )
}

#' cat_estimation_svyby
#' @param estimation Estimation
#' @param call Call
#' @importFrom data.table data.table melt
#' @keywords internal
#' @noRd

cat_estimation.svyby <- function(estimation, call) {
  dt <- data.table(estimation)

  se_cols <- grep("^se\\.", names(dt), value = TRUE)

  est_cols <- setdiff(names(dt), se_cols)

  dt_melted <- melt(dt,
    measure.vars = est_cols,
    variable.name = "stat",
    value.name = "value"
  )

  for (col in se_cols) {
    stat_name <- gsub("^se\\.", "", col)
    dt_melted[stat == stat_name, se := dt[[col]]]
  }

  return(dt_melted)
}


#' cat_estimation_default
#' @param estimation Estimation
#' @param call Call
#' @importFrom data.table data.table
#' @importFrom survey SE cv
#' @importFrom stats coef
#' @keywords internal

cat_estimation.default <- function(estimation, call) {
  confint_estimation <- stats::confint(estimation)


  dt <- data.table(
    stat = paste0(call, ": ", names(estimation)),
    value = coef(estimation),
    se = unname(SE(estimation)),
    cv = unname(cv(estimation)),
    confint_lower = unname(confint_estimation[, 1]),
    confint_upper = unname(confint_estimation[, 2])
  )
  names(dt) <- c("stat", "value", "se", "cv", "confint_lower", "confint_upper")
  return(dt)
}

#' cat_estimation_svyratio
#' @param estimation Estimation
#' @param call Call
#' @importFrom data.table data.table
#' @importFrom survey SE cv
#' @importFrom stats coef
#' @keywords internal
#' @noRd

cat_estimation.svyratio <- function(estimation, call) {
  confint_estimation <- confint(estimation)



  dt <- data.table(
    stat = paste0(call, ": ", names(SE(estimation))),
    value = coef(estimation),
    se = unname(SE(estimation)),
    cv = unname(cv(estimation)),
    confint_lower = unname(confint_estimation[, 1]),
    confint_upper = unname(confint_estimation[, 2])
  )
  names(dt) <- c("stat", "value", "se", "cv", "confint_lower", "confint_upper")
  return(dt)
}
