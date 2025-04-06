#' @title Workflow
#' @description Workflow
#' @keywords Surveymethods
#' @param survey Survey object
#' @param ... Calls
#' @param estimation_type Estimation type
#' @importFrom data.table rbindlist
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
