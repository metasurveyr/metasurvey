#' @title Workflow
#' @description Workflow
#' @keywords Surveymethods
#' @param survey Survey object
#' @param ... Calls
#' @param estimation_type Estimation type
#' @importFrom data.table rbindlist
#' @export
#' @return Data
#'
workflow <- function(survey, ..., estimation_type = "monthly") {

  .calls <- substitute(list(...))
  
  partial_result = sapply(
    estimation_type,
    function(x) {
      lapply(
        X = seq_along(survey),
        function(i) {
          survey <- survey[[i]]

          result <- rbindlist(
            lapply(
              2:length(.calls),
              function(i) {
                call <- as.list(.calls[[i]])
                name_function <- deparse(call[[1]])
                call[["design"]] <- substitute(design)
                call <- as.call(call)
                estimation <- eval(call, envir = list(design = survey$design[[x]]))



                return(
                  cat_estimation(estimation, name_function)
                )
              }
            )
          )

      return(
        result
      )
    }
  )

  names(partial_result) <- estimation_type
  return(partial_result)

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
  dt <- data.table(
    stat = paste0(call, ": ", names(estimation)),
    value = coef(estimation),
    se = unname(SE(estimation)),
    cv = unname(cv(estimation))
  )
  names(dt) <- c("stat", "value", "se", "cv")
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
  dt <- data.table(
    stat = paste0(call, ": ", names(SE(estimation))),
    value = coef(estimation),
    se = unname(SE(estimation)),
    cv = unname(cv(estimation))
  )
  names(dt) <- c("stat", "value", "se", "cv")
  return(dt)
}
