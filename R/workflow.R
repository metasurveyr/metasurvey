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

workflow_default <- function(survey,..., estimation_type = "monthly") {
  .calls <- substitute(list(...))

  partial_result <- lapply(
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
    }
  )

  names(partial_result) <- estimation_type
  return(partial_result)
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
  
  survey <- survey$surveys[[estimation_type_first]]
  
  estimation_type_vector <- names(survey)
  
  
  
  # Generamos la lista final, con nombres de los tipos de estimación
  final_result <- lapply(
    estimation_type_vector,
    function(x) {
      # Para cada tipo de estimación, creamos un data.table combinando los resultados
      results_by_type <- rbindlist(
        lapply(
          seq_along(survey[[x]]),
          function(i) {
            survey_item <- survey[[x]][[i]]
            
            # Ejecutamos cada función en .calls
            result <- rbindlist(
              lapply(
                2:length(.calls),
                function(j) {
                  call <- as.list(.calls[[j]])
                  name_function <- deparse(call[[1]])
                  call[["design"]] <- substitute(design)
                  call <- as.call(call)
                  
                  # Evaluamos la función en el entorno
                  estimation <- eval(call, envir = list(design = survey_item$design[[estimation_type]]))
                  
                  # Llamamos a cat_estimation para procesar la estimación y etiquetarla
                  return(
                    cat_estimation(estimation, name_function)
                  )
                }
              )
            )
            result[['survey']] <- survey_item$edition
            return(result)
          }
        )
      )
      
      # Devolvemos un data.table por cada tipo de estimación
      return(results_by_type)
    }
  )
  
  # Asignamos nombres a los resultados para tener la estructura por tipo de estimación
  names(final_result) <- estimation_type_vector
  
  if (estimation_type_first == estimation_type) {
    return(final_result)
  } else {
  
    final_result <- lapply(
      final_result,
      function(x) {
        numeric_vars <- names(x)[sapply(x, is.numeric)]
        
        x[, lapply(.SD, mean), by = list(stat), .SDcols = numeric_vars]
      }
    )
    
    return(final_result)
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
