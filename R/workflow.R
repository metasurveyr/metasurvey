#' Execute estimation workflow for surveys
#'
#' This function executes a sequence of statistical estimations on Survey
#' objects, applying functions from the R survey package with appropriate
#' metadata. Automatically handles different survey types and periodicities.
#'
#' @param svy A **list** of Survey objects, or a PoolSurvey.
#'   Even for a single survey, wrap it in `list()`:
#'   `workflow(svy = list(my_survey), ...)`.
#'   Must contain properly configured sample design.
#' @param ... Calls to survey package functions (such as \code{svymean},
#'   \code{svytotal}, \code{svyratio}, etc.) that will be executed sequentially
#' @param estimation_type Type of estimation (default `"monthly"`) that
#'   determines which weight to use. Options:
#'   `"monthly"`, `"quarterly"`, `"annual"`, or vector
#'   with multiple types
#'
#' @return \code{data.table} with results from all
#'   estimations, including columns:
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
#' # Simple estimation with a test survey
#' dt <- data.table::data.table(
#'   x = rnorm(100), g = sample(c("a", "b"), 100, TRUE),
#'   w = rep(1, 100)
#' )
#' svy <- Survey$new(
#'   data = dt, edition = "2023", type = "test",
#'   psu = NULL, engine = "data.table",
#'   weight = add_weight(annual = "w")
#' )
#' result <- workflow(
#'   svy = list(svy),
#'   survey::svymean(~x, na.rm = TRUE),
#'   estimation_type = "annual"
#' )
#'
#' \donttest{
#' # ECH example with domain estimations
#' # result <- workflow(
#' #   survey = list(ech_2023),
#' #   svyby(~unemployed, ~region, svymean, na.rm = TRUE),
#' #   estimation_type = "annual")
#' }
#'
#' @seealso
#' \code{\link[survey]{svymean}} for population means
#' \code{\link[survey]{svytotal}} for population totals
#' \code{\link[survey]{svyratio}} for ratios
#' \code{\link[survey]{svyby}} for domain estimations
#' \code{\link{PoolSurvey}} for survey pooling
#'
#' @keywords survey
#' @family workflows
#' @export

workflow <- function(svy, ..., estimation_type = "monthly") {
  if (is(svy, "PoolSurvey")) {
    return(workflow_pool(
      svy, ...,
      estimation_type = estimation_type
    ))
  } else {
    return(workflow_default(
      svy, ...,
      estimation_type = estimation_type
    ))
  }
}


#' @title Workflow default
#' @description Workflow default
#' @keywords survey
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

              survey$ensure_design()

              partial_result <- rbindlist(
                lapply(
                  seq.int(2L, length(.calls)),
                  function(i) {
                    call <- as.list(.calls[[i]])
                    name_function <- deparse(call[[1]])
                    call[["design"]] <- substitute(design)
                    call <- as.call(call)
                    estimation <- eval(
                      call,
                      envir = list(design = survey$design[[x]])
                    )

                    return(cat_estimation(
                      estimation, name_function
                    ))
                  }
                ),
                fill = TRUE
              )
              return(partial_result)
            }
          ),
          fill = TRUE
        )
      }
    ),
    fill = TRUE
  )

  # Auto-capture: build RecipeWorkflow if surveys have recipes
  wf <- .capture_workflow(survey, .calls, estimation_type)
  if (!is.null(wf)) {
    attr(result, "workflow") <- wf
  }

  # Attach provenance from first survey
  if (length(survey) > 0 && inherits(survey[[1]], "Survey")) {
    prov <- survey[[1]]$provenance
    if (!is.null(prov)) {
      prov$estimation <- list(
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        estimation_type = estimation_type
      )
      attr(result, "provenance") <- prov
    }
  }

  return(result)
}

#' @title Workflow pool
#' @description Workflow pool
#' @keywords survey
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

              survey_item$ensure_design()

              result <- rbindlist(
                lapply(
                  seq.int(2L, length(.calls)),
                  function(j) {
                    call <- as.list(.calls[[j]])
                    name_function <- deparse(call[[1]])
                    call[["design"]] <- substitute(design)
                    call <- as.call(call)
                    estimation <- eval(
                      call,
                      envir = list(
                        design = survey_item$design[[estimation_type]]
                      )
                    )
                    return(cat_estimation(
                      estimation, name_function
                    ))
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
    out <- data.table(result)
  } else {
    numeric_vars <- names(result)[
      vapply(result, is.numeric, logical(1))
    ]
    agg <- result[
      , lapply(.SD, mean),
      by = list(stat, type), .SDcols = numeric_vars
    ]
    agg[, se := vapply(
      variance, adj_se, numeric(1),
      rho = rho, R = R
    )]
    agg[, cv := se / value]
    agg[, evaluate := vapply(cv, evaluate_cv, character(1))]
    out <- data.table(agg[order(stat), ])
  }

  # Auto-capture for pool workflows
  # Collect all surveys from the pool structure for recipe extraction
  all_surveys <- unlist(survey, recursive = FALSE)
  wf <- .capture_workflow(all_surveys, .calls, estimation_type)
  if (!is.null(wf)) {
    attr(out, "workflow") <- wf
  }

  return(out)
}


cat_estimation <- function(estimation, call) {
  class_estimation <- class(estimation)[1]

  if (class_estimation != "svyby" && class_estimation != "svyratio") {
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
  by_vars <- attr(estimation, "svyby")$margins
  all_names <- names(estimation)

  # margins can be integer indices — convert to names
  if (is.numeric(by_vars)) {
    by_vars <- all_names[by_vars]
  }
  if (is.null(by_vars)) by_vars <- character(0)

  # SE columns: "se.varname" (multi-stat) or "se" (single-stat)
  se_cols <- grep("^se(\\.|$)", all_names, value = TRUE)
  stat_cols <- setdiff(all_names, c(by_vars, se_cols))

  ci <- tryCatch(stats::confint(estimation), error = function(e) NULL)
  cv_mat <- tryCatch(survey::cv(estimation), error = function(e) NULL)

  n_groups <- nrow(estimation)
  results <- list()

  for (j in seq_along(stat_cols)) {
    s <- stat_cols[j]

    # Match SE column: try "se.varname" first, fall back to "se"
    se_col <- paste0("se.", s)
    if (!se_col %in% all_names && "se" %in% all_names) {
      se_col <- "se"
    }

    vals <- as.numeric(estimation[[s]])
    ses <- as.numeric(
      if (se_col %in% all_names) {
        estimation[[se_col]]
      } else {
        rep(NA_real_, n_groups)
      }
    )

    if (!is.null(cv_mat)) {
      cvs <- as.numeric(
        if (is.matrix(cv_mat) || is.data.frame(cv_mat)) {
          cv_mat[, j]
        } else {
          cv_mat
        }
      )
    } else {
      cvs <- ses / vals
    }

    ci_start <- (j - 1) * n_groups + 1
    ci_end <- j * n_groups
    if (!is.null(ci) && nrow(ci) >= ci_end) {
      ci_lo <- ci[ci_start:ci_end, 1]
      ci_hi <- ci[ci_start:ci_end, 2]
    } else {
      ci_lo <- vals - 1.96 * ses
      ci_hi <- vals + 1.96 * ses
    }

    # Build by-variable label for stat column
    by_labels <- vapply(seq_len(n_groups), function(i) {
      paste(
        vapply(by_vars, function(bv) {
          paste0(bv, "=", estimation[[bv]][i])
        }, character(1)),
        collapse = ", "
      )
    }, character(1))

    cols <- list(
      stat = paste0(call, ": ", s, " [", by_labels, "]"),
      value = vals,
      se = ses,
      cv = cvs,
      confint_lower = ci_lo,
      confint_upper = ci_hi
    )

    for (bv in by_vars) {
      cols[[bv]] <- estimation[[bv]]
    }

    results[[length(results) + 1]] <- data.table::as.data.table(cols)
  }

  rbindlist(results, fill = TRUE)
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
  confint_estimation <- stats::confint(estimation)



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

#' Auto-capture workflow from survey list and calls
#' @param survey_list List of Survey objects
#' @param .calls Substituted call list
#' @param estimation_type Character vector of estimation types
#' @return RecipeWorkflow or NULL if no recipes found
#' @keywords internal
#' @noRd
.capture_workflow <- function(survey_list, .calls, estimation_type) {
  # Collect recipe IDs from surveys
  recipe_ids <- character(0)
  svy_type <- "Unknown"
  svy_edition <- "Unknown"
  svy_user <- "Unknown"

  for (s in survey_list) {
    if (!inherits(s, "Survey")) next
    if (svy_type == "Unknown") svy_type <- s$type %||% "Unknown"
    if (svy_edition == "Unknown") svy_edition <- s$edition %||% "Unknown"

    # Extract recipe IDs from the survey's recipes
    if (length(s$recipes) > 0) {
      for (r in s$recipes) {
        if (inherits(r, "Recipe") && !is.null(r$id)) {
          rid <- as.character(r$id)
          if (!rid %in% recipe_ids) {
            recipe_ids <- c(recipe_ids, rid)
          }
          if (svy_user == "Unknown" && !is.null(r$user)) {
            svy_user <- r$user
          }
        }
      }
    }
  }

  # Only auto-capture if surveys have recipes
  if (length(recipe_ids) == 0) {
    return(NULL)
  }

  # Extract weight specification from first survey with weights
  weight_spec <- NULL
  for (s in survey_list) {
    if (!inherits(s, "Survey")) next
    if (!is.null(s$weight) && length(s$weight) > 0) {
      weight_spec <- .serialize_weight_spec(s$weight, s$edition)
      break
    }
  }

  # Parse call metadata from .calls
  calls_str <- list()
  call_metadata <- list()

  if (length(.calls) > 1) {
    for (i in seq.int(2L, length(.calls))) {
      raw_call <- .calls[[i]]
      deparsed <- deparse(raw_call, width.cutoff = 200)
      calls_str[[length(calls_str) + 1]] <- paste(
        deparsed,
        collapse = " "
      )

      # Extract type and formula from the call
      call_list <- as.list(raw_call)
      fn_name <- deparse(call_list[[1]])

      # Extract formula (first argument after function name)
      formula_str <- if (length(call_list) > 1) {
        deparse(call_list[[2]], width.cutoff = 200)
      } else {
        ""
      }

      # Extract by= for svyby
      by_str <- NULL
      if (fn_name == "svyby" && length(call_list) > 2) {
        by_str <- deparse(
          call_list[[3]],
          width.cutoff = 200
        )
      }

      call_metadata[[length(call_metadata) + 1]] <- list(
        type = fn_name,
        formula = paste(formula_str, collapse = " "),
        by = by_str,
        description = ""
      )
    }
  }

  RecipeWorkflow$new(
    name = paste("Workflow:", svy_type, svy_edition),
    description = paste(
      "Auto-captured workflow with",
      length(calls_str), "estimations"
    ),
    user = svy_user,
    survey_type = svy_type,
    edition = svy_edition,
    estimation_type = estimation_type,
    recipe_ids = recipe_ids,
    calls = calls_str,
    call_metadata = call_metadata,
    weight_spec = weight_spec
  )
}
