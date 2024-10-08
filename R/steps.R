#' @importFrom data.table copy
compute <- function(svy, ..., .by = NULL, use_copy = use_copy_default()) {
  if (!use_copy) {
    .data <- get_data(svy)
  } else {
    .clone <- svy$clone()
    .data <- copy(get_data(.clone))
  }

  .exprs <- substitute(list(...))

  if (!is.null(.by)) {
    .agg <- .data[, j, by = .by, env = list(j = .exprs)]

    .data <- merge(.data, .agg, by = .by, all.x = TRUE)
  } else {
    .exprs <- eval(.exprs, .data)
    .data[
      ,
      (names(.exprs)) := .exprs,
      by = .by
    ]
  }


  if (!use_copy) {
    return(set_data(svy, .data))
  } else {
    return(set_data(.clone, .data))
  }
}


#' @importFrom data.table copy

recode <- function(svy, new_var, ..., .default = NA_character_, ordered = FALSE, use_copy = use_copy_default(),.to_factor = FALSE) {
  if (!use_copy) {
    .data <- svy$get_data()
  } else {
    .clone <- svy$clone()
    .data <- copy(get_data(.clone))
  }

  .exprs <- substitute(list(...))
  .exprs <- eval(.exprs, .data, parent.frame())

  .labels <- c(
    .default,
    unique(
      sapply(
        X = seq_along(.exprs),
        FUN = function(x) {
          .exprs[[x]][[3]]
        }
      )
    )
  )





  if (.to_factor) {
    .data[
      ,
      (new_var) := factor(
        .default,
        levels = .labels,
        ordered = ordered
      )
    ]
  } else {
    .data[
      ,
      (new_var) := .default
    ]
  }

  lapply(
    FUN = function(.expr) {
      .filter <- .exprs[[.expr]][[2]]
      .label <- .exprs[[.expr]][[3]]

      .data[
        eval(
          .filter,
          .data,
          parent.frame()
        ),
        (new_var) := .label
      ]
      invisible(NULL)
    },
    X = seq_along(.exprs)
  )

  if (!use_copy) {
    return(set_data(svy, .data))
  } else {
    return(set_data(.clone, .data))
  }
}

#' Step compute
#' @param svy Survey object
#' @param ... Expressions to compute
#' @param use_copy Use copy
#' @param .by By
#' @param comment Comment
#' @return Survey object
#' @keywords Steps
#' @export

step_compute <- function(svy = NULL, ..., .by = NULL, use_copy = use_copy_default(), comment = "Compute step") {
  .call <- match.call()

  check_svy <- is.null(
    get_data(svy)
  )

  if (check_svy) {
    return(.call)
  }

  exprs <- substitute(list(...))

  depends_on <- unique(
    c(sapply(
      X = 2:length(exprs),
      FUN = function(x) {
        find_dependencies(
          call_expr = exprs[[x]],
          survey = get_data(svy)
        )
      }
    ))
  )

  if (!is.null(svy)) {
    .names_before <- names(copy(get_data(svy$clone())))

    if (use_copy) {
      .svy_after <- compute(svy, ..., .by = .by, use_copy = use_copy)


      .names_after <- names(get_data(.svy_after))
      .new_vars <- .names_after[!.names_after %in% .names_before]

      if (length(.new_vars) > 0) {
        .name_step <- paste0(
          "New variable: ",
          paste0(
            .new_vars,
            collapse = ", "
          )
        )

        step <- Step$new(
          name = .name_step,
          edition = get_edition(.svy_after),
          survey_type = get_type(.svy_after),
          type = "compute",
          new_var = paste0(
            .new_vars,
            collapse = ", "
          ),
          exprs = substitute(list(...)),
          call = .call,
          svy_before = svy,
          default_engine = get_engine(),
          depends_on = depends_on,
          comment = comment
        )



        .svy_after$add_step(
          step
        )
        return(.svy_after)
      } else {
        message("No news variable created: ", substitute(list(...)))
        return(svy)
      }
    } else {
      compute(svy, ..., .by = .by, use_copy = use_copy)

      .names_after <- names(get_data(svy))

      .new_vars <- .names_after[!.names_after %in% .names_before]

      if (length(.new_vars) == 0) {
        stop(message("No news variable created: ", substitute(list(...))))
      }

      .name_step <- paste0(
        "New variable: ",
        paste0(
          .new_vars,
          collapse = ", "
        )
      )


      step <- Step$new(
        name = .name_step,
        edition = get_edition(svy),
        survey_type = get_type(svy),
        type = "compute",
        new_var = paste0(
          .new_vars,
          collapse = ", "
        ),
        exprs = substitute(list(...)),
        call = .call,
        svy_before = NULL,
        default_engine = get_engine(),
        comment = comment,
        depends_on = depends_on,
      )

      svy$add_step(
        step
      )

      invisible(svy)
    }
  } else {
    return(.call)
  }
}

#' Step recode
#' @param svy Survey object
#' @param new_var New variable
#' @param ... Expressions to recode
#' @param .default Default value
#' @param .name_step Name of the step
#' @param ordered Ordered
#' @param use_copy Use copy
#' @param comment Comment
#' @param .to_factor To factor
#' @return Survey object
#' @keywords Steps
#' @export

step_recode <- function(svy = survey_empty(), new_var, ..., .default = NA_character_, .name_step = NULL, ordered = FALSE, use_copy = use_copy_default(), comment = "Recode step",.to_factor = FALSE) {
  .call <- match.call()

  new_var <- as.character(substitute(new_var))

  check_svy <- is.null(
    get_data(svy)
  )

  if (check_svy) {
    return(.call)
  }

  if (is.null(.name_step)) {
    .name_step <- paste0(
      "New group: ",
      new_var
    )
  }

  depends_on <- unique(
    c(sapply(
      X = seq_along(list(...)),
      FUN = function(x) {
        find_dependencies(
          call_expr = list(...)[[x]],
          survey = get_data(svy)
        )
      }
    ))
  )

  if (use_copy) {
    .svy_after <- recode(
      svy = svy,
      new_var = new_var,
      ...,
      .default = .default,
      use_copy = use_copy,
      .to_factor = .to_factor
    )

    step <- Step$new(
      name = .name_step,
      edition = get_edition(.svy_after),
      survey_type = get_type(.svy_after),
      type = "recode",
      new_var = new_var,
      exprs = substitute(list(...)),
      call = .call,
      svy_before = svy,
      default_engine = get_engine(),
      depends_on = depends_on,
      comment = comment
    )

    .svy_after$add_step(
      step
    )

    return(.svy_after)
  } else {
    recode(
      svy = svy,
      new_var = new_var,
      ...,
      .default = .default,
      use_copy = use_copy
    )

    step <- Step$new(
      name = .name_step,
      edition = get_edition(svy),
      survey_type = get_type(svy),
      type = "recode",
      new_var = new_var,
      exprs = substitute(list(...)),
      call = .call,
      svy_before = NULL,
      default_engine = get_engine(),
      depends_on = depends_on,
      comment = comment
    )

    svy$add_step(
      step
    )

    invisible(svy)
  }
}


#' Get formulas
#' @param steps List of steps
#' @return List of formulas
#' @noRd

get_formulas <- function(steps) {
  if (length(steps) > 0) {
    sapply(
      X = seq_along(steps),
      FUN = function(x) {
        step <- steps[[x]]
        exprs <- step$exprs
        if (step$type == "recode") {
          paste0(
            step$new_var,
            ": ",
            paste(
              deparse1(
                step$exprs
              ),
              collapse = "\n"
            )
          )
        } else {
          deparse1(exprs)
        }
      }
    )
  } else {
    NULL
  }
}

#' Get comments
#' @param steps List of steps
#' @return List of comments
#' @noRd

get_comments <- function(steps) {
  if (length(steps) > 0) {
    sapply(
      X = seq_along(steps),
      FUN = function(x) {
        step <- steps[[x]]
        step$comments
      }
    )
  } else {
    NULL
  }
}

#' Get type of step
#' @param steps List of steps
#' @return List of types
#' @noRd

get_type_step <- function(steps) {
  if (length(steps) > 0) {
    sapply(
      X = seq_along(steps),
      FUN = function(x) {
        step <- steps[[x]]
        step$type
      }
    )
  } else {
    NULL
  }
}

#' View graph
#' @param svy Survey object
#' @param init_step Initial step
#' @importFrom visNetwork visNetwork
#' @importFrom visNetwork visGroups
#' @importFrom visNetwork visEdges
#' @importFrom visNetwork visHierarchicalLayout
#' @importFrom visNetwork visLegend
#' @importFrom visNetwork visOptions
#' @importFrom visNetwork addFontAwesome
#' @return Graph
#' @keywords Survey methods
#' @keywords Steps
#' @export


view_graph <- function(svy, init_step = "Load survey") {
  steps <- get_steps(svy)
  steps_type <- get_type_step(steps)
  formulas <- get_formulas(steps)
  comments <- get_comments(steps)

  if (init_step == "Load survey") {
    init_step <- glue::glue_col(
      "


            Type: {type}
            Edition: {edition}
            Weight: {weight}
            ",
      type = get_type(svy),
      edition = get_edition(svy),
      weight = get_weight(svy)
    )
  }

  names_step <- c(
    init_step,
    names(steps)
  )

  title <- c(
    init_step,
    paste(
      paste("<h2>", comments, "</h2>", sep = "\n"),
      paste(
        "<h5>",
        formulas,
        "</h5>",
        sep = "\n"
      ),
      sep = "\n"
    )
  )


  nodes <- data.frame(
    id = seq_along(names_step),
    label = names_step,
    title = title,
    group = c(
      "Load survey",
      steps_type
    )
  )


  edges <- data.frame(
    from = seq_along(names_step),
    to = c(
      2:length(names_step),
      rep(
        NA,
        length(init_step)
      )
    )
  )

  visNetwork(
    nodes = nodes,
    edges = edges,
    height = "500px", width = "100%"
  ) |>
    visGroups(
      groupname = "Load survey",
      shape = "icon",
      icon = list(
        code = "f1c0",
        color = "#440154"
      ),
      shadow = list(enabled = TRUE)
    ) |>
    visGroups(
      groupname = "compute",
      shape = "icon",
      icon = list(
        code = "f1ec",
        color = "#31688e"
      ),
      shadow = list(enabled = TRUE)
    ) |>
    visGroups(
      groupname = "recode",
      shape = "icon",
      icon = list(
        code = "f0e8",
        color = "#21918c"
      ),
      shadow = list(enabled = TRUE)
    ) |>
    addFontAwesome() |>
    visEdges(arrows = "to") |>
    visHierarchicalLayout(
      direction = "LR",
      levelSeparation = 200
    ) |>
    visNetwork::visOptions(
      nodesIdSelection = TRUE,
      clickToUse = TRUE,
      manipulation = FALSE
    ) |>
    visLegend(
      width = 0.2,
      position = "left",
      main = "Type",
      zoom = FALSE
    )
}


new_step <- function(id = 1, name, description, depends = NULL, type, new_var = NULL, ...) {
  if (type == "recode") {
    if (is.null(new_var)) {
      stop("new_var is required for recode")
    }
  }

  call <- do.call(
    paste0(
      "step_",
      type
    ),
    args = list(
      svy = survey_empty(),
      new_var = new_var,
      ...
    )
  )

  list(
    id = id,
    name = name,
    description = description,
    depends = depends,
    type = type,
    new_var = new_var,
    call = call
  )
}

#' @title Find dependencies
#' @description Find dependencies
#' @param call_expr Call expression
#' @param survey Survey
#' @keywords internal
#' @return List of dependencies
#' @noRd
#'
find_dependencies <- function(call_expr, survey) {
  dependencies <- character()

  if (is.call(call_expr)) {
    for (i in seq_along(call_expr)) {
      result <- find_dependencies(call_expr[[i]], survey)
      if (!is.null(result)) {
        dependencies <- unique(c(dependencies, result))
      }
    }
  } else if (is.name(call_expr) && as.character(call_expr) %in% names(survey)) {
    dependencies <- unique(c(dependencies, as.character(call_expr)))
  }

  return(unique(dependencies))
}
