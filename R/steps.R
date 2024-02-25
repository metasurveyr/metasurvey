#' @importFrom data.table copy
compute <- function(svy, ...) {
    
    .clone <- svy$clone()
    .data <- copy(get_data(.clone))

    .exprs <- substitute(
        list(...)
    )
    .exprs <- eval(
        .exprs, 
        .data
    )

    .data[
        , 
        (names(.exprs)) := .exprs
    ]

    return(set_data(.clone, .data))
}

#' @importFrom data.table copy

recode <- function(svy, new_var, ..., .default = NA_character_) {
    
    .clone <- svy$clone()
    .data <- copy(get_data(.clone))

    .exprs <- substitute(list(...))
    .exprs <- eval(.exprs, .data, parent.frame())

    .data[, (new_var) := .default]

    for (.expr in 1:length(.exprs)) {

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
    }

    return(set_data(.clone, .data))
}

#' Step compute
#' @param svy Survey object
#' @param ... Expressions to compute
#' @return Survey object
#' @export

step_compute <- function(svy, ...) {
    
  
    .call = match.call()
    .names_before = names(get_data(svy))
    .svy_after = compute(svy, ...)
    
    
    
    .names_after = names(get_data(.svy_after))
    .name_step <- paste0(
        "New variable: ", 
        paste0(
            .names_after[!.names_after %in% .names_before], 
            collapse = ", "
        )
    )
    
    
    
    .svy_after$add_step(
         list(
             name = .name_step,
             type = "compute",
             new_var = paste0(
               .names_after[!.names_after %in% .names_before], 
               collapse = ", "
            ),
            exprs = substitute(list(...)),
            call = .call,
            svy_before = svy
        )
    )

    return(.svy_after)
}

#' Step recode
#' @param svy Survey object
#' @param new_var New variable
#' @param ... Expressions to recode
#' @param .default Default value
#' @param .name_step Name of the step
#' @return Survey object
#' @export

step_recode <- function(svy,new_var, ..., .default = NA_character_,.name_step = NULL) {

    .call = match.call()
    if (is.null(.name_step)) {
        .name_step <- paste0(
            "New group: ", 
            new_var
        )
    }
    
    
    .svy_after = recode(
        svy = svy, 
        new_var = new_var, 
        ..., 
        .default = .default
    )
   
    
    .svy_after$add_step(
        list(
            name = .name_step,
            type = "recode",
            new_var = new_var,
            exprs = list(...),
            call = .call,
            svy_before = svy
        )
    )

    return(.svy_after)
}


#' Get formulas
#' @param steps List of steps
#' @return List of formulas
#' @noRd 

get_formulas <- function(steps) {
    if (length(steps) > 0) {
        sapply(
            X = 1:length(steps),
            FUN = function(x) {
                step <- steps[[x]]
                exprs <- step$exprs
                if (step$type == "recode") {
                    paste0(
                        step$new_var,
                        ": ",
                        paste(
                          deparse(
                            step$exprs
                          ),
                          collapse = "\n"
                        )
                    )
                } else {
                    deparse(exprs)
                }
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
            X = 1:length(steps),
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
#' @return Graph
#' @export
#' @importFrom visNetwork visNetwork
#' @importFrom visNetwork visGroups
#' @importFrom visNetwork visEdges
#' @importFrom visNetwork visHierarchicalLayout
#' @importFrom visNetwork visLegend
#' @importFrom visNetwork visOptions
#' @importFrom visNetwork addFontAwesome

view_graph <- function(svy, init_step = "Load survey") {
    steps <- get_steps(svy)
    steps_type <- get_type_step(steps)
    formulas <- get_formulas(steps)
    
    if (init_step == "Load survey"){
      init_step = glue::glue_col(
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

    nodes <- data.frame(
        id = 1:length(names_step),
        label = names_step,
        title = c(init_step, formulas),
        group = c(
            "Load survey",
            steps_type
        )
    )


    edges <- data.frame(
        from = 1:length(names_step),
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
    ) %>%
        visGroups(
            groupname = "Load survey",
            shape = "icon",
            icon = list(
                code = "f1c0"
            )
        ) %>%
        visGroups(
            groupname = "compute",
            shape = "icon",
            icon = list(
                code = "f1ec"
            )
        ) %>%
        visGroups(
            groupname = "recode",
            shape = "icon",
            icon = list(
                code = "f0e8"
            )
        ) %>%
        addFontAwesome() %>%
        visEdges(arrows = "to") %>%
        visHierarchicalLayout(
            direction = "LR", 
            levelSeparation = 300
        ) %>%
        visNetwork::visOptions(
            nodesIdSelection = TRUE
        ) %>%
        visLegend(
            width = 0.2, 
            position = "left", 
            main = "Type", 
            zoom = FALSE
        )
}


