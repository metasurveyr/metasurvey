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
    
    .names_before = names(get_data(svy))
    svy_after = compute(svy, ...)
    
    .names_after = names(get_data(svy_after))
    .name_step <- paste0(
        "New variable: ", 
        paste0(
            .names_after[!.names_after %in% .names_before], 
            collapse = ", "
        )
    )
    
    svy_after$add_step(
         list(
             name = .name_step,
             type = "compute",
             new_var = paste0(
               .names_after[!.names_after %in% .names_before], 
               collapse = ", "
            ),
            exprs = substitute(list(...))
        )
    )

    return(svy_after)
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

    if (is.null(.name_step)) {
        .name_step <- paste0(
            "New group: ", 
            new_var
        )
    }
    
    
    svy_after = recode(
        svy = svy, 
        new_var = new_var, 
        ..., 
        .default = .default
    )
   

    svy_after$add_step(
        list(
            name = .name_step,
            type = "recode",
            new_var = new_var,
            exprs = list(...)
        )
    )

    return(svy_after)
}