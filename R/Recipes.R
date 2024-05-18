Recipe <- R6Class("Recipe",
    public = list(
        name = NULL,
        edition = NULL,
        survey_type = NULL,
        default_engine = NULL,
        depends_on = list(),
        user = NULL,
        description = NULL,
        steps = list(),
        initialize = function(name, edition, survey_type, default_engine, depends_on, user, description, steps) {
            self$name <- name
            self$edition <- edition
            self$survey_type <- survey_type
            self$default_engine <- default_engine
            self$depends_on <- depends_on
            self$user <- user
            self$description <- description
            self$steps <- steps
        }
    )
)

metadata_recipe <- function() {
    return(
        c(
            "name",
            "user",
            "svy",
            "description"
        )
    )
}

#' Recipe
#' @export
#' @param ... A list with the following metadata: name, user, svy, description
#' @keywords Survey methods
#' @keywords Recipes
#' @return A Recipe object

recipe <- function(...) {
    
    dots <- list(...)

    metadata_recipes_names <- metadata_recipe()

    check_args = sum(metadata_recipes_names %in% names(dots))



    if (!(check_args == length(metadata_recipes_names))) {
        stop(
            message(
                "The recipe must have the following metadata: ",
                paste(metadata_recipe(), collapse = ", ")
            )
        )
    }

    index_steps <- which(names(dots) %in% metadata_recipe())


    if ("steps" %in% names(dots)) {
        return(
            Recipe$new(
                name = dots$name,
                user = dots$user,
                edition = dots$svy$edition,
                survey_type = dots$svy$type,
                default_engine = default_engine(),
                depends_on = list(),
                description = dots$description,
                steps = eval(dots$steps)
            )
        )
    } else {
        return(
            Recipe$new(
                name = dots$name,
                user = dots$user,
                edition = dots$svy$edition,
                survey_type = dots$svy$type,
                default_engine = default_engine(),
                depends_on = list(),
                description = dots$description,
                steps = dots[-index_steps]
            )
        )
    }
}

#' Encoding and decoding recipes
#' @param recipe A Recipe object
#' @return A Recipe object
#' @keywords internal
#' @noRd 

encoding_recipe <- function(recipe) {

    recipe$steps <- lapply(recipe$steps, function(step) {
        step_string <- deparse(step)
        return(step_string)
    })

    return(recipe)
}

#' Encoding and decoding recipes
#' @param recipe A Recipe object
#' @return A Recipe object
#' @keywords internal
#' @noRd

decode_recipe <- function(recipe) {
    recipe$steps <- as.call(
        lapply(
            recipe$steps,
            function(step_string) as.call(parse(text = step_string))[[1]]
        )
    )

    return(
        recipe
    )
}

#' Save a recipe to a file
#' @param recipe A Recipe object
#' @param file A character string with the file path
#' @importFrom jsonlite write_json
#' @return NULL
#' @keywords Survey methods
#' @keywords Recipes
#' @export

save_recipe <- function(recipe, file) {

    recipe = list(
        name = recipe$name,
        user = recipe$user,
        svy_type = recipe$survey_type,
        edition = recipe$edition,
        description = recipe$description,
        steps = recipe$steps
    )

    recipe |> 
        encoding_recipe() |> 
        write_json(path = file, simplifyVector = TRUE)
    
    message(
        glue::glue("The recipe has been saved in {file}")
    )
}

#' Load a recipe from a file
#' @param file A character string with the file path
#' @importFrom jsonlite read_json
#' @return A Recipe object
#' @keywords Survey methods
#' @keywords Recipes
#' @export

read_recipe <- function(file) {
    
    file |> 
        read_json(simplifyVector = TRUE) |> 
        decode_recipe()

}

#' Get a recipe from the API
#' @param topic A character string with the topic of the recipe
#' @param svy_type A character string with the survey type of the recipe
#' @param svy_edition A character string with the survey edition of the recipe
#' @param allowMultiple A logical value to allow multiple recipes
#' @importFrom httr POST
#' @importFrom jsonlite parse_json
#' @importFrom httr content
#' @importFrom httr add_headers
#' @return A Recipe object
#' @keywords Survey methods
#' @keywords Recipes
#' @export

get_recipe <- function(
    svy_type = NULL,
    svy_edition = NULL,
    topic = NULL,
    allowMultiple = TRUE
) {

    filterList = list(
        svy_type = svy_type,
        svy_edition = svy_edition,
        topic = topic
    )

    method = "findOne"

    if (allowMultiple) {
        method = "find"
    }

    filterList <- filterList[!sapply(filterList, is.null)]

    baseUrl = url_api_host()
    
    url = paste0(
        baseUrl,
        method
    )
    
    headers <- c(
        "Content-Type" = "application/json",
        "Access-Control-Request-Headers" = "*",
        "api-key" = get_api_key()
    )

    body <- list(
        collection = "recipes",
        database = "metasurvey",
        dataSource = "Cluster0",
        filter = filterList
    )

    response <- POST(
        url, 
        body = body,
        encode = "json",
        add_headers(.headers = headers)
    )

    content = content(response, "text", encoding = "UTF-8")

    if (response$status != 200) {
        stop(
            message(
                "The API returned an error: ",
                response$status
            )
        )
    }

    content_json = parse_json(content)

    n_recipe = get_distinct_recipes(content_json)

    message(
        glue::glue("The API returned {n_recipe} recipes")
    )


    if (n_recipe == 1) {
        recipe = content_json$document
        return(
            Recipe$new(
                name = unlist(recipe$name),
                user = unlist(recipe$user),
                edition = unlist(recipe$svy_edition),
                survey_type = unlist(recipe$svy_type),
                default_engine = default_engine(),
                depends_on = list(),
                description = unlist(recipe$description),
                steps = recipe$steps
            )
        )
    } else {
        return(
            lapply(
                X = 1:n_recipe,
                FUN = function(x) {
                    recipe = content_json$documents[[x]]
                    Recipe$new(
                        name = unlist(recipe$name),
                        user = unlist(recipe$user),
                        edition = unlist(recipe$svy_edition),
                        survey_type = unlist(recipe$svy_type),
                        default_engine = default_engine(),
                        depends_on = list(),
                        description = unlist(recipe$description),
                        steps = recipe$steps
                    )
                }
            )
        )
    }

}

#' Convert a list of steps to a recipe
#' @param name A character string with the name of the recipe
#' @param user A character string with the user of the recipe
#' @param svy A Survey object
#' @param description A character string with the description of the recipe
#' @param steps A list with the steps of the recipe
#' @return A Recipe object
#' @keywords Survey methods
#' @keywords Recipes
#' @export

steps_to_recipe <- function(
    name,
    user,
    svy = survey_empty(type = "eaii", edition = "2019-2021"),
    description,
    steps
) {
    return(
        recipe(
            name = name,
            user = user,
            svy = svy,
            description = description,
            steps = unname(lapply(
                steps,
                function(step) {
                    unname(step$call)
                }
            ))
        )
    )
}


get_distinct_recipes = function(content_json) {
    if (is.null(content_json$documents)) {
        return(1)
    } else {
        return(
            length(
                unique(
                    sapply(
                        X = 1:length(content_json$documents),
                        FUN = function(x) {
                            content_json$documents[[x]][['_id']]
                        }
                    )
                )
            )
        )
    }
}


