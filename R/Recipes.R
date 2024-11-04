Recipe <- R6Class("Recipe",
  public = list(
    name = NULL,
    edition = NULL,
    survey_type = NULL,
    default_engine = NULL,
    depends_on = list(),
    user = NULL,
    description = NULL,
    id = NULL,
    steps = list(),
    doi = NULL,
    bake = FALSE,
    initialize = function(name, edition, survey_type, default_engine, depends_on, user, description, steps, id, doi) {
      self$name <- name
      self$edition <- edition
      self$survey_type <- survey_type
      self$default_engine <- default_engine
      self$depends_on <- depends_on
      self$user <- user
      self$description <- description
      self$steps <- steps
      self$id <- id
      self$doi <- doi
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


  class_dots <- sapply(dots, class)

  metadata_recipes_names <- metadata_recipe()

  check_args <- sum(metadata_recipes_names %in% names(dots))

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
        id = dots$id %||% stats::runif(1, 0, 1),
        name = dots$name,
        user = dots$user,
        edition = dots$svy$edition,
        survey_type = dots$svy$type,
        default_engine = default_engine(),
        depends_on = list(),
        description = dots$description,
        steps = eval(dots$steps),
        doi = dots$doi %||% NULL
      )
    )
  } else {
    return(
      Recipe$new(
        id = dots$id %||% stats::runif(1, 0, 1),
        name = dots$name,
        user = dots$user,
        edition = dots$svy$edition,
        survey_type = dots$svy$type,
        default_engine = default_engine(),
        depends_on = list(),
        description = dots$description,
        steps = dots[-index_steps],
        doi = dots$doi %||% NULL
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

decode_step <- function(steps) {
  steps <- as.call(
    sapply(
      steps,
      function(step_string) as.call(parse(text = step_string))[[1]]
    )
  )

  return(
    steps
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
  recipe <- list(
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
  decode_step(read_json(file, simplifyVector = TRUE)$steps)
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
    allowMultiple = TRUE) {
  filterList <- list(
    svy_type = svy_type,
    svy_edition = svy_edition,
    topic = topic
  )

  method <- "findOne"

  if (allowMultiple) {
    method <- "find"
  }

  filterList <- filterList[!sapply(filterList, is.null)]


  content_json <- request_api(method, filterList)

  n_recipe <- get_distinct_recipes_json(content_json)

  if (n_recipe == 0) {
    stop(
      message(
        "The API returned no recipes"
      )
    )
  }

  message(
    glue::glue("The API returned {n_recipe} recipes")
  )


  if (n_recipe == 1) {
    recipe <- content_json$document[[1]]
    return(
      Recipe$new(
        name = unlist(recipe$name),
        user = unlist(recipe$user),
        edition = unlist(recipe$svy_edition),
        survey_type = unlist(recipe$svy_type),
        default_engine = default_engine(),
        depends_on = list(),
        description = unlist(recipe$description),
        steps = decode_step(recipe$steps),
        id = recipe[["_id"]],
        doi = unlist(recipe$DOI)
      )
    )
  } else {
    return(
      lapply(
        X = 1:n_recipe,
        FUN = function(x) {
          recipe <- content_json$documents[[x]]

          Recipe$new(
            name = unlist(recipe$name),
            user = unlist(recipe$user),
            edition = unlist(recipe$svy_edition),
            survey_type = unlist(recipe$svy_type),
            default_engine = default_engine(),
            depends_on = list(),
            description = unlist(recipe$description),
            steps = decode_step(recipe$steps),
            id = recipe[["_id"]],
            doi = unlist(recipe$doi)
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
#' @param doi A character string with the DOI of the recipe
#' @return A Recipe object
#' @keywords Survey methods
#' @keywords Recipes
#' @export

steps_to_recipe <- function(
    name,
    user,
    svy = survey_empty(type = "eaii", edition = "2019-2021"),
    description,
    steps, doi = NULL) {
  return(
    recipe(
      name = name,
      user = user,
      svy = svy,
      description = description,
      steps = lapply(
        steps,
        function(step) {
          (step$call)
        }
      ),
      doi = doi
    )
  )
}


get_distinct_recipes_json <- function(content_json) {
  tryCatch(
    {
      if (is.null(content_json$documents)) {
        return(1)
      } else {
        return(
          length(
            unique(
              sapply(
                X = seq_along(content_json$documents),
                FUN = function(x) {
                  content_json$documents[[x]][["_id"]]
                }
              )
            )
          )
        )
      }
    },
    error = function(e) {
      return(0)
    }
  )
}


get_distinct_recipes <- function(recipe) {
  tryCatch(
    {
      length(unique(
        sapply(
          X = seq_along(recipe),
          FUN = function(x) {
            recipe <- recipe[[x]]
            recipe$id
          }
        )
      ))
    },
    error = function(e) {
      return(0)
    }
  )
}

#' API Recipe
#' @noRd
#' @keywords internal

request_api <- function(method, filterList) {
  baseUrl <- url_api_host()

  url <- paste0(
    baseUrl,
    method
  )

  key <- get_api_key()

  headers <- switch(key$methodAuth,
    apiKey = {
      c(
        "Content-Type" = "application/json",
        "Access-Control-Request-Headers" = "*",
        "apiKey" = paste(
          key$token
        )
      )
    },
    anonUser = {
      c(
        "Content-Type" = "application/json",
        "Access-Control-Request-Headers" = "*",
        "Authorization" = paste(
          "Bearer",
          key$token
        )
      )
    },
    userPassword = {
      c(
        "Content-Type" = "application/json",
        "Access-Control-Request-Headers" = "*",
        "email" = key$email,
        "password" = key$password
      )
    }
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

  content <- content(response, "text", encoding = "UTF-8")

  switch(response$status_code,
    "400" = stop(
      message(
        "The API returned an error for a bad request: ",
        response$status
      )
    ),
    "401" = stop(
      message(
        "The API returned an error for unauthorized access: ",
        response$status
      )
    ),
    "403" = stop(
      message(
        "The API returned an error for forbidden access: ",
        response$status
      )
    ),
    "404" = stop(
      message(
        "The API returned an error for not found: ",
        response$status
      )
    )
  )


  return(
    content_json = parse_json(content)
  )
}
