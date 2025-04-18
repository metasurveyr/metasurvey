#' @title Recipe Class
#' @rdname Recipe_class
#' @description This class represents a recipe for processing survey data. A recipe includes metadata such as name, edition, survey type, user, description, and a list of steps to be applied to the survey data.
#' @field name A string representing the name of the recipe.
#' @field edition A string indicating the edition of the survey associated with the recipe.
#' @field survey_type A string specifying the type of survey (e.g., "eaii", "ech").
#' @field default_engine A string indicating the default engine used for processing.
#' @field depends_on A list of dependencies required by the recipe.
#' @field user A string representing the user or creator of the recipe.
#' @field description A string providing a description of the recipe.
#' @field id A unique identifier for the recipe.
#' @field steps A list of steps to be applied to the survey data.
#' @field doi A string representing the DOI (Digital Object Identifier) of the recipe.
#' @field bake A logical value indicating whether the recipe should be baked (processed) immediately.
#' @field topic A string specifying the topic of the recipe.
#' @keywords Recipes
#' @export
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
    topic = NULL,

    #' @description Initializes a new instance of the Recipe class.
    #' @param name A string representing the name of the recipe.
    #' @param edition A string indicating the edition of the survey associated with the recipe.
    #' @param survey_type A string specifying the type of survey (e.g., "eaii", "ech").
    #' @param default_engine A string indicating the default engine used for processing.
    #' @param depends_on A list of dependencies required by the recipe.
    #' @param user A string representing the user or creator of the recipe.
    #' @param description A string providing a description of the recipe.
    #' @param steps A list of steps to be applied to the survey data.
    #' @param id A unique identifier for the recipe.
    #' @param doi A string representing the DOI (Digital Object Identifier) of the recipe.
    #' @param topic A string specifying the topic of the recipe.
    initialize = function(name, edition, survey_type, default_engine, depends_on, user, description, steps, id, doi, topic) {
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
      self$topic <- topic
    }
  )
)

#' @title Metadata for Recipe
#' @description Returns the metadata fields required for a recipe.
#' @return A character vector of metadata field names.
#' @keywords internal
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

#' @title Recipe
#' @description Creates a new Recipe object for processing survey data.
#' @param ... A list with the following metadata: name, user, svy, description.
#' @return A Recipe object.
#' @keywords Survey methods, Recipes
#' @seealso See \code{\link{Recipe}} for the class definition.
#' @details This function validates the provided metadata and creates a Recipe object. If steps are included, they are added to the Recipe object.
#' @examples
#' # Example of creating a Recipe object
#' recipe_obj <- recipe(
#'   name = "Example Recipe",
#'   user = "Metasurvey User",
#'   svy = survey_empty(type = "eaii", edition = "2019-2021"),
#'   description = "This is an example recipe."
#' )
#' print(recipe_obj)
#' @export
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
        depends_on = unique(sapply(
          X = dots$steps,
          FUN = function(step) {
            step$depends_on
          }
        )),
        description = dots$description,
        steps = dots$steps_call,
        doi = dots$doi %||% NULL,
        topic = dots$topic
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
        doi = dots$doi %||% NULL,
        topic = dots$topic
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

#' @title Save Recipe
#' @description Saves a Recipe object to a file in JSON format.
#' @param recipe A Recipe object.
#' @param file A character string specifying the file path.
#' @return NULL.
#' @keywords utils
#' @details This function encodes the Recipe object and writes it to a JSON file.
#' @examples
#' # Example of saving a Recipe object
#' save_recipe(recipe_obj, "recipe.json")
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

#' recipe to json
#' @param recipe A Recipe object
#' @return A JSON object
#' @keywords Survey methods
#' @keywords Recipes

recipe_to_json <- function(recipe) {
  recipe <- list(
    name = recipe$name,
    user = recipe$user,
    svy_type = recipe$svy_type,
    edition = recipe$edition,
    description = recipe$description,
    steps = recipe$steps
  )

  recipe |>
    encoding_recipe() |>
    jsonlite::toJSON(simplifyVector = TRUE, raw = "mongo")
}

#' @title Read Recipe
#' @description Reads a Recipe object from a JSON file.
#' @param file A character string specifying the file path.
#' @return A Recipe object.
#' @details This function reads a JSON file and decodes it into a Recipe object.
#' @examples
#' # Example of reading a Recipe object
#' recipe_obj <- read_recipe("recipe.json")
#' print(recipe_obj)
#' @export
#' @keywords utils
read_recipe <- function(file) {
  decode_step(read_json(file, simplifyVector = TRUE)$steps)
}

#' @title Get Recipe
#' @description Retrieves a Recipe object from the API based on specified criteria.
#' @param svy_type A character string specifying the survey type.
#' @param svy_edition A character string specifying the survey edition.
#' @param topic A character string specifying the topic of the recipe.
#' @param allowMultiple A logical value indicating whether to allow multiple recipes.
#' @return A Recipe object or a list of Recipe objects.
#' @details This function queries the API to retrieve recipes matching the specified criteria.
#' @examples
#' # Example of retrieving a Recipe object from the API
#' recipe_obj <- get_recipe(svy_type = "eaii", svy_edition = "2019-2021")
#' print(recipe_obj)
#' @keywords utils
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
        depends_on = unlist(recipe$depends_on),
        description = unlist(recipe$description),
        steps = decode_step(recipe$steps),
        id = recipe[["_id"]],
        doi = unlist(recipe$DOI),
        topic = unlist(recipe$topic)
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
            doi = unlist(recipe$doi),
            topic = unlist(recipe$topic)
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
#' @param topic A character string with the topic of the recipe
#' @keywords Steps
#' @keywords Survey methods
#' @return A Recipe object
#' @keywords Survey methods
#' @keywords Recipes, utils
#' @export

steps_to_recipe <- function(
    name,
    user,
    svy = survey_empty(type = "eaii", edition = "2019-2021"),
    description,
    steps, doi = NULL, topic = NULL) {
  return(
    recipe(
      name = name,
      user = user,
      svy = svy,
      description = description,
      steps = steps,
      steps_call = eval(lapply(
        steps,
        function(step) {
          deparse(step$call)
        }
      )),
      doi = doi,
      topic = topic
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
#' @importFrom httr add_headers
#' @importFrom jsonlite parse_json
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

#' @title Publish Recipe
#' @description Publishes a Recipe object to the API.
#' @param recipe A Recipe object.
#' @return A JSON object containing the API response.
#' @details This function sends a Recipe object to the API for publication.
#' @examples
#' # Example of publishing a Recipe object to the API
#' publish_recipe(recipe_obj)
#' @keywords utils
#' @export
publish_recipe <- function(recipe) {
  recipe <- list(
    name = recipe$name,
    user = recipe$user,
    description = recipe$description,
    svy_type = recipe$svy_type,
    svy_edition = recipe$edition,
    steps = recipe$steps,
    topic = recipe$topic,
    doi = recipe$doi,
    depends_on = unlist(recipe$depends_on)
  )

  recipe <- recipe

  api_url <- paste0(url_api_host(), "insertOne")
  database_name <- "metasurvey"
  collection_name <- "recipes"
  key <- get_api_key()

  # Verificar que el JSON no esté vacío
  if (is.null(recipe) || length(recipe) == 0) {
    stop("No recipe data provided.")
  }

  # Estructurar el payload para MongoDB Atlas Data API
  payload <- list(
    dataSource = "Cluster0", # Reemplaza con el nombre de tu clúster si es diferente
    database = database_name,
    collection = collection_name,
    document = recipe
  )

  # Estructurar encabezados de acuerdo al método de autenticación
  headers <- switch(key$methodAuth,
    apiKey = c(
      "Content-Type" = "application/json",
      "Access-Control-Request-Headers" = "*",
      "apiKey" = key$token
    ),
    anonUser = c(
      "Content-Type" = "application/json",
      "Access-Control-Request-Headers" = "*",
      "Authorization" = paste("Bearer", key$token)
    ),
    userPassword = c(
      "Content-Type" = "application/ejson",
      "Accept" = "application/json",
      "email" = key$email,
      "password" = key$password
    )
  )

  # Realizar la solicitud POST a la Data API de MongoDB Atlas
  response <- tryCatch(
    {
      POST(
        url = api_url,
        add_headers(headers),
        body = jsonlite::toJSON(payload, auto_unbox = TRUE),
        encode = "json"
      )
    },
    error = function(e) {
      stop("Failed to publish recipe to MongoDB Atlas Data API: ", e$message)
    }
  )

  # Verificar la respuesta de la API
  if (response$status_code < 300) {
    message("Recipe successfully published to metasurvey API. Thanks for your contribution :). Status code: ", response$status_code)
    return(content(response, "parsed")) # Devuelve el contenido de la respuesta
  } else {
    stop(
      "Failed to publish recipe. Status code: ", response$status_code,
      " - ", content(response, "text", encoding = "UTF-8")
    )
  }
}
