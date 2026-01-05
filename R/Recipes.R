#' Recipe R6 class
#'
#' R6 class representing a reproducible data transformation recipe for
#' surveys. It encapsulates metadata, declared dependencies, and a list of
#' transformation steps to be applied to a Survey object.
#'
#' @name Recipe
#' @docType class
#' @format An R6 class generator (R6ClassGenerator)
#'
#' @field name Descriptive name of the recipe (character).
#' @field edition Target edition/period (character or Date).
#' @field survey_type Survey type (character), e.g., "ech", "eaii".
#' @field default_engine Default evaluation engine (character).
#' @field depends_on Vector/list of dependencies declared by the steps.
#' @field user Author/owner (character).
#' @field description Recipe description (character).
#' @field id Unique identifier (character/numeric).
#' @field steps List of step calls that make up the workflow.
#' @field doi DOI or external identifier (character|NULL).
#' @field bake Logical flag indicating whether it has been applied.
#' @field topic Recipe topic (character|NULL).
#'
#' @section Methods:
#' \describe{
#'   \item{$new(name, edition, survey_type, default_engine, depends_on, user, description, steps, id, doi, topic)}{Class constructor.}
#' }
#'
#' @seealso \code{\link{recipe}}, \code{\link{save_recipe}},
#'   \code{\link{read_recipe}}, \code{\link{bake_recipes}}
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
    #' @description
    #' Create a Recipe object
    #' @param name Descriptive name of the recipe (character)
    #' @param edition Target edition/period (character or Date)
    #' @param survey_type Survey type (character), e.g., "ech", "eaii"
    #' @param default_engine Default evaluation engine (character)
    #' @param depends_on Vector or list of declared dependencies
    #' @param user Author or owner of the recipe (character)
    #' @param description Detailed description of the recipe (character)
    #' @param steps List of step calls that make up the workflow
    #' @param id Unique identifier (character or numeric)
    #' @param doi DOI or external identifier (character or NULL)
    #' @param topic Recipe topic (character or NULL)
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

#' Crear receta de transformación de datos de encuesta
#'
#' Esta función crea un objeto Recipe que encapsula una secuencia de
#' transformaciones de datos que pueden ser aplicadas a encuestas de manera
#' reproducible. Las recetas permiten documentar, compartir y reutilizar
#' workflows de procesamiento de datos.
#'
#' @param ... Lista con la metadata requerida y steps opcionales. Los
#'   parámetros obligatorios son:
#'   \itemize{
#'     \item \code{name}: Nombre descriptivo de la receta
#'     \item \code{user}: Usuario/autor que crea la receta
#'     \item \code{svy}: Objeto Survey base (usar \code{survey_empty()} para recetas genéricas)
#'     \item \code{description}: Descripción detallada del propósito de la receta
#'   }
#'   Parámetros opcionales incluyen steps de transformación de datos.
#'
#' @return Objeto `Recipe` que contiene:
#'   \itemize{
#'     \item Metadata completa de la receta
#'     \item Lista de steps de transformación
#'     \item Información de dependencias
#'     \item Configuración de motor por defecto
#'   }
#'
#' @details
#' Las recetas son fundamentales para:
#' \itemize{
#'   \item Reproducibilidad: Garantizar que las transformaciones se apliquen consistentemente
#'   \item Documentación: Mantener registro de qué transformaciones se realizan y por qué
#'   \item Colaboración: Compartir workflows entre usuarios y equipos
#'   \item Versionado: Mantener diferentes versiones de procesamiento para distintas ediciones
#'   \item Automatización: Aplicar transformaciones complejas automáticamente
#' }
#'
#' Los steps incluidos en la receta pueden ser cualquier combinación de
#' \code{step_compute}, \code{step_recode}, u otros steps de transformación.
#'
#' Las recetas se pueden guardar con \code{save_recipe()}, cargar con
#' \code{read_recipe()}, y aplicar automáticamente con \code{bake_recipes()}.
#'
#' @examples
#' \dontrun{
#' # Receta básica sin steps
#' receta_base <- recipe(
#'   name = "Indicadores ECH Básicos",
#'   user = "Analista INE",
#'   svy = survey_empty(type = "ech", edition = "2023"),
#'   description = "Crea indicadores laborales básicos para ECH 2023"
#' )
#'
#' # Receta con steps incluidos
#' receta_completa <- recipe(
#'   name = "Mercado Laboral ECH",
#'   user = "Equipo Laboral",
#'   svy = survey_empty(type = "ech", edition = "2023"),
#'   description = "Análisis completo del mercado laboral uruguayo",
#'
#'   # Steps de transformación
#'   step_recode(
#'     condicion_actividad,
#'     POBPCOAC == 2 ~ "Ocupado",
#'     POBPCOAC %in% 3:5 ~ "Desocupado",
#'     POBPCOAC %in% 6:8 ~ "Inactivo",
#'     .default = "Sin dato"
#'   ),
#'   step_compute(
#'     tasa_actividad = (ocupados + desocupados) / poblacion_14_mas * 100,
#'     tasa_empleo = ocupados / poblacion_14_mas * 100,
#'     tasa_desempleo = desocupados / (ocupados + desocupados) * 100
#'   )
#' )
#'
#' # Aplicar receta a datos
#' ech_procesada <- load_survey(
#'   path = "ech_2023.dta",
#'   svy_type = "ech",
#'   svy_edition = "2023",
#'   recipes = receta_completa,
#'   bake = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link{Recipe}} para la definición de la clase
#' \code{\link{save_recipe}} para guardar recetas
#' \code{\link{read_recipe}} para cargar recetas
#' \code{\link{get_recipe}} para obtener recetas del repositorio
#' \code{\link{bake_recipes}} para aplicar recetas a datos
#'
#' @keywords Survey methods, Recipes
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
#' \dontrun{
#' # Example of saving a Recipe object
#' save_recipe(recipe_obj, "recipe.json")
#' }
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
    jsonlite::write_json(path = file, simplifyVector = TRUE)

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
#' @keywords utils
#' @examples
#' \dontrun{
#' # Example of reading a Recipe object
#' recipe_obj <- read_recipe("recipe.json")
#' print(recipe_obj)
#' }
#' @export

read_recipe <- function(file) {
  decode_step(jsonlite::read_json(file, simplifyVector = TRUE)$steps)
}

#' Get recipe from repository or API
#'
#' This function retrieves data transformation recipes from the metasurvey
#' repository or API, based on specific criteria such as survey type, edition,
#' and topic. It is the primary way to access predefined and community-validated
#' recipes.
#'
#' @param svy_type String specifying the survey type. Examples:
#'   "ech", "eaii", "eai", "eph"
#' @param svy_edition String specifying the survey edition.
#'   Supported formats: "YYYY", "YYYYMM", "YYYY-YYYY"
#' @param topic String specifying the recipe topic. Examples:
#'   "labor_market", "poverty", "income", "demographics"
#' @param allowMultiple Logical indicating whether multiple recipes are allowed.
#'   If FALSE and multiple matches exist, returns the most recent one
#'
#' @return `Recipe` object or list of `Recipe` objects according to the
#'   specified criteria and the value of \code{allowMultiple}
#'
#' @details
#' This function is essential for:
#' \itemize{
#'   \item Accessing official recipes: Get validated and maintained recipes
#'     by specialized teams
#'   \item Reproducibility: Ensure different users apply the same standard
#'     transformations
#'   \item Automation: Integrate recipes into automatic pipelines
#'   \item Collaboration: Share methodologies between teams and organizations
#'   \item Versioning: Access different recipe versions according to edition
#' }
#'
#' The function first searches in local repositories and then queries the API
#' if necessary. Recipes are cached to improve performance.
#'
#' Search criteria are combined with AND operator, so all specified criteria
#' must match for a recipe to be returned.
#'
#' @examples
#' \dontrun{
#' # Get specific recipe for ECH 2023
#' ech_recipe <- get_recipe(
#'   svy_type = "ech",
#'   svy_edition = "2023"
#' )
#'
#' # Recipe for specific topic
#' labor_recipe <- get_recipe(
#'   svy_type = "ech",
#'   svy_edition = "2023",
#'   topic = "labor_market"
#' )
#'
#' # Allow multiple recipes
#' available_recipes <- get_recipe(
#'   svy_type = "eaii",
#'   svy_edition = "2019-2021",
#'   allowMultiple = TRUE
#' )
#'
#' # Use recipe in load_survey
#' ech_with_recipe <- load_survey(
#'   path = "ech_2023.dta",
#'   svy_type = "ech",
#'   svy_edition = "2023",
#'   recipes = get_recipe("ech", "2023"),
#'   bake = TRUE
#' )
#'
#' # For year ranges
#' panel_recipe <- get_recipe(
#'   svy_type = "ech_panel",
#'   svy_edition = "2020-2023"
#' )
#' }
#'
#' @seealso
#' \code{\link{recipe}} to create custom recipes
#' \code{\link{save_recipe}} to save recipes locally
#' \code{\link{read_recipe}} to read recipes from file
#' \code{\link{publish_recipe}} to publish recipes to the repository
#' \code{\link{load_survey}} where recipes are used
#'
#' @keywords utils
#' @export

get_recipe <- function(
  svy_type = NULL,
  svy_edition = NULL,
  topic = NULL,
  allowMultiple = TRUE
) {
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
#' @keywords Recipes
#' @export

steps_to_recipe <- function(
  name,
  user,
  svy = survey_empty(type = "eaii", edition = "2019-2021"),
  description,
  steps, doi = NULL, topic = NULL
) {
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
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite parse_json
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

#' @title Publish Recipe
#' @description Publishes a Recipe object to the API.
#' @param recipe A Recipe object.
#' @return A JSON object containing the API response.
#' @details This function sends a Recipe object to the API for publication.
#' @examples
#' \dontrun{
#' # Example of publishing a Recipe object to the API
#' publish_recipe(recipe_obj)
#' }
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
