#' @title WorkflowBackend
#' @description Backend-agnostic factory for workflow storage and retrieval.
#' Supports "local" (JSON-backed WorkflowRegistry) and "mongo" (MongoDB Atlas API) backends.
#'
#' @field type Character backend type ("local" or "mongo").
#'
#' @examples
#' \dontrun{
#' # Local backend
#' backend <- WorkflowBackend$new("local", path = "workflows.json")
#' backend$publish(my_workflow)
#' backend$search("labor")
#' }
#'
#' @export
WorkflowBackend <- R6::R6Class(
  "WorkflowBackend",
  public = list(
    type = NULL,

    #' @description Create a new WorkflowBackend
    #' @param type Character. "local" or "mongo".
    #' @param path Character. File path for local backend (optional).
    initialize = function(type, path = NULL) {
      valid_types <- c("local", "mongo")
      if (!(type %in% valid_types)) {
        stop("Backend type must be one of: ", paste(valid_types, collapse = ", "))
      }
      self$type <- type
      if (type == "local") {
        private$.path <- path
        private$.registry <- WorkflowRegistry$new()
        if (!is.null(path) && file.exists(path)) {
          private$.registry$load(path)
        }
      }
    },

    #' @description Publish a workflow to the backend
    #' @param wf RecipeWorkflow object
    publish = function(wf) {
      if (self$type == "local") {
        private$.registry$register(wf)
        if (!is.null(private$.path)) {
          private$.registry$save(private$.path)
        }
      } else if (self$type == "mongo") {
        # MongoDB publication via Data API
        wf_data <- wf$to_list()
        api_url <- paste0(url_api_host(), "insertOne")
        key <- get_api_key()
        headers <- switch(key$methodAuth,
          apiKey = c("Content-Type" = "application/json",
                     "Access-Control-Request-Headers" = "*",
                     "apiKey" = key$token),
          anonUser = c("Content-Type" = "application/json",
                       "Access-Control-Request-Headers" = "*",
                       "Authorization" = paste("Bearer", key$token)),
          userPassword = c("Content-Type" = "application/ejson",
                           "Accept" = "application/json",
                           "email" = key$email,
                           "password" = key$password)
        )
        payload <- list(
          dataSource = "Cluster0",
          database = "metasurvey",
          collection = "workflows",
          document = wf_data
        )
        response <- httr::POST(
          url = api_url,
          httr::add_headers(headers),
          body = jsonlite::toJSON(payload, auto_unbox = TRUE),
          encode = "json"
        )
        if (response$status_code < 300) {
          message("Workflow published to metasurvey API. Status: ", response$status_code)
        } else {
          stop("Failed to publish workflow. Status: ", response$status_code)
        }
      }
    },

    #' @description Search workflows
    #' @param query Character search string
    #' @return List of matching RecipeWorkflow objects
    search = function(query) {
      if (self$type == "local") {
        private$.registry$search(query)
      } else if (self$type == "mongo") {
        list() # TODO: mongo search
      }
    },

    #' @description Get a workflow by id
    #' @param id Workflow id
    #' @return RecipeWorkflow object or NULL
    get = function(id) {
      if (self$type == "local") {
        private$.registry$get(id)
      } else if (self$type == "mongo") {
        NULL # TODO: mongo get
      }
    },

    #' @description Increment download count for a workflow
    #' @param id Workflow id
    increment_downloads = function(id) {
      if (self$type == "local") {
        w <- private$.registry$get(id)
        if (!is.null(w)) {
          w$increment_downloads()
          if (!is.null(private$.path)) {
            private$.registry$save(private$.path)
          }
        }
      }
    },

    #' @description Find workflows that reference a specific recipe
    #' @param recipe_id Character recipe ID
    #' @return List of RecipeWorkflow objects
    find_by_recipe = function(recipe_id) {
      if (self$type == "local") {
        private$.registry$find_by_recipe(recipe_id)
      } else if (self$type == "mongo") {
        list() # TODO: mongo query
      }
    },

    #' @description Rank workflows by downloads
    #' @param n Integer max to return
    #' @return List of RecipeWorkflow objects
    rank = function(n = NULL) {
      if (self$type == "local") {
        private$.registry$rank_by_downloads(n)
      } else if (self$type == "mongo") {
        list()
      }
    },

    #' @description Filter workflows by criteria
    #' @param svy_type Character or NULL
    #' @param edition Character or NULL
    #' @param recipe_id Character or NULL
    #' @param certification_level Character or NULL
    #' @return List of matching RecipeWorkflow objects
    filter = function(svy_type = NULL, edition = NULL, recipe_id = NULL, certification_level = NULL) {
      if (self$type == "local") {
        private$.registry$filter(svy_type = svy_type, edition = edition,
                                  recipe_id = recipe_id, certification_level = certification_level)
      } else if (self$type == "mongo") {
        list()
      }
    },

    #' @description List all workflows
    #' @return List of RecipeWorkflow objects
    list_all = function() {
      if (self$type == "local") {
        private$.registry$list_all()
      } else if (self$type == "mongo") {
        list()
      }
    },

    #' @description Save local backend to disk
    save = function() {
      if (self$type == "local" && !is.null(private$.path)) {
        private$.registry$save(private$.path)
      }
    },

    #' @description Load local backend from disk
    load = function() {
      if (self$type == "local" && !is.null(private$.path) && file.exists(private$.path)) {
        private$.registry$load(private$.path)
      }
    }
  ),
  private = list(
    .registry = NULL,
    .path = NULL
  )
)

#' @title Set workflow backend
#' @description Configure the active workflow backend via options.
#' @param type Character. "local" or "mongo".
#' @param path Character. File path for local backend.
#' @examples
#' \dontrun{
#' set_workflow_backend("local", path = "my_workflows.json")
#' }
#' @export
set_workflow_backend <- function(type, path = NULL) {
  backend <- WorkflowBackend$new(type, path = path)
  options(metasurvey.workflow_backend = backend)
}

#' @title Get workflow backend
#' @description Returns the currently configured workflow backend.
#' Defaults to "local" if not configured.
#' @return WorkflowBackend object
#' @examples
#' \dontrun{
#' backend <- get_workflow_backend()
#' backend$search("labor")
#' }
#' @export
get_workflow_backend <- function() {
  backend <- getOption("metasurvey.workflow_backend")
  if (is.null(backend)) {
    backend <- WorkflowBackend$new("local")
  }
  backend
}
