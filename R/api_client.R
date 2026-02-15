#' @title API Client for metasurvey
#' @description Simple HTTP client for metasurvey serverless API
#' @keywords internal

# Get configured API URL
api_url <- function() {
  getOption("metasurvey.api_url", default = NULL)
}

# Get configured API key for admin/write operations
api_key <- function() {
  getOption("metasurvey.api_key", default = NULL)
}

# Get user JWT token
api_token <- function() {
  getOption("metasurvey.api_token", default = NULL)
}

#' @title Configure metasurvey API
#' @description Set API URL and key for remote recipe/workflow access
#' @param url API base URL (e.g., "https://your-project.vercel.app/api")
#' @param key API key for admin operations (optional)
#' @export
#' @examples
#' \dontrun{
#' configure_api(
#'   url = "https://metasurvey-api.vercel.app/api"
#' )
#' }
configure_api <- function(url, key = NULL) {
  options(metasurvey.api_url = url)
  if (!is.null(key)) {
    options(metasurvey.api_key = key)
  }
  message("API configured: ", url)
  invisible(NULL)
}

# Internal HTTP request helper
api_request <- function(endpoint, method = "GET", body = NULL, params = NULL) {
  base_url <- api_url()
  
  if (is.null(base_url)) {
    stop(
      "API not configured. Use configure_api() first:\n",
      "  configure_api(url = 'https://your-project.vercel.app/api')"
    )
  }
  
  url <- paste0(base_url, "/", endpoint)
  
  # Add query parameters
  if (!is.null(params)) {
    query_string <- paste(
      names(params), 
      sapply(params, utils::URLencode, reserved = TRUE),
      sep = "=",
      collapse = "&"
    )
    url <- paste0(url, "?", query_string)
  }
  
  # Prepare headers
  headers <- c("Content-Type" = "application/json")
  
  # Add JWT token first (preferred), fallback to API key
  token <- api_token()
  if (!is.null(token)) {
    headers <- c(headers, "Authorization" = paste("Bearer", token))
  } else if (method %in% c("POST", "PUT", "DELETE")) {
    # Fallback to API key for write operations
    key <- api_key()
    if (!is.null(key)) {
      headers <- c(headers, "x-api-key" = key)
    }
  }
  
  # Make request
  tryCatch({
    if (method == "GET") {
      resp <- httr::GET(
        url,
        httr::add_headers(.headers = headers),
        httr::timeout(10)
      )
    } else if (method == "POST") {
      resp <- httr::POST(
        url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
        httr::add_headers(.headers = headers),
        encode = "raw",
        httr::timeout(10)
      )
    } else if (method == "PUT") {
      resp <- httr::PUT(
        url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
        httr::add_headers(.headers = headers),
        encode = "raw",
        httr::timeout(10)
      )
    } else if (method == "DELETE") {
      resp <- httr::DELETE(
        url,
        httr::add_headers(.headers = headers),
        httr::timeout(10)
      )
    } else {
      stop("Unsupported HTTP method: ", method)
    }
    
    # Parse response
    if (httr::status_code(resp) >= 400) {
      content <- httr::content(resp, "text", encoding = "UTF-8")
      stop("API error (", httr::status_code(resp), "): ", content)
    }
    
    result <- jsonlite::fromJSON(
      httr::content(resp, "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
    
    return(result)
    
  }, error = function(e) {
    stop("API request failed: ", e$message)
  })
}

#' @title List recipes from API
#' @description Fetch recipes from remote API with optional filters
#' @param user Filter by user email
#' @param edition Filter by survey edition
#' @param survey_type Filter by survey type
#' @param topic Filter by topic
#' @param limit Maximum number of results (default: 100)
#' @return List of Recipe objects
#' @export
#' @examples
#' \dontrun{
#' # List all recipes
#' recipes <- api_list_recipes()
#' 
#' # Filter by survey type
#' ech_recipes <- api_list_recipes(survey_type = "ech", edition = "2023")
#' }
api_list_recipes <- function(user = NULL, edition = NULL, survey_type = NULL, 
                             topic = NULL, limit = 100) {
  params <- list(limit = limit)
  if (!is.null(user)) params$user <- user
  if (!is.null(edition)) params$edition <- edition
  if (!is.null(survey_type)) params$survey_type <- survey_type
  if (!is.null(topic)) params$topic <- topic
  
  result <- api_request("recipes", method = "GET", params = params)
  
  # Parse recipes
  recipes <- lapply(result$recipes, function(doc) {
    tryCatch(
      # Use existing parser from RecipeAPI
      parse_recipe_from_json(doc),
      error = function(e) {
        warning("Failed to parse recipe: ", e$message)
        NULL
      }
    )
  })
  
  Filter(Negate(is.null), recipes)
}

#' @title Get single recipe from API
#' @param id Recipe ID
#' @return Recipe object or NULL
#' @export
api_get_recipe <- function(id) {
  result <- api_request("recipes", method = "GET", params = list(id = id))
  
  if (length(result$recipes) == 0) {
    return(NULL)
  }
  
  tryCatch(
    parse_recipe_from_json(result$recipes[[1]]),
    error = function(e) {
      warning("Failed to parse recipe: ", e$message)
      NULL
    }
  )
}

#' @title Publish recipe to API
#' @param recipe Recipe object
#' @return API response
#' @export
api_publish_recipe <- function(recipe) {
  if (!inherits(recipe, "Recipe")) {
    stop("recipe must be a Recipe object")
  }
  
  # Convert to JSON-friendly format
  recipe_data <- recipe$to_list()
  
  result <- api_request("recipes", method = "POST", body = recipe_data)
  
  if (isTRUE(result$ok)) {
    message("Recipe published successfully: ", result$id)
  }
  
  invisible(result)
}

#' @title List workflows from API
#' @param user Filter by user email
#' @param edition Filter by edition
#' @param survey_type Filter by survey type
#' @param recipe_id Filter by recipe ID
#' @param limit Maximum results
#' @return List of RecipeWorkflow objects
#' @export
api_list_workflows <- function(user = NULL, edition = NULL, survey_type = NULL,
                               recipe_id = NULL, limit = 100) {
  params <- list(limit = limit)
  if (!is.null(user)) params$user <- user
  if (!is.null(edition)) params$edition <- edition
  if (!is.null(survey_type)) params$survey_type <- survey_type
  if (!is.null(recipe_id)) params$recipe_id <- recipe_id
  
  result <- api_request("workflows", method = "GET", params = params)
  
  workflows <- lapply(result$workflows, function(doc) {
    tryCatch(
      workflow_from_list(doc),
      error = function(e) {
        warning("Failed to parse workflow: ", e$message)
        NULL
      }
    )
  })
  
  Filter(Negate(is.null), workflows)
}

#' @title Publish workflow to API
#' @param workflow RecipeWorkflow object
#' @return API response
#' @export
api_publish_workflow <- function(workflow) {
  if (!inherits(workflow, "RecipeWorkflow")) {
    stop("workflow must be a RecipeWorkflow object")
  }
  
  workflow_data <- workflow$to_list()
  
  result <- api_request("workflows", method = "POST", body = workflow_data)
  
  if (isTRUE(result$ok)) {
    message("Workflow published successfully: ", result$id)
  }
  
  invisible(result)
}

#' @title Increment download counter
#' @param type "recipe" or "workflow"
#' @param id Object ID
#' @keywords internal
api_track_download <- function(type, id) {
  tryCatch(
    api_request("stats", method = "POST", body = list(type = type, id = id)),
    error = function(e) {
      # Silent failure - tracking is not critical
      invisible(NULL)
    }
  )
}

# Helper to parse recipe from JSON (reuse existing logic or implement)
parse_recipe_from_json <- function(doc) {
  # This would use the existing Recipe$from_list() or similar
  # For now, placeholder
  Recipe$new(
    name = doc$name %||% "Unnamed",
    user = doc$user %||% "Unknown",
    edition = doc$svy_edition %||% doc$edition %||% "Unknown",
    survey_type = doc$svy_type %||% doc$survey_type %||% "Unknown",
    default_engine = "data.table",
    depends_on = doc$depends_on %||% list(),
    description = doc$description %||% "",
    steps = doc$steps %||% list(),
    id = doc[["_id"]] %||% doc$id,
    doi = doc$doi,
    topic = doc$topic
  )
}
