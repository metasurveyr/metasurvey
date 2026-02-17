#' @title WorkflowRegistry
#' @description Local JSON-backed catalog for workflow
#'   discovery, ranking, and filtering.
#'
#' @examples
#' # Use the tidy API instead:
#' # set_workflow_backend("local", path = "workflows.json")
#' # publish_workflow(my_workflow)
#' # search_workflows("labor")
#'
#' @keywords internal
WorkflowRegistry <- R6::R6Class(
  "WorkflowRegistry",
  public = list(
    #' @description Create a new empty WorkflowRegistry
    initialize = function() {
      private$.workflows <- list()
    },

    #' @description Register a workflow in the catalog
    #' @param wf RecipeWorkflow object to register
    register = function(wf) {
      if (!inherits(wf, "RecipeWorkflow")) {
        stop("Can only register RecipeWorkflow objects")
      }
      id <- as.character(wf$id)
      private$.workflows[[id]] <- wf
    },

    #' @description Remove a workflow from the catalog by id
    #' @param workflow_id Workflow id to remove
    unregister = function(workflow_id) {
      id <- as.character(workflow_id)
      private$.workflows[[id]] <- NULL
    },

    #' @description Search workflows by name or description (case-insensitive)
    #' @param query Character search query
    #' @return List of matching RecipeWorkflow objects
    search = function(query) {
      pattern <- tolower(query)
      Filter(function(w) {
        grepl(pattern, tolower(w$name)) ||
          grepl(pattern, tolower(w$description))
      }, private$.workflows)
    },

    #' @description Filter workflows by criteria
    #' @param survey_type Character survey type or NULL
    #' @param edition Character edition or NULL
    #' @param recipe_id Character recipe ID or NULL
    #'   (find workflows using this recipe)
    #' @param certification_level Character certification level or NULL
    #' @return List of matching RecipeWorkflow objects
    filter = function(survey_type = NULL,
                      edition = NULL,
                      recipe_id = NULL,
                      certification_level = NULL) {
      results <- private$.workflows
      if (!is.null(survey_type)) {
        results <- Filter(function(w) w$survey_type == survey_type, results)
      }
      if (!is.null(edition)) {
        results <- Filter(function(w) w$edition == edition, results)
      }
      if (!is.null(recipe_id)) {
        results <- Filter(function(w) recipe_id %in% w$recipe_ids, results)
      }
      if (!is.null(certification_level)) {
        results <- Filter(
          function(w) {
            w$certification$level == certification_level
          },
          results
        )
      }
      results
    },

    #' @description Find workflows that reference a specific recipe
    #' @param recipe_id Character recipe ID
    #' @return List of RecipeWorkflow objects referencing this recipe
    find_by_recipe = function(recipe_id) {
      self$filter(recipe_id = recipe_id)
    },

    #' @description Rank workflows by download count (descending)
    #' @param n Integer max number to return, or NULL for all
    #' @return List of RecipeWorkflow objects sorted by downloads
    rank_by_downloads = function(n = NULL) {
      workflows <- private$.workflows
      if (length(workflows) == 0) {
        return(list())
      }
      downloads <- vapply(
        workflows, function(w) w$downloads, integer(1)
      )
      ordered <- workflows[order(downloads, decreasing = TRUE)]
      if (!is.null(n)) {
        ordered <- ordered[seq_len(min(n, length(ordered)))]
      }
      ordered
    },

    #' @description Get a single workflow by id
    #' @param workflow_id Workflow id
    #' @return RecipeWorkflow object or NULL
    get = function(workflow_id) {
      id <- as.character(workflow_id)
      private$.workflows[[id]]
    },

    #' @description List all registered workflows
    #' @return List of all RecipeWorkflow objects
    list_all = function() {
      private$.workflows
    },

    #' @description Save the registry catalog to a JSON file
    #' @param path Character file path
    save = function(path) {
      wf_data <- lapply(private$.workflows, function(w) w$to_list())
      jsonlite::write_json(
        wf_data, path,
        auto_unbox = TRUE, pretty = TRUE
      )
    },

    #' @description Load a registry catalog from a JSON file
    #' @param path Character file path
    load = function(path) {
      json_data <- jsonlite::read_json(path)
      private$.workflows <- list()
      for (item in json_data) {
        wf <- workflow_from_list(item)
        self$register(wf)
      }
    },

    #' @description Get registry statistics
    #' @return List with total, by_survey_type, by_certification counts
    stats = function() {
      workflows <- private$.workflows
      total <- length(workflows)

      by_survey_type <- list()
      for (w in workflows) {
        st <- w$survey_type
        by_survey_type[[st]] <- (by_survey_type[[st]] %||% 0L) + 1L
      }

      by_certification <- list()
      for (w in workflows) {
        lvl <- w$certification$level
        by_certification[[lvl]] <- (by_certification[[lvl]] %||% 0L) + 1L
      }

      list(
        total = total,
        by_survey_type = by_survey_type,
        by_certification = by_certification
      )
    },

    #' @description Print registry summary
    #' @param ... Additional arguments (not used)
    print = function(...) {
      s <- self$stats()
      cat(
        cli::style_bold("WorkflowRegistry"),
        paste0("(", s$total, " workflows)\n")
      )
      if (length(s$by_survey_type) > 0) {
        cat(
          "  Survey types:",
          paste(names(s$by_survey_type), collapse = ", "),
          "\n"
        )
      }
      if (length(s$by_certification) > 0) {
        cat("  Certification:", paste(
          vapply(
            names(s$by_certification),
            function(k) {
              paste0(k, ": ", s$by_certification[[k]])
            },
            character(1)
          ),
          collapse = ", "
        ), "\n")
      }
      invisible(self)
    }
  ),
  private = list(
    .workflows = list()
  )
)
