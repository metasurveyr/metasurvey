# Tidyverse-style functional API for the Workflow Ecosystem

#' Search workflows
#'
#' Search for workflows by name or description in the active workflow backend.
#'
#' @param query Character search string.
#' @return List of matching RecipeWorkflow objects.
#'
#' @examples
#' set_workflow_backend("local", path = tempfile(fileext = ".json"))
#' results <- search_workflows("labor market")
#' length(results)
#'
#' @seealso \code{\link{filter_workflows}}, \code{\link{rank_workflows}}
#' @family tidy-api
#' @export
search_workflows <- function(query) {
  get_workflow_backend()$search(query)
}

#' Rank workflows by downloads
#'
#' Get the top workflows ranked by download count.
#'
#' @param n Integer or `NULL` (default `NULL`). Maximum number to return,
#'   or `NULL` for all.
#' @return List of RecipeWorkflow objects sorted by downloads.
#'
#' @examples
#' set_workflow_backend("local", path = tempfile(fileext = ".json"))
#' top5 <- rank_workflows(n = 5)
#' length(top5)
#'
#' @seealso \code{\link{search_workflows}}, \code{\link{filter_workflows}}
#' @family tidy-api
#' @export
rank_workflows <- function(n = NULL) {
  get_workflow_backend()$rank(n = n)
}

#' Filter workflows by criteria
#'
#' Filter workflows in the active backend by survey type, edition, recipe ID,
#' or certification level.
#'
#' @param survey_type Character survey type or `NULL` (default `NULL`).
#' @param edition Character edition or `NULL` (default `NULL`).
#' @param recipe_id Character recipe ID or `NULL` (default `NULL`). Find
#'   workflows using this recipe.
#' @param certification_level Character certification level or `NULL`
#'   (default `NULL`).
#' @return List of matching RecipeWorkflow objects.
#'
#' @examples
#' set_workflow_backend("local", path = tempfile(fileext = ".json"))
#' ech_wf <- filter_workflows(survey_type = "ech")
#' length(ech_wf)
#'
#' @seealso \code{\link{search_workflows}},
#'   \code{\link{find_workflows_for_recipe}}
#' @family tidy-api
#' @export
filter_workflows <- function(survey_type = NULL, edition = NULL,
                             recipe_id = NULL, certification_level = NULL) {
  get_workflow_backend()$filter(
    survey_type = survey_type, edition = edition,
    recipe_id = recipe_id, certification_level = certification_level
  )
}

#' List all workflows
#'
#' List all workflows from the active workflow backend.
#'
#' @return List of all RecipeWorkflow objects.
#'
#' @examples
#' set_workflow_backend("local", path = tempfile(fileext = ".json"))
#' all <- list_workflows()
#' length(all)
#'
#' @seealso \code{\link{search_workflows}}, \code{\link{filter_workflows}}
#' @family tidy-api
#' @export
list_workflows <- function() {
  get_workflow_backend()$list_all()
}

#' Find workflows that use a specific recipe
#'
#' Cross-reference query: find all workflows that reference a given recipe ID.
#'
#' @param recipe_id Character recipe ID to search for.
#' @return List of RecipeWorkflow objects that reference this recipe.
#'
#' @examples
#' set_workflow_backend("local", path = tempfile(fileext = ".json"))
#' wfs <- find_workflows_for_recipe("recipe_001")
#' length(wfs)
#'
#' @seealso \code{\link{filter_workflows}}
#' @family tidy-api
#' @export
find_workflows_for_recipe <- function(recipe_id) {
  get_workflow_backend()$find_by_recipe(recipe_id)
}

#' Publish a workflow to the active backend
#'
#' Publishes a RecipeWorkflow to the configured workflow backend.
#'
#' @param wf A RecipeWorkflow object.
#' @return NULL (called for side effect).
#'
#' @examples
#' set_workflow_backend("local", path = tempfile(fileext = ".json"))
#' wf <- RecipeWorkflow$new(
#'   name = "Example", description = "Test",
#'   survey_type = "ech", edition = "2023",
#'   recipe_ids = "r_001", estimation_type = "svymean"
#' )
#' publish_workflow(wf)
#'
#' @seealso \code{\link{set_workflow_backend}}, \code{\link{RecipeWorkflow}}
#' @family workflows
#' @export
publish_workflow <- function(wf) {
  get_workflow_backend()$publish(wf)
}
