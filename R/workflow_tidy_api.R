# Tidyverse-style functional API for the Workflow Ecosystem

#' Search workflows
#'
#' Search for workflows by name or description in the active workflow backend.
#'
#' @param query Character search string.
#' @return List of matching RecipeWorkflow objects.
#'
#' @examples
#' \dontrun{
#' results <- search_workflows("labor market")
#' }
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
#' @param n Integer. Maximum number to return, or NULL for all.
#' @return List of RecipeWorkflow objects sorted by downloads.
#'
#' @examples
#' \dontrun{
#' top5 <- rank_workflows(n = 5)
#' }
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
#' @param survey_type Character survey type or NULL.
#' @param edition Character edition or NULL.
#' @param recipe_id Character recipe ID or NULL (find
#'   workflows using this recipe).
#' @param certification_level Character certification level or NULL.
#' @return List of matching RecipeWorkflow objects.
#'
#' @examples
#' \dontrun{
#' ech_wf <- filter_workflows(survey_type = "ech")
#' for_recipe <- filter_workflows(recipe_id = "recipe_001")
#' }
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
#' \dontrun{
#' all <- list_workflows()
#' }
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
#' \dontrun{
#' wfs <- find_workflows_for_recipe("recipe_001")
#' }
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
#' \dontrun{
#' set_workflow_backend("local", path = "workflows.json")
#' publish_workflow(my_workflow)
#' }
#'
#' @seealso \code{\link{set_workflow_backend}}, \code{\link{RecipeWorkflow}}
#' @family workflows
#' @export
publish_workflow <- function(wf) {
  get_workflow_backend()$publish(wf)
}
