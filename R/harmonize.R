# Topological sort of recipes by depends_on_recipes (Kahn's algorithm)
#
# @param recipes List of Recipe objects
# @return List of Recipe objects in dependency order
# @noRd
topo_sort_recipes <- function(recipes) {
  if (length(recipes) == 0) {
    return(list())
  }

  ids <- vapply(recipes, function(r) as.character(r$id), character(1))
  recipe_map <- stats::setNames(recipes, ids)

  # Build adjacency: for each recipe, which recipes in the set depend on it
  in_degree <- stats::setNames(rep(0L, length(ids)), ids)
  # edges: dependency -> dependent (dep must come before dependent)
  adj <- stats::setNames(
    replicate(length(ids), character(0), simplify = FALSE), ids
  )

  for (r in recipes) {
    rid <- as.character(r$id)
    deps <- as.character(unlist(r$depends_on_recipes))
    # Only consider deps that are in the current set
    internal_deps <- deps[deps %in% ids]
    in_degree[[rid]] <- length(internal_deps)
    for (d in internal_deps) {
      adj[[d]] <- c(adj[[d]], rid)
    }
  }

  # Kahn's algorithm
  queue <- ids[in_degree == 0L]
  sorted <- character(0)

  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    sorted <- c(sorted, current)
    for (neighbor in adj[[current]]) {
      in_degree[[neighbor]] <- in_degree[[neighbor]] - 1L
      if (in_degree[[neighbor]] == 0L) {
        queue <- c(queue, neighbor)
      }
    }
  }

  if (length(sorted) != length(ids)) {
    stop(
      "Cycle detected in recipe dependencies: cannot sort recipes ",
      paste(ids[!ids %in% sorted], collapse = ", "),
      call. = FALSE
    )
  }

  recipe_map[sorted]
}


#' Harmonize surveys using registry recipes
#'
#' For each survey in the list, fetches matching recipes from the active
#' backend (by survey type and edition), sorts them by dependency order,
#' applies them via \code{\link{bake_recipes}}, and assembles the results
#' into a \code{\link{PoolSurvey}} ready for \code{\link{workflow}}.
#'
#' @param surveys List of \code{\link{Survey}} objects to harmonize.
#' @param survey_type Character. Override survey type for recipe lookup.
#'   If \code{NULL} (default), uses each survey's own \code{type} field.
#' @param topic Character. Filter recipes by topic
#'   (e.g. \code{"compatibilizada"}). Default \code{NULL} (no filter).
#' @param category Character. Optional category filter for recipes.
#' @param certification_level Character. Optional certification level
#'   filter (e.g. \code{"official"}, \code{"reviewed"}).
#' @param grouping Character. Time hierarchy for the PoolSurvey structure.
#'   One of \code{"annual"} (default), \code{"quarterly"},
#'   \code{"monthly"}, \code{"biannual"}.
#' @param group_name Character. Name of the group in the PoolSurvey
#'   structure (default \code{"series"}).
#' @param .verbose Logical. Print progress messages (default uses
#'   \code{metasurvey.verbose} option).
#'
#' @return A \code{\link{PoolSurvey}} object containing the harmonized
#'   surveys, structured as
#'   \code{list(<grouping> = list(<group_name> = list(svy1, svy2, ...)))}.
#'
#' @details
#' The function performs the following steps for each survey:
#' \enumerate{
#'   \item Calls \code{\link{filter_recipes}} with the survey's type and
#'     edition (plus optional category/certification filters).
#'   \item Sorts matching recipes using topological sort on
#'     \code{depends_on_recipes}.
#'   \item Attaches recipes via \code{\link[=Survey]{add_recipe}} and
#'     applies them with \code{\link{bake_recipes}}.
#' }
#'
#' If no recipes are found for a survey, it is included in the pool
#' unchanged (with a warning).
#'
#' @examples
#' \donttest{
#' # Set up a local backend with a temp file
#' old <- set_backend("local", path = tempfile(fileext = ".json"))
#'
#' # Create a small survey and a recipe
#' dt <- data.table::data.table(
#'   id = 1:10, x = 1:10, w = rep(1, 10)
#' )
#' svy <- Survey$new(
#'   data = dt, edition = "2023", type = "test",
#'   psu = NULL, engine = "data.table",
#'   weight = add_weight(annual = "w")
#' )
#' r <- Recipe$new(
#'   name = "demo", edition = "2023", survey_type = "test",
#'   default_engine = "data.table", depends_on = list(),
#'   user = "demo", description = "demo recipe",
#'   steps = list("step_compute(., z = x * 2)"),
#'   id = "demo_recipe"
#' )
#' publish_recipe(r)
#'
#' pool <- harmonize(list(svy), .verbose = FALSE)
#'
#' # Restore backend
#' options(metasurvey.backend = old)
#' }
#'
#' @seealso \code{\link{filter_recipes}}, \code{\link{bake_recipes}},
#'   \code{\link{PoolSurvey}}, \code{\link{workflow}}
#' @family recipes
#' @export
harmonize <- function(surveys,
                      survey_type = NULL,
                      topic = NULL,
                      category = NULL,
                      certification_level = NULL,
                      grouping = "annual",
                      group_name = "series",
                      .verbose = getOption("metasurvey.verbose", TRUE)) {
  if (!is.list(surveys) || length(surveys) == 0) {
    stop("'surveys' must be a non-empty list of Survey objects", call. = FALSE)
  }

  valid_groupings <- c("annual", "quarterly", "monthly", "biannual")
  if (!grouping %in% valid_groupings) {
    stop(
      "'grouping' must be one of: ",
      paste(valid_groupings, collapse = ", "),
      call. = FALSE
    )
  }

  for (i in seq_along(surveys)) {
    if (!inherits(surveys[[i]], "Survey")) {
      stop(
        "Element ", i, " of 'surveys' is not a Survey object",
        call. = FALSE
      )
    }
  }

  harmonized <- vector("list", length(surveys))

  for (i in seq_along(surveys)) {
    svy <- surveys[[i]]
    svy_type <- survey_type %||% svy$type
    svy_edition <- svy$edition

    if (.verbose) {
      metasurvey_msg(
        "harmonize: processing survey ", i, "/", length(surveys),
        " (", svy_type, " ", svy_edition, ")"
      )
    }

    recipes <- filter_recipes(
      survey_type = svy_type,
      edition = svy_edition,
      category = category,
      certification_level = certification_level,
      topic = topic
    )

    if (length(recipes) == 0) {
      warning(
        "No recipes found for survey ", i,
        " (", svy_type, " ", svy_edition, ")",
        ". Including unchanged.",
        call. = FALSE
      )
      harmonized[[i]] <- svy
      next
    }

    sorted_recipes <- topo_sort_recipes(recipes)

    if (.verbose) {
      metasurvey_msg(
        "  applying ", length(sorted_recipes), " recipe(s): ",
        paste(
          vapply(sorted_recipes, function(r) r$name, character(1)),
          collapse = " -> "
        )
      )
    }

    result <- svy
    # If survey_type was overridden, temporarily align survey type
    # so add_recipe validation passes
    original_type <- result$type
    type_overridden <- !is.null(survey_type) && !is.null(original_type) &&
      tolower(original_type) != tolower(survey_type)
    if (type_overridden) {
      result$type <- survey_type
    }
    for (r in sorted_recipes) {
      result$add_recipe(r)
    }
    result <- bake_recipes(result)
    # Restore original type
    if (type_overridden) {
      result$type <- original_type
    }

    harmonized[[i]] <- result
  }

  pool_structure <- stats::setNames(
    list(stats::setNames(list(harmonized), group_name)),
    grouping
  )

  if (.verbose) {
    metasurvey_msg(
      "harmonize: assembled PoolSurvey with ", length(harmonized),
      " surveys (", grouping, "/", group_name, ")"
    )
  }

  PoolSurvey$new(pool_structure)
}
