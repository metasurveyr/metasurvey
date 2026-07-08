# Migrado de metasurvey-legacy/R/utils.R (corte C3)
# Ver RFC-MODULARIZACION.md · metapaquete metasurvey
# Estas funciones orquestan core + anda + backend, por eso viven en el metapaquete.

#' Resolve a portable weight specification to a usable weight configuration
#'
#' Converts the portable weight_spec from a RecipeWorkflow back into the
#' format expected by \code{load_survey()} and \code{add_weight()}.
#' For replicate weights with ANDA sources, automatically downloads the
#' replicate file.
#'
#' @param weight_spec Named list from RecipeWorkflow$weight_spec
#' @param dest_dir Character directory for downloaded files (default: tempdir())
#' @return Named list compatible with add_weight() output
#' @export
#' @examples
#' \dontrun{
#' wf <- api_get_workflow("w_123")
#' weight <- resolve_weight_spec(wf$weight_spec)
#' }
#' @family weights
resolve_weight_spec <- function(weight_spec, dest_dir = tempdir()) {
  if (is.null(weight_spec)) {
    return(NULL)
  }

  resolved <- lapply(weight_spec, function(ws) {
    if (ws$type == "simple") {
      ws$variable
    } else if (ws$type == "replicate") {
      replicate_path <- NULL
      src <- ws$replicate_source

      if (!is.null(src) && src$provider == "anda") {
        replicate_path <- metasurvey.anda::anda_download_microdata(
          edition = src$edition,
          resource = src$resource,
          dest_dir = dest_dir
        )
      } else if (!is.null(src) && src$provider == "local") {
        warning(
          "Replicate source is local-only ('", src$path_hint,
          "'). Please provide the file manually.",
          call. = FALSE
        )
      }

      rep_id <- NULL
      if (!is.null(ws$replicate_id)) {
        rep_id <- stats::setNames(
          ws$replicate_id$replicate_key,
          ws$replicate_id$survey_key
        )
      }

      metasurvey.core::add_replicate(
        weight = ws$variable,
        replicate_pattern = ws$replicate_pattern,
        replicate_path = replicate_path,
        replicate_id = rep_id,
        replicate_type = ws$replicate_type
      )
    }
  })

  names(resolved) <- names(weight_spec)
  resolved
}

#' Reproduce a workflow from its published specification
#'
#' Given a RecipeWorkflow (typically fetched from the registry), downloads
#' the data, resolves the weight configuration, fetches referenced recipes,
#' and returns a Survey object ready for \code{workflow()} estimation.
#'
#' @param wf RecipeWorkflow object
#' @param data_path Character path to survey microdata. If NULL, attempts to
#'   download from ANDA for ECH surveys.
#' @param dest_dir Character directory for downloaded files
#' @return Survey object with recipes applied and weight configuration set
#' @export
#' @examples
#' \dontrun{
#' wf <- api_get_workflow("w_123")
#' svy <- reproduce_workflow(wf)
#' }
#' @family workflows
reproduce_workflow <- function(wf, data_path = NULL, dest_dir = tempdir()) {
  if (!inherits(wf, "RecipeWorkflow")) {
    stop("wf must be a RecipeWorkflow object", call. = FALSE)
  }

  svy_weight <- resolve_weight_spec(wf$weight_spec, dest_dir = dest_dir)

  if (is.null(data_path) && tolower(wf$survey_type) == "ech") {
    edition <- as.character(wf$edition)
    data_path <- metasurvey.anda::anda_download_microdata(edition,
      resource = "implantation",
      dest_dir = dest_dir
    )
  }

  if (is.null(data_path)) {
    stop("Cannot resolve data source. Please provide data_path.", call. = FALSE)
  }

  recipes <- NULL
  if (length(wf$recipe_ids) > 0) {
    backend <- tryCatch(metasurvey.core::get_backend(), error = function(e) NULL)
    if (!is.null(backend)) {
      recipe_list <- list()
      for (rid in wf$recipe_ids) {
        r <- tryCatch(backend$get(rid), error = function(e) NULL)
        if (!is.null(r)) recipe_list <- c(recipe_list, list(r))
      }
      if (length(recipe_list) > 0) recipes <- recipe_list
    }
  }

  metasurvey.core::load_survey(
    path = data_path,
    svy_type = wf$survey_type,
    svy_edition = as.character(wf$edition),
    svy_weight = svy_weight,
    recipes = recipes,
    bake = !is.null(recipes)
  )
}

#' Evaluate estimation with Coefficient of Variation
#' @param cv Numeric coefficient of variation value.
#' @return Character string with the quality category
#'   (e.g. "Excellent", "Good"), or \code{NA} when the CV is not
#'   defined (e.g. a zero estimate, where CV = 0/0).
#' @keywords utils
#' @examples
#' evaluate_cv(3) # "Excellent"
#' evaluate_cv(12) # "Good"
#' evaluate_cv(30) # "Use with caution"
#' evaluate_cv(NaN) # NA
#' @family workflows
#' @export
