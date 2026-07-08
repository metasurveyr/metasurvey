# Ver RFC-MODULARIZACION.md · metapaquete metasurvey

#' @title metasurvey: Reproducible Survey Data Processing (ecosystem)
#'
#' @description
#' `metasurvey` is a meta-package that loads and re-exports the whole
#' 'metasurvey' ecosystem with a single `library(metasurvey)`, in the style of
#' the 'tidyverse':
#'
#' \itemize{
#'   \item \pkg{metasurvey.core} — local processing engine (Survey/Step/Recipe/
#'     Workflow, complex sampling designs).
#'   \item \pkg{metasurvey.fromstata} — STATA `.do` transpiler.
#'   \item \pkg{metasurvey.anda} — ANDA microdata/metadata client.
#'   \item \pkg{metasurvey.explorer.backend} — R client for the recipe/workflow
#'     API.
#' }
#'
#' The Shiny explorer (\pkg{metasurvey.explorer.frontend}) is optional and used
#' via `metasurvey.explorer.frontend::explore_recipes()`.
#'
#' This package also provides high-level orchestration that spans several
#' sub-packages, such as [reproduce_workflow()].
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
