#' Launch the Recipe Explorer Shiny App
#'
#' Opens an interactive web application to explore, search, and browse
#' metasurvey recipes with visual documentation cards. Supports user
#' registration and login via MongoDB Atlas.
#'
#' @param port Integer port number, or NULL for automatic.
#' @param launch.browser Logical. Open the app in a browser?
#'
#' @examples
#' \dontrun{
#' explore_recipes()
#' }
#'
#' @export
explore_recipes <- function(port = NULL, launch.browser = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install it with: install.packages('shiny')")
  }
  if (!requireNamespace("bslib", quietly = TRUE)) {
    stop("Package 'bslib' is required. Install it with: install.packages('bslib')")
  }
  app_dir <- system.file("shiny", package = "metasurvey")
  if (app_dir == "") {
    stop("Shiny app not found. Reinstall metasurvey.")
  }
  shiny::runApp(app_dir, port = port, launch.browser = launch.browser)
}
