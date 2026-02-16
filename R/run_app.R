#' Launch the Recipe Explorer Shiny App
#'
#' Opens an interactive web application to explore, search, and browse
#' metasurvey recipes with visual documentation cards. Supports user
#' registration and login via MongoDB Atlas.
#'
#' @param port Integer port number, or NULL for automatic.
#' @param host Character. The host to listen on. Defaults
#'   to \code{"127.0.0.1"} for local use. Set to
#'   \code{"0.0.0.0"} for server deployments (Railway,
#'   etc.).
#' @param launch.browser Logical. Open the app in a browser?
#'
#' @return NULL (called for side effect of launching the app).
#' @examples
#' \dontrun{
#' # Local / RStudio viewer
#' explore_recipes()
#'
#' # Server deployment (Railway, Docker, etc.)
#' explore_recipes(host = "0.0.0.0", port = 3838, launch.browser = FALSE)
#' }
#'
#' @family recipes
#' @export
explore_recipes <- function(port = NULL,
                            host = "127.0.0.1",
                            launch.browser = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Package 'shiny' is required. ",
      "Install it with: install.packages('shiny')"
    )
  }
  if (!requireNamespace("bslib", quietly = TRUE)) {
    stop(
      "Package 'bslib' is required. ",
      "Install it with: install.packages('bslib')"
    )
  }
  app_dir <- system.file("shiny", package = "metasurvey")
  if (app_dir == "") {
    stop("Shiny app not found. Reinstall metasurvey.")
  }
  shiny::runApp(
    app_dir, port = port, host = host,
    launch.browser = launch.browser
  )
}
