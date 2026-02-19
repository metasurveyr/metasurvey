# metasurvey Recipe & Workflow Explorer
# Launch with: metasurvey::explore_recipes()

library(shiny)
library(bslib)
library(bsicons)
library(htmltools)
library(metasurvey)

# Source app modules — prefer local dev files if running from source tree
app_dir <- if (file.exists("R/api_utils.R")) {
  "."
} else {
  system.file("shiny", package = "metasurvey")
}
for (f in list.files(
  file.path(app_dir, "R"),
  full.names = TRUE,
  pattern = "\\.R$"
)) {
  source(f, local = TRUE)
}

app_theme <- bs_theme(
  version = 5,
  primary = "#6366f1",
  secondary = "#64748b",
  success = "#10b981",
  warning = "#f59e0b",
  info = "#0ea5e9",
  danger = "#f43f5e",
  "body-bg" = "#f8fafc",
  "body-color" = "#1e293b",
  font_scale = 1,
  "navbar-bg" = "#0f172a",
  "navbar-dark-color" = "#e2e8f0",
  "enable-rounded" = TRUE,
  "border-radius" = ".5rem",
  "card-border-color" = "#e2e8f0"
)

# UI
ui <- page_navbar(
  id = "main_nav",
  title = tags$span(
    style = "display: flex; align-items: center; gap: .5rem; font-weight: 700;",
    bs_icon("journal-code", size = "1.3rem"),
    "metasurvey"
  ),
  theme = app_theme,
  fillable = FALSE,
  header = app_css(),

  # Recipes tab
  nav_panel(
    title = tags$span(bs_icon("grid-3x3-gap-fill", size = ".9rem"), " Recipes"),
    value = "explore",
    tags$div(
      class = "container-fluid",
      style = "max-width: 1200px; padding-top: 1rem;",
      explore_ui("explore")
    )
  ),

  # Workflows tab
  nav_panel(
    title = tags$span(bs_icon("bar-chart-fill", size = ".9rem"), " Workflows"),
    value = "workflows",
    tags$div(
      class = "container-fluid",
      style = "max-width: 1200px; padding-top: 1rem;",
      explore_workflows_ui("workflows")
    )
  ),

  # Auth / Profile tab (dynamic)
  nav_panel(
    title = uiOutput("nav_auth_label", inline = TRUE),
    value = "auth",
    tags$div(
      class = "container-fluid",
      style = "max-width: 1200px; padding-top: 1rem;",
      uiOutput("auth_or_profile")
    )
  ),

  # Spacer + About
  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://metasurveyr.github.io/metasurvey",
      target = "_blank",
      class = "nav-link",
      style = "opacity: .7; font-size: .85rem;",
      bs_icon("github", size = ".9rem"), " Docs"
    )
  )
)

# Server
server <- function(input, output, session) {
  # Auth state
  auth <- reactiveValues(
    user = NULL,
    logged_in = FALSE,
    email = NULL
  )

  # Dynamic nav label
  output$nav_auth_label <- renderUI({
    if (isTRUE(auth$logged_in) && !is.null(auth$user)) {
      tags$span(
        bs_icon("person-check-fill", size = ".9rem"),
        " ", auth$user$name
      )
    } else {
      tags$span(bs_icon("box-arrow-in-right", size = ".9rem"), " Login")
    }
  })

  # Dynamic content: auth form or profile
  output$auth_or_profile <- renderUI({
    if (isTRUE(auth$logged_in)) {
      profile_ui("profile")
    } else {
      auth_ui("auth")
    }
  })

  # Cross-navigation shared state
  pending_recipe_id <- reactiveVal(NULL)
  pending_workflow_id <- reactiveVal(NULL)

  navigate_to_recipe <- function(recipe_id) {
    nav_select("main_nav", "explore", session = session)
    # Use list with nonce so identical IDs still trigger
    pending_recipe_id(list(id = recipe_id, ts = as.numeric(Sys.time())))
  }

  navigate_to_workflow <- function(workflow_id) {
    nav_select("main_nav", "workflows", session = session)
    pending_workflow_id(list(id = workflow_id, ts = as.numeric(Sys.time())))
  }

  # Module servers
  auth_server("auth", auth)
  profile_server("profile", auth)

  # Recipes module returns reactive of all recipes
  explore_recipes_rv <- explore_server("explore", auth,
    navigate_to_workflow = navigate_to_workflow,
    pending_recipe_id = pending_recipe_id
  )

  # Workflows module receives all_recipes for cross-refs
  explore_workflows_server("workflows", auth,
    all_recipes = explore_recipes_rv,
    navigate_to_recipe = navigate_to_recipe,
    pending_workflow_id = pending_workflow_id
  )
}

shinyApp(ui, server)
