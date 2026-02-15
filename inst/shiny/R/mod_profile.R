# Profile Module

profile_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("profile_content"))
}

profile_server <- function(id, auth_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$profile_content <- shiny::renderUI({
      if (!isTRUE(auth_state$logged_in) || is.null(auth_state$user)) {
        return(htmltools::tags$div(class = "empty-state",
          bsicons::bs_icon("person-x-fill", size = "4rem"),
          htmltools::tags$h5("Not logged in"),
          htmltools::tags$p("Please login or create an account.")
        ))
      }

      user <- auth_state$user
      type_label <- switch(user$user_type,
        "individual" = "Individual Researcher",
        "institutional_member" = "Institutional Member",
        "institution" = "Institution",
        user$user_type
      )
      type_icon <- switch(user$user_type,
        "individual" = bsicons::bs_icon("person-fill", size = "1.2rem"),
        "institutional_member" = bsicons::bs_icon("person-badge-fill", size = "1.2rem"),
        "institution" = bsicons::bs_icon("building", size = "1.2rem"),
        ""
      )
      trust_stars <- paste(rep("\u2605", user$trust_level()), collapse = "")

      htmltools::tagList(
        # Profile header
        htmltools::tags$div(class = "profile-header",
          htmltools::tags$div(
            style = "display: flex; align-items: center; gap: 1.5rem;",
            htmltools::tags$div(
              style = "width: 70px; height: 70px; border-radius: 50%; background: rgba(255,255,255,.15); display: flex; align-items: center; justify-content: center;",
              bsicons::bs_icon("person-fill", size = "2rem")
            ),
            htmltools::tags$div(
              htmltools::tags$h3(user$name),
              htmltools::tags$div(class = "user-type-label", type_icon, " ", type_label),
              htmltools::tags$div(
                style = "margin-top: .5rem; font-size: .85rem; opacity: .85;",
                if (!is.null(user$email)) htmltools::tags$span(
                  bsicons::bs_icon("envelope-fill", size = ".75rem"),
                  paste(" ", user$email, "  ")
                ),
                htmltools::tags$span(
                  style = "color: #f1c40f; letter-spacing: 2px;",
                  paste("Trust:", trust_stars)
                )
              ),
              if (!is.null(user$institution)) {
                htmltools::tags$div(
                  style = "margin-top: .3rem; font-size: .85rem; opacity: .85;",
                  bsicons::bs_icon("building", size = ".75rem"),
                  paste(" ", user$institution$name)
                )
              }
            )
          )
        ),

        # Actions
        htmltools::tags$div(
          style = "display: flex; gap: 1rem; margin-bottom: 1.5rem;",
          shiny::actionButton(ns("btn_my_recipes"), "My Recipes",
                            icon = shiny::icon("book"),
                            class = "btn-outline-primary"),
          shiny::actionButton(ns("btn_logout"), "Logout",
                            icon = shiny::icon("sign-out-alt"),
                            class = "btn-outline-danger")
        ),

        # My recipes section
        shiny::uiOutput(ns("my_recipes_section"))
      )
    })

    # Logout
    shiny::observeEvent(input$btn_logout, {
      auth_state$user <- NULL
      auth_state$logged_in <- FALSE
      auth_state$email <- NULL
      shiny::showNotification("Logged out successfully.", type = "message")
    })

    # My recipes
    shiny::observeEvent(input$btn_my_recipes, {
      email <- auth_state$email
      if (is.null(email)) return()

      recipes <- tryCatch(
        mongo_fetch_user_recipes(email),
        error = function(e) list()
      )

      output$my_recipes_section <- shiny::renderUI({
        if (length(recipes) == 0) {
          return(htmltools::tags$div(class = "empty-state",
            style = "padding: 2rem;",
            bsicons::bs_icon("journal-x", size = "3rem"),
            htmltools::tags$h5("No recipes published yet"),
            htmltools::tags$p("Use the metasurvey package to create and publish recipes.")
          ))
        }

        htmltools::tags$div(
          htmltools::tags$div(class = "section-title",
            bsicons::bs_icon("journal-bookmark-fill"),
            paste("My Recipes (", length(recipes), ")")
          ),
          htmltools::tags$div(class = "recipe-grid",
            lapply(seq_along(recipes), function(i) {
              recipe_card_ui(recipes[[i]], ns, i)
            })
          )
        )
      })
    })
  })
}
