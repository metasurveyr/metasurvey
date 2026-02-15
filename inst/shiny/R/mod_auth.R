# Auth Module: Login + Register

auth_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tags$div(class = "auth-container",
    shiny::tabsetPanel(
      id = ns("auth_tabs"), type = "pills",

      # Login Tab
      shiny::tabPanel("Login",
        htmltools::tags$div(style = "padding-top: 1.5rem;",
          htmltools::tags$h3(
            bsicons::bs_icon("box-arrow-in-right", size = "1.5rem"),
            " Welcome back"
          ),
          shiny::textInput(ns("login_email"), "Email",
                          placeholder = "your@email.com"),
          shiny::passwordInput(ns("login_password"), "Password",
                              placeholder = "Enter password"),
          htmltools::tags$br(),
          shiny::actionButton(ns("btn_login"), "Login",
                            class = "btn-primary btn-lg",
                            icon = shiny::icon("sign-in-alt")),
          shiny::uiOutput(ns("login_feedback"))
        )
      ),

      # Register Tab
      shiny::tabPanel("Register",
        htmltools::tags$div(style = "padding-top: 1.5rem;",
          htmltools::tags$h3(
            bsicons::bs_icon("person-plus-fill", size = "1.5rem"),
            " Create account"
          ),
          shiny::textInput(ns("reg_name"), "Full Name",
                          placeholder = "Juan Perez"),
          shiny::textInput(ns("reg_email"), "Email",
                          placeholder = "your@email.com"),
          shiny::passwordInput(ns("reg_password"), "Password",
                              placeholder = "Min. 6 characters"),
          shiny::selectInput(ns("reg_type"), "Account Type",
                           choices = c("Individual" = "individual",
                                     "Institutional Member" = "institutional_member",
                                     "Institution" = "institution")),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'institutional_member'", ns("reg_type")),
            shiny::textInput(ns("reg_institution"), "Institution Name",
                           placeholder = "e.g., Instituto de Economia")
          ),
          htmltools::tags$br(),
          shiny::actionButton(ns("btn_register"), "Create Account",
                            class = "btn-primary btn-lg",
                            icon = shiny::icon("user-plus")),
          shiny::uiOutput(ns("register_feedback"))
        )
      )
    )
  )
}

auth_server <- function(id, auth_state) {
  shiny::moduleServer(id, function(input, output, session) {

    # Login
    shiny::observeEvent(input$btn_login, {
      email <- trimws(input$login_email)
      password <- input$login_password

      if (!nzchar(email) || !nzchar(password)) {
        output$login_feedback <- shiny::renderUI(
          htmltools::tags$div(class = "alert alert-warning mt-3",
            bsicons::bs_icon("exclamation-triangle-fill"),
            " Please fill in all fields."
          )
        )
        return()
      }

      result <- tryCatch(
        mongo_login(email, password),
        error = function(e) list(ok = FALSE, error = e$message)
      )

      if (isTRUE(result$ok)) {
        auth_state$user <- result$user
        auth_state$logged_in <- TRUE
        auth_state$email <- email
        shiny::showNotification(
          paste("Welcome,", result$user$name, "!"),
          type = "message", duration = 3
        )
      } else {
        output$login_feedback <- shiny::renderUI(
          htmltools::tags$div(class = "alert alert-danger mt-3",
            bsicons::bs_icon("x-circle-fill"),
            paste(" ", result$error %||% "Login failed.")
          )
        )
      }
    })

    # Register
    shiny::observeEvent(input$btn_register, {
      name <- trimws(input$reg_name)
      email <- trimws(input$reg_email)
      password <- input$reg_password
      user_type <- input$reg_type
      institution <- trimws(input$reg_institution %||% "")

      if (!nzchar(name) || !nzchar(email) || !nzchar(password)) {
        output$register_feedback <- shiny::renderUI(
          htmltools::tags$div(class = "alert alert-warning mt-3",
            bsicons::bs_icon("exclamation-triangle-fill"),
            " Please fill in all required fields."
          )
        )
        return()
      }

      if (nchar(password) < 6) {
        output$register_feedback <- shiny::renderUI(
          htmltools::tags$div(class = "alert alert-warning mt-3",
            bsicons::bs_icon("exclamation-triangle-fill"),
            " Password must be at least 6 characters."
          )
        )
        return()
      }

      inst_arg <- if (user_type == "institutional_member" && nzchar(institution)) institution else NULL

      result <- tryCatch(
        mongo_register(name, email, password, user_type, inst_arg),
        error = function(e) list(ok = FALSE, error = e$message)
      )

      if (isTRUE(result$ok)) {
        auth_state$user <- result$user
        auth_state$logged_in <- TRUE
        auth_state$email <- email
        shiny::showNotification(
          paste("Account created! Welcome,", name),
          type = "message", duration = 3
        )
      } else {
        output$register_feedback <- shiny::renderUI(
          htmltools::tags$div(class = "alert alert-danger mt-3",
            bsicons::bs_icon("x-circle-fill"),
            paste(" ", result$error %||% "Registration failed.")
          )
        )
      }
    })
  })
}
