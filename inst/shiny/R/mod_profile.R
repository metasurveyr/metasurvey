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
        return(htmltools::tags$div(
          class = "empty-state",
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
        htmltools::tags$div(
          class = "profile-header",
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
                if (!is.null(user$email)) {
                  htmltools::tags$span(
                    bsicons::bs_icon("envelope-fill", size = ".75rem"),
                    paste(" ", user$email, "  ")
                  )
                },
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
            class = "btn-outline-primary"
          ),
          shiny::actionButton(ns("btn_logout"), "Logout",
            icon = shiny::icon("sign-out-alt"),
            class = "btn-outline-danger"
          )
        ),

        # API Token section
        shiny::uiOutput(ns("token_section")),

        # Admin panel for pending reviews
        shiny::uiOutput(ns("admin_panel")),

        # My recipes section
        shiny::uiOutput(ns("my_recipes_section"))
      )
    })

    # Token section
    generated_token <- shiny::reactiveVal(NULL)

    output$token_section <- shiny::renderUI({
      if (!isTRUE(auth_state$logged_in) || is.null(auth_state$token)) {
        return(NULL)
      }

      token_val <- generated_token()

      token_content <- if (!is.null(token_val)) {
        code_text <- paste0(
          '<span class="code-comment"># Configure metasurvey API access</span>\n',
          "library(metasurvey)\n",
          '<span class="code-func">Sys.setenv</span>(METASURVEY_TOKEN = <span class="code-string">"', token_val, '"</span>)\n',
          "\n",
          '<span class="code-comment"># Or add to your .Renviron file:</span>\n',
          '<span class="code-comment"># METASURVEY_TOKEN=', token_val, "</span>\n",
          "\n",
          '<span class="code-comment"># Browse available recipes</span>\n',
          'recipes &lt;- <span class="code-func">list_recipes</span>()\n',
          '<span class="code-func">search_recipes</span>(<span class="code-string">"employment"</span>)\n',
          "\n",
          '<span class="code-comment"># Download and apply a recipe</span>\n',
          'recipe &lt;- <span class="code-func">api_get_recipe</span>(<span class="code-string">"recipe_id"</span>)\n',
          'svy &lt;- <span class="code-func">survey_empty</span>(type = <span class="code-string">"ech"</span>, edition = <span class="code-string">"2023"</span>)\n',
          'svy &lt;- <span class="code-func">set_data</span>(svy, my_data)\n',
          'svy &lt;- <span class="code-func">add_recipe</span>(svy, recipe)\n',
          'svy &lt;- <span class="code-func">bake_recipes</span>(svy)'
        )
        htmltools::tagList(
          code_block_ui(code_text, label = "R Code — API Access", block_id = "profile_token_code"),
          htmltools::tags$p(
            class = "token-help",
            bsicons::bs_icon("shield-lock", size = ".75rem"),
            " Token expires in 90 days. Keep it private and do not share it."
          )
        )
      } else {
        htmltools::tags$p(
          style = "color: #6c757d; font-size: .9rem;",
          "Generate a long-lived token for use in R scripts and automated pipelines."
        )
      }

      htmltools::tags$div(
        class = "token-section",
        htmltools::tags$h5(
          bsicons::bs_icon("key-fill", size = "1.1rem"),
          " API Access Token"
        ),
        shiny::actionButton(ns("btn_generate_token"),
          if (is.null(token_val)) "Generate Token" else "Regenerate Token",
          icon = shiny::icon("key"),
          class = if (is.null(token_val)) "btn-primary btn-sm" else "btn-outline-secondary btn-sm"
        ),
        token_content
      )
    })

    shiny::observeEvent(input$btn_generate_token, {
      result <- tryCatch(
        shiny_generate_token(auth_state$token),
        error = function(e) list(ok = FALSE, error = e$message)
      )
      if (isTRUE(result$ok)) {
        generated_token(result$token)
        shiny::showNotification("API token generated (valid for 90 days)", type = "message")
      } else {
        shiny::showNotification(
          paste("Token generation failed:", result$error %||% "Unknown error"),
          type = "error"
        )
      }
    })

    # Logout
    shiny::observeEvent(input$btn_logout, {
      auth_state$user <- NULL
      auth_state$logged_in <- FALSE
      auth_state$email <- NULL
      auth_state$token <- NULL
      shiny::showNotification("Logged out successfully.", type = "message")
    })

    # Admin panel — show pending reviews if admin
    output$admin_panel <- shiny::renderUI({
      if (!isTRUE(auth_state$logged_in) || is.null(auth_state$token)) {
        return(NULL)
      }
      # Only show for admin users (check by fetching pending — if 403, not admin)
      result <- tryCatch(
        shiny_fetch_pending_users(auth_state$token),
        error = function(e) list(ok = FALSE)
      )
      if (!isTRUE(result$ok)) {
        return(NULL)
      }

      pending <- result$users %||% list()
      if (length(pending) == 0) {
        return(htmltools::tags$div(
          style = "padding: 1rem; background: #f0f9ff; border-radius: 12px; margin-bottom: 1.5rem;",
          bsicons::bs_icon("shield-check", size = "1.2rem"),
          htmltools::tags$strong(" Admin Panel"),
          htmltools::tags$p(
            style = "margin: .5rem 0 0; color: #6c757d;",
            "No accounts pending review."
          )
        ))
      }

      user_cards <- lapply(seq_along(pending), function(i) {
        u <- pending[[i]]
        htmltools::tags$div(
          style = "display: flex; align-items: center; justify-content: space-between; padding: .75rem 1rem; background: #fff; border-radius: 8px; margin-bottom: .5rem; border: 1px solid #e9ecef;",
          htmltools::tags$div(
            htmltools::tags$strong(u$name %||% "Unknown"),
            htmltools::tags$span(style = "color: #6c757d; margin-left: .5rem;", u$email %||% ""),
            htmltools::tags$br(),
            htmltools::tags$span(
              class = paste0("badge bg-", if (u$user_type == "institution") "primary" else "info"),
              u$user_type
            ),
            if (!is.null(u$institution) && !is.na(u$institution)) {
              htmltools::tags$span(
                style = "color: #6c757d; margin-left: .5rem; font-size: .85rem;",
                paste("Institution:", u$institution)
              )
            }
          ),
          htmltools::tags$div(
            style = "display: flex; gap: .5rem;",
            shiny::actionButton(ns(paste0("approve_", i)), "Approve",
              class = "btn-sm btn-success", icon = shiny::icon("check")
            ),
            shiny::actionButton(ns(paste0("reject_", i)), "Reject",
              class = "btn-sm btn-danger", icon = shiny::icon("times")
            )
          )
        )
      })

      htmltools::tags$div(
        style = "padding: 1.25rem; background: #f0f9ff; border-radius: 12px; margin-bottom: 1.5rem;",
        htmltools::tags$div(
          style = "display: flex; align-items: center; gap: .5rem; margin-bottom: 1rem;",
          bsicons::bs_icon("shield-lock-fill", size = "1.2rem"),
          htmltools::tags$strong(paste("Admin Panel -", length(pending), "pending")),
          shiny::actionButton(ns("refresh_admin"), "",
            icon = shiny::icon("sync"),
            class = "btn-outline-secondary btn-sm", style = "margin-left: auto;"
          )
        ),
        htmltools::tagList(user_cards)
      )
    })

    # Admin approve/reject handlers
    shiny::observe({
      result <- tryCatch(shiny_fetch_pending_users(auth_state$token), error = function(e) list(ok = FALSE))
      if (!isTRUE(result$ok)) {
        return()
      }
      pending <- result$users %||% list()

      lapply(seq_along(pending), function(i) {
        approve_id <- paste0("approve_", i)
        reject_id <- paste0("reject_", i)
        user_email <- pending[[i]]$email

        shiny::observeEvent(input[[approve_id]],
          {
            res <- tryCatch(shiny_approve_user(user_email, auth_state$token), error = function(e) list(ok = FALSE))
            if (isTRUE(res$ok)) {
              shiny::showNotification(paste("Approved:", user_email), type = "message")
            }
          },
          ignoreInit = TRUE,
          once = TRUE
        )

        shiny::observeEvent(input[[reject_id]],
          {
            res <- tryCatch(shiny_reject_user(user_email, auth_state$token), error = function(e) list(ok = FALSE))
            if (isTRUE(res$ok)) {
              shiny::showNotification(paste("Rejected:", user_email), type = "warning")
            }
          },
          ignoreInit = TRUE,
          once = TRUE
        )
      })
    }) |> shiny::bindEvent(auth_state$logged_in)

    # Refresh admin panel
    shiny::observeEvent(input$refresh_admin, {
      output$admin_panel <- shiny::renderUI({
        # Re-trigger by invalidating
        shiny::invalidateLater(0)
      })
    })

    # My recipes
    shiny::observeEvent(input$btn_my_recipes, {
      email <- auth_state$email
      if (is.null(email)) {
        return()
      }

      recipes <- tryCatch(
        shiny_fetch_user_recipes(email),
        error = function(e) list()
      )

      output$my_recipes_section <- shiny::renderUI({
        if (length(recipes) == 0) {
          return(htmltools::tags$div(
            class = "empty-state",
            style = "padding: 2rem;",
            bsicons::bs_icon("journal-x", size = "3rem"),
            htmltools::tags$h5("No recipes published yet"),
            htmltools::tags$p("Use the metasurvey package to create and publish recipes.")
          ))
        }

        htmltools::tags$div(
          htmltools::tags$div(
            class = "section-title",
            bsicons::bs_icon("journal-bookmark-fill"),
            paste("My Recipes (", length(recipes), ")")
          ),
          htmltools::tags$div(
            class = "recipe-grid",
            lapply(seq_along(recipes), function(i) {
              recipe_card_ui(recipes[[i]], ns, i)
            })
          )
        )
      })
    })
  })
}
