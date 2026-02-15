# Explore Workflows Module: Search, Filter, Grid

explore_workflows_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    # Hero section
    hero_section_ui("workflows"),

    # Search & Filters
    htmltools::tags$div(class = "search-container",
      htmltools::tags$div(class = "search-row",
        htmltools::tags$div(class = "search-field",
          shiny::textInput(ns("search"), NULL,
                          placeholder = "Search workflows...",
                          width = "100%")
        ),
        htmltools::tags$div(class = "filter-field",
          shiny::selectInput(ns("filter_svy"), NULL,
                           choices = c("Survey" = "", "ECH" = "ech", "EAII" = "eaii",
                                     "EPH" = "eph", "EAI" = "eai"),
                           width = "100%")
        ),
        htmltools::tags$div(class = "filter-field",
          shiny::selectInput(ns("filter_est_type"), NULL,
                           choices = c("Estimation" = "",
                                     "Annual" = "annual",
                                     "Quarterly" = "quarterly",
                                     "Monthly" = "monthly"),
                           width = "100%")
        ),
        htmltools::tags$div(class = "filter-field",
          shiny::selectInput(ns("filter_cert"), NULL,
                           choices = c("Certification" = "",
                                     "Official" = "official",
                                     "Reviewed" = "reviewed",
                                     "Community" = "community"),
                           width = "100%")
        ),
        htmltools::tags$div(class = "refresh-field",
          shiny::actionButton(ns("btn_refresh"), "",
                            icon = shiny::icon("sync"),
                            class = "btn-outline-secondary btn-sm")
        )
      )
    ),

    # Stats row
    shiny::uiOutput(ns("stats_row")),

    # Workflow grid
    shiny::uiOutput(ns("workflow_grid")),

    # Detail modal (hidden)
    shiny::uiOutput(ns("detail_modal"))
  )
}

explore_workflows_server <- function(id, auth_state, all_recipes = shiny::reactiveVal(list()), navigate_to_recipe = NULL, pending_workflow_id = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: all workflows from MongoDB
    all_workflows <- shiny::reactiveVal(list())

    load_workflows <- function() {
      shiny::withProgress(message = "Loading workflows...", {
        workflows <- tryCatch(
          shiny_fetch_workflows(),
          error = function(e) {
            shiny::showNotification(
              paste("Could not load workflows:", e$message),
              type = "error", duration = 5
            )
            list()
          }
        )
        all_workflows(workflows)
      })
    }

    # Load on startup
    shiny::observe({ load_workflows() }, priority = 100)

    # Refresh button
    shiny::observeEvent(input$btn_refresh, { load_workflows() })

    # Filtered workflows
    filtered <- shiny::reactive({
      workflows <- all_workflows()
      if (length(workflows) == 0) return(list())

      query <- tolower(trimws(input$search %||% ""))
      svy <- input$filter_svy %||% ""
      est_type <- input$filter_est_type %||% ""
      cert <- input$filter_cert %||% ""

      result <- workflows
      if (nzchar(query)) {
        result <- Filter(function(wf) {
          grepl(query, tolower(wf$name %||% "")) ||
          grepl(query, tolower(wf$description %||% ""))
        }, result)
      }
      if (nzchar(svy)) {
        result <- Filter(function(wf) wf$survey_type == svy, result)
      }
      if (nzchar(est_type)) {
        result <- Filter(function(wf) est_type %in% wf$estimation_type, result)
      }
      if (nzchar(cert)) {
        result <- Filter(function(wf) {
          (wf$certification$level %||% "community") == cert
        }, result)
      }

      # Sort by downloads desc
      if (length(result) > 1) {
        dls <- vapply(result, function(wf) wf$downloads %||% 0L, integer(1))
        result <- result[order(dls, decreasing = TRUE)]
      }

      result
    })

    # Stats row
    output$stats_row <- shiny::renderUI({
      workflows <- all_workflows()
      n_total <- length(workflows)
      n_official <- sum(vapply(workflows, function(wf)
        (wf$certification$level %||% "community") == "official", logical(1)))
      total_downloads <- sum(vapply(workflows, function(wf) wf$downloads %||% 0L, integer(1)))

      htmltools::tags$div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1rem; margin-bottom: 1rem;",
        htmltools::tags$div(class = "stat-box",
          htmltools::tags$div(class = "stat-number", n_total),
          htmltools::tags$div(class = "stat-label", "Total Workflows")
        ),
        htmltools::tags$div(class = "stat-box",
          htmltools::tags$div(class = "stat-number", n_official),
          htmltools::tags$div(class = "stat-label", "Official")
        ),
        htmltools::tags$div(class = "stat-box",
          htmltools::tags$div(class = "stat-number", format_downloads(total_downloads)),
          htmltools::tags$div(class = "stat-label", "Total Downloads")
        )
      )
    })

    # Workflow grid
    output$workflow_grid <- shiny::renderUI({
      workflows <- filtered()

      if (length(workflows) == 0) {
        return(htmltools::tags$div(class = "empty-state",
          bsicons::bs_icon("bar-chart-line", size = "4rem"),
          htmltools::tags$h5("No workflows found"),
          htmltools::tags$p("Try adjusting your search or filters.")
        ))
      }

      cards <- lapply(seq_along(workflows), function(i) {
        workflow_card_ui(workflows[[i]], ns, i)
      })

      htmltools::tags$div(class = "recipe-grid", cards)
    })

    # Helper: open workflow detail modal
    open_workflow_modal <- function(wf) {
      # Increment downloads
      tryCatch(
        shiny_increment_workflow_downloads(wf$id),
        error = function(e) NULL
      )

      shiny::showModal(
        shiny::modalDialog(
          workflow_detail_ui(wf, recipes_list = all_recipes(), ns = ns),
          size = "l",
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )
    }

    # Card click -> open detail modal
    shiny::observeEvent(input$wf_card_click, {
      idx <- input$wf_card_click
      workflows <- filtered()
      if (idx < 1 || idx > length(workflows)) return()
      open_workflow_modal(workflows[[idx]])
    })

    # Cross-navigation: open workflow by ID from another module
    if (!is.null(pending_workflow_id)) {
      shiny::observeEvent(pending_workflow_id(), {
        req <- pending_workflow_id()
        if (is.null(req)) return()
        wid <- req$id
        workflows <- all_workflows()
        for (wf in workflows) {
          if (as.character(wf$id) == wid) {
            open_workflow_modal(wf)
            return()
          }
        }
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
    }

    # Cross-reference: navigate to recipe from workflow detail
    shiny::observeEvent(input$navigate_recipe, {
      if (!is.null(navigate_to_recipe)) {
        shiny::removeModal()
        navigate_to_recipe(input$navigate_recipe)
      }
    })

    # Return reactive of all workflows for cross-reference from recipe module
    all_workflows
  })
}
