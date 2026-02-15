# Visual UI components for the Recipe Explorer

app_css <- function() {
  htmltools::tags$style(htmltools::HTML("
    /* ── Global ── */
    body { background: #f8f9fa; }
    .navbar { box-shadow: 0 2px 10px rgba(0,0,0,.08); }

    /* ── Recipe Grid ── */
    .recipe-grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
      gap: 1.25rem;
      padding: 1rem 0;
    }

    /* ── Recipe Card ── */
    .recipe-card {
      border: none;
      border-radius: 12px;
      overflow: hidden;
      transition: transform .2s ease, box-shadow .2s ease;
      cursor: pointer;
      background: #fff;
      box-shadow: 0 2px 8px rgba(0,0,0,.06);
    }
    .recipe-card:hover {
      transform: translateY(-4px);
      box-shadow: 0 8px 25px rgba(0,0,0,.12);
    }
    .recipe-card-header {
      padding: 1rem 1.25rem .75rem;
      color: #fff;
      position: relative;
    }
    .recipe-card-header.cert-community {
      background: linear-gradient(135deg, #f39c12, #e67e22);
    }
    .recipe-card-header.cert-reviewed {
      background: linear-gradient(135deg, #3498db, #2980b9);
    }
    .recipe-card-header.cert-official {
      background: linear-gradient(135deg, #2ecc71, #27ae60);
    }
    .recipe-card-header h5 {
      margin: 0;
      font-weight: 700;
      font-size: 1.05rem;
      line-height: 1.3;
      text-shadow: 0 1px 2px rgba(0,0,0,.15);
    }
    .recipe-card-header .card-subtitle {
      opacity: .9;
      font-size: .8rem;
      margin-top: .25rem;
    }
    .recipe-card-body {
      padding: 1rem 1.25rem;
    }
    .recipe-card-body .description {
      color: #6c757d;
      font-size: .85rem;
      line-height: 1.5;
      display: -webkit-box;
      -webkit-line-clamp: 2;
      -webkit-box-orient: vertical;
      overflow: hidden;
      margin-bottom: .75rem;
    }
    .recipe-card-footer {
      padding: .75rem 1.25rem;
      background: #f8f9fa;
      border-top: 1px solid #eee;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }
    .recipe-card-footer .downloads {
      color: #6c757d;
      font-size: .8rem;
      display: flex;
      align-items: center;
      gap: .35rem;
    }
    .recipe-card-footer .btn-view {
      font-size: .8rem;
      padding: .25rem .75rem;
      border-radius: 20px;
      font-weight: 600;
    }

    /* ── Category Tags ── */
    .category-tag {
      display: inline-block;
      padding: .15rem .6rem;
      border-radius: 20px;
      font-size: .72rem;
      font-weight: 600;
      margin: .1rem;
      letter-spacing: .02em;
    }
    .tag-labor_market { background: #d4efdf; color: #1e8449; }
    .tag-income       { background: #d6eaf8; color: #2471a3; }
    .tag-education    { background: #fdebd0; color: #ca6f1e; }
    .tag-health       { background: #fadbd8; color: #c0392b; }
    .tag-demographics { background: #e8daef; color: #7d3c98; }
    .tag-housing      { background: #d5dbdb; color: #566573; }
    .tag-default      { background: #eaecee; color: #515a5a; }

    /* ── Cert Badge ── */
    .cert-badge {
      display: inline-flex;
      align-items: center;
      gap: .35rem;
      padding: .2rem .7rem;
      border-radius: 20px;
      font-size: .75rem;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: .04em;
    }
    .cert-badge-community { background: #fef9e7; color: #d4ac0d; border: 1px solid #f9e79f; }
    .cert-badge-reviewed  { background: #ebf5fb; color: #2e86c1; border: 1px solid #aed6f1; }
    .cert-badge-official  { background: #eafaf1; color: #1e8449; border: 1px solid #a9dfbf; }

    /* ── Detail Modal ── */
    .recipe-detail-header {
      padding: 2rem 2rem 1.5rem;
      color: #fff;
      border-radius: 0;
      position: relative;
    }
    .recipe-detail-header.cert-community {
      background: linear-gradient(135deg, #f39c12 0%, #e67e22 50%, #d35400 100%);
    }
    .recipe-detail-header.cert-reviewed {
      background: linear-gradient(135deg, #3498db 0%, #2980b9 50%, #1a5276 100%);
    }
    .recipe-detail-header.cert-official {
      background: linear-gradient(135deg, #2ecc71 0%, #27ae60 50%, #1e8449 100%);
    }
    .recipe-detail-header h3 {
      font-weight: 800;
      margin-bottom: .5rem;
      text-shadow: 0 2px 4px rgba(0,0,0,.15);
    }
    .recipe-detail-header .meta-row {
      display: flex;
      flex-wrap: wrap;
      gap: 1.5rem;
      font-size: .9rem;
      opacity: .95;
    }
    .recipe-detail-header .meta-item {
      display: flex;
      align-items: center;
      gap: .4rem;
    }

    /* ── Pipeline Graph ── */
    .pipeline-graph-container {
      background: #f8f9fa;
      border: 1px solid #eee;
      border-radius: 10px;
      overflow: hidden;
      margin-bottom: .5rem;
    }

    /* ── Workflow Timeline ── */
    .workflow-step {
      display: flex;
      align-items: flex-start;
      margin-bottom: 1rem;
      position: relative;
      padding-left: 2.5rem;
    }
    .workflow-step::before {
      content: '';
      position: absolute;
      left: 14px;
      top: 30px;
      bottom: -1rem;
      width: 2px;
      background: #dee2e6;
    }
    .workflow-step:last-child::before { display: none; }
    .wf-dot {
      position: absolute;
      left: 4px;
      top: 6px;
      width: 22px;
      height: 22px;
      border-radius: 50%;
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: .6rem;
      font-weight: 800;
      color: #fff;
      z-index: 1;
      background: #f39c12;
    }
    .wf-dot-svymean   { background: #f39c12; }
    .wf-dot-svytotal  { background: #e74c3c; }
    .wf-dot-svyratio  { background: #8e44ad; }
    .wf-dot-svyby     { background: #2980b9; }
    .step-type-svymean   { color: #f39c12; }
    .step-type-svytotal  { color: #e74c3c; }
    .step-type-svyratio  { color: #8e44ad; }
    .step-type-svyby     { color: #2980b9; }

    /* ── Pipeline Timeline ── */
    .pipeline-section { padding: 1.5rem 0; }
    .pipeline-step {
      display: flex;
      align-items: flex-start;
      margin-bottom: 1rem;
      position: relative;
      padding-left: 2.5rem;
    }
    .pipeline-step::before {
      content: '';
      position: absolute;
      left: 14px;
      top: 30px;
      bottom: -1rem;
      width: 2px;
      background: #dee2e6;
    }
    .pipeline-step:last-child::before { display: none; }
    .step-dot {
      position: absolute;
      left: 4px;
      top: 6px;
      width: 22px;
      height: 22px;
      border-radius: 50%;
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: .65rem;
      font-weight: 800;
      color: #fff;
      z-index: 1;
    }
    .step-dot-compute  { background: #3498db; }
    .step-dot-recode   { background: #9b59b6; }
    .step-dot-rename   { background: #e67e22; }
    .step-dot-remove   { background: #e74c3c; }
    .step-dot-join     { background: #1abc9c; }
    .step-dot-default  { background: #95a5a6; }
    .step-content {
      background: #fff;
      border: 1px solid #eee;
      border-radius: 8px;
      padding: .6rem 1rem;
      flex: 1;
      box-shadow: 0 1px 3px rgba(0,0,0,.04);
    }
    .step-type {
      font-weight: 700;
      font-size: .8rem;
      text-transform: uppercase;
      letter-spacing: .04em;
    }
    .step-type-compute  { color: #3498db; }
    .step-type-recode   { color: #9b59b6; }
    .step-type-rename   { color: #e67e22; }
    .step-type-remove   { color: #e74c3c; }
    .step-type-join     { color: #1abc9c; }
    .step-outputs {
      color: #2c3e50;
      font-size: .85rem;
      font-weight: 600;
    }

    /* ── Variable Chips ── */
    .var-chip {
      display: inline-block;
      padding: .2rem .65rem;
      border-radius: 6px;
      font-size: .78rem;
      font-family: 'SFMono-Regular', monospace;
      margin: .15rem;
      font-weight: 500;
    }
    .var-chip-input  { background: #ebf5fb; color: #2471a3; border: 1px solid #aed6f1; }
    .var-chip-output { background: #eafaf1; color: #1e8449; border: 1px solid #a9dfbf; }
    .var-chip-output-cat { background: #f5eef8; color: #7d3c98; border: 1px solid #d2b4de; }

    /* ── Cross-reference Chips ── */
    .cross-ref-chip {
      display: inline-block;
      padding: .25rem .75rem;
      border-radius: 20px;
      font-size: .8rem;
      font-weight: 600;
      cursor: pointer;
      transition: all .2s ease;
      margin: .15rem;
      text-decoration: none;
    }
    .cross-ref-recipe {
      background: #ebf5fb; color: #2471a3; border: 1px solid #aed6f1;
    }
    .cross-ref-recipe:hover {
      background: #2471a3; color: #fff;
    }
    .cross-ref-workflow {
      background: #fef9e7; color: #d4ac0d; border: 1px solid #f9e79f;
    }
    .cross-ref-workflow:hover {
      background: #d4ac0d; color: #fff;
    }
    .est-type-badge {
      display: inline-block;
      padding: .15rem .55rem;
      border-radius: 12px;
      font-size: .72rem;
      font-weight: 600;
      margin: .1rem;
    }
    .est-type-annual    { background: #d4efdf; color: #1e8449; }
    .est-type-quarterly { background: #d6eaf8; color: #2471a3; }
    .est-type-monthly   { background: #fdebd0; color: #ca6f1e; }

    /* ── Section Titles ── */
    .section-title {
      font-weight: 700;
      font-size: .95rem;
      color: #2c3e50;
      padding-bottom: .5rem;
      margin-bottom: 1rem;
      border-bottom: 2px solid #eee;
      display: flex;
      align-items: center;
      gap: .5rem;
    }
    .section-title svg { opacity: .6; }

    /* ── Stats Row ── */
    .stat-box {
      text-align: center;
      padding: 1rem;
      border-radius: 10px;
      background: #f8f9fa;
      border: 1px solid #eee;
    }
    .stat-box .stat-number {
      font-size: 1.8rem;
      font-weight: 800;
      color: #2c3e50;
    }
    .stat-box .stat-label {
      font-size: .75rem;
      color: #95a5a6;
      text-transform: uppercase;
      letter-spacing: .05em;
    }

    /* ── Search Bar ── */
    .search-container {
      background: #fff;
      border-radius: 12px;
      padding: 1.25rem;
      box-shadow: 0 2px 8px rgba(0,0,0,.06);
      margin-bottom: 1.25rem;
    }

    /* ── Auth Forms ── */
    .auth-container {
      max-width: 450px;
      margin: 3rem auto;
      background: #fff;
      border-radius: 16px;
      padding: 2.5rem;
      box-shadow: 0 4px 20px rgba(0,0,0,.08);
    }
    .auth-container h3 {
      text-align: center;
      font-weight: 800;
      color: #2c3e50;
      margin-bottom: 1.5rem;
    }
    .auth-container .btn-primary {
      width: 100%;
      padding: .6rem;
      font-weight: 600;
      border-radius: 8px;
    }

    /* ── Empty State ── */
    .empty-state {
      text-align: center;
      padding: 4rem 2rem;
      color: #95a5a6;
    }
    .empty-state svg { opacity: .3; margin-bottom: 1rem; }
    .empty-state h5 { color: #7f8c8d; }

    /* ── Profile ── */
    .profile-header {
      background: linear-gradient(135deg, #2c3e50, #34495e);
      color: #fff;
      padding: 2.5rem;
      border-radius: 16px;
      margin-bottom: 1.5rem;
    }
    .profile-header h3 { font-weight: 800; }
    .profile-header .user-type-label {
      display: inline-block;
      padding: .2rem .7rem;
      border-radius: 20px;
      background: rgba(255,255,255,.2);
      font-size: .8rem;
      margin-top: .5rem;
    }
  "))
}

# --- Component builders ---

cert_badge <- function(level) {
  cls <- paste0("cert-badge cert-badge-", level)
  icon <- switch(level,
    "official"  = bsicons::bs_icon("star-fill", size = ".75rem"),
    "reviewed"  = bsicons::bs_icon("check-circle-fill", size = ".75rem"),
    "community" = bsicons::bs_icon("people-fill", size = ".75rem"),
    ""
  )
  label <- switch(level,
    "official" = "Official", "reviewed" = "Reviewed", "community" = "Community", level
  )
  htmltools::tags$span(class = cls, icon, label)
}

category_tag <- function(name) {
  cls <- paste0("category-tag tag-", name)
  # Fallback for unknown categories
  if (!name %in% c("labor_market", "income", "education", "health", "demographics", "housing")) {
    cls <- "category-tag tag-default"
  }
  pretty <- gsub("_", " ", name)
  htmltools::tags$span(class = cls, pretty)
}

format_downloads <- function(n) {
  n <- as.integer(n)
  if (n >= 1000) paste0(round(n / 1000, 1), "k")
  else as.character(n)
}

recipe_card_ui <- function(recipe, ns, index) {
  doc <- recipe$doc()
  cert_level <- recipe$certification$level %||% "community"
  cert_cls <- paste0("cert-", cert_level)

  cat_tags <- if (length(recipe$categories) > 0) {
    htmltools::tagList(lapply(recipe$categories, function(c) category_tag(c$name)))
  } else if (!is.null(recipe$topic)) {
    category_tag(recipe$topic)
  }

  htmltools::tags$div(
    class = "recipe-card",
    onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})",
                      ns("card_click"), index),

    htmltools::tags$div(class = paste("recipe-card-header", cert_cls),
      htmltools::tags$h5(recipe$name),
      htmltools::tags$div(class = "card-subtitle",
        bsicons::bs_icon("person-fill", size = ".75rem"),
        recipe$user,
        htmltools::tags$span(style = "margin-left: .75rem;",
          bsicons::bs_icon("journal-code", size = ".75rem"),
          paste(recipe$survey_type, "/", recipe$edition)
        )
      )
    ),

    htmltools::tags$div(class = "recipe-card-body",
      htmltools::tags$div(class = "description",
        recipe$description %||% "No description provided."
      ),
      htmltools::tags$div(style = "margin-bottom: .5rem;", cat_tags),
      cert_badge(cert_level)
    ),

    htmltools::tags$div(class = "recipe-card-footer",
      htmltools::tags$div(class = "downloads",
        bsicons::bs_icon("download", size = ".85rem"),
        format_downloads(recipe$downloads)
      ),
      htmltools::tags$span(style = "color: #95a5a6; font-size: .75rem;",
        paste0("v", recipe$version %||% "1.0.0")
      ),
      htmltools::tags$button(
        class = paste0("btn btn-sm btn-outline-", if (cert_level == "official") "success"
                       else if (cert_level == "reviewed") "primary" else "warning"),
        class = "btn-view",
        bsicons::bs_icon("arrow-right", size = ".75rem"),
        " View"
      )
    )
  )
}

recipe_detail_ui <- function(recipe, ns = NULL, referencing_workflows = list()) {
  doc <- recipe$doc()
  cert_level <- recipe$certification$level %||% "community"
  cert_cls <- paste0("cert-", cert_level)

  # Header
  header <- htmltools::tags$div(
    class = paste("recipe-detail-header", cert_cls),
    htmltools::tags$h3(recipe$name),
    htmltools::tags$div(class = "meta-row",
      htmltools::tags$span(class = "meta-item",
        bsicons::bs_icon("person-fill"), recipe$user),
      htmltools::tags$span(class = "meta-item",
        bsicons::bs_icon("journal-code"),
        paste(recipe$survey_type, "/", recipe$edition)),
      htmltools::tags$span(class = "meta-item",
        bsicons::bs_icon("tag-fill"), paste0("v", recipe$version %||% "1.0.0")),
      htmltools::tags$span(class = "meta-item",
        bsicons::bs_icon("download"),
        format_downloads(recipe$downloads)),
      if (!is.null(recipe$doi)) htmltools::tags$span(class = "meta-item",
        bsicons::bs_icon("link-45deg"), recipe$doi)
    ),
    htmltools::tags$div(style = "margin-top: .75rem;",
      cert_badge(cert_level),
      if (!is.null(recipe$certification$certified_by)) {
        htmltools::tags$span(
          style = "color: rgba(255,255,255,.85); font-size: .8rem; margin-left: .5rem;",
          paste("by", recipe$certification$certified_by$name)
        )
      }
    )
  )

  # Description
  desc_section <- if (!is.null(recipe$description) && nzchar(recipe$description)) {
    htmltools::tags$div(style = "padding: 1.25rem 0;",
      htmltools::tags$p(style = "color: #555; font-size: .95rem; line-height: 1.7;",
        recipe$description
      )
    )
  }

  # Categories
  cat_section <- if (length(recipe$categories) > 0) {
    htmltools::tags$div(style = "padding-bottom: 1rem;",
      htmltools::tags$div(class = "section-title",
        bsicons::bs_icon("tags-fill"), "Categories"),
      htmltools::tagList(lapply(recipe$categories, function(c) {
        category_tag(c$name)
      }))
    )
  }

  # Pipeline graph (visNetwork)
  has_visnetwork <- requireNamespace("visNetwork", quietly = TRUE)
  graph_section <- if (length(doc$pipeline) > 0 && has_visnetwork && !is.null(ns)) {
    htmltools::tags$div(style = "padding: 1rem 0;",
      htmltools::tags$div(class = "section-title",
        bsicons::bs_icon("diagram-3-fill"),
        paste("Pipeline Graph (", length(doc$pipeline), "steps )")
      ),
      htmltools::tags$div(
        class = "pipeline-graph-container",
        visNetwork::visNetworkOutput(ns("recipe_graph"), height = "350px")
      )
    )
  }

  # Pipeline timeline
  pipeline_section <- if (length(doc$pipeline) > 0) {
    steps_html <- lapply(doc$pipeline, function(step) {
      step_type <- step$type %||% "unknown"
      base_type <- gsub("ast_", "", step_type)
      dot_cls <- paste0("step-dot step-dot-", if (base_type %in% c("compute", "recode", "rename", "remove", "join")) base_type else "default")
      type_cls <- paste0("step-type step-type-", base_type)

      outputs_str <- if (length(step$outputs) > 0) paste(step$outputs, collapse = ", ") else "(no output)"
      comment_html <- if (!is.null(step$comment) && length(step$comment) == 1 && nzchar(step$comment)) {
        htmltools::tags$div(style = "font-size:.75rem; color:#95a5a6; font-style:italic;", step$comment)
      }

      htmltools::tags$div(class = "pipeline-step",
        htmltools::tags$div(class = dot_cls, step$index),
        htmltools::tags$div(class = "step-content",
          htmltools::tags$span(class = type_cls, step_type),
          htmltools::tags$span(style = "margin: 0 .35rem; color: #bbb;",
            bsicons::bs_icon("arrow-right", size = ".7rem")),
          htmltools::tags$span(class = "step-outputs", outputs_str),
          comment_html
        )
      )
    })

    htmltools::tags$div(class = "pipeline-section",
      htmltools::tags$div(class = "section-title",
        bsicons::bs_icon("list-ol"),
        "Step Details"
      ),
      htmltools::tagList(steps_html)
    )
  }

  # Input variables
  input_section <- if (length(doc$input_variables) > 0) {
    htmltools::tags$div(style = "padding: 1rem 0;",
      htmltools::tags$div(class = "section-title",
        bsicons::bs_icon("box-arrow-in-right"),
        paste("Requires (", length(doc$input_variables), "variables )")
      ),
      htmltools::tagList(lapply(doc$input_variables, function(v) {
        htmltools::tags$span(class = "var-chip var-chip-input", v)
      }))
    )
  }

  # Output variables
  output_section <- if (length(doc$output_variables) > 0) {
    # Build type map from pipeline
    type_map <- list()
    for (step in doc$pipeline) {
      if (length(step$outputs) > 0 && !is.null(step$inferred_type)) {
        for (o in step$outputs) type_map[[o]] <- step$inferred_type
      }
    }

    htmltools::tags$div(style = "padding: 1rem 0;",
      htmltools::tags$div(class = "section-title",
        bsicons::bs_icon("box-arrow-right"),
        paste("Produces (", length(doc$output_variables), "variables )")
      ),
      htmltools::tagList(lapply(doc$output_variables, function(v) {
        var_type <- type_map[[v]] %||% "unknown"
        cls <- if (var_type == "categorical") "var-chip var-chip-output-cat"
               else "var-chip var-chip-output"
        htmltools::tags$span(class = cls,
          v,
          htmltools::tags$span(
            style = "font-size:.65rem; opacity:.7; margin-left:.3rem;",
            paste0("[", var_type, "]")
          )
        )
      }))
    )
  }

  # Used in Workflows (cross-reference section)
  workflow_section <- if (length(referencing_workflows) > 0) {
    wf_chips <- lapply(referencing_workflows, function(wf) {
      wf_name <- wf$name %||% wf$id
      n_est <- length(wf$call_metadata)
      if (!is.null(ns)) {
        htmltools::tags$span(
          class = "cross-ref-chip cross-ref-workflow",
          onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                            ns("navigate_workflow"), wf$id),
          bsicons::bs_icon("bar-chart-fill", size = ".75rem"),
          " ", wf_name,
          htmltools::tags$span(
            style = "font-size:.65rem; opacity:.7; margin-left:.3rem;",
            paste0("(", n_est, " est.)")
          )
        )
      } else {
        htmltools::tags$span(
          class = "cross-ref-chip cross-ref-workflow",
          bsicons::bs_icon("bar-chart-fill", size = ".75rem"),
          " ", wf_name
        )
      }
    })

    htmltools::tags$div(style = "padding: 1rem 0;",
      htmltools::tags$div(class = "section-title",
        bsicons::bs_icon("bar-chart-fill"),
        paste("Used in Workflows (", length(referencing_workflows), ")")
      ),
      htmltools::tagList(wf_chips)
    )
  }

  # Assemble
  htmltools::tags$div(
    header,
    htmltools::tags$div(style = "padding: 0 2rem 1.5rem;",
      desc_section,
      cat_section,
      graph_section,
      pipeline_section,
      workflow_section,
      input_section,
      output_section
    )
  )
}

# ── Build a visNetwork graph from recipe pipeline data ──
recipe_pipeline_graph <- function(recipe) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) return(NULL)

  doc <- recipe$doc()
  pipeline <- doc$pipeline
  if (length(pipeline) == 0) return(NULL)

  input_vars  <- doc$input_variables
  output_vars <- doc$output_variables
  workflows   <- list()  # Workflows now live as separate RecipeWorkflow objects

  # ── Palette (aligned with view_graph + Shiny app) ──
  pal <- list(
    primary    = "#2C3E50",
    compute    = "#3498db",
    recode     = "#9b59b6",
    join       = "#1abc9c",
    remove     = "#e74c3c",
    rename     = "#e67e22",
    default    = "#95a5a6",
    bg         = "#f8f9fa",
    edge       = "#bdc3c7",
    edge_dep   = "#e67e22",
    input_var  = "#2471a3",
    output_var = "#1e8449",
    wf_mean    = "#f39c12",
    wf_total   = "#e74c3c",
    wf_ratio   = "#8e44ad",
    wf_by      = "#2980b9"
  )

  step_color <- function(type) {
    base <- gsub("^(ast_|step_)", "", type)
    switch(base,
      "compute" = pal$compute, "recode" = pal$recode,
      "join" = pal$join, "remove" = pal$remove, "rename" = pal$rename,
      pal$default
    )
  }

  wf_color <- function(type) {
    base <- gsub("^survey::", "", type)
    switch(base,
      "svymean" = pal$wf_mean, "svytotal" = pal$wf_total,
      "svyratio" = pal$wf_ratio, "svyby" = pal$wf_by,
      pal$wf_mean
    )
  }

  wf_icon <- function(type) {
    base <- gsub("^survey::", "", type)
    switch(base,
      "svymean"  = "f201",
      "svytotal" = "f1fe",
      "svyratio" = "f080",
      "svyby"    = "f1de",
      "f080"
    )
  }

  # ── Build output-to-step map first (for inter-step edges) ──
  produced_by <- list()
  for (s in pipeline) {
    if (length(s$outputs) > 0) {
      for (o in s$outputs) produced_by[[o]] <- s$index
    }
  }

  # ── Build nodes & edges ──
  node_id <- 0L
  nodes <- list()
  edges <- list()
  add_node <- function(df) { nodes[[length(nodes) + 1L]] <<- df }
  add_edge <- function(from, to, edgetype = "default") {
    edges[[length(edges) + 1L]] <<- data.frame(from = from, to = to, edgetype = edgetype, stringsAsFactors = FALSE)
  }

  # Survey root
  node_id <- node_id + 1L
  survey_id <- node_id
  add_node(data.frame(
    id = survey_id,
    label = paste0(toupper(recipe$survey_type), " ", recipe$edition),
    title = paste0(
      "<div style='font-family:system-ui,sans-serif;padding:10px 14px;'>",
      "<div style='font-weight:800;color:", pal$primary, ";font-size:13px;'>",
      htmltools::htmlEscape(recipe$name), "</div>",
      "<div style='font-size:11px;color:#6c757d;margin-top:4px;'>",
      htmltools::htmlEscape(recipe$survey_type), " / ",
      htmltools::htmlEscape(recipe$edition), "</div></div>"
    ),
    group = "survey", level = 0L,
    stringsAsFactors = FALSE
  ))

  # Input variable nodes
  input_ids <- list()
  for (v in input_vars) {
    node_id <- node_id + 1L
    input_ids[[v]] <- node_id
    add_node(data.frame(
      id = node_id, label = v,
      title = paste0(
        "<div style='font-family:SFMono-Regular,monospace;padding:8px 12px;",
        "font-size:12px;color:", pal$input_var, ";'>Input: <b>",
        htmltools::htmlEscape(v), "</b></div>"
      ),
      group = "input_var", level = 1L,
      stringsAsFactors = FALSE
    ))
    add_edge(survey_id, node_id)
  }

  # Step nodes — with proper inter-step dependency edges
  step_ids <- list()
  for (s in pipeline) {
    node_id <- node_id + 1L
    stype <- s$type %||% "unknown"
    sidx <- as.character(s$index)
    step_ids[[sidx]] <- node_id

    outputs_str <- if (length(s$outputs) > 0) paste(s$outputs, collapse = ", ") else ""
    comment_str <- if (!is.null(s$comment) && nzchar(s$comment)) s$comment else ""

    add_node(data.frame(
      id = node_id,
      label = paste0(s$index, ". ", outputs_str),
      title = paste0(
        "<div style='font-family:system-ui,sans-serif;padding:10px 14px;max-width:300px;'>",
        "<div style='font-weight:700;font-size:13px;color:", step_color(stype), ";",
        "text-transform:uppercase;letter-spacing:.04em;margin-bottom:4px;'>",
        htmltools::htmlEscape(stype), "</div>",
        if (nzchar(outputs_str)) paste0(
          "<div style='font-size:12px;font-weight:600;color:", pal$primary, ";'>",
          htmltools::htmlEscape(outputs_str), "</div>"
        ) else "",
        if (nzchar(comment_str)) paste0(
          "<div style='font-size:11px;color:#95a5a6;font-style:italic;margin-top:4px;'>",
          htmltools::htmlEscape(comment_str), "</div>"
        ) else "",
        "</div>"
      ),
      group = stype, level = as.integer(s$index) + 1L,
      stringsAsFactors = FALSE
    ))

    # Edges: from input vars OR from producing step
    step_inputs <- s$inputs
    has_any_edge <- FALSE
    if (length(step_inputs) > 0) {
      for (inp in step_inputs) {
        if (inp %in% names(input_ids)) {
          add_edge(input_ids[[inp]], node_id)
          has_any_edge <- TRUE
        } else if (inp %in% names(produced_by)) {
          src_step <- step_ids[[as.character(produced_by[[inp]])]]
          if (!is.null(src_step)) {
            add_edge(src_step, node_id, "dependency")
            has_any_edge <- TRUE
          }
        }
      }
    }
    # Fallback: chain to previous step if no edges created
    if (!has_any_edge && s$index > 1) {
      prev_id <- step_ids[[as.character(s$index - 1)]]
      if (!is.null(prev_id)) add_edge(prev_id, node_id)
    }
  }

  # Output variable nodes
  type_map <- list()
  for (s in pipeline) {
    if (length(s$outputs) > 0 && !is.null(s$inferred_type)) {
      for (o in s$outputs) type_map[[o]] <- s$inferred_type
    }
  }

  output_node_ids <- list()
  max_step_level <- if (length(pipeline) > 0) max(sapply(pipeline, function(s) s$index)) + 1L else 1L
  for (v in output_vars) {
    node_id <- node_id + 1L
    output_node_ids[[v]] <- node_id
    vtype <- type_map[[v]] %||% "unknown"
    from_id <- if (v %in% names(produced_by)) {
      step_ids[[as.character(produced_by[[v]])]]
    } else if (length(step_ids) > 0) {
      step_ids[[names(step_ids)[length(step_ids)]]]
    } else {
      survey_id
    }
    add_node(data.frame(
      id = node_id, label = v,
      title = paste0(
        "<div style='font-family:SFMono-Regular,monospace;padding:8px 12px;",
        "font-size:12px;color:", pal$output_var, ";'>Output: <b>",
        htmltools::htmlEscape(v), "</b> [", htmltools::htmlEscape(vtype), "]</div>"
      ),
      group = paste0("output_", vtype), level = max_step_level + 1L,
      stringsAsFactors = FALSE
    ))
    add_edge(from_id, node_id)
  }

  # Workflow nodes
  if (length(workflows) > 0) {
    wf_level <- max_step_level + 2L
    for (i in seq_along(workflows)) {
      wf <- workflows[[i]]
      node_id <- node_id + 1L
      wf_type <- wf$type %||% "svymean"
      base_type <- gsub("^survey::", "", wf_type)
      wf_name <- wf$name %||% paste0(base_type, "(", wf$formula %||% "", ")")
      formula_str <- wf$formula %||% ""
      by_str <- if (!is.null(wf$by) && nzchar(wf$by)) paste0(" by ", wf$by) else ""
      desc_str <- wf$description %||% ""

      add_node(data.frame(
        id = node_id,
        label = wf_name,
        title = paste0(
          "<div style='font-family:system-ui,sans-serif;padding:10px 14px;max-width:320px;'>",
          "<div style='font-weight:700;font-size:13px;color:", wf_color(wf_type), ";",
          "text-transform:uppercase;letter-spacing:.04em;margin-bottom:4px;'>",
          htmltools::htmlEscape(base_type), "</div>",
          "<div style='font-family:SFMono-Regular,monospace;font-size:11px;color:", pal$primary, ";",
          "background:", pal$bg, ";padding:6px 8px;border-radius:4px;margin:4px 0;'>",
          htmltools::htmlEscape(paste0(base_type, "(", formula_str, by_str, ")")), "</div>",
          if (nzchar(desc_str)) paste0(
            "<div style='font-size:11px;color:#95a5a6;font-style:italic;margin-top:4px;'>",
            htmltools::htmlEscape(desc_str), "</div>"
          ) else "",
          "</div>"
        ),
        group = paste0("wf_", base_type), level = wf_level,
        stringsAsFactors = FALSE
      ))

      # Connect workflow to the output variables it uses
      wf_vars <- wf$uses %||% character(0)
      connected <- FALSE
      for (wv in wf_vars) {
        if (wv %in% names(output_node_ids)) {
          add_edge(output_node_ids[[wv]], node_id, "workflow")
          connected <- TRUE
        } else if (wv %in% names(input_ids)) {
          add_edge(input_ids[[wv]], node_id, "workflow")
          connected <- TRUE
        }
      }
      # Fallback: connect to last step
      if (!connected && length(step_ids) > 0) {
        last_step <- step_ids[[names(step_ids)[length(step_ids)]]]
        add_edge(last_step, node_id, "workflow")
      }
    }
  }

  # ── Assemble data.frames ──
  all_nodes <- do.call(rbind, nodes)
  all_edges <- if (length(edges) > 0) do.call(rbind, edges) else {
    data.frame(from = integer(), to = integer(), edgetype = character(), stringsAsFactors = FALSE)
  }

  # ── Build visNetwork ──
  graph <- visNetwork::visNetwork(
    nodes = all_nodes, edges = all_edges,
    height = "400px", width = "100%",
    background = pal$bg
  ) |>
    # Survey node
    visNetwork::visGroups(
      groupname = "survey",
      shape = "icon",
      icon = list(code = "f1c0", size = 55, color = pal$primary),
      font = list(size = 14, color = pal$primary, face = "bold"),
      shadow = list(enabled = TRUE, size = 8, color = "rgba(44,62,80,.15)")
    ) |>
    # Input variables
    visNetwork::visGroups(
      groupname = "input_var",
      shape = "box",
      color = list(background = "#ebf5fb", border = "#aed6f1",
                   highlight = list(background = "#d4e6f1", border = pal$input_var)),
      font = list(size = 11, color = pal$input_var, face = "monospace"),
      borderWidth = 1,
      shadow = list(enabled = TRUE, size = 3, color = "rgba(36,113,163,.1)")
    ) |>
    # Step groups
    visNetwork::visGroups(
      groupname = "compute",
      shape = "icon",
      icon = list(code = "f1ec", size = 45, color = pal$compute),
      font = list(size = 13, color = pal$primary, face = "bold"),
      shadow = list(enabled = TRUE, size = 6, color = "rgba(52,152,219,.2)")
    ) |>
    visNetwork::visGroups(
      groupname = "recode",
      shape = "icon",
      icon = list(code = "f0e8", size = 45, color = pal$recode),
      font = list(size = 13, color = pal$primary, face = "bold"),
      shadow = list(enabled = TRUE, size = 6, color = "rgba(155,89,182,.2)")
    ) |>
    visNetwork::visGroups(
      groupname = "join", shape = "icon",
      icon = list(code = "f0c1", size = 45, color = pal$join),
      font = list(size = 13, color = pal$primary, face = "bold"),
      shadow = list(enabled = TRUE, size = 6, color = "rgba(26,188,156,.2)")
    ) |>
    visNetwork::visGroups(
      groupname = "step_join", shape = "icon",
      icon = list(code = "f0c1", size = 45, color = pal$join),
      font = list(size = 13, color = pal$primary, face = "bold"),
      shadow = list(enabled = TRUE, size = 6, color = "rgba(26,188,156,.2)")
    ) |>
    visNetwork::visGroups(
      groupname = "step_remove", shape = "icon",
      icon = list(code = "f1f8", size = 45, color = pal$remove),
      font = list(size = 13, color = pal$primary, face = "bold"),
      shadow = list(enabled = TRUE, size = 6, color = "rgba(231,76,60,.2)")
    ) |>
    visNetwork::visGroups(
      groupname = "remove", shape = "icon",
      icon = list(code = "f1f8", size = 45, color = pal$remove),
      font = list(size = 13, color = pal$primary, face = "bold"),
      shadow = list(enabled = TRUE, size = 6, color = "rgba(231,76,60,.2)")
    ) |>
    visNetwork::visGroups(
      groupname = "rename", shape = "icon",
      icon = list(code = "f044", size = 45, color = pal$rename),
      font = list(size = 13, color = pal$primary, face = "bold"),
      shadow = list(enabled = TRUE, size = 6, color = "rgba(230,126,34,.2)")
    ) |>
    visNetwork::visGroups(
      groupname = "step_rename", shape = "icon",
      icon = list(code = "f044", size = 45, color = pal$rename),
      font = list(size = 13, color = pal$primary, face = "bold"),
      shadow = list(enabled = TRUE, size = 6, color = "rgba(230,126,34,.2)")
    ) |>
    # Output variable groups
    visNetwork::visGroups(
      groupname = "output_numeric", shape = "box",
      color = list(background = "#eafaf1", border = "#a9dfbf",
                   highlight = list(background = "#d5f5e3", border = pal$output_var)),
      font = list(size = 11, color = pal$output_var, face = "monospace"),
      borderWidth = 1,
      shadow = list(enabled = TRUE, size = 3, color = "rgba(30,132,73,.1)")
    ) |>
    visNetwork::visGroups(
      groupname = "output_categorical", shape = "box",
      color = list(background = "#f5eef8", border = "#d2b4de",
                   highlight = list(background = "#ebdef0", border = "#7d3c98")),
      font = list(size = 11, color = "#7d3c98", face = "monospace"),
      borderWidth = 1,
      shadow = list(enabled = TRUE, size = 3, color = "rgba(125,60,152,.1)")
    ) |>
    visNetwork::visGroups(
      groupname = "output_unknown", shape = "box",
      color = list(background = "#f8f9fa", border = "#dee2e6",
                   highlight = list(background = "#eee", border = "#95a5a6")),
      font = list(size = 11, color = "#6c757d", face = "monospace"),
      borderWidth = 1
    ) |>
    # Workflow groups
    visNetwork::visGroups(
      groupname = "wf_svymean", shape = "icon",
      icon = list(code = "f201", size = 40, color = pal$wf_mean),
      font = list(size = 12, color = pal$primary, face = "bold"),
      shadow = list(enabled = TRUE, size = 6, color = "rgba(243,156,18,.2)")
    ) |>
    visNetwork::visGroups(
      groupname = "wf_svytotal", shape = "icon",
      icon = list(code = "f1fe", size = 40, color = pal$wf_total),
      font = list(size = 12, color = pal$primary, face = "bold"),
      shadow = list(enabled = TRUE, size = 6, color = "rgba(231,76,60,.2)")
    ) |>
    visNetwork::visGroups(
      groupname = "wf_svyratio", shape = "icon",
      icon = list(code = "f080", size = 40, color = pal$wf_ratio),
      font = list(size = 12, color = pal$primary, face = "bold"),
      shadow = list(enabled = TRUE, size = 6, color = "rgba(142,68,173,.2)")
    ) |>
    visNetwork::visGroups(
      groupname = "wf_svyby", shape = "icon",
      icon = list(code = "f1de", size = 40, color = pal$wf_by),
      font = list(size = 12, color = pal$primary, face = "bold"),
      shadow = list(enabled = TRUE, size = 6, color = "rgba(41,128,185,.2)")
    ) |>
    visNetwork::addFontAwesome() |>
    visNetwork::visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 0.6, type = "arrow")),
      color = list(color = pal$edge, highlight = pal$compute, hover = pal$compute),
      width = 1.5,
      smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.08)
    ) |>
    visNetwork::visHierarchicalLayout(
      direction = "LR",
      levelSeparation = 180,
      nodeSpacing = 100,
      sortMethod = "directed"
    ) |>
    visNetwork::visInteraction(
      hover = TRUE,
      tooltipDelay = 100,
      tooltipStyle = paste0(
        "position:fixed;visibility:hidden;padding:0;",
        "background:#fff;border-radius:10px;",
        "box-shadow:0 4px 20px rgba(0,0,0,.12);",
        "border:1px solid #eee;pointer-events:none;z-index:9999;"
      ),
      navigationButtons = TRUE
    ) |>
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)
    )

  graph
}

# ── Workflow Card UI ──

workflow_card_ui <- function(wf, ns, index) {
  cert_level <- wf$certification$level %||% "community"
  cert_cls <- paste0("cert-", cert_level)
  n_recipes <- length(wf$recipe_ids)
  n_est <- length(wf$call_metadata)

  est_badges <- if (length(wf$estimation_type) > 0) {
    htmltools::tagList(lapply(wf$estimation_type, function(et) {
      cls <- paste0("est-type-badge est-type-", et)
      htmltools::tags$span(class = cls, et)
    }))
  }

  htmltools::tags$div(
    class = "recipe-card",
    onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})",
                      ns("wf_card_click"), index),

    htmltools::tags$div(class = paste("recipe-card-header", cert_cls),
      htmltools::tags$h5(wf$name),
      htmltools::tags$div(class = "card-subtitle",
        bsicons::bs_icon("person-fill", size = ".75rem"),
        wf$user,
        htmltools::tags$span(style = "margin-left: .75rem;",
          bsicons::bs_icon("bar-chart-fill", size = ".75rem"),
          paste(n_est, "estimations"),
          bsicons::bs_icon("journal-code", size = ".75rem", class = "ms-2"),
          paste(n_recipes, "recipes")
        )
      )
    ),

    htmltools::tags$div(class = "recipe-card-body",
      htmltools::tags$div(class = "description",
        wf$description %||% "No description provided."
      ),
      htmltools::tags$div(style = "margin-bottom: .5rem;", est_badges),
      cert_badge(cert_level)
    ),

    htmltools::tags$div(class = "recipe-card-footer",
      htmltools::tags$div(class = "downloads",
        bsicons::bs_icon("download", size = ".85rem"),
        format_downloads(wf$downloads)
      ),
      htmltools::tags$span(style = "color: #95a5a6; font-size: .75rem;",
        paste0("v", wf$version %||% "1.0.0")
      ),
      htmltools::tags$button(
        class = paste0("btn btn-sm btn-outline-", if (cert_level == "official") "success"
                       else if (cert_level == "reviewed") "primary" else "warning"),
        class = "btn-view",
        bsicons::bs_icon("arrow-right", size = ".75rem"),
        " View"
      )
    )
  )
}

# ── Workflow Detail UI ──

workflow_detail_ui <- function(wf, recipes_list = list(), ns = NULL) {
  doc <- wf$doc()
  cert_level <- wf$certification$level %||% "community"
  cert_cls <- paste0("cert-", cert_level)

  # Header
  header <- htmltools::tags$div(class = paste("recipe-detail-header", cert_cls),
    htmltools::tags$h3(wf$name),
    htmltools::tags$div(class = "meta-row",
      htmltools::tags$div(class = "meta-item",
        bsicons::bs_icon("person-fill"), wf$user
      ),
      htmltools::tags$div(class = "meta-item",
        bsicons::bs_icon("clipboard-data"), paste(wf$survey_type, "/", wf$edition)
      ),
      if (length(wf$estimation_type) > 0) htmltools::tags$div(class = "meta-item",
        bsicons::bs_icon("bar-chart-fill"),
        paste(wf$estimation_type, collapse = ", ")
      ),
      htmltools::tags$div(class = "meta-item",
        bsicons::bs_icon("tag-fill"), paste0("v", wf$version)
      ),
      htmltools::tags$div(class = "meta-item",
        bsicons::bs_icon("download"), format_downloads(wf$downloads)
      )
    )
  )

  # Description
  desc_section <- if (!is.null(wf$description) && nzchar(wf$description)) {
    htmltools::tags$div(style = "padding: 1rem 1.5rem;",
      htmltools::tags$p(style = "color: #6c757d; line-height: 1.6;", wf$description)
    )
  }

  # Uses Recipes (cross-refs)
  recipe_refs <- NULL
  if (length(wf$recipe_ids) > 0) {
    # Build clickable chips for each recipe
    ref_chips <- lapply(wf$recipe_ids, function(rid) {
      # Find recipe name from the list
      rname <- rid
      for (r in recipes_list) {
        if (as.character(r$id) == rid) {
          rname <- r$name
          break
        }
      }
      if (!is.null(ns)) {
        htmltools::tags$span(
          class = "cross-ref-chip cross-ref-recipe",
          onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                            ns("navigate_recipe"), rid),
          bsicons::bs_icon("journal-code", size = ".75rem"),
          " ", rname
        )
      } else {
        htmltools::tags$span(
          class = "cross-ref-chip cross-ref-recipe",
          bsicons::bs_icon("journal-code", size = ".75rem"),
          " ", rname
        )
      }
    })

    recipe_refs <- htmltools::tags$div(style = "padding: 0 1.5rem 1rem;",
      htmltools::tags$div(class = "section-title",
        bsicons::bs_icon("journal-code"), " Uses Recipes"
      ),
      htmltools::tagList(ref_chips)
    )
  }

  # Estimations timeline
  est_section <- NULL
  if (length(doc$estimations) > 0) {
    est_items <- lapply(seq_along(doc$estimations), function(i) {
      cm <- doc$estimations[[i]]
      est_type <- cm$type %||% "unknown"
      dot_cls <- paste0("wf-dot wf-dot-", est_type)
      type_cls <- paste0("step-type step-type-", est_type)

      htmltools::tags$div(class = "workflow-step",
        htmltools::tags$div(class = dot_cls, i),
        htmltools::tags$div(class = "step-content",
          htmltools::tags$div(class = type_cls, est_type),
          htmltools::tags$div(class = "step-outputs",
            cm$formula %||% ""
          ),
          if (!is.null(cm$by)) htmltools::tags$div(
            style = "color: #7f8c8d; font-size: .8rem;",
            paste("by:", cm$by)
          ),
          if (!is.null(cm$description) && nzchar(cm$description %||% "")) htmltools::tags$div(
            style = "color: #95a5a6; font-size: .8rem; font-style: italic;",
            cm$description
          )
        )
      )
    })

    est_section <- htmltools::tags$div(style = "padding: 0 1.5rem 1rem;",
      htmltools::tags$div(class = "section-title",
        bsicons::bs_icon("bar-chart-fill"), " Estimations"
      ),
      htmltools::tagList(est_items)
    )
  }

  # Estimation type badges
  est_type_section <- NULL
  if (length(wf$estimation_type) > 0) {
    est_badges <- lapply(wf$estimation_type, function(et) {
      cls <- paste0("est-type-badge est-type-", et)
      htmltools::tags$span(class = cls, et)
    })
    est_type_section <- htmltools::tags$div(style = "padding: 0 1.5rem 1rem;",
      htmltools::tags$div(class = "section-title",
        bsicons::bs_icon("clock-fill"), " Estimation Types"
      ),
      htmltools::tagList(est_badges)
    )
  }

  htmltools::tagList(header, desc_section, recipe_refs, est_type_section, est_section)
}
