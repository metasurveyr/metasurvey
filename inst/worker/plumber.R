# ==============================================================================
# metasurvey Compute Worker — plumber + metasurvey
#
# Internal service that executes survey estimations. NOT exposed to the
# public internet. Only the main API or internal services call this worker.
#
# Endpoints:
#   POST /compute  — Execute recipe + workflow on private microdata
#   GET  /health   — Worker health check
#
# The worker:
#   1. Loads microdata from private storage (SURVEY_DATA_DIR)
#   2. Fetches recipes from MongoDB
#   3. Applies recipes to the survey (bake_recipes)
#   4. Runs workflow() with optional filters (by variable, domain subset)
#   5. Returns the computed result (value, se, cv, CI)
#
# Config (env vars):
#   METASURVEY_MONGO_URI   — MongoDB connection string (required)
#   METASURVEY_DB          — Database name (default: metasurvey)
#   SURVEY_DATA_DIR        — Path to private microdata directory (required)
#   METASURVEY_API_URL     — Main API URL (for publishing indicators)
#
# Launch:
#   Rscript -e 'plumber::plumb("inst/worker/plumber.R")$run(port=8788)'
# ==============================================================================

library(plumber)
library(jsonlite)
library(mongolite)
library(metasurvey)
library(data.table)
library(survey)

# -- Config -------------------------------------------------------------------

MONGO_URI <- Sys.getenv("METASURVEY_MONGO_URI", "")
DATABASE <- Sys.getenv("METASURVEY_DB", "metasurvey")
DATA_DIR <- Sys.getenv("SURVEY_DATA_DIR", "/data/surveys")

if (!nzchar(MONGO_URI)) {
  stop("METASURVEY_MONGO_URI environment variable is required.")
}

if (!dir.exists(DATA_DIR)) {
  warning(sprintf(
    "[worker] SURVEY_DATA_DIR '%s' does not exist. Set it to the microdata directory.",
    DATA_DIR
  ))
}

# -- MongoDB connections (recipes + workflows for metadata) -------------------

db_recipes <- mongolite::mongo(
  collection = "recipes",
  db = DATABASE, url = MONGO_URI
)
db_workflows <- mongolite::mongo(
  collection = "workflows",
  db = DATABASE, url = MONGO_URI
)

message(sprintf(
  "[metasurvey-worker] Connected to MongoDB (db: %s)",
  DATABASE
))
message(sprintf(
  "[metasurvey-worker] Data dir: %s (%s)",
  DATA_DIR,
  if (dir.exists(DATA_DIR)) "exists" else "NOT FOUND"
))

# -- Helpers ------------------------------------------------------------------

#' Find microdata file for a survey type + edition
#' Looks for: {survey_type}_{edition}.csv, .rds, .sav, .dta
find_survey_file <- function(survey_type, edition) {
  base <- file.path(DATA_DIR, survey_type)
  if (!dir.exists(base)) base <- DATA_DIR

  patterns <- c(
    sprintf("%s_%s.rds", survey_type, edition),
    sprintf("%s_%s.csv", survey_type, edition),
    sprintf("%s_%s.sav", survey_type, edition),
    sprintf("%s_%s.dta", survey_type, edition)
  )

  for (p in patterns) {
    path <- file.path(base, p)
    if (file.exists(path)) return(path)
  }

  NULL
}

#' Load survey data from file
load_survey_data <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    rds = readRDS(path),
    csv = data.table::fread(path),
    sav = {
      if (!requireNamespace("haven", quietly = TRUE)) {
        stop("Package 'haven' required to read .sav files")
      }
      data.table::as.data.table(haven::read_sav(path))
    },
    dta = {
      if (!requireNamespace("haven", quietly = TRUE)) {
        stop("Package 'haven' required to read .dta files")
      }
      data.table::as.data.table(haven::read_dta(path))
    },
    stop(sprintf("Unsupported file format: .%s", ext))
  )
}

#' Fetch a recipe from MongoDB and reconstruct it
fetch_recipe <- function(recipe_id) {
  iter <- db_recipes$iterate(
    query = toJSON(list(id = recipe_id), auto_unbox = TRUE),
    limit = 1
  )
  doc <- iter$one()
  if (is.null(doc)) return(NULL)
  doc
}

#' Fetch a workflow from MongoDB
fetch_workflow <- function(workflow_id) {
  iter <- db_workflows$iterate(
    query = toJSON(list(id = workflow_id), auto_unbox = TRUE),
    limit = 1
  )
  doc <- iter$one()
  if (is.null(doc)) return(NULL)
  doc
}

#' Apply recipe steps to a survey
apply_recipe_steps <- function(svy, steps) {
  for (step_str in steps) {
    step_expr <- tryCatch(
      parse(text = step_str)[[1]],
      error = function(e) {
        warning(sprintf("Cannot parse step: %s", step_str))
        NULL
      }
    )
    if (is.null(step_expr)) next

    # Replace first argument with current survey
    step_call <- as.list(step_expr)
    step_call[[2]] <- quote(svy)
    step_expr <- as.call(step_call)

    svy <- tryCatch(
      eval(step_expr),
      error = function(e) {
        warning(sprintf(
          "Step failed: %s — %s",
          step_str, e$message
        ))
        svy
      }
    )
  }
  svy
}

#' Build and execute estimation calls
execute_estimation <- function(svy, calls, estimation_type, filters) {
  svy$ensure_design()
  design <- svy$design[[estimation_type]]

  if (is.null(design)) {
    available <- names(svy$design)
    stop(sprintf(
      "Design '%s' not available. Available: %s",
      estimation_type,
      paste(available, collapse = ", ")
    ))
  }

  # Apply domain filter if specified
  if (!is.null(filters$domain) && nzchar(filters$domain)) {
    domain_expr <- tryCatch(
      parse(text = filters$domain)[[1]],
      error = function(e) {
        stop(sprintf(
          "Invalid domain filter: %s",
          filters$domain
        ))
      }
    )
    design <- subset(design, eval(domain_expr))
  }

  results <- list()

  for (i in seq_along(calls)) {
    call_str <- calls[[i]]

    # If by_variable is specified, wrap non-svyby calls
    if (!is.null(filters$by_variable) &&
        nzchar(filters$by_variable) &&
        !grepl("svyby", call_str)) {
      # Transform svymean(~var, ...) -> svyby(~var, ~by, design, svymean, ...)
      call_str <- transform_to_svyby(call_str, filters$by_variable)
    }

    call_expr <- tryCatch(
      parse(text = call_str)[[1]],
      error = function(e) {
        warning(sprintf("Cannot parse call: %s", call_str))
        NULL
      }
    )
    if (is.null(call_expr)) next

    # Inject design into the call
    call_list <- as.list(call_expr)
    fn_name <- deparse(call_list[[1]])

    estimation <- tryCatch(
      {
        if (grepl("svyby", fn_name)) {
          # svyby(formula, by, design, FUN, ...)
          call_list[["design"]] <- quote(design)
          eval(as.call(call_list))
        } else {
          # svymean(formula, design, ...)
          call_list[["design"]] <- quote(design)
          eval(as.call(call_list))
        }
      },
      error = function(e) {
        warning(sprintf(
          "Estimation failed: %s — %s",
          call_str, e$message
        ))
        NULL
      }
    )
    if (is.null(estimation)) next

    result_dt <- format_estimation(
      estimation, fn_name, call_str
    )
    results[[length(results) + 1L]] <- result_dt
  }

  if (length(results) == 0) {
    return(data.table::data.table(
      stat = character(0),
      value = numeric(0),
      se = numeric(0),
      cv = numeric(0),
      confint_lower = numeric(0),
      confint_upper = numeric(0)
    ))
  }

  data.table::rbindlist(results, fill = TRUE)
}

#' Transform a svymean/svytotal call to svyby
transform_to_svyby <- function(call_str, by_variable) {
  # Parse: svymean(~var, na.rm = TRUE) ->
  #   svyby(~var, ~by_variable, design, svymean, na.rm = TRUE)
  call_expr <- parse(text = call_str)[[1]]
  call_list <- as.list(call_expr)
  fn_name <- deparse(call_list[[1]])

  # Extract formula (first arg after function name)
  formula <- call_list[[2]]
  # Get remaining args (skip design if present)
  extra_args <- list()
  if (length(call_list) > 2) {
    for (j in 3:length(call_list)) {
      nm <- names(call_list)[j]
      if (!is.null(nm) && nm == "design") next
      extra_args <- c(extra_args, call_list[j])
    }
  }

  by_formula <- parse(text = paste0("~", by_variable))[[1]]
  fn_sym <- as.symbol(fn_name)

  new_call <- as.call(c(
    list(quote(svyby), formula, by_formula, quote(design), fn_sym),
    extra_args
  ))
  deparse(new_call, width.cutoff = 500L)
}

#' Format estimation output to a data.table
format_estimation <- function(estimation, fn_name, call_str) {
  if (inherits(estimation, "svyby")) {
    # svyby result — has by-variable columns
    margins <- attr(estimation, "svyby")$margins
    by_vars <- if (is.numeric(margins)) {
      names(estimation)[margins]
    } else {
      margins
    }

    stat_vars <- attr(estimation, "svyby")$variables
    if (is.null(stat_vars)) {
      # Fallback: non-by, non-se columns
      all_cols <- names(estimation)
      se_cols <- grepl("^se\\.", all_cols)
      stat_vars <- all_cols[!se_cols & !all_cols %in% by_vars]
    }

    rows <- list()
    for (sv in stat_vars) {
      se_col <- paste0("se.", sv)
      if (!se_col %in% names(estimation)) {
        se_col <- "se"
      }

      for (r in seq_len(nrow(estimation))) {
        by_label <- paste(
          vapply(by_vars, function(bv) {
            paste0(bv, "=", estimation[r, bv])
          }, character(1)),
          collapse = ", "
        )

        val <- as.numeric(estimation[r, sv])
        se_val <- if (se_col %in% names(estimation)) {
          as.numeric(estimation[r, se_col])
        } else {
          NA_real_
        }
        cv_val <- if (!is.na(se_val) && val != 0) {
          se_val / abs(val)
        } else {
          NA_real_
        }

        rows[[length(rows) + 1L]] <- data.table::data.table(
          stat = sprintf("%s: %s [%s]", fn_name, sv, by_label),
          value = val,
          se = se_val,
          cv = cv_val,
          confint_lower = val - 1.96 * se_val,
          confint_upper = val + 1.96 * se_val,
          by_group = by_label,
          call = call_str
        )
      }
    }
    return(data.table::rbindlist(rows, fill = TRUE))
  }

  # Simple svymean/svytotal result
  ci <- tryCatch(
    stats::confint(estimation),
    error = function(e) NULL
  )

  var_names <- names(estimation)
  rows <- list()
  for (i in seq_along(var_names)) {
    val <- as.numeric(estimation[i])
    se_val <- as.numeric(SE(estimation)[i])
    cv_val <- if (val != 0) se_val / abs(val) else NA_real_
    ci_lo <- if (!is.null(ci)) ci[i, 1] else val - 1.96 * se_val
    ci_hi <- if (!is.null(ci)) ci[i, 2] else val + 1.96 * se_val

    rows[[length(rows) + 1L]] <- data.table::data.table(
      stat = sprintf("%s: %s", fn_name, var_names[i]),
      value = val,
      se = se_val,
      cv = cv_val,
      confint_lower = ci_lo,
      confint_upper = ci_hi,
      call = call_str
    )
  }
  data.table::rbindlist(rows, fill = TRUE)
}

# -- Global serializer --------------------------------------------------------

#* @plumber
function(pr) {
  pr$setSerializer(serializer_json(auto_unbox = TRUE))
  spec <- pr$getApiSpec()
  spec$info <- list(
    title = "metasurvey Worker",
    description = paste(
      "Internal compute service for metasurvey.",
      "Executes survey recipes and workflows",
      "on private microdata.",
      "NOT exposed to the public internet."
    ),
    version = "1.0.0"
  )
  pr$setApiSpec(spec)
}

# ==============================================================================
# COMPUTE
# ==============================================================================

#* Execute a survey estimation. Loads microdata,
#* applies recipe steps, runs workflow calls,
#* and returns computed results.
#*
#* Request body:
#*   recipe_ids: array of recipe IDs to apply
#*   workflow_id: workflow ID (for calls + metadata)
#*   survey_type: survey type (e.g. "ech")
#*   edition: survey edition (e.g. "2024")
#*   estimation_type: weight period ("annual", "monthly", "quarterly")
#*   weight: weight variable name (e.g. "W_ANO")
#*   filters: optional object with:
#*     by_variable: variable for subgroup estimation (svyby)
#*     domain: R expression for domain subset (e.g. "region == 1")
#*
#* @tag Compute
#* @post /compute
#* @response 200 Returns {ok, results: [{stat, value, se, cv, ...}]}.
#* @response 400 Missing required fields or invalid request.
#* @response 404 Recipe, workflow, or data file not found.
#* @response 500 Computation error.
function(req, res) {
  body <- req$body
  if (is.null(body)) {
    res$status <- 400L
    return(list(error = "Request body is required"))
  }

  # Validate required fields
  required <- c("workflow_id", "survey_type", "edition")
  missing_fields <- required[!required %in% names(body)]
  if (length(missing_fields) > 0) {
    res$status <- 400L
    return(list(error = paste(
      "Missing required fields:",
      paste(missing_fields, collapse = ", ")
    )))
  }

  survey_type <- body$survey_type
  edition <- body$edition
  workflow_id <- body$workflow_id
  recipe_ids <- body$recipe_ids
  estimation_type <- body$estimation_type %||% "annual"
  weight_var <- body$weight
  filters <- body$filters %||% list()

  tryCatch(
    {
      # 1. Fetch workflow
      wf <- fetch_workflow(workflow_id)
      if (is.null(wf)) {
        res$status <- 404L
        return(list(error = paste(
          "Workflow not found:", workflow_id
        )))
      }

      # Use recipe_ids from request, or from workflow
      if (is.null(recipe_ids) || length(recipe_ids) == 0) {
        recipe_ids <- wf$recipe_ids
      }

      # 2. Find and load microdata
      data_path <- find_survey_file(survey_type, edition)
      if (is.null(data_path)) {
        res$status <- 404L
        return(list(error = sprintf(
          "Microdata not found for %s %s in %s",
          survey_type, edition, DATA_DIR
        )))
      }

      message(sprintf(
        "[worker] Loading %s (%s)",
        data_path,
        format(file.size(data_path), big.mark = ",")
      ))
      dt <- load_survey_data(data_path)

      # 3. Create survey object
      weight_spec <- if (!is.null(weight_var)) {
        args <- list()
        args[[estimation_type]] <- weight_var
        do.call(add_weight, args)
      } else if (!is.null(wf$weight_spec)) {
        wf$weight_spec
      } else {
        NULL
      }

      svy <- Survey$new(
        data = dt,
        edition = edition,
        type = survey_type,
        engine = "data.table",
        weight = weight_spec
      )

      # 4. Fetch and apply recipes
      if (length(recipe_ids) > 0) {
        for (rid in recipe_ids) {
          recipe_doc <- fetch_recipe(rid)
          if (is.null(recipe_doc)) {
            warning(sprintf(
              "Recipe not found: %s (skipping)",
              rid
            ))
            next
          }

          steps <- recipe_doc$steps
          if (length(steps) > 0) {
            message(sprintf(
              "[worker] Applying recipe '%s' (%d steps)",
              recipe_doc$name, length(steps)
            ))
            svy <- apply_recipe_steps(svy, steps)
          }
        }

        svy <- bake_steps(svy)
      }

      # 5. Execute estimation calls
      calls <- wf$calls
      if (is.null(calls) || length(calls) == 0) {
        res$status <- 400L
        return(list(error = "Workflow has no estimation calls"))
      }

      message(sprintf(
        "[worker] Executing %d estimation calls (type: %s)",
        length(calls), estimation_type
      ))

      result_dt <- execute_estimation(
        svy, calls, estimation_type, filters
      )

      # 6. Return results
      results <- lapply(
        seq_len(nrow(result_dt)),
        function(i) as.list(result_dt[i, ])
      )

      list(
        ok = TRUE,
        survey_type = survey_type,
        edition = edition,
        estimation_type = estimation_type,
        workflow_id = workflow_id,
        recipe_ids = recipe_ids,
        filters = filters,
        count = length(results),
        results = results
      )
    },
    error = function(e) {
      message(sprintf("[worker] ERROR: %s", e$message))
      res$status <- 500L
      list(error = paste(
        "Computation failed:", e$message
      ))
    }
  )
}

# ==============================================================================
# HEALTH
# ==============================================================================

#* Worker health check. Returns service status,
#* MongoDB connection, and data directory info.
#* @tag System
#* @get /health
#* @response 200 Returns {status, service, ...}.
function() {
  mongo_ok <- tryCatch(
    {
      db_recipes$count()
      TRUE
    },
    error = function(e) FALSE
  )

  data_ok <- dir.exists(DATA_DIR)
  data_files <- if (data_ok) {
    length(list.files(
      DATA_DIR,
      pattern = "\\.(csv|rds|sav|dta)$",
      recursive = TRUE
    ))
  } else {
    0L
  }

  list(
    status = if (mongo_ok && data_ok) "ok" else "degraded",
    service = "metasurvey-worker",
    version = "1.0.0",
    mongodb = if (mongo_ok) "connected" else "disconnected",
    data_dir = DATA_DIR,
    data_dir_exists = data_ok,
    survey_files = data_files,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  )
}
