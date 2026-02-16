#!/usr/bin/env Rscript
# ══════════════════════════════════════════════════════════════════════════════
# transpile_iecon_all.R — Transpile all IECON do-files (2006-2022) to
# metasurvey Recipe JSON and merge into inst/seed-data/recipes.json
#
# Usage:
#   Rscript inst/scripts/transpile_iecon_all.R
#
# Requires: devtools::load_all() (or metasurvey installed)
# ══════════════════════════════════════════════════════════════════════════════

# Load the package in dev mode
if (file.exists("DESCRIPTION")) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(metasurvey)
}

YEARS <- 2006:2022
DO_DIR <- "do_files_iecon"
SEED_FILE <- "inst/seed-data/recipes.json"

if (!dir.exists(DO_DIR)) {
  stop("do_files_iecon directory not found. Run from package root.")
}

cat("==========================================================\n")
cat("  IECON STATA→metasurvey Transpilation (2006-2022)\n")
cat("==========================================================\n\n")

# ── Transpile each year ────────────────────────────────────────────────────
all_recipes <- list()
total_steps <- 0L
total_warnings <- 0L

for (yr in YEARS) {
  year_dir <- file.path(DO_DIR, as.character(yr))
  if (!dir.exists(year_dir)) {
    cat(sprintf("  [SKIP] %d — directory not found\n", yr))
    next
  }

  tryCatch({
    recipes <- transpile_stata_module(year_dir, yr, user = "iecon")
    n_steps <- sum(vapply(recipes, function(r) length(r$steps), integer(1)))
    total_steps <- total_steps + n_steps

    for (module_name in names(recipes)) {
      rec <- recipes[[module_name]]
      key <- rec$id
      all_recipes[[key]] <- rec
    }

    cat(sprintf("  [OK]   %d — %d modules, %d steps\n",
                yr, length(recipes), n_steps))
  }, error = function(e) {
    cat(sprintf("  [ERR]  %d — %s\n", yr, e$message))
  })
}

cat(sprintf("\nTotal: %d recipes, %d steps\n",
            length(all_recipes), total_steps))

# ── Load existing seed recipes (hand-written) ─────────────────────────────
existing <- jsonlite::fromJSON(SEED_FILE, simplifyVector = FALSE)
cat(sprintf("\nExisting seed recipes: %d\n", length(existing)))

# Remove any previous transpiled recipes (IDs starting with "ech_20")
existing_clean <- Filter(
  function(r) !grepl("^ech_20\\d{2}_", r$id),
  existing
)
cat(sprintf("After removing old transpiled: %d\n", length(existing_clean)))

# ── Convert transpiled recipes to list format ─────────────────────────────
transpiled_lists <- lapply(all_recipes, function(rec) {
  doc_info <- rec$doc()
  lst <- list(
    name = rec$name,
    user = rec$user,
    survey_type = rec$survey_type,
    edition = rec$edition,
    description = rec$description,
    topic = rec$topic,
    id = rec$id,
    version = rec$version,
    downloads = rec$downloads,
    depends_on = rec$depends_on,
    depends_on_recipes = rec$depends_on_recipes,
    certification = if (!is.null(rec$certification)) rec$certification$to_list() else NULL,
    user_info = if (!is.null(rec$user_info)) rec$user_info$to_list() else NULL,
    doc = list(
      input_variables = doc_info$input_variables,
      output_variables = doc_info$output_variables,
      pipeline = doc_info$pipeline
    ),
    steps = rec$steps
  )
  # Add labels if present
  if (!is.null(rec$labels)) {
    lst$labels <- rec$labels
  }
  lst
})

# ── Merge and write ───────────────────────────────────────────────────────
merged <- c(existing_clean, unname(transpiled_lists))
cat(sprintf("Merged total: %d recipes (%d hand-written + %d transpiled)\n",
            length(merged), length(existing_clean), length(transpiled_lists)))

jsonlite::write_json(
  merged,
  SEED_FILE,
  simplifyVector = TRUE,
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)

cat(sprintf("\nWritten to %s\n", SEED_FILE))
cat("Done!\n")
