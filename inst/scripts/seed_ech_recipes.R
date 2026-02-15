#!/usr/bin/env Rscript
# ══════════════════════════════════════════════════════════════════════════════
# seed_ech_recipes.R — Seed MongoDB from JSON files
#
# Single source of truth: inst/seed-data/*.json
#   - recipes.json   → recipes collection
#   - workflows.json → workflows collection
#   - users.json     → users collection (seed/demo users)
#
# Usage:
#   METASURVEY_MONGO_URI="mongodb+srv://user:pass@cluster" \
#     Rscript inst/scripts/seed_ech_recipes.R
#
# Environment variables:
#   METASURVEY_MONGO_URI  (required) MongoDB connection string
#   METASURVEY_DB         (optional) Database name, default "metasurvey"
#
# Requires: jsonlite, mongolite
# ══════════════════════════════════════════════════════════════════════════════

library(jsonlite)
library(mongolite)

# ── Config ───────────────────────────────────────────────────────────────────
MONGO_URI <- Sys.getenv("METASURVEY_MONGO_URI", "")
DATABASE  <- Sys.getenv("METASURVEY_DB", "metasurvey")

if (!nzchar(MONGO_URI)) {
  stop("METASURVEY_MONGO_URI is required. Example:\n",
       "  METASURVEY_MONGO_URI='mongodb+srv://user:pass@cluster' ",
       "Rscript inst/scripts/seed_ech_recipes.R")
}

# ── Locate seed-data directory ───────────────────────────────────────────────
seed_dir <- if (file.exists("inst/seed-data/recipes.json")) {
  "inst/seed-data"
} else {
  system.file("seed-data", package = "metasurvey")
}

if (!nzchar(seed_dir) || !dir.exists(seed_dir)) {
  stop("Cannot find seed-data directory. Run from package root or install metasurvey first.")
}

# ── Load JSON files ──────────────────────────────────────────────────────────
cat("==========================================================\n")
cat("  metasurvey MongoDB Seed\n")
cat("==========================================================\n\n")

recipes_file   <- file.path(seed_dir, "recipes.json")
workflows_file <- file.path(seed_dir, "workflows.json")
users_file     <- file.path(seed_dir, "users.json")

recipes <- fromJSON(recipes_file, simplifyVector = FALSE)
cat(sprintf("  recipes.json    : %d entries\n", length(recipes)))

workflows <- if (file.exists(workflows_file)) {
  fromJSON(workflows_file, simplifyVector = FALSE)
} else {
  list()
}
cat(sprintf("  workflows.json  : %d entries\n", length(workflows)))

seed_users <- if (file.exists(users_file)) {
  fromJSON(users_file, simplifyVector = FALSE)
} else {
  list()
}
cat(sprintf("  users.json      : %d entries\n", length(seed_users)))

# ── Connect ──────────────────────────────────────────────────────────────────
db_users     <- mongolite::mongo(collection = "users",     db = DATABASE, url = MONGO_URI)
db_recipes   <- mongolite::mongo(collection = "recipes",   db = DATABASE, url = MONGO_URI)
db_workflows <- mongolite::mongo(collection = "workflows", db = DATABASE, url = MONGO_URI)

cat(sprintf("\nConnected to '%s'\n", DATABASE))
cat(sprintf("  Before: users=%d, recipes=%d, workflows=%d\n",
            db_users$count(), db_recipes$count(), db_workflows$count()))

# ── 1. Cleanup ───────────────────────────────────────────────────────────────
cat("\n[cleanup] Removing existing seed data...\n")

for (rid in vapply(recipes, function(r) r$id %||% "", character(1))) {
  db_recipes$remove(toJSON(list(id = rid), auto_unbox = TRUE))
}

for (wid in vapply(workflows, function(w) w$id %||% "", character(1))) {
  db_workflows$remove(toJSON(list(id = wid), auto_unbox = TRUE))
}

for (u in seed_users) {
  db_users$remove(toJSON(list(email = u$email), auto_unbox = TRUE))
}

cat(sprintf("  Cleared: %d recipe slots, %d workflow slots, %d user slots\n",
            length(recipes), length(workflows), length(seed_users)))

# ── 2. Insert users ─────────────────────────────────────────────────────────
if (length(seed_users) > 0) {
  cat(sprintf("\n[users] Inserting %d users...\n", length(seed_users)))
  for (u in seed_users) {
    tryCatch({
      db_users$insert(toJSON(u, auto_unbox = TRUE, null = "null"))
      cat(sprintf("  + %s (%s)\n", u$name, u$email))
    }, error = function(e) message("  ERROR: ", e$message))
  }
}

# ── 3. Insert recipes ───────────────────────────────────────────────────────
cat(sprintf("\n[recipes] Inserting %d recipes...\n", length(recipes)))
for (r in recipes) {
  tryCatch({
    db_recipes$insert(toJSON(r, auto_unbox = TRUE, null = "null"))
    cat(sprintf("  + %s [%s]\n", r$name, r$id))
  }, error = function(e) message("  ERROR inserting ", r$id, ": ", e$message))
}

# ── 4. Insert workflows ─────────────────────────────────────────────────────
cat(sprintf("\n[workflows] Inserting %d workflows...\n", length(workflows)))
for (wf in workflows) {
  tryCatch({
    db_workflows$insert(toJSON(wf, auto_unbox = TRUE, null = "null"))
    cat(sprintf("  + %s [%s]\n", wf$name, wf$id))
  }, error = function(e) message("  ERROR inserting ", wf$id, ": ", e$message))
}

# ── Summary ──────────────────────────────────────────────────────────────────
cat(sprintf("\n  After: users=%d, recipes=%d, workflows=%d\n",
            db_users$count(), db_recipes$count(), db_workflows$count()))
cat("\nSeed complete!\n")
