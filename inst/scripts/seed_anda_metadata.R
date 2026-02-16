#!/usr/bin/env Rscript
# ══════════════════════════════════════════════════════════════════════════════
# seed_anda_metadata.R — Fetch DDI from ANDA5, parse, seed MongoDB
#
# Downloads DDI XML from INE Uruguay's ANDA5 catalog, extracts variable-level
# metadata, and inserts into the anda_variables MongoDB collection.
# Also saves a backup JSON in inst/seed-data/anda_variables.json.
#
# Usage:
#   METASURVEY_MONGO_URI="mongodb+srv://..." Rscript inst/scripts/seed_anda_metadata.R
#
# Environment variables:
#   METASURVEY_MONGO_URI  (required)
#   METASURVEY_DB         (optional, default "metasurvey")
#   ANDA_CATALOG_ID       (optional, default 767 = ECH 2024)
#
# Requires: httr, xml2, jsonlite, mongolite
# ══════════════════════════════════════════════════════════════════════════════

library(httr)
library(jsonlite)
library(mongolite)

# ── Config ───────────────────────────────────────────────────────────────────
MONGO_URI <- Sys.getenv("METASURVEY_MONGO_URI", "")
DATABASE <- Sys.getenv("METASURVEY_DB", "metasurvey")
CATALOG_ID <- as.integer(Sys.getenv("ANDA_CATALOG_ID", "767"))
SURVEY_TYPE <- "ech"
EDITION <- "2024"
BASE_URL <- "https://www4.ine.gub.uy/Anda5"

if (!nzchar(MONGO_URI)) {
  stop("METASURVEY_MONGO_URI is required.")
}

if (!requireNamespace("xml2", quietly = TRUE)) {
  stop("Package 'xml2' is required. Install with: install.packages('xml2')")
}

cat("==========================================================\n")
cat("  ANDA Metadata Seed\n")
cat("==========================================================\n\n")

# ── 1. Download DDI XML ──────────────────────────────────────────────────────
ddi_url <- paste0(BASE_URL, "/metadata/export/", CATALOG_ID, "/ddi")
ddi_file <- tempfile(fileext = ".xml")

cat(sprintf("Downloading DDI from %s ...\n", ddi_url))
resp <- httr::GET(
  ddi_url,
  httr::config(ssl_verifypeer = FALSE),
  httr::write_disk(ddi_file, overwrite = TRUE),
  httr::timeout(120)
)

if (httr::status_code(resp) != 200) {
  stop("Failed to download DDI: HTTP ", httr::status_code(resp))
}

file_size <- file.info(ddi_file)$size
cat(sprintf("  Downloaded: %.1f KB\n", file_size / 1024))

# ── 2. Parse DDI XML ────────────────────────────────────────────────────────
cat("\nParsing DDI XML...\n")

doc <- xml2::read_xml(ddi_file)
ns <- xml2::xml_ns(doc)
vars <- xml2::xml_find_all(doc, ".//d1:dataDscr/d1:var", ns)
cat(sprintf("  Found %d <var> elements\n", length(vars)))

seen <- character(0)
parsed <- list()

for (v in vars) {
  name <- xml2::xml_attr(v, "name")
  if (is.na(name) || !nzchar(name)) next

  name_lower <- tolower(name)
  if (name_lower %in% seen) next
  seen <- c(seen, name_lower)

  intrvl <- xml2::xml_attr(v, "intrvl")
  var_type <- if (!is.na(intrvl) && intrvl == "contin") "continuous" else "discrete"

  labl_node <- xml2::xml_find_first(v, "d1:labl", ns)
  label <- if (!inherits(labl_node, "xml_missing")) trimws(xml2::xml_text(labl_node)) else ""

  txt_node <- xml2::xml_find_first(v, "d1:txt", ns)
  description <- if (!inherits(txt_node, "xml_missing")) trimws(xml2::xml_text(txt_node)) else ""

  # Value labels
  catgry_nodes <- xml2::xml_find_all(v, "d1:catgry", ns)
  value_labels <- NULL
  if (length(catgry_nodes) > 0) {
    value_labels <- list()
    for (cat_node in catgry_nodes) {
      cat_val <- xml2::xml_find_first(cat_node, "d1:catValu", ns)
      cat_lab <- xml2::xml_find_first(cat_node, "d1:labl", ns)
      if (!inherits(cat_val, "xml_missing") && !inherits(cat_lab, "xml_missing")) {
        val <- trimws(xml2::xml_text(cat_val))
        lab <- trimws(xml2::xml_text(cat_lab))
        value_labels[[val]] <- lab
      }
    }
    if (length(value_labels) == 0) value_labels <- NULL
  }

  parsed[[length(parsed) + 1L]] <- list(
    survey_type       = SURVEY_TYPE,
    name              = name_lower,
    label             = label,
    type              = var_type,
    value_labels      = value_labels,
    description       = description,
    source_edition    = EDITION,
    source_catalog_id = CATALOG_ID
  )
}

cat(sprintf("  Parsed %d unique variables\n", length(parsed)))

# ── 3. Save backup JSON ─────────────────────────────────────────────────────
seed_dir <- if (dir.exists("inst/seed-data")) "inst/seed-data" else NULL

if (!is.null(seed_dir)) {
  json_file <- file.path(seed_dir, "anda_variables.json")
  writeLines(
    toJSON(parsed, auto_unbox = TRUE, null = "null", pretty = TRUE),
    json_file
  )
  cat(sprintf("\nSaved backup: %s (%d variables)\n", json_file, length(parsed)))
}

# ── 4. Seed MongoDB ─────────────────────────────────────────────────────────
db_anda <- mongolite::mongo(collection = "anda_variables", db = DATABASE, url = MONGO_URI)

current_count <- db_anda$count()
cat(sprintf("\nConnected to '%s.anda_variables' (%d existing docs)\n", DATABASE, current_count))

# Clean existing entries for this survey type
db_anda$remove(toJSON(list(survey_type = SURVEY_TYPE), auto_unbox = TRUE))
cat(sprintf("[cleanup] Cleared survey_type='%s'\n", SURVEY_TYPE))

# Insert
cat(sprintf("\n[insert] Inserting %d variables...\n", length(parsed)))
errors <- 0L
for (p in parsed) {
  tryCatch(
    {
      db_anda$insert(toJSON(p, auto_unbox = TRUE, null = "null"))
    },
    error = function(e) {
      message("  ERROR inserting ", p$name, ": ", e$message)
      errors <<- errors + 1L
    }
  )
}

final_count <- db_anda$count()
cat(sprintf("\n  After: %d docs in anda_variables (%d errors)\n", final_count, errors))
cat("\nSeed complete!\n")
