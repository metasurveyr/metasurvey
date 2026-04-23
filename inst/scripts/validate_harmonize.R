#!/usr/bin/env Rscript
# ==============================================================================
# validate_harmonize.R — Validate harmonize() output against IECON .dta files
#
# Compares the result of applying metasurvey's transpiled recipes (via
# harmonize()) on raw ECH microdata against the reference .dta files
# produced by IECON's original STATA code.
#
# Usage:
#   Rscript inst/scripts/validate_harmonize.R path/to/dta_folder
#   Rscript inst/scripts/validate_harmonize.R path/to/dta_folder --cache-dir /tmp/ech_cache
#
# Inputs:
#   - Raw ECH microdata: downloaded from ANDA5 (HyP DAT files preferred)
#   - IECON .dta files:  pYYYY.dta (e.g. p2020.dta) in the provided folder
#   - Recipes:           seed-data/recipes.json (local JSON backend)
#
# Output:
#   - Console summary with PASS/WARN/FAIL per year
#   - validation_results.csv with per-column detail
#
# Notes:
#   ANDA provides ECH in several formats. The "Terceros" SAV/CSV only has
#   ~164 household-level variables. The DAT RAR contains HyP_YYYY_Terceros.dat
#   with the full person-level dataset (~556 cols, ~100k rows) needed by
#   the IECON recipes. This script prefers DAT > SAV > CSV.
#
#   Column names in ANDA CSVs differ from STATA .dta names due to STATA's
#   name truncation (e.g. mto_cuota -> mto_cuot). A normalization step
#   maps these before applying recipes.
# ==============================================================================

library(metasurvey)
library(data.table)

if (!requireNamespace("haven", quietly = TRUE)) {
  stop("Package 'haven' is required. Install with: install.packages('haven')")
}
if (!requireNamespace("cli", quietly = TRUE)) {
  stop("Package 'cli' is required. Install with: install.packages('cli')")
}

# ── Parse CLI args ───────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop(
    "Usage: Rscript inst/scripts/validate_harmonize.R <dta_dir> [--cache-dir <dir>]\n",
    "  dta_dir    Directory with IECON .dta files (p2007.dta ... p2022.dta)\n",
    "  --cache-dir  Directory to cache downloaded ECH files (default: tempdir())",
    call. = FALSE
  )
}

dta_dir <- args[1]
if (!dir.exists(dta_dir)) {
  stop("Directory not found: ", dta_dir, call. = FALSE)
}

cache_dir <- tempdir()
cache_idx <- match("--cache-dir", args)
if (!is.na(cache_idx) && length(args) >= cache_idx + 1) {
  cache_dir <- args[cache_idx + 1]
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
}

# ── Setup backend with seed recipes ─────────────────────────────────────────
recipes_path <- system.file("seed-data/recipes.json", package = "metasurvey")
if (!nzchar(recipes_path) || !file.exists(recipes_path)) {
  recipes_path <- "inst/seed-data/recipes.json"
}
if (!file.exists(recipes_path)) {
  stop("Cannot find recipes.json", call. = FALSE)
}

set_backend("local", path = recipes_path)
cli::cli_alert_info("Loaded {length(list_recipes())} recipes from {recipes_path}")

# ── Detect weight column name ───────────────────────────────────────────────
detect_weight <- function(col_names) {
  candidates <- c("pesoano", "pesoan", "pesomen")
  for (w in candidates) {
    match <- grep(paste0("^", w, "$"), col_names,
      value = TRUE,
      ignore.case = TRUE
    )
    if (length(match) > 0) {
      return(match[1])
    }
  }
  match <- grep("^peso", col_names, value = TRUE, ignore.case = TRUE)
  if (length(match) > 0) {
    return(match[1])
  }
  stop(
    "Could not detect weight column. Available: ",
    paste(head(col_names, 20), collapse = ", ")
  )
}

# ── Normalize column names (ANDA -> recipe expectations) ─────────────────────
# Two issues: (1) STATA truncates long variable names (mto_cuota -> mto_cuot),
# (2) ANDA DAT files have UPPER/mixed case while recipes expect lowercase.
normalize_colnames <- function(dt) {
  # 1. Lowercase all column names (ANDA DAT has HT11, PT1, YHOG etc.)
  old_names <- names(dt)
  new_names <- tolower(old_names)
  changed <- old_names != new_names
  if (any(changed)) {
    for (i in which(changed)) {
      if (!new_names[i] %in% new_names[-i] && !new_names[i] %in% old_names[-i]) {
        setnames(dt, old_names[i], new_names[i])
      }
    }
  }

  # 2. STATA name truncation: ANDA has full names, recipes use STATA-truncated
  #    The recipe rename step handles e45/g148/INE vars (with silent skip for
  #    missing columns). Only mto_* needs pre-processing because the recipe's
  #    step_compute uses truncated names AFTER the rename step.
  trunc_map <- c(
    mto_cuota  = "mto_cuot",
    mto_hogcon = "mto_hogc",
    mto_desay  = "mto_desa",
    mto_almue  = "mto_almu",
    mto_vacas  = "mto_vaca",
    mto_oveja  = "mto_ovej",
    mto_caball = "mto_caba",
    lecheenpol = "lecheenp"
  )

  aliased <- 0L
  for (src_name in names(trunc_map)) {
    tgt_name <- trunc_map[[src_name]]
    if (src_name %in% names(dt) && !tgt_name %in% names(dt)) {
      setnames(dt, src_name, tgt_name)
      aliased <- aliased + 1L
    }
  }

  cli::cli_alert_info(
    "Normalized: {sum(changed)} lowercased, {aliased} truncated"
  )

  invisible(dt)
}

# ── Download and load ECH for a given year ──────────────────────────────────
# Prefers DAT RAR (has HyP = Hogares y Personas, full dataset) over SAV
# (only household-level, ~164 cols).
download_ech <- function(year, cache_dir) {
  yr <- as.character(year)
  catalog_id <- metasurvey:::.ech_catalog_ids[[yr]]
  if (is.null(catalog_id)) {
    return(NULL)
  }

  # Get resource list from ANDA
  accept_url <- paste0(
    "https://www4.ine.gub.uy/Anda5/index.php/catalog/",
    catalog_id, "/get-microdata"
  )
  req <- httr2::request(accept_url)
  req <- httr2::req_user_agent(req, metasurvey:::metasurvey_user_agent())
  req <- httr2::req_body_form(req, accept = "1")
  req <- httr2::req_options(
    req,
    ssl_verifypeer = getOption("metasurvey.ssl_verify", TRUE)
  )
  req <- httr2::req_timeout(req, 30)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) != 200) {
    return(NULL)
  }
  resources <- metasurvey:::.anda_parse_resources(
    httr2::resp_body_string(resp), catalog_id
  )

  # Strategy 1: Look for DAT RAR (has HyP file with all variables)
  # Only works for 2012+ (older DAT files are fixed-width without headers)
  if (as.integer(yr) >= 2012) {
    dat_idx <- grep("_DAT", resources$title, ignore.case = TRUE)
    if (length(dat_idx) > 0) {
      cli::cli_alert("Downloading DAT archive...")
      dt <- try_download_dat(resources, dat_idx[1], catalog_id, yr, cache_dir)
      if (!is.null(dt)) {
        return(dt)
      }
    }
  }

  # Strategy 2: Use anda_download_microdata (gets CSV or SAV)
  cli::cli_alert("Falling back to default download...")
  path <- tryCatch(
    anda_download_microdata(yr, resource = "implantation", dest_dir = cache_dir),
    error = function(e) NULL
  )
  if (is.null(path)) {
    return(NULL)
  }

  # Read file (CSV or SAV)
  dt <- read_ech_file(path[1])
  if (is.null(dt)) {
    return(NULL)
  }

  # Check if semester-split (months 1-6 only)
  mes_col <- grep("^mes$", names(dt), value = TRUE, ignore.case = TRUE)
  if (length(mes_col) > 0 && max(dt[[mes_col[1]]], na.rm = TRUE) <= 6) {
    cli::cli_alert_info("Only semester 1 ({nrow(dt)} rows). Looking for sem2...")
    dt2 <- try_download_sem2(resources, catalog_id, yr, cache_dir)
    if (!is.null(dt2)) {
      dt <- rbind(dt, dt2, fill = TRUE)
      cli::cli_alert_success("Combined: {nrow(dt)} rows, {ncol(dt)} cols")
    }
  }

  dt
}

# Download and extract DAT RAR, prefer HyP file
try_download_dat <- function(resources, idx, catalog_id, yr, cache_dir) {
  rid <- resources$id[idx]
  dest_raw <- file.path(cache_dir, paste0("ech_", yr, "_dat_raw"))
  dest_dir <- file.path(cache_dir, paste0("ech_", yr, "_dat"))

  # Check cache
  hyp_cached <- list.files(dest_dir,
    pattern = "^HyP.*\\.(dat|csv)$",
    ignore.case = TRUE, full.names = TRUE
  )
  if (length(hyp_cached) > 0) {
    cli::cli_alert_info("Using cached DAT: {basename(hyp_cached[1])}")
    return(read_ech_file(hyp_cached[1]))
  }

  # Download
  dl_url <- paste0(
    "https://www4.ine.gub.uy/Anda5/index.php/catalog/",
    catalog_id, "/download/", rid
  )
  dl_req <- httr2::request(dl_url)
  dl_req <- httr2::req_user_agent(dl_req, metasurvey:::metasurvey_user_agent())
  dl_req <- httr2::req_options(
    dl_req,
    ssl_verifypeer = getOption("metasurvey.ssl_verify", TRUE)
  )
  dl_req <- httr2::req_timeout(dl_req, 300)
  dl_req <- httr2::req_error(dl_req, is_error = function(resp) FALSE)
  dl_resp <- httr2::req_perform(dl_req, path = dest_raw)

  if (httr2::resp_status(dl_resp) != 200) {
    return(NULL)
  }

  # Extract RAR
  if (!dir.exists(dest_dir)) dir.create(dest_dir)
  unrar <- Sys.which("unrar")
  if (!nzchar(unrar)) {
    cli::cli_alert_warning("unrar not available, cannot extract DAT")
    return(NULL)
  }
  system2(unrar, c("x", "-o+", dest_raw, dest_dir), stdout = FALSE)

  # Find HyP file (Hogares y Personas = full dataset)
  hyp <- list.files(dest_dir,
    pattern = "^HyP", full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(hyp) > 0) {
    cli::cli_alert_success("Found HyP: {basename(hyp[1])}")
    return(read_ech_file(hyp[1]))
  }

  # No HyP, try largest file
  all_files <- list.files(dest_dir, full.names = TRUE)
  if (length(all_files) > 0) {
    sizes <- file.size(all_files)
    biggest <- all_files[which.max(sizes)]
    cli::cli_alert_info("No HyP found, using largest: {basename(biggest)}")
    return(read_ech_file(biggest))
  }

  NULL
}

# Try to download semester 2 data
try_download_sem2 <- function(resources, catalog_id, yr, cache_dir) {
  sem2_idx <- grep("sem.*2|semestre.*2|sem_2", resources$title,
    ignore.case = TRUE
  )
  csv_idx <- grep("\\.csv$", resources$title[sem2_idx], ignore.case = TRUE)
  if (length(csv_idx) > 0) sem2_idx <- sem2_idx[csv_idx]

  if (length(sem2_idx) == 0) {
    return(NULL)
  }

  rid <- resources$id[sem2_idx[1]]
  dest_raw <- file.path(cache_dir, paste0("ech_", yr, "_", rid, "_raw"))
  dl_url <- paste0(
    "https://www4.ine.gub.uy/Anda5/index.php/catalog/",
    catalog_id, "/download/", rid
  )
  dl_req <- httr2::request(dl_url)
  dl_req <- httr2::req_user_agent(dl_req, metasurvey:::metasurvey_user_agent())
  dl_req <- httr2::req_options(
    dl_req,
    ssl_verifypeer = getOption("metasurvey.ssl_verify", TRUE)
  )
  dl_req <- httr2::req_timeout(dl_req, 300)
  dl_req <- httr2::req_error(dl_req, is_error = function(resp) FALSE)
  dl_resp <- httr2::req_perform(dl_req, path = dest_raw)

  if (httr2::resp_status(dl_resp) != 200) {
    return(NULL)
  }

  extracted <- tryCatch(
    metasurvey:::.anda_extract_file(
      dest_raw, cache_dir, paste0("ech_", yr, "_sem2")
    ),
    error = function(e) NULL
  )
  if (is.null(extracted)) {
    return(NULL)
  }
  read_ech_file(extracted)
}

# Read a data file (CSV, DAT, SAV, DTA)
read_ech_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  tryCatch(
    {
      if (ext == "sav") {
        dt <- as.data.table(haven::read_sav(path))
        # Strip haven_labelled
        for (col in names(dt)) {
          if (inherits(dt[[col]], "haven_labelled")) {
            dt[[col]] <- as.numeric(dt[[col]])
          }
        }
        dt
      } else {
        # CSV or DAT — try fread
        fread(path)
      }
    },
    error = function(e) {
      cli::cli_alert_warning("Could not read {basename(path)}: {e$message}")
      NULL
    }
  )
}

# ── Compare two datasets column by column ───────────────────────────────────
compare_columns <- function(dt_meta, dt_ref, year) {
  common_cols <- intersect(names(dt_meta), names(dt_ref))
  if (length(common_cols) == 0) {
    cli::cli_alert_warning("Year {year}: no common columns found!")
    return(data.table(
      year = integer(0), column = character(0), n_rows = integer(0),
      match_pct = numeric(0), na_meta = integer(0), na_ref = integer(0),
      max_diff = numeric(0), status = character(0)
    ))
  }

  results <- rbindlist(lapply(common_cols, function(col) {
    v_meta <- dt_meta[[col]]
    v_ref <- dt_ref[[col]]
    n <- length(v_ref)

    if (inherits(v_ref, "haven_labelled")) v_ref <- as.numeric(v_ref)
    if (inherits(v_meta, "haven_labelled")) v_meta <- as.numeric(v_meta)

    if (length(v_meta) != n) {
      return(data.table(
        year = year, column = col, n_rows = n,
        match_pct = NA_real_, na_meta = NA_integer_, na_ref = NA_integer_,
        max_diff = NA_real_, status = "SKIP_NROW"
      ))
    }

    na_meta <- sum(is.na(v_meta))
    na_ref <- sum(is.na(v_ref))

    if (is.numeric(v_meta) && is.numeric(v_ref)) {
      both_na <- is.na(v_meta) & is.na(v_ref)
      both_valid <- !is.na(v_meta) & !is.na(v_ref)
      match_exact <- both_na | (both_valid & abs(v_meta - v_ref) < 1e-6)
      match_pct <- sum(match_exact) / n * 100
      diffs <- abs(v_meta[both_valid] - v_ref[both_valid])
      max_diff <- if (length(diffs) > 0) max(diffs) else 0
    } else {
      v_meta_c <- as.character(v_meta)
      v_ref_c <- as.character(v_ref)
      both_na <- is.na(v_meta_c) & is.na(v_ref_c)
      both_valid <- !is.na(v_meta_c) & !is.na(v_ref_c)
      match_exact <- both_na | (both_valid & v_meta_c == v_ref_c)
      match_pct <- sum(match_exact) / n * 100
      max_diff <- NA_real_
    }

    status <- if (match_pct >= 99) {
      "PASS"
    } else if (match_pct >= 90) {
      "WARN"
    } else {
      "FAIL"
    }

    data.table(
      year = year, column = col, n_rows = n,
      match_pct = round(match_pct, 2),
      na_meta = na_meta, na_ref = na_ref,
      max_diff = max_diff, status = status
    )
  }))

  results
}

# ── Main loop ────────────────────────────────────────────────────────────────
years <- 2007:2022
all_results <- list()
summary_rows <- list()

cli::cli_h1("Validating harmonize() vs IECON .dta files")
cli::cli_alert_info("Years: {min(years)}-{max(years)}")
cli::cli_alert_info("DTA dir: {dta_dir}")
cli::cli_alert_info("Cache dir: {cache_dir}")

for (year in years) {
  dta_file <- file.path(dta_dir, paste0("p", year, ".dta"))

  if (!file.exists(dta_file)) {
    summary_rows[[as.character(year)]] <- data.table(
      year = year, n_cols = 0L, n_pass = 0L, n_warn = 0L,
      n_fail = 0L, status = "SKIP"
    )
    next
  }

  cli::cli_h2("Year {year}")

  # 1. Download raw ECH (prefers HyP DAT)
  cli::cli_alert("Downloading ECH {year} from ANDA...")
  raw_dt <- tryCatch(
    download_ech(year, cache_dir),
    error = function(e) {
      cli::cli_alert_danger("Download failed: {e$message}")
      NULL
    }
  )

  if (is.null(raw_dt)) {
    summary_rows[[as.character(year)]] <- data.table(
      year = year, n_cols = 0L, n_pass = 0L, n_warn = 0L,
      n_fail = 0L, status = "DL_FAIL"
    )
    next
  }

  # 2. Normalize column names (ANDA long -> STATA truncated)
  normalize_colnames(raw_dt)

  # 3. Detect weight
  weight_col <- tryCatch(detect_weight(names(raw_dt)), error = function(e) {
    cli::cli_alert_danger("{e$message}")
    NULL
  })
  if (is.null(weight_col)) {
    summary_rows[[as.character(year)]] <- data.table(
      year = year, n_cols = 0L, n_pass = 0L, n_warn = 0L,
      n_fail = 0L, status = "NO_WEIGHT"
    )
    next
  }

  cli::cli_alert_info(
    "Rows: {nrow(raw_dt)} | Cols: {ncol(raw_dt)} | Weight: {weight_col}"
  )

  # 4. Write to CSV and load as Survey
  tmp_csv <- file.path(cache_dir, paste0("ech_", year, "_combined.csv"))
  fwrite(raw_dt, tmp_csv)

  svy <- tryCatch(
    load_survey(
      path = tmp_csv,
      svy_type = "ech",
      svy_edition = as.character(year),
      svy_weight = add_weight(annual = weight_col)
    ),
    error = function(e) {
      cli::cli_alert_danger("load_survey failed: {e$message}")
      NULL
    }
  )
  if (is.null(svy)) {
    summary_rows[[as.character(year)]] <- data.table(
      year = year, n_cols = 0L, n_pass = 0L, n_warn = 0L,
      n_fail = 0L, status = "LOAD_FAIL"
    )
    next
  }

  # 5. Harmonize
  cli::cli_alert("Applying recipes via harmonize()...")
  pool <- tryCatch(
    harmonize(list(svy), category = "compatibilizada", .verbose = FALSE),
    error = function(e) {
      cli::cli_alert_danger("harmonize failed: {e$message}")
      NULL
    }
  )
  if (is.null(pool)) {
    summary_rows[[as.character(year)]] <- data.table(
      year = year, n_cols = 0L, n_pass = 0L, n_warn = 0L,
      n_fail = 0L, status = "HARM_FAIL"
    )
    next
  }

  # Extract harmonized survey from pool
  harmonized_svy <- pool$surveys[["annual"]][["series"]][[1]]
  dt_meta <- copy(harmonized_svy$data)

  # 6. Load reference .dta
  cli::cli_alert("Loading reference .dta...")
  dt_ref <- as.data.table(haven::read_dta(dta_file))

  # Normalize column names to lowercase
  setnames(dt_meta, tolower(names(dt_meta)))
  setnames(dt_ref, tolower(names(dt_ref)))

  cli::cli_alert_info(
    "metasurvey: {nrow(dt_meta)}x{ncol(dt_meta)} | ref: {nrow(dt_ref)}x{ncol(dt_ref)}"
  )

  if (nrow(dt_meta) != nrow(dt_ref)) {
    cli::cli_alert_warning(
      "Row mismatch: meta={nrow(dt_meta)} vs ref={nrow(dt_ref)}"
    )
  }

  # 7. Compare
  common_cols <- intersect(names(dt_meta), names(dt_ref))
  only_ref <- setdiff(names(dt_ref), names(dt_meta))
  only_meta <- setdiff(names(dt_meta), names(dt_ref))
  cli::cli_alert(
    "{length(common_cols)} common cols ({length(only_ref)} ref-only, {length(only_meta)} meta-only)"
  )

  results <- compare_columns(dt_meta, dt_ref, year)
  all_results[[as.character(year)]] <- results

  # 8. Summarize
  n_pass <- sum(results$status == "PASS")
  n_warn <- sum(results$status == "WARN")
  n_fail <- sum(results$status == "FAIL")
  n_cols <- nrow(results)

  overall <- if (n_fail == 0 && n_warn == 0) {
    "PASS"
  } else if (n_fail == 0) {
    "WARN"
  } else {
    "FAIL"
  }

  summary_rows[[as.character(year)]] <- data.table(
    year = year, n_cols = n_cols, n_pass = n_pass, n_warn = n_warn,
    n_fail = n_fail, status = overall
  )

  status_style <- switch(overall,
    PASS = cli::col_green,
    WARN = cli::col_yellow,
    FAIL = cli::col_red,
    identity
  )
  cli::cli_alert_success(
    "{status_style(overall)} ({n_pass} PASS / {n_warn} WARN / {n_fail} FAIL of {n_cols} cols)"
  )

  if (n_fail > 0) {
    worst <- results[status == "FAIL"][order(match_pct)][1:min(5, n_fail)]
    cli::cli_alert_warning("Worst columns:")
    for (j in seq_len(nrow(worst))) {
      cli::cli_alert_info(
        "  {worst$column[j]}: {worst$match_pct[j]}% (na_m={worst$na_meta[j]}, na_r={worst$na_ref[j]})"
      )
    }
  }
}

# ── Summary ──────────────────────────────────────────────────────────────────
cli::cli_h1("Summary")
summary_dt <- rbindlist(summary_rows)
cat(format(summary_dt), sep = "\n")

if (length(all_results) > 0) {
  detail_dt <- rbindlist(all_results)
  out_path <- "validation_results.csv"
  fwrite(detail_dt, out_path)
  cli::cli_alert_success("Detailed results written to {out_path}")
  n_p <- sum(detail_dt$status == "PASS")
  n_w <- sum(detail_dt$status == "WARN")
  n_f <- sum(detail_dt$status == "FAIL")
  cli::cli_alert_info("Total: {n_p} PASS, {n_w} WARN, {n_f} FAIL")
} else {
  cli::cli_alert_warning("No results to write")
}
