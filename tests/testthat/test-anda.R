# Tests for R/anda.R — ANDA5 integration (pure functions + mocks)

# ── .ech_catalog_ids ───────────────────────────────────────────────────────────

test_that(".ech_catalog_ids contains expected editions", {
  ids <- .ech_catalog_ids
  expect_true(is.list(ids))
  expect_true("2023" %in% names(ids))
  expect_true("2024" %in% names(ids))
  expect_true("2007" %in% names(ids))
  expect_equal(ids[["2023"]], 735L)
  expect_equal(ids[["2024"]], 767L)
})

test_that("anda_list_editions returns data.frame with all editions", {
  df <- anda_list_editions()
  expect_true(is.data.frame(df))
  expect_true("edition" %in% names(df))
  expect_true("catalog_id" %in% names(df))
  expect_true("2023" %in% df$edition)
  expect_true(nrow(df) >= 18)
})

# ── .anda_parse_resources ──────────────────────────────────────────────────────

test_that(".anda_parse_resources parses HTML with data-file-id", {
  html <- paste0(
    '<a data-file-id="1264" class="download" title="Implantacion 2023">Download</a>',
    '<a data-file-id="1265" class="download" title="ECH_01_2023">Download</a>',
    '<a data-file-id="1270" class="download" title="Pesos replicados Bootstrap anuales 2023">Download</a>'
  )
  resources <- .anda_parse_resources(html, 735)
  expect_true(is.data.frame(resources))
  expect_equal(nrow(resources), 3)
  expect_equal(resources$id, c("1264", "1265", "1270"))
  expect_equal(resources$title[1], "Implantacion 2023")
})

test_that(".anda_parse_resources returns empty data.frame for no matches", {
  html <- "<html><body>No resources here</body></html>"
  resources <- .anda_parse_resources(html, 735)
  expect_true(is.data.frame(resources))
  expect_equal(nrow(resources), 0)
})

# ── .anda_select_resource ──────────────────────────────────────────────────────

test_that(".anda_select_resource selects implantation for >= 2022", {
  resources <- data.frame(
    id = c("1241", "1264", "1265"),
    title = c("FIES 2023", "Implantacion 2023", "ECH_01_2023"),
    stringsAsFactors = FALSE
  )
  selected <- .anda_select_resource(resources, "implantation", "2023")
  expect_equal(nrow(selected), 1)
  expect_equal(selected$id, "1264")
})

test_that(".anda_select_resource finds ECH_YYYY.csv when no implantacion label", {
  # ECH 2024 pattern: no "implantacion" label, just "ECH_2024.csv"
  resources <- data.frame(
    id = c("100", "101", "102", "103"),
    title = c("ECH_01_24.csv", "ECH_02_24.csv", "ECH_2024.csv", "FIES_2024.csv"),
    stringsAsFactors = FALSE
  )
  selected <- .anda_select_resource(resources, "implantation", "2024")
  expect_equal(nrow(selected), 1)
  expect_equal(selected$id, "102")
})

test_that(".anda_select_resource prefers SAV for < 2022, excludes FIES", {
  resources <- data.frame(
    id = c("100", "101", "102", "103"),
    title = c("FIES 2018", "ECH_2018_sav", "ECH_2018.csv", "Estrato"),
    stringsAsFactors = FALSE
  )
  selected <- .anda_select_resource(resources, "implantation", "2018")
  expect_equal(nrow(selected), 1)
  # Should select CSV or SAV, not FIES or Estrato
  expect_true(selected$id %in% c("101", "102"))
})

test_that(".anda_select_resource selects monthly files", {
  resources <- data.frame(
    id = c("1264", "1265", "1266", "1267"),
    title = c("Implantacion 2023", "ech_01_2023", "ech_02_2023", "Bootstrap"),
    stringsAsFactors = FALSE
  )
  selected <- .anda_select_resource(resources, "monthly", "2023")
  expect_equal(nrow(selected), 2)
  expect_true(all(selected$id %in% c("1265", "1266")))
})

test_that(".anda_select_resource errors for missing monthly in pre-2022", {
  resources <- data.frame(
    id = c("100"),
    title = c("ECH_2018"),
    stringsAsFactors = FALSE
  )
  expect_error(
    .anda_select_resource(resources, "monthly", "2018"),
    "No monthly files"
  )
})

test_that(".anda_select_resource selects bootstrap_annual", {
  resources <- data.frame(
    id = c("1264", "1270"),
    title = c("Implantacion 2023", "Pesos replicados Bootstrap anuales 2023"),
    stringsAsFactors = FALSE
  )
  selected <- .anda_select_resource(resources, "bootstrap_annual", "2023")
  expect_equal(nrow(selected), 1)
  expect_equal(selected$id, "1270")
})

test_that(".anda_select_resource selects bootstrap_monthly", {
  resources <- data.frame(
    id = c("1264", "1271"),
    title = c("Implantacion 2023", "Pesos replicados Bootstrap mensuales enero_junio 2023"),
    stringsAsFactors = FALSE
  )
  selected <- .anda_select_resource(resources, "bootstrap_monthly", "2023")
  expect_equal(nrow(selected), 1)
  expect_equal(selected$id, "1271")
})

test_that(".anda_select_resource selects bootstrap_quarterly", {
  resources <- data.frame(
    id = c("1264", "1272"),
    title = c("Implantacion 2023", "Pesos replicados Bootstrap trimestrales 2023"),
    stringsAsFactors = FALSE
  )
  selected <- .anda_select_resource(resources, "bootstrap_quarterly", "2023")
  expect_equal(nrow(selected), 1)
  expect_equal(selected$id, "1272")
})

test_that(".anda_select_resource selects poverty data", {
  resources <- data.frame(
    id = c("1264", "1280"),
    title = c("Implantacion 2023", "Microdatos_LP 2023"),
    stringsAsFactors = FALSE
  )
  selected <- .anda_select_resource(resources, "poverty", "2023")
  expect_equal(nrow(selected), 1)
  expect_equal(selected$id, "1280")
})

test_that(".anda_select_resource errors for unknown resource type", {
  resources <- data.frame(id = "1", title = "Test", stringsAsFactors = FALSE)
  expect_error(.anda_select_resource(resources, "unknown_type", "2023"), "Unknown resource")
})

# ── .anda_extract_file ─────────────────────────────────────────────────────────

test_that(".anda_extract_file handles CSV (plain text)", {
  tmp_dir <- tempdir()
  csv_path <- file.path(tmp_dir, "test_raw_csv")
  writeLines("col1,col2\n1,2\n3,4", csv_path)
  on.exit(unlink(file.path(tmp_dir, "ech_test.csv")))

  result <- .anda_extract_file(csv_path, tmp_dir, "test")
  expect_true(file.exists(result))
  expect_match(result, "\\.csv$")
})

test_that(".anda_extract_file handles ZIP archive", {
  tmp_dir <- tempfile("anda_zip_test_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create a CSV inside a ZIP
  csv_file <- file.path(tmp_dir, "data.csv")
  writeLines("x,y\n1,2\n3,4", csv_file)
  zip_path <- file.path(tmp_dir, "test_raw_zip.zip")
  utils::zip(zip_path, csv_file, flags = "-j")
  unlink(csv_file)

  # Rename to raw (no extension) as anda_extract_file expects
  raw_path <- file.path(tmp_dir, "test_raw_zip")
  file.rename(zip_path, raw_path)

  result <- .anda_extract_file(raw_path, tmp_dir, "ziptest")
  expect_true(file.exists(result))
  expect_match(result, "\\.csv$")
})

# ── .anda_find_data_file ──────────────────────────────────────────────────────

test_that(".anda_find_data_file prefers CSV over SAV", {
  tmp_dir <- tempfile("anda_find_test_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("x,y\n1,2", file.path(tmp_dir, "data.csv"))
  writeLines("fake sav content", file.path(tmp_dir, "data.sav"))

  result <- .anda_find_data_file(tmp_dir, "test")
  expect_match(result, "\\.csv$")
})

test_that(".anda_find_data_file errors on empty directory", {
  tmp_dir <- tempfile("anda_empty_test_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_error(.anda_find_data_file(tmp_dir, "test"), "empty")
})

# ── anda_parse_variables (DDI XML parsing) ─────────────────────────────────────

test_that("anda_parse_variables parses DDI XML with variables", {
  skip_if_not_installed("xml2")

  ddi_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<codeBook xmlns="ddi:codebook:2_5" version="2.5">
  <dataDscr>
    <var name="EDAD" intrvl="contin">
      <labl>Edad en años</labl>
      <txt>Edad del encuestado en años cumplidos</txt>
    </var>
    <var name="SEXO" intrvl="discrete">
      <labl>Sexo</labl>
      <catgry>
        <catValu>1</catValu>
        <labl>Hombre</labl>
      </catgry>
      <catgry>
        <catValu>2</catValu>
        <labl>Mujer</labl>
      </catgry>
    </var>
    <var name="REGION" intrvl="discrete">
      <labl>Región</labl>
    </var>
  </dataDscr>
</codeBook>'

  tmp <- tempfile(fileext = ".xml")
  on.exit(unlink(tmp))
  writeLines(ddi_xml, tmp)

  result <- anda_parse_variables(tmp)
  expect_length(result, 3)

  # EDAD: continuous, with label and description
  expect_equal(result[[1]]$name, "edad")
  expect_equal(result[[1]]$label, "Edad en años")
  expect_equal(result[[1]]$type, "continuous")
  expect_equal(result[[1]]$description, "Edad del encuestado en años cumplidos")
  expect_null(result[[1]]$value_labels)

  # SEXO: discrete, with value labels
  expect_equal(result[[2]]$name, "sexo")
  expect_equal(result[[2]]$type, "discrete")
  expect_equal(result[[2]]$value_labels[["1"]], "Hombre")
  expect_equal(result[[2]]$value_labels[["2"]], "Mujer")

  # REGION: discrete, no value labels
  expect_equal(result[[3]]$name, "region")
})

test_that("anda_parse_variables handles empty dataDscr", {
  skip_if_not_installed("xml2")

  ddi_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<codeBook xmlns="ddi:codebook:2_5" version="2.5">
  <dataDscr>
  </dataDscr>
</codeBook>'

  tmp <- tempfile(fileext = ".xml")
  on.exit(unlink(tmp))
  writeLines(ddi_xml, tmp)

  expect_warning(result <- anda_parse_variables(tmp), "No variables found")
  expect_length(result, 0)
})

test_that("anda_parse_variables deduplicates by name", {
  skip_if_not_installed("xml2")

  ddi_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<codeBook xmlns="ddi:codebook:2_5" version="2.5">
  <dataDscr>
    <var name="EDAD" intrvl="contin"><labl>Age 1</labl></var>
    <var name="edad" intrvl="contin"><labl>Age 2</labl></var>
    <var name="SEXO" intrvl="discrete"><labl>Sex</labl></var>
  </dataDscr>
</codeBook>'

  tmp <- tempfile(fileext = ".xml")
  on.exit(unlink(tmp))
  writeLines(ddi_xml, tmp)

  result <- anda_parse_variables(tmp)
  expect_length(result, 2) # EDAD deduplicated
})

# ── .anda_extract_file SAV detection ──────────────────────────────────────────

test_that(".anda_extract_file detects SAV by magic bytes", {
  tmp_dir <- tempfile("anda_sav_test_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create fake SAV file (magic: $FL2)
  sav_path <- file.path(tmp_dir, "test_raw_sav")
  con <- file(sav_path, "wb")
  writeBin(charToRaw("$FL2fake SAV content"), con)
  close(con)

  result <- .anda_extract_file(sav_path, tmp_dir, "savtest")
  expect_true(file.exists(result))
  expect_match(result, "\\.sav$")
})

# ── .anda_find_data_file prefers SAV over XLSX ─────────────────────────────────

test_that(".anda_find_data_file prefers SAV over XLSX when no CSV", {
  tmp_dir <- tempfile("anda_find_sav_test_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("fake sav", file.path(tmp_dir, "data.sav"))
  writeLines("fake xlsx", file.path(tmp_dir, "data.xlsx"))

  result <- .anda_find_data_file(tmp_dir, "test")
  expect_match(result, "\\.sav$")
})

# ── anda_download_microdata error cases ────────────────────────────────────────

test_that("anda_download_microdata errors for unknown edition", {
  expect_error(anda_download_microdata("1999"), "Unknown ECH edition")
})

# ── anda_variables (mock api_get_anda_variables) ──────────────────────────────

test_that("anda_variables returns data.frame from API", {
  local_mocked_bindings(
    api_get_anda_variables = function(survey_type, var_names) {
      list(
        list(name = "edad", label = "Edad", type = "continuous"),
        list(name = "sexo", label = "Sexo", type = "discrete")
      )
    }
  )

  df <- anda_variables("ech", c("edad", "sexo"))
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 2)
  expect_equal(df$name, c("edad", "sexo"))
  expect_equal(df$label, c("Edad", "Sexo"))
})

test_that("anda_variables returns empty data.frame when no results", {
  local_mocked_bindings(
    api_get_anda_variables = function(survey_type, var_names) {
      list()
    }
  )

  df <- anda_variables("ech")
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 0)
})

test_that("anda_variable_detail returns single variable", {
  local_mocked_bindings(
    api_get_anda_variables = function(survey_type, var_names) {
      list(
        list(name = "edad", label = "Edad", type = "continuous", description = "Age in years")
      )
    }
  )

  result <- anda_variable_detail("ech", "edad")
  expect_true(is.list(result))
  expect_equal(result$name, "edad")
  expect_equal(result$description, "Age in years")
})

test_that("anda_variable_detail returns NULL when not found", {
  local_mocked_bindings(
    api_get_anda_variables = function(survey_type, var_names) {
      list()
    }
  )

  result <- anda_variable_detail("ech", "nonexistent")
  expect_null(result)
})

# ── .anda_select_resource: bootstrap_semestral ───────────────────────────────

test_that(".anda_select_resource selects bootstrap_semestral", {
  resources <- data.frame(
    id = c("1264", "1275"),
    title = c("Implantacion 2023", "Pesos replicados Bootstrap semestrales 2023"),
    stringsAsFactors = FALSE
  )
  selected <- .anda_select_resource(resources, "bootstrap_semestral", "2023")
  expect_equal(nrow(selected), 1)
  expect_equal(selected$id, "1275")
})

test_that(".anda_select_resource errors when no bootstrap_semestral found", {
  resources <- data.frame(
    id = "1264", title = "Implantacion 2023", stringsAsFactors = FALSE
  )
  expect_error(
    .anda_select_resource(resources, "bootstrap_semestral", "2023"),
    "No semestral bootstrap"
  )
})

# ── .anda_select_resource: implantation pre-2022 paths ──────────────────────

test_that(".anda_select_resource pre-2022 prefers CSV over SAV", {
  resources <- data.frame(
    id = c("100", "101", "102"),
    title = c("FIES_2018.csv", "ECH_2018.csv", "ECH_2018_sav"),
    stringsAsFactors = FALSE
  )
  selected <- .anda_select_resource(resources, "implantation", "2018")
  expect_equal(nrow(selected), 1)
  # Should select ECH_2018.csv (not FIES)
  expect_equal(selected$id, "101")
})

test_that(".anda_select_resource pre-2022 falls back to SAV when no CSV", {
  resources <- data.frame(
    id = c("100", "101"),
    title = c("FIES_2018", "ECH_2018_sav"),
    stringsAsFactors = FALSE
  )
  selected <- .anda_select_resource(resources, "implantation", "2018")
  expect_equal(nrow(selected), 1)
  expect_equal(selected$id, "101")
})

test_that(".anda_select_resource pre-2022 fallback to first non-excluded", {
  resources <- data.frame(
    id = c("100", "101"),
    title = c("FIES_2018", "ECH_raw_2018"),
    stringsAsFactors = FALSE
  )
  selected <- .anda_select_resource(resources, "implantation", "2018")
  expect_equal(nrow(selected), 1)
  # FIES is excluded, so selects ECH_raw_2018
  expect_equal(selected$id, "101")
})

test_that(".anda_select_resource >= 2022 errors when no implantation found", {
  resources <- data.frame(
    id = c("100", "101"),
    title = c("ech_01_2023", "ech_02_2023"),
    stringsAsFactors = FALSE
  )
  expect_error(
    .anda_select_resource(resources, "implantation", "2023"),
    "No implantation file found"
  )
})

# ── .anda_extract_file: RAR detection branch ─────────────────────────────────

test_that(".anda_extract_file detects RAR but errors without archive package", {
  tmp_dir <- tempfile("anda_rar_test_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create file with RAR magic bytes
  rar_path <- file.path(tmp_dir, "test_raw_rar")
  con <- file(rar_path, "wb")
  writeBin(as.raw(c(0x52, 0x61, 0x72, 0x21, 0x1a, 0x07, 0x00)), con)
  writeBin(charToRaw("fake rar content"), con)
  close(con)

  # If archive package is not installed, should error; otherwise would extract
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) FALSE,
    .package = "base"
  )
  expect_error(.anda_extract_file(rar_path, tmp_dir, "rartest"), "archive")
})

# ── .anda_find_data_file: XLSX fallback ──────────────────────────────────────

test_that(".anda_find_data_file falls back to XLSX when no CSV/SAV", {
  tmp_dir <- tempfile("anda_xlsx_test_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("fake xlsx", file.path(tmp_dir, "data.xlsx"))
  result <- .anda_find_data_file(tmp_dir, "test")
  expect_match(result, "\\.xlsx$")
})

# ── anda_fetch_ddi / anda_catalog_search: mocked HTTP ────────────────────────

test_that("anda_fetch_ddi errors on non-200 response", {
  local_mocked_bindings(
    GET = function(...) {
      structure(list(status_code = 404L), class = "response")
    },
    status_code = function(resp) resp$status_code,
    write_disk = function(...) list(),
    config = function(...) list(),
    timeout = function(...) list(),
    .package = "httr"
  )
  expect_error(anda_fetch_ddi(999), "Failed to download DDI")
})

test_that("anda_catalog_search returns data.frame on success", {
  local_mocked_bindings(
    GET = function(...) {
      structure(list(
        status_code = 200L,
        content = list(result = list(rows = list(
          list(id = 735, title = "ECH 2023", year_start = "2023", year_end = "2023"),
          list(id = 767, title = "ECH 2024", year_start = "2024", year_end = "2024")
        )))
      ), class = "response")
    },
    status_code = function(resp) resp$status_code,
    content = function(resp, ...) resp$content,
    config = function(...) list(),
    timeout = function(...) list(),
    .package = "httr"
  )
  result <- anda_catalog_search("ECH")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true("title" %in% names(result))
})

test_that("anda_catalog_search errors on non-200", {
  local_mocked_bindings(
    GET = function(...) {
      structure(list(status_code = 500L), class = "response")
    },
    status_code = function(resp) resp$status_code,
    config = function(...) list(),
    timeout = function(...) list(),
    .package = "httr"
  )
  expect_error(anda_catalog_search("ECH"), "catalog search failed")
})

test_that("anda_catalog_search returns empty df when no results", {
  local_mocked_bindings(
    GET = function(...) {
      structure(list(
        status_code = 200L,
        content = list(result = list(rows = list()))
      ), class = "response")
    },
    status_code = function(resp) resp$status_code,
    content = function(resp, ...) resp$content,
    config = function(...) list(),
    timeout = function(...) list(),
    .package = "httr"
  )
  result <- anda_catalog_search("nonexistent")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})
