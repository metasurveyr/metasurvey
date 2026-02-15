# ══════════════════════════════════════════════════════════════════════════════
# ANDA Metadata Integration
#
# Functions to fetch and parse DDI XML metadata from INE Uruguay's ANDA5
# catalog (https://www4.ine.gub.uy/Anda5/). Provides variable-level
# metadata (labels, value labels, types) for ECH survey variables.
# ══════════════════════════════════════════════════════════════════════════════

#' Fetch DDI XML from ANDA5 catalog
#'
#' Downloads the DDI (Data Documentation Initiative) XML file for a given
#' catalog entry from INE Uruguay's ANDA5 catalog.
#'
#' @param catalog_id Integer catalog ID (e.g., 767 for ECH 2024)
#' @param base_url Character base URL of the ANDA5 instance
#' @param dest_file Character path where to save the XML file. If NULL, uses a
#'   temporary file.
#' @return Character path to the downloaded DDI XML file
#' @export
anda_fetch_ddi <- function(catalog_id,
                           base_url = "https://www4.ine.gub.uy/Anda5",
                           dest_file = NULL) {
  url <- paste0(base_url, "/metadata/export/", catalog_id, "/ddi")

  if (is.null(dest_file)) {
    dest_file <- tempfile(fileext = ".xml")
  }

  resp <- httr::GET(
    url,
    httr::config(ssl_verifypeer = FALSE),
    httr::write_disk(dest_file, overwrite = TRUE),
    httr::timeout(60)
  )

  if (httr::status_code(resp) != 200) {
    stop("Failed to download DDI from ANDA5 (catalog_id=", catalog_id,
         "): HTTP ", httr::status_code(resp))
  }

  dest_file
}

#' Search ANDA5 catalog
#'
#' Queries the ANDA5 REST API to list available survey datasets.
#'
#' @param keyword Character search term (e.g., "ECH")
#' @param base_url Character base URL of the ANDA5 instance
#' @param limit Integer max results to return
#' @return A data.frame with columns: id, title, year_start, year_end
#' @export
anda_catalog_search <- function(keyword = "ECH",
                                base_url = "https://www4.ine.gub.uy/Anda5",
                                limit = 50) {
  url <- paste0(base_url, "/index.php/api/catalog/search")

  resp <- httr::GET(
    url,
    query = list(sk = keyword, ps = limit, format = "json"),
    httr::config(ssl_verifypeer = FALSE),
    httr::timeout(30)
  )

  if (httr::status_code(resp) != 200) {
    stop("ANDA5 catalog search failed: HTTP ", httr::status_code(resp))
  }

  body <- httr::content(resp, as = "parsed", type = "application/json")
  rows <- body$result$rows %||% list()

  if (length(rows) == 0) {
    return(data.frame(
      id = character(), title = character(),
      year_start = character(), year_end = character(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    id         = vapply(rows, function(r) as.character(r$id %||% ""), character(1)),
    title      = vapply(rows, function(r) r$title %||% "", character(1)),
    year_start = vapply(rows, function(r) r$year_start %||% "", character(1)),
    year_end   = vapply(rows, function(r) r$year_end %||% "", character(1)),
    stringsAsFactors = FALSE
  )
}

#' Parse variables from a DDI XML file
#'
#' Reads a DDI Codebook XML file and extracts variable-level metadata:
#' name, label, type (discrete/continuous), and value labels.
#'
#' @param ddi_xml_path Character path to the DDI XML file
#' @return A list of variable metadata, each with: name, label, type,
#'   value_labels (named list or NULL), description
#' @export
anda_parse_variables <- function(ddi_xml_path) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Package 'xml2' is required for DDI parsing. Install with: install.packages('xml2')")
  }

  doc <- xml2::read_xml(ddi_xml_path)
  ns <- xml2::xml_ns(doc)

  # Find all <var> elements in <dataDscr>
  vars <- xml2::xml_find_all(doc, ".//d1:dataDscr/d1:var", ns)

  if (length(vars) == 0) {
    warning("No variables found in DDI file")
    return(list())
  }

  seen <- character(0)
  result <- list()

  for (v in vars) {
    name <- xml2::xml_attr(v, "name")
    if (is.na(name) || !nzchar(name)) next

    # Deduplicate: same variable may appear in multiple files
    name_lower <- tolower(name)
    if (name_lower %in% seen) next
    seen <- c(seen, name_lower)

    intrvl <- xml2::xml_attr(v, "intrvl")
    var_type <- if (!is.na(intrvl) && intrvl == "contin") "continuous" else "discrete"

    # Label
    labl_node <- xml2::xml_find_first(v, "d1:labl", ns)
    label <- if (!is.null(labl_node) && !inherits(labl_node, "xml_missing")) {
      trimws(xml2::xml_text(labl_node))
    } else {
      ""
    }

    # Description text
    txt_node <- xml2::xml_find_first(v, "d1:txt", ns)
    description <- if (!is.null(txt_node) && !inherits(txt_node, "xml_missing")) {
      trimws(xml2::xml_text(txt_node))
    } else {
      ""
    }

    # Value labels (categories)
    catgry_nodes <- xml2::xml_find_all(v, "d1:catgry", ns)
    value_labels <- NULL
    if (length(catgry_nodes) > 0) {
      value_labels <- list()
      for (cat in catgry_nodes) {
        cat_val <- xml2::xml_find_first(cat, "d1:catValu", ns)
        cat_lab <- xml2::xml_find_first(cat, "d1:labl", ns)
        if (!inherits(cat_val, "xml_missing") && !inherits(cat_lab, "xml_missing")) {
          val <- trimws(xml2::xml_text(cat_val))
          lab <- trimws(xml2::xml_text(cat_lab))
          value_labels[[val]] <- lab
        }
      }
      if (length(value_labels) == 0) value_labels <- NULL
    }

    result[[length(result) + 1L]] <- list(
      name         = name_lower,
      label        = label,
      type         = var_type,
      value_labels = value_labels,
      description  = description
    )
  }

  result
}

# Known ANDA5 catalog IDs for ECH editions
.ech_catalog_ids <- list(
  "2007" = 49L,  "2008" = 50L,  "2009" = 51L,  "2010" = 734L,
  "2011" = 726L, "2012" = 725L, "2013" = 724L, "2014" = 723L,
  "2015" = 721L, "2016" = 722L, "2017" = 720L, "2018" = 719L,
  "2019" = 715L, "2020" = 714L, "2021" = 716L, "2022" = 730L,
  "2023" = 735L, "2024" = 767L
)

#' List available ECH editions in ANDA5
#'
#' Returns the known ECH editions available in INE Uruguay's ANDA5 catalog.
#'
#' @return A data.frame with columns: edition, catalog_id
#' @export
anda_list_editions <- function() {
  ids <- .ech_catalog_ids
  data.frame(
    edition    = names(ids),
    catalog_id = unlist(ids, use.names = FALSE),
    stringsAsFactors = FALSE
  )
}

#' Download ECH microdata from ANDA5
#'
#' Downloads the main CSV microdata file for a given ECH edition from
#' INE Uruguay's ANDA5 catalog. Automatically accepts the terms of use
#' and downloads the first available CSV resource.
#'
#' @param edition Character year (e.g., "2023")
#' @param dest_dir Character directory where to save the file. Defaults to
#'   a temporary directory.
#' @param base_url Character base URL of the ANDA5 instance
#' @return Character path to the downloaded CSV file
#' @export
anda_download_microdata <- function(edition,
                                    dest_dir = tempdir(),
                                    base_url = "https://www4.ine.gub.uy/Anda5") {
  catalog_id <- .ech_catalog_ids[[as.character(edition)]]
  if (is.null(catalog_id)) {
    avail <- paste(names(.ech_catalog_ids), collapse = ", ")
    stop("Unknown ECH edition '", edition, "'. Available: ", avail)
  }

  message("Downloading ECH ", edition, " from ANDA5 (catalog ", catalog_id, ")...")

  # Accept terms to get download page
  accept_url <- paste0(base_url, "/index.php/catalog/", catalog_id, "/get-microdata")
  resp <- httr::POST(
    accept_url,
    body = list(accept = "1"),
    encode = "form",
    httr::config(ssl_verifypeer = FALSE),
    httr::timeout(30)
  )

  if (httr::status_code(resp) != 200) {
    stop("Failed to access microdata page: HTTP ", httr::status_code(resp))
  }

  # Extract first CSV download resource ID
  page_html <- httr::content(resp, as = "text", encoding = "UTF-8")
  resource_ids <- unique(regmatches(
    page_html,
    gregexpr(paste0("catalog/", catalog_id, "/download/(\\d+)"), page_html)
  )[[1]])
  resource_ids <- gsub(paste0("catalog/", catalog_id, "/download/"), "", resource_ids)

  if (length(resource_ids) == 0) {
    stop("No downloadable resources found for ECH ", edition)
  }

  # Download the first resource (main implantation file)
  resource_id <- resource_ids[1]
  dl_url <- paste0(base_url, "/index.php/catalog/", catalog_id, "/download/", resource_id)
  dest_file <- file.path(dest_dir, paste0("ech_", edition, ".csv"))

  message("  Downloading resource ", resource_id, " -> ", dest_file)
  dl_resp <- httr::GET(
    dl_url,
    httr::config(ssl_verifypeer = FALSE),
    httr::write_disk(dest_file, overwrite = TRUE),
    httr::timeout(300)
  )

  if (httr::status_code(dl_resp) != 200) {
    stop("Failed to download resource: HTTP ", httr::status_code(dl_resp))
  }

  file_size <- file.info(dest_file)$size
  message(sprintf("  Done: %.1f MB", file_size / 1024 / 1024))
  dest_file
}

#' Query ANDA variable metadata from the API
#'
#' Fetches variable metadata (labels, types, value labels) from the
#' metasurvey API's ANDA endpoint.
#'
#' @param survey_type Character survey type (default "ech")
#' @param var_names Character vector of variable names to look up. If NULL,
#'   returns all variables for the survey type.
#' @return A data.frame with columns: name, label, type
#' @export
anda_variables <- function(survey_type = "ech", var_names = NULL) {
  vars <- api_get_anda_variables(survey_type, var_names)

  if (length(vars) == 0) {
    return(data.frame(
      name = character(), label = character(), type = character(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    name  = vapply(vars, function(v) v$name %||% "", character(1)),
    label = vapply(vars, function(v) v$label %||% "", character(1)),
    type  = vapply(vars, function(v) v$type %||% "unknown", character(1)),
    stringsAsFactors = FALSE
  )
}

#' Get detailed metadata for a single ANDA variable
#'
#' @param survey_type Character survey type (default "ech")
#' @param var_name Character variable name
#' @return A list with: name, label, type, value_labels, description.
#'   NULL if not found.
#' @export
anda_variable_detail <- function(survey_type = "ech", var_name) {
  vars <- api_get_anda_variables(survey_type, var_name)
  if (length(vars) == 0) return(NULL)
  vars[[1]]
}
