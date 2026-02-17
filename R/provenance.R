#' Get provenance from a survey or workflow result
#'
#' Returns the provenance metadata recording the full data lineage:
#' source file, step history with row counts, and environment info.
#'
#' @param x A [Survey] object or a `data.table` from [workflow()].
#' @param ... Additional arguments (unused).
#'
#' @return A `metasurvey_provenance` list, or `NULL` if no provenance
#'   is available.
#'
#' @examples
#' svy <- Survey$new(
#'   data = data.table::data.table(id = 1:10, age = 20:29, w = 1),
#'   edition = "2023", type = "test", psu = NULL,
#'   engine = "data.table", weight = add_weight(annual = "w")
#' )
#' provenance(svy)
#'
#' @family provenance
#' @export
provenance <- function(x, ...) {
  UseMethod("provenance")
}

#' @rdname provenance
#' @export
provenance.Survey <- function(x, ...) {
  x$provenance
}

#' @rdname provenance
#' @export
provenance.data.table <- function(x, ...) {
  attr(x, "provenance")
}

#' @rdname provenance
#' @export
provenance.default <- function(x, ...) {
  NULL
}


#' Print provenance information
#'
#' @param x A `metasurvey_provenance` list.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns `x`.
#'
#' @family provenance
#' @export
print.metasurvey_provenance <- function(x, ...) {
  cli::cat_rule("Data Provenance")

  if (!is.null(x$source$path)) {
    cat("Source:", x$source$path, "\n")
  }
  if (!is.null(x$source$timestamp)) {
    cat("Loaded:", x$source$timestamp, "\n")
  }
  if (!is.null(x$source$initial_n)) {
    cat("Initial rows:", x$source$initial_n, "\n")
  }
  if (!is.null(x$source$hash)) {
    cat("Hash:", x$source$hash, "\n")
  }

  if (length(x$steps) > 0) {
    cat("\nPipeline:\n")
    for (i in seq_along(x$steps)) {
      s <- x$steps[[i]]
      delta <- ""
      if (!is.null(s$n_before) && !is.null(s$n_after)) {
        diff_n <- s$n_after - s$n_before
        if (diff_n != 0) {
          pct <- round(diff_n / s$n_before * 100, 1)
          delta <- sprintf(
            "  N=%d -> %d (%+.1f%%)", s$n_before, s$n_after, pct
          )
        } else {
          delta <- sprintf("  N=%d", s$n_after)
        }
      }
      dur <- if (!is.null(s$duration_ms)) {
        sprintf(" [%.1fms]", s$duration_ms)
      } else {
        ""
      }
      cat(sprintf("  %d. %s%s%s\n", i, s$name, delta, dur))
    }
  }

  if (!is.null(x$estimation)) {
    cat("\nEstimation:\n")
    cat("  Type:", x$estimation$estimation_type %||% "unknown", "\n")
    cat("  Timestamp:", x$estimation$timestamp %||% "unknown", "\n")
  }

  if (!is.null(x$environment)) {
    cat("\nEnvironment:\n")
    if (!is.null(x$environment$metasurvey_version)) {
      cat("  metasurvey:", x$environment$metasurvey_version, "\n")
    }
    if (!is.null(x$environment$r_version)) {
      cat("  R:", x$environment$r_version, "\n")
    }
    if (!is.null(x$environment$survey_version)) {
      cat("  survey:", x$environment$survey_version, "\n")
    }
  }

  invisible(x)
}


#' Export provenance to JSON
#'
#' Serializes a provenance object to JSON format, optionally writing
#' to a file.
#'
#' @param prov A `metasurvey_provenance` list.
#' @param path File path to write JSON. If `NULL`, returns the JSON string.
#'
#' @return JSON string (invisibly if `path` is provided).
#'
#' @examples
#' svy <- Survey$new(
#'   data = data.table::data.table(id = 1:5, w = rep(1, 5)),
#'   edition = "2023", type = "test",
#'   engine = "data.table", weight = add_weight(annual = "w")
#' )
#' prov <- provenance(svy)
#' provenance_to_json(prov)
#'
#' @family provenance
#' @export
provenance_to_json <- function(prov, path = NULL) {
  json <- jsonlite::toJSON(unclass(prov), auto_unbox = TRUE, pretty = TRUE,
    null = "null"
  )
  if (!is.null(path)) {
    writeLines(json, path)
    invisible(json)
  } else {
    json
  }
}


#' Compare two provenance objects
#'
#' Shows differences between two provenance records, useful for
#' comparing processing across survey editions.
#'
#' @param prov1 First provenance list.
#' @param prov2 Second provenance list.
#'
#' @return A `metasurvey_provenance_diff` list with detected differences.
#'
#' @examples
#' svy1 <- Survey$new(
#'   data = data.table::data.table(id = 1:5, w = rep(1, 5)),
#'   edition = "2023", type = "test",
#'   engine = "data.table", weight = add_weight(annual = "w")
#' )
#' svy2 <- Survey$new(
#'   data = data.table::data.table(id = 1:5, w = rep(2, 5)),
#'   edition = "2024", type = "test",
#'   engine = "data.table", weight = add_weight(annual = "w")
#' )
#' provenance_diff(provenance(svy1), provenance(svy2))
#'
#' @family provenance
#' @export
provenance_diff <- function(prov1, prov2) {
  diffs <- list()

  # Compare sources
  if (!identical(prov1$source$path, prov2$source$path)) {
    diffs$source_path <- list(
      from = prov1$source$path, to = prov2$source$path
    )
  }

  # Compare final row counts
  n1 <- .provenance_final_n(prov1)
  n2 <- .provenance_final_n(prov2)
  if (!identical(n1, n2)) {
    diffs$final_n <- list(from = n1, to = n2)
  }

  # Compare step counts
  diffs$n_steps <- list(
    from = length(prov1$steps),
    to = length(prov2$steps)
  )

  # Compare environment
  if (!identical(
    prov1$environment$metasurvey_version,
    prov2$environment$metasurvey_version
  )) {
    diffs$metasurvey_version <- list(
      from = prov1$environment$metasurvey_version,
      to = prov2$environment$metasurvey_version
    )
  }

  class(diffs) <- "metasurvey_provenance_diff"
  diffs
}


#' Print provenance diff
#'
#' @param x A `metasurvey_provenance_diff` list.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns `x`.
#'
#' @family provenance
#' @export
print.metasurvey_provenance_diff <- function(x, ...) {
  cli::cat_rule("Provenance Diff")
  if (!is.null(x$source_path)) {
    cat("Source:", x$source_path$from, "->", x$source_path$to, "\n")
  }
  if (!is.null(x$final_n)) {
    cat("Final N:", x$final_n$from, "->", x$final_n$to, "\n")
  }
  if (!is.null(x$n_steps)) {
    cat("Steps:", x$n_steps$from, "->", x$n_steps$to, "\n")
  }
  if (!is.null(x$metasurvey_version)) {
    cat("Version:", x$metasurvey_version$from, "->",
      x$metasurvey_version$to, "\n"
    )
  }
  invisible(x)
}


# Get final row count from provenance
.provenance_final_n <- function(prov) {
  if (length(prov$steps) > 0) {
    prov$steps[[length(prov$steps)]]$n_after
  } else {
    prov$source$initial_n
  }
}
