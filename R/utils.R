is_blank <- function(x) {
  return(
    is.na(x) || x == ""
  )
}


"%||%" <- function(x, y) if (is.null(x)) y else x

"%@%" <- function(x, y) {
  if (is_blank(x)) {
    y
  } else {
    x
  }
}

#' Validate weight
#' @param svy Survey
#' @param weight Weight
#' @return Weight
#' @keywords utils
#' @keywords internal
#' @noRd

validate_weight <- function(svy, weight) {
  if (is.null(svy)) {
    return(NULL)
  }

  if (!is.character(weight)) {
    stop("Weight must be a character")
  }

  if (!weight %in% colnames(svy)) {
    stop(glue_col(
      "{red Weight {weight} not found in survey}",
      .literal = TRUE
    ))
  } else {
    weight
  }
}

#' Validate Replicate
#' @param svy Survey
#' @param replicate Replicate
#' @return Replicate
#' @keywords utils
#' @keywords internal
#' @noRd

validate_replicate <- function(svy, replicate) {
  if (is.null(svy)) {
    return(NULL)
  }

  if (!is.null(replicate$replicate_id)) {
    if (!is.character(replicate$replicate_id)) {
      stop("Replicate ID must be a character")
    }

    if (!all(names(replicate$replicate_id) %in% colnames(svy))) {
      stop(glue_col(
        "{red Replicate ID {replicate$replicate_id} not found in survey}",
        .literal = TRUE
      ))
    }
  }
  
  


  replicate_file <- read_file(replicate$replicate_path)


  if (!is.null(replicate$replicate_pattern)) {
    if (!is.character(replicate$replicate_pattern)) {
      stop("Replicate pattern must be a character")
    }

    column_names <- names(replicate_file)

    if (!any(grepl(replicate$replicate_pattern, column_names))) {
      stop("Replicate pattern not found in replicate file")
    }
  }

  replicate[["replicate_file"]] <- replicate_file

  return(
    replicate
  )
}

#' Validate Weight time pattern
#' @param svy Survey
#' @param weight_time_pattern Weight time pattern
#' @return Weight time pattern
#' @keywords utils
#' @keywords internal
#' @noRd

validate_weight_time_pattern <- function(svy, weight_list) {
  if (is.null(svy)) {
    return(NULL)
  }

  if (!is.list(weight_list)) {
    stop("Weight time pattern must be a list")
  }

  Map(
    f = function(x) {
      if (is.character(x = x)) {
        validate_weight(svy = svy, weight = x)
      } else {
        if (is.list(x)) {
          validate_replicate(svy = svy, replicate = x)
        }
      }
    },
    weight_list
  )
}


#' Load survey example
#' @param svy_type Survey type
#' @param svy_edition Survey edition
#' @keywords utils
#' @export

load_survey_example <- function(svy_type, svy_edition) {
  baseUrl <- "https://raw.githubusercontent.com/metasurveyr/metasurvey_data/main/"

  f <- tempfile(fileext = ".csv")
  if (file.exists(f)) {
    return(f)
  } else {
    utils::download.file(
      paste0(
        baseUrl,
        glue::glue(
          "{svy_type}/{svy_edition}.csv"
        )
      ),
      f,
      method = "auto"
    )
    return(f)
  }


  return(file.path(tempdir(), paste0(svy_type, paste0(svy_edition, ".csv"), sep = "/")))
}

#' Get use_copy option
#' @return Use copy
#' @keywords utils
#' @export

use_copy_default <- function() {
  getOption("use_copy", default = TRUE)
}

#' Set use_copy option
#' @param use_copy Use copy
#' @export
#' @keywords utils
#' @examples
#' set_use_copy(FALSE)
#' use_copy_default()
#' set_use_copy(TRUE)
#' use_copy_default()
set_use_copy <- function(use_copy) {
  if (!is.logical(use_copy)) {
    stop("use_copy must be a logical")
  }

  options(use_copy = use_copy)
}


#' Get User
#' @return User
#' @keywords utils
#' @keywords internal
#' @noRd

get_user <- function() {
  user_key <- NULL

  api_key <- getOption("metasurvey.api_key", default = NULL)

  if (!is.null(api_key)) {
    user_key <- "apiKey"
  }

  getOption("metasurvey.user", default = NULL) %||% user_key %||% "public"
}


#' URL API Host
#' @return URL API Host
#' @keywords utils
#' @keywords internal
#' @noRd

url_api_host <- function() {
  default_host <- "https://data.mongodb-api.com/app/data-vonssxi/endpoint/data/v1/action/"

  getOption("metasurvey.base_url") %||% default_host
}

#' Get API Key
#' @return API Key
#' @keywords utils
#' @export

get_api_key <- function() {
  api_key <- getOption("metasurvey.api_key", default = NULL)

  user <- getOption("metasurvey.user", default = NULL)
  password <- getOption("metasurvey.password", default = NULL)

  payload <- list(
    methodAuth = "apiKey",
    token = api_key
  )

  if (is.null(api_key)) {
    if (is.null(user) || is.null(password)) {
      api_key <- public_key()
      payload <- list(
        methodAuth = "anonUser",
        token = api_key
      )
    } else {
      payload <- list(
        methodAuth = "userPassword",
        email = user,
        password = password
      )
    }
  }

  return(payload)
}

#' Public Key
#' @return Public Key
#' @keywords utils
#' @keywords internal
#' @noRd

public_key <- function() {
  url <- "https://services.cloud.mongodb.com/api/client/v2.0/app/data-vonssxi/auth/providers/anon-user/login"
  response <- POST(url)
  content <- content(response)

  if (response$status_code != 200) {
    stop(message("Error getting public key", content))
  }

  return(content$access_token)
}


#' Set API Key
#' @param api_key API Key
#' @keywords utils
#' @export

set_api_key <- function(api_key) {
  options(metasurvey.api_key = api_key)
}


#' Lazy processing
#' @return Value
#' @keywords utils
#' @export

lazy_default <- function() {
  getOption("lazy_processing", default = TRUE)
}

#' Set lazy processing
#' @param lazy Lazy processing
#' @keywords utils
#' @export

set_lazy_processing <- function(lazy) {
  if (!is.logical(lazy)) {
    stop("lazy must be a logical")
  }

  options(lazy_processing = lazy)
}


#' Extract time pattern
#' @param svy_edition Survey edition
#' @return List
#' @keywords utils
#' @export

extract_time_pattern <- function(svy_edition) {
  # Limpiar la entrada: reemplazar espacios y guiones por guiones bajos y quitar guiones bajos extra
  svy_edition <- gsub("[\\s\\-\\/]+", "_", svy_edition, perl = TRUE)
  svy_edition <- gsub("[_*]+", "_", svy_edition, perl = TRUE)
  svy_edition <- trimws(svy_edition, which = "both")

  # Inicializar variables
  type <- NA
  year <- NA
  year_start <- NA
  year_end <- NA
  month <- NA
  periodicity <- NA

  # Extraer el tipo si hay texto al inicio
  if (grepl("$[^0-9]*", svy_edition)) {
    type <- sub("_.*", "", svy_edition,perl = TRUE)
    svy_edition <- gsub("[^0-9]*", "", svy_edition, perl = TRUE)
  }

  # Caso: Mensual en formato YYYYMM (e.g., "202312")
  if (grepl("^(\\d{4})(\\d{2})$", svy_edition) && as.numeric(sub("^(\\d{4})(\\d{2})$", "\\2", svy_edition)) <= 12) {
    year <- as.numeric(sub("^(\\d{4})(\\d{2})$", "\\1", svy_edition))
    month <- as.numeric(sub("^(\\d{4})(\\d{2})$", "\\2", svy_edition))

    if (month >= 1 && month <= 12) {
      periodicity <- "Monthly"
    } else {
      month <- NA
      periodicity <- "Formato incorrecto"
    }

    # Caso: Mensual en formato MMYYYY (e.g., "122023")
  } else if (grepl("^(\\d{2})(\\d{4})$", svy_edition) && as.numeric(sub("^(\\d{2})(\\d{4})$", "\\1", svy_edition)) <= 12) {
    month <- as.numeric(sub("^(\\d{2})(\\d{4})$", "\\1", svy_edition))
    year <- as.numeric(sub("^(\\d{2})(\\d{4})$", "\\2", svy_edition))

    if (month >= 1 && month <= 12) {
      periodicity <- "Monthly"
    } else {
      month <- NA
      periodicity <- "Formato incorrecto"
    }

    # Caso: Mensual con formato MM_YYYY o MM-YYYY (e.g., "01_2023", "12_2023")
  } else if (grepl("^(\\d{2})[_-](\\d{4})$", svy_edition) && as.numeric(sub("^(\\d{2})[_-](\\d{4})$", "\\1", svy_edition)) <= 12) {
    month <- as.numeric(sub("^(\\d{2})[_-](\\d{4})$", "\\1", svy_edition))
    year <- as.numeric(sub("^(\\d{2})[_-](\\d{4})$", "\\2", svy_edition))

    if (month >= 1 && month <= 12) {
      periodicity <- "Monthly"
    } else {
      month <- NA
      periodicity <- "Formato incorrecto"
    }

    # Caso: Mensual con formato YYYY_MM o YYYY-MM (e.g., "2023_12")
  } else if (grepl("^(\\d{4})[_-](\\d{2})$", svy_edition) && as.numeric(sub("^(\\d{4})[_-](\\d{2})$", "\\2", svy_edition)) <= 12) {
    year <- as.numeric(sub("^(\\d{4})[_-](\\d{2})$", "\\1", svy_edition))
    month <- as.numeric(sub("^(\\d{4})[_-](\\d{2})$", "\\2", svy_edition))

    if (month >= 1 && month <= 12) {
      periodicity <- "Monthly"
    } else {
      month <- NA
      periodicity <- "Formato incorrecto"
    }

    # Caso: Encuesta con rango de años (e.g., "2019_2021")
  } else if (grepl("^(\\d{4})(\\d{4})$", svy_edition)) {
    years <- as.numeric(unlist(regmatches(svy_edition, gregexpr("\\d{4}", svy_edition))))
    year_start <- min(years)
    year_end <- max(years)
    periodicity <- if (year_end - year_start + 1 == 3) "Trianual" else "Multianual"

    # Caso: Anual (e.g., "2023")
  } else if (grepl("^\\d{4}$", svy_edition) && as.numeric(svy_edition) >= 1900) {
    year <- as.numeric(svy_edition)
    periodicity <- "Annual"

    # Caso: Mensual con formato YY_MM o MM_YY (e.g., "23_05" que se interpreta como "2023-05")

  } else if ((grepl("^(\\d{2})[_-](\\d{2})$", svy_edition) || grepl("^(\\d{2})(\\d{2})$", svy_edition)))  {
    part1 <- as.numeric(sub("^(\\d{2})[_-](\\d{2})$", "\\1", svy_edition))
    part2 <- as.numeric(sub("^(\\d{2})[_-](\\d{2})$", "\\2", svy_edition))

    # Si part1 puede ser mes (1-12), entonces part1 es mes y part2 es año
    if (part1 >= 1 && part1 <= 12) {
      month <- part1
      year <- 2000 + part2 # Interpretar como 20XX
    } else if (part2 >= 1 && part2 <= 12) {
      year <- 2000 + part1 # Interpretar como 20XX
      month <- part2
    } else {
      part1 <- as.numeric(sub("^(\\d{2})(\\d{2})$", "\\1", svy_edition))
      part2 <- as.numeric(sub("^(\\d{2})(\\d{2})$", "\\2", svy_edition))
      if (part1 >= 1 && part1 <= 12) {
        month <- part1
        year <- 2000 + part2 # Interpretar como 20XX
      } else if (part2 >= 1 && part2 <= 12) {
        year <- 2000 + part1 # Interpretar como 20XX
        month <- part2
      }
    }

    if (!is.na(month) && month >= 1 && month <= 12) {
      periodicity <- "Monthly"
    }
  } else {
    periodicity <- "Formato desconocido"
  }

  # Devolver los resultados
  result <- list(type = type, year = year, month = month, year_start = year_start, year_end = year_end, periodicity = periodicity)
  clean_result <- result[!sapply(result, is.na)]

  return(clean_result)
}










#' Validate time pattern
#' @param svy_edition Survey edition
#' @param svy_type Survey type
#' @return Logical
#' @keywords utils
#' @return List
#' @export

validate_time_pattern <- function(svy_type = NULL, svy_edition = NULL) {
  time_pattern <- extract_time_pattern(svy_edition)

  if (is.null(time_pattern$type) && is.null(svy_type)) {
    stop("Type not found. Please provide a valid type in the survey edition or as an argument")
  }

  if (!is.null(time_pattern$type) && toupper(time_pattern$type) != toupper(svy_type)) {
    stop("Type does not match. Please provide a valid type in the survey edition or as an argument")
  }

  # Remove svy_type from time_pattern

  names_time <- names(time_pattern)

  remove_attributes <- c("type", "periodicity")


  svy_editions <- ""

  if (!is.null(time_pattern$month) && !is.na(time_pattern$month) && !is.null(time_pattern$year) && !is.na(time_pattern$year)) {
    date_string <- sprintf("%04d-%02d-01", time_pattern$year, time_pattern$month)
    svy_edition <- as.Date(date_string)
  } else {
    # Si no hay mes o año, crear una cadena básica
    svy_edition <- Reduce(
      time_pattern[names_time[!names_time %in% remove_attributes]],
      f = function(x, y) {
        paste(x, y, sep = "_")
      }
    )
  }



  return(
    list(
      svy_type = time_pattern$type %||% svy_type,
      svy_edition = svy_edition,
      svy_periodicity = time_pattern$periodicity %||% "Annual"
    )
  )
}


#' Group dates
#' @param dates Dates
#' @param type Type
#' @return Group
#' @keywords utils
#' @export

group_dates <- function(dates, type = c("monthly", "quarterly", "biannual")) {
  type <- match.arg(type)
  dates_lt <- as.POSIXlt(dates)

  group <- integer(length(dates))

  if (type == "monthly") {
    group <- dates_lt$mon + 1
  } else if (type == "quarterly") {
    group <- (dates_lt$mon %/% 3) + 1
  } else if (type == "biannual") {
    group <- (dates_lt$mon %/% 6) + 1
  }

  names(group) <- dates

  return(group)
}


#' Add Weight time pattern
#' @param monthly Weight monthly
#' @param annual Weight annual
#' @param quarterly Weight quarterly
#' @param biannual Weight biannual
#' @keywords utils
#' @export
#'
add_weight <- function(
    monthly = NULL,
    annual = NULL,
    quarterly = NULL,
    biannual = NULL) {
  weight_list <- list(
    monthly = monthly,
    annual = annual,
    quarterly = quarterly,
    biannual = biannual
  )

  weight_list_clean <- weight_list[!sapply(weight_list, is.null)]

  return(weight_list_clean)
}

#' add_replicate
#' @param weight Weight
#' @param replicate_pattern Replicate pattern
#' @param replicate_path Replicate file
#' @param replicate_id Replicate ID
#' @param replicate_type Replicate type
#' @keywords utils
#' @export

add_replicate <- function(
    weight,
    replicate_pattern,
    replicate_path = NULL,
    replicate_id = NULL,
    replicate_type) {
  replicate_list <- list(
    weight = weight,
    replicate_pattern = replicate_pattern,
    replicate_path = replicate_path,
    replicate_id = replicate_id,
    replicate_type = replicate_type
  )

  replicate_list_clean <- replicate_list[!sapply(replicate_list, is.null)]

  return(replicate_list_clean)
}

#' Evaluate estimation with Coeficient of Variation
#' @param cv Coeficient of Variation
#' @return character
#' @keywords utils
#' @export

evaluate_cv <- function(cv) {
  if (cv < 5) {
    return("Excelente")
  } else if (cv >= 5 && cv < 10) {
    return("Muy bueno")
  } else if (cv >= 10 && cv < 15) {
    return("Bueno")
  } else if (cv >= 15 && cv < 25) {
    return("Aceptable")
  } else if (cv >= 25 && cv < 35) {
    return("Utilizar con precaucion")
  } else {
    return("No publicar")
  }
}
