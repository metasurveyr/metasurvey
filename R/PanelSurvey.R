#' @title RotativePanelSurvey Class
#' @description This class represents a rotative panel survey, which includes implantation and follow-up surveys.
#' It provides methods to access and manipulate survey data, steps, recipes, workflows, and designs.
#' @field implantation A survey object representing the implantation survey.
#' @field follow_up A list of survey objects representing the follow-up surveys.
#' @field type A string indicating the type of the survey.
#' @field default_engine A string specifying the default engine used for processing.
#' @field steps A list of steps applied to the survey.
#' @field recipes A list of recipes associated with the survey.
#' @field workflows A list of workflows associated with the survey.
#' @field design A design object for the survey.
#' @field periodicity A list containing the periodicity of the implantation and follow-up surveys.
#' @keywords Surveymethods, RotativePanelSurvey
#' @export
RotativePanelSurvey <- R6Class(
  "RotativePanelSurvey",
  public = list(
    implantation = NULL,
    follow_up = NULL,
    type = NULL,
    default_engine = NULL,
    steps = NULL,
    recipes = NULL,
    workflows = NULL,
    design = NULL,
    periodicity = NULL,

    #' @description Initializes a new instance of the RotativePanelSurvey class.
    #' @param implantation A survey object representing the implantation survey.
    #' @param follow_up A list of survey objects representing the follow-up surveys.
    #' @param type A string indicating the type of the survey.
    #' @param default_engine A string specifying the default engine used for processing.
    #' @param steps A list of steps applied to the survey.
    #' @param recipes A list of recipes associated with the survey.
    #' @param workflows A list of workflows associated with the survey.
    #' @param design A design object for the survey.
    initialize = function(implantation, follow_up, type, default_engine, steps, recipes, workflows, design) {
      self$implantation <- implantation
      self$follow_up <- follow_up
      self$type <- type
      self$default_engine <- default_engine
      self$steps <- steps
      self$recipes <- recipes
      self$workflows <- workflows
      self$design <- design

      follow_up_types <- sapply(self$follow_up, function(f) f$periodicity)

      if (length(unique(follow_up_types)) > 1) {
        stop("All follow-up surveys must have the same type")
      }

      self$periodicity <- list(
        implantation = self$implantation$periodicity,
        follow_up = unique(follow_up_types)
      )
    },

    #' @description Retrieves the implantation survey.
    #' @return A survey object representing the implantation survey.
    get_implantation = function() {
      return(self$implantation)
    },

    #' @description Retrieves the follow-up surveys.
    #' @param index An integer specifying the index of the follow-up survey to retrieve.
    #' @param monthly A vector of integers specifying monthly intervals.
    #' @param quarterly A vector of integers specifying quarterly intervals.
    #' @param semiannual A vector of integers specifying semiannual intervals.
    #' @param annual A vector of integers specifying annual intervals.
    #' @return A list of follow-up surveys matching the specified criteria.
    get_follow_up = function(index = length(self$follow_up), monthly = NULL, quarterly = NULL, semiannual = NULL, annual = NULL) {
      return(self$follow_up[index])
    },

    #' @description Retrieves the type of the survey.
    #' @return A string indicating the type of the survey.
    get_type = function() {
      return(self$type)
    },

    #' @description Retrieves the default engine used for processing.
    #' @return A string specifying the default engine.
    get_default_engine = function() {
      return(self$default_engine)
    },

    #' @description Retrieves the steps applied to the survey.
    #' @return A list containing the steps for the implantation and follow-up surveys.
    get_steps = function() {
      steps_implantation <- self$implantation$steps
      steps_follow_up <- sapply(self$follow_up, function(f) f$steps)

      return(list(
        implantation = steps_implantation,
        follow_up = steps_follow_up
      ))
    },

    #' @description Retrieves the recipes associated with the survey.
    #' @return A list of recipes.
    get_recipes = function() {
      return(self$recipes)
    },

    #' @description Retrieves the workflows associated with the survey.
    #' @return A list of workflows.
    get_workflows = function() {
      return(self$workflows)
    },

    #' @description Retrieves the design object for the survey.
    #' @return A design object.
    get_design = function() {
      return(self$design)
    },

    #' @description Prints metadata about the RotativePanelSurvey object.
    print = function() {
      get_metadata(self = self)
    }
  )
)

#' Extraer encuestas por periodicidad de panel rotativo
#'
#' Esta función extrae subconjuntos de encuestas de un objeto RotativePanelSurvey
#' basándose en criterios temporales específicos. Permite obtener encuestas para
#' diferentes tipos de análisis (mensual, trimestral, anual) respetando la
#' estructura temporal del panel rotativo.
#'
#' @param RotativePanelSurvey Objeto `RotativePanelSurvey` que contiene las
#'   encuestas del panel rotativo organizadas temporalmente
#' @param index Vector de enteros que especifica índices específicos de
#'   encuestas a extraer. Si es un solo valor, devuelve esa encuesta;
#'   si es un vector, devuelve una lista
#' @param monthly Vector de enteros que especifica qué meses extraer para
#'   análisis mensual (1-12)
#' @param annual Vector de enteros que especifica qué años extraer para
#'   análisis anual
#' @param quarterly Vector de enteros que especifica qué trimestres extraer
#'   para análisis trimestral (1-4)
#' @param biannual Vector de enteros que especifica qué semestres extraer
#'   para análisis semestral (1-2)
#' @param use.parallel Lógico que indica si usar procesamiento en paralelo
#'   para operaciones intensivas. Por defecto FALSE
#'
#' @return Lista de objetos `Survey` que corresponden a los criterios
#'   especificados, o un solo objeto `Survey` si se especifica un índice único
#'
#' @details
#' Esta función es esencial para trabajar con paneles rotativos porque:
#' \itemize{
#'   \item Facilita análisis por periodicidad: Permite extraer datos para
#'     diferentes tipos de estimaciones temporales
#'   \item Mantiene estructura temporal: Respeta las relaciones temporales
#'     entre las diferentes ondas del panel
#'   \item Optimiza memoria: Solo carga las encuestas necesarias para el análisis
#'   \item Facilita comparaciones: Permite extraer períodos específicos para
#'     análisis comparativos
#'   \item Soporta paralelización: Para operaciones con grandes volúmenes de datos
#' }
#'
#' Los criterios de extracción se interpretan según la frecuencia de la encuesta:
#' - Para ECH mensual: monthly=c(1,3,6) extrae enero, marzo y junio
#' - Para análisis anual: annual=1 típicamente extrae el primer año disponible
#' - Para análisis trimestral: quarterly=c(1,4) extrae Q1 y Q4
#'
#' Si no se especifica ningún criterio, la función devuelve la encuesta de
#' implantación con una advertencia.
#'
#' @examples
#' \dontrun{
#' # Cargar panel rotativo
#' panel_ech <- load_panel_survey(
#'   path = "ech_panel_2023.dta",
#'   svy_type = "ech_panel",
#'   svy_edition = "2023"
#' )
#'
#' # Extraer encuestas mensuales específicas
#' ech_trimestre1 <- extract_surveys(
#'   panel_ech,
#'   monthly = c(1, 2, 3) # Enero, febrero, marzo
#' )
#'
#' # Extraer por índice
#' ech_primera <- extract_surveys(panel_ech, index = 1)
#' ech_varias <- extract_surveys(panel_ech, index = c(1, 3, 6))
#'
#' # Análisis trimestral
#' ech_Q1_Q4 <- extract_surveys(
#'   panel_ech,
#'   quarterly = c(1, 4)
#' )
#'
#' # Para análisis anual (típicamente todas las encuestas del año)
#' ech_anual <- extract_surveys(
#'   panel_ech,
#'   annual = 1
#' )
#'
#' # Con procesamiento paralelo para grandes volúmenes
#' ech_completo <- extract_surveys(
#'   panel_ech,
#'   monthly = 1:12,
#'   use.parallel = TRUE
#' )
#'
#' # Usar en workflow
#' resultados <- workflow(
#'   survey = extract_surveys(panel_ech, quarterly = c(1, 2)),
#'   svymean(~desocupado, na.rm = TRUE),
#'   estimation_type = "quarterly"
#' )
#' }
#'
#' @seealso
#' \code{\link{load_panel_survey}} para cargar paneles rotativos
#' \code{\link{get_implantation}} para obtener datos de implantación
#' \code{\link{get_follow_up}} para obtener datos de seguimiento
#' \code{\link{workflow}} para usar las encuestas extraídas en análisis
#'
#' @keywords Surveymethods RotativePanelSurvey
#' @export

extract_surveys <- function(RotativePanelSurvey, index = NULL, monthly = NULL, annual = NULL, quarterly = NULL, biannual = NULL, use.parallel = FALSE) {
  if (is.null(monthly) && is.null(annual) && is.null(quarterly) && is.null(biannual) && is.null(index)) {
    warning("At least one interval argument must be different from NULL. Returning the implantation survey.")
    annual <- 1
  }

  if (!inherits(RotativePanelSurvey, "RotativePanelSurvey")) {
    stop("The `RotativeSurvey` argument must be an object of class `RotativePanelSurvey`")
  }

  follow_up <- RotativePanelSurvey$follow_up

  if (!is.null(index)) {
    if (length(index) > 1) {
      return(follow_up[index])
    }
    return(follow_up[[index]])
  }

  dates <- as.Date(sapply(unname(follow_up), function(x) x$edition))

  ts_series <- stats::ts(1:length(follow_up), start = c(as.numeric(format(min(dates), "%Y")), as.numeric(format(min(dates), "%m"))), frequency = 12)

  apply_interval <- function(ts_series, start_year, start_month, end_year, end_month) {
    as.vector(stats::window(ts_series, start = c(start_year, start_month), end = c(end_year, end_month)))
  }

  apply_func <- if (use.parallel) {
    requireNamespace("parallel")
    parallel::mclapply
  } else {
    base::lapply
  }

  results <- list()

  # Crear solo los intervalos especificados y no dejar listas vacías
  month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  if (!is.null(monthly)) {
    results$monthly <- list()
    for (month in monthly) {
      indices <- apply_interval(ts_series, as.numeric(format(min(dates), "%Y")), month, as.numeric(format(max(dates), "%Y")), month)
      results$monthly[[month_names[month]]] <- follow_up[indices]
    }
  }

  if (!is.null(annual)) {
    results$annual <- list()
    for (year in annual) {
      indices <- apply_interval(ts_series, year, 1, year, 12)
      results$annual[[as.character(year)]] <- follow_up[indices]
    }
  }

  quarter_names <- c("Q1", "Q2", "Q3", "Q4")
  if (!is.null(quarterly)) {
    results$quarterly <- list()
    for (quarter in quarterly) {
      start_month <- (quarter - 1) * 3 + 1
      end_month <- start_month + 2
      indices <- apply_interval(ts_series, as.numeric(format(min(dates), "%Y")), start_month, as.numeric(format(max(dates), "%Y")), end_month)
      results$quarterly[[quarter_names[quarter]]] <- follow_up[indices]
    }
  }

  biannual_names <- c("H1", "H2")
  if (!is.null(biannual)) {
    results$biannual <- list()
    for (semester in biannual) {
      start_month <- ifelse(semester == 1, 1, 7)
      end_month <- ifelse(semester == 1, 6, 12)
      indices <- apply_interval(ts_series, as.numeric(format(min(dates), "%Y")), start_month, as.numeric(format(max(dates), "%Y")), end_month)
      results$biannual[[biannual_names[semester]]] <- follow_up[indices]
    }
  }

  return(PoolSurvey$new(results))
}

#' @title PoolSurvey Class
#' @description This class represents a collection of surveys grouped by specific periods (e.g., monthly, quarterly, annual).
#' It provides methods to access and manipulate the grouped surveys.
#' @field surveys A list containing the grouped surveys.
#' @keywords Surveymethods, PoolSurvey
#' @export
PoolSurvey <- R6Class(
  "PoolSurvey",
  public = list(
    surveys = NULL,

    #' @description Initializes a new instance of the PoolSurvey class.
    #' @param surveys A list containing the grouped surveys.
    initialize = function(surveys) {
      self$surveys <- surveys
    },

    #' @description Retrieves surveys for a specific period.
    #' @param period A string specifying the period to retrieve (e.g., "monthly", "quarterly").
    #' @return A list of surveys for the specified period.
    get_surveys = function(period = NULL) {
      if (!is.null(period)) {
        return(self$surveys[[1]][[period]])
      } else {
        return(self$surveys)
      }
    },

    #' @description Prints metadata about the PoolSurvey object.
    print = function() {
      get_metadata(self = self)
    }
  )
)

#' Obtener encuesta de implantación de panel rotativo
#'
#' Esta función extrae la encuesta de implantación (primera onda) de un objeto
#' RotativePanelSurvey. La encuesta de implantación representa la primera
#' recolección de datos del panel y es fundamental para establecer la línea base
#' y las características estructurales del panel.
#'
#' @param RotativePanelSurvey Objeto `RotativePanelSurvey` del cual extraer
#'   la encuesta de implantación
#'
#' @return Objeto `Survey` que contiene la encuesta de implantación con todos
#'   sus metadatos, datos y configuración de diseño
#'
#' @details
#' La encuesta de implantación es especial en un panel rotativo porque:
#' \itemize{
#'   \item Establece la línea base: Define las características iniciales de
#'     todas las unidades del panel
#'   \item Contiene toda la muestra: Incluye todas las unidades que participarán
#'     en las diferentes ondas del panel
#'   \item Define estructura temporal: Establece los patrones de rotación y
#'     seguimiento del panel
#'   \item Configura metadatos: Contiene información sobre periodicidad,
#'     variables clave y estratificación
#'   \item Base para seguimiento: Sirve como referencia para tracking de
#'     unidades en ondas posteriores
#' }
#'
#' Esta función es esencial para análisis que requieren:
#' - Comparaciones temporales desde la línea base
#' - Análisis de la estructura completa del panel
#' - Configuración de modelos longitudinales
#' - Evaluación de la calidad del diseño muestral
#'
#' @examples
#' \dontrun{
#' # Cargar panel rotativo de ECH
#' panel_ech <- load_panel_survey(
#'   path = "ech_panel_2023.dta",
#'   svy_type = "ech_panel",
#'   svy_edition = "2023"
#' )
#'
#' # Obtener encuesta de implantación
#' ech_implantacion <- get_implantation(panel_ech)
#'
#' # Verificar características de la implantación
#' cat("Tamaño muestra implantación:", nrow(ech_implantacion$data))
#' cat("Variables disponibles:", ncol(ech_implantacion$data))
#'
#' # Usar en análisis de línea base
#' baseline_stats <- workflow(
#'   survey = ech_implantacion,
#'   svymean(~tasa_actividad, na.rm = TRUE),
#'   estimation_type = "baseline"
#' )
#'
#' # Comparar con follow-up
#' followup_1 <- get_follow_up(panel_ech, index = 1)[[1]]
#'
#' # Análisis de cambios desde implantación
#' panel_comparison <- list(
#'   implantacion = ech_implantacion,
#'   seguimiento = followup_1
#' )
#' }
#'
#' @seealso
#' \code{\link{get_follow_up}} para obtener encuestas de seguimiento
#' \code{\link{extract_surveys}} para extraer múltiples encuestas por criterios
#' \code{\link{load_panel_survey}} para cargar paneles rotativos
#' \code{\link{workflow}} para análisis con la encuesta de implantación
#'
#' @keywords Surveymethods
#' @keywords RotativePanelSurvey
#' @export

get_implantation <- function(RotativePanelSurvey) {
  if (!inherits(RotativePanelSurvey, "RotativePanelSurvey")) {
    stop("The `RotativeSurvey` argument must be an object of class `RotativePanelSurvey`")
  }

  return(RotativePanelSurvey$implantation)
}

#' Obtener encuestas de seguimiento de panel rotativo
#'
#' Esta función extrae una o múltiples encuestas de seguimiento (ondas posteriores
#' a la implantación) de un objeto RotativePanelSurvey. Las encuestas de seguimiento
#' representan las recolecciones posteriores del panel y son esenciales para
#' análisis longitudinales y de cambio temporal.
#'
#' @param RotativePanelSurvey Objeto `RotativePanelSurvey` del cual extraer
#'   las encuestas de seguimiento
#' @param index Vector de enteros que especifica cuáles encuestas de seguimiento
#'   extraer. Por defecto extrae todas las disponibles (1:length(follow_up)).
#'   Puede ser un solo índice o un vector de índices
#'
#' @return Lista de objetos `Survey` correspondientes a las encuestas de
#'   seguimiento especificadas. Si se especifica un solo índice, devuelve
#'   una lista con un elemento
#'
#' @details
#' Las encuestas de seguimiento son fundamentales en paneles rotativos porque:
#' \itemize{
#'   \item Permiten análisis longitudinal: Seguimiento de las mismas unidades
#'     a través del tiempo
#'   \item Capturan cambios temporales: Evolución de variables económicas,
#'     sociales y demográficas
#'   \item Mantienen representatividad: Cada onda mantiene representatividad
#'     poblacional mediante rotación controlada
#'   \item Optimizan recursos: Reutilizan información de ondas anteriores
#'     para reducir costos de recolección
#'   \item Facilitan comparaciones: Estructura temporal consistente para
#'     análisis de tendencias
#' }
#'
#' En paneles rotativos como ECH:
#' - Cada onda de seguimiento cubre un período específico (mensual/trimestral)
#' - Las unidades rotan gradualmente manteniendo overlap temporal
#' - Los índices corresponden al orden cronológico de recolección
#' - Cada seguimiento mantiene consistencia metodológica con implantación
#'
#' @examples
#' \dontrun{
#' # Cargar panel rotativo
#' panel_ech <- load_panel_survey(
#'   path = "ech_panel_2023.dta",
#'   svy_type = "ech_panel",
#'   svy_edition = "2023"
#' )
#'
#' # Obtener primera encuesta de seguimiento
#' seguimiento_1 <- get_follow_up(panel_ech, index = 1)[[1]]
#'
#' # Obtener múltiples seguimientos
#' seguimientos_trim1 <- get_follow_up(panel_ech, index = c(1, 2, 3))
#'
#' # Obtener todos los seguimientos disponibles
#' todos_seguimientos <- get_follow_up(panel_ech)
#'
#' # Verificar número de seguimientos disponibles
#' n_seguimientos <- length(get_follow_up(panel_ech))
#' cat("Seguimientos disponibles:", n_seguimientos)
#'
#' # Análisis longitudinal con seguimientos
#' implantacion <- get_implantation(panel_ech)
#' seguimiento_final <- get_follow_up(panel_ech, index = n_seguimientos)[[1]]
#'
#' # Comparar tasas entre implantación y seguimiento final
#' tasa_inicial <- workflow(
#'   survey = implantacion,
#'   svymean(~tasa_desempleo, na.rm = TRUE)
#' )
#'
#' tasa_final <- workflow(
#'   survey = seguimiento_final,
#'   svymean(~tasa_desempleo, na.rm = TRUE)
#' )
#'
#' # Análisis trimestral con seguimientos específicos
#' trimestre_actual <- workflow(
#'   survey = seguimientos_trim1,
#'   svymean(~ingreso_laboral, na.rm = TRUE),
#'   estimation_type = "quarterly"
#' )
#' }
#'
#' @seealso
#' \code{\link{get_implantation}} para obtener la encuesta de implantación
#' \code{\link{extract_surveys}} para extraer encuestas por criterios temporales
#' \code{\link{load_panel_survey}} para cargar paneles rotativos
#' \code{\link{workflow}} para análisis con encuestas de seguimiento
#'
#' @keywords Surveymethods
#' @keywords RotativePanelSurvey
#' @export


get_follow_up <- function(RotativePanelSurvey, index = 1:length(RotativePanelSurvey$follow_up)) {
  if (!inherits(RotativePanelSurvey, "RotativePanelSurvey")) {
    stop("The `RotativeSurvey` argument must be an object of class `RotativePanelSurvey`")
  }

  return(RotativePanelSurvey$follow_up[index])
}
