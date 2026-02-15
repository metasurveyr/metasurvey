# MongoDB Atlas Data API utilities for Shiny app

mongo_base_url <- function() {
  getOption("metasurvey.base_url") %||%
    "https://data.mongodb-api.com/app/data-vonssxi/endpoint/data/v1/action/"
}

mongo_anon_token <- function() {
  url <- "https://services.cloud.mongodb.com/api/client/v2.0/app/data-vonssxi/auth/providers/anon-user/login"
  resp <- tryCatch(httr::POST(url, httr::timeout(5)), error = function(e) NULL)
  if (is.null(resp) || resp$status_code != 200) return(NULL)
  httr::content(resp)$access_token
}

mongo_headers <- function() {
  api_key <- getOption("metasurvey.api_key", default = NULL)
  if (!is.null(api_key)) {
    return(c(
      "Content-Type" = "application/json",
      "Access-Control-Request-Headers" = "*",
      "apiKey" = api_key
    ))
  }
  token <- mongo_anon_token()
  if (is.null(token)) {
    return(c("Content-Type" = "application/json"))
  }
  c(
    "Content-Type" = "application/json",
    "Access-Control-Request-Headers" = "*",
    "Authorization" = paste("Bearer", token)
  )
}

mongo_request <- function(action, collection, body) {
  url <- paste0(mongo_base_url(), action)
  payload <- c(
    list(
      collection = collection,
      database = "metasurvey",
      dataSource = "Cluster0"
    ),
    body
  )
  resp <- tryCatch(
    httr::POST(
      url,
      body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
      httr::add_headers(.headers = mongo_headers()),
      encode = "raw",
      httr::timeout(10)
    ),
    error = function(e) NULL
  )
  if (is.null(resp)) {
    return(list(ok = FALSE, error = "Connection failed", status = 0))
  }
  if (resp$status_code >= 400) {
    msg <- httr::content(resp, "text", encoding = "UTF-8")
    return(list(ok = FALSE, error = msg, status = resp$status_code))
  }
  parsed <- jsonlite::parse_json(httr::content(resp, "text", encoding = "UTF-8"))
  parsed$ok <- TRUE
  parsed
}

# --- User auth ---

hash_password <- function(password) {
  digest::digest(password, algo = "sha256", serialize = FALSE)
}

# In-memory user store for offline/demo mode
.local_users <- new.env(parent = emptyenv())
.local_users$data <- list()

mongo_register <- function(name, email, password, user_type = "individual", institution = NULL) {
  # Try MongoDB first
  check <- mongo_request("findOne", "users", list(filter = list(email = email)))

  if (isTRUE(check$ok)) {
    # Online mode
    if (!is.null(check$document)) {
      return(list(ok = FALSE, error = "Email already registered"))
    }
    doc <- list(
      name = name, email = email,
      password_hash = hash_password(password),
      user_type = user_type, institution = institution,
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
    )
    result <- mongo_request("insertOne", "users", list(document = doc))
    if (!isTRUE(result$ok)) {
      return(list(ok = FALSE, error = result$error %||% "Registration failed"))
    }
  } else {
    # Offline fallback: local store
    if (email %in% names(.local_users$data)) {
      return(list(ok = FALSE, error = "Email already registered"))
    }
    .local_users$data[[email]] <- list(
      name = name, email = email,
      password_hash = hash_password(password),
      user_type = user_type, institution = institution
    )
  }

  inst_obj <- if (!is.null(institution) && nzchar(institution) && user_type == "institutional_member") {
    metasurvey::RecipeUser$new(name = institution, user_type = "institution")
  } else NULL

  user <- metasurvey::RecipeUser$new(
    name = name, user_type = user_type, email = email, institution = inst_obj
  )
  list(ok = TRUE, user = user)
}

mongo_login <- function(email, password) {
  pw_hash <- hash_password(password)

  # Try MongoDB first
  result <- mongo_request("findOne", "users", list(
    filter = list(email = email, password_hash = pw_hash)
  ))

  doc <- NULL
  if (isTRUE(result$ok) && !is.null(result$document)) {
    doc <- result$document
  } else if (email %in% names(.local_users$data)) {
    # Offline fallback
    local_u <- .local_users$data[[email]]
    if (identical(local_u$password_hash, pw_hash)) doc <- local_u
  }

  if (is.null(doc)) {
    return(list(ok = FALSE, error = "Invalid email or password"))
  }

  inst <- NULL
  if (doc$user_type == "institutional_member" && !is.null(doc$institution)) {
    inst <- metasurvey::RecipeUser$new(name = doc$institution, user_type = "institution")
  }
  user <- metasurvey::RecipeUser$new(
    name = doc$name, user_type = doc$user_type, email = doc$email, institution = inst
  )
  list(ok = TRUE, user = user)
}

# --- Recipe operations ---

mongo_fetch_recipes <- function(filter = list()) {
  result <- mongo_request("find", "recipes", list(filter = filter))

  if (isTRUE(result$ok)) {
    docs <- result$documents %||% list()
    recipes <- lapply(docs, parse_recipe_doc)
    recipes <- Filter(Negate(is.null), recipes)
    if (length(recipes) > 0) return(recipes)
  }

  # Fallback: example recipes for local/demo mode
  example_recipes()
}

parse_recipe_doc <- function(doc) {
  categories <- list()
  if (!is.null(doc$categories)) {
    categories <- lapply(doc$categories, function(c) {
      tryCatch(metasurvey::RecipeCategory$from_list(c), error = function(e) NULL)
    })
    categories <- Filter(Negate(is.null), categories)
  }
  certification <- NULL
  if (!is.null(doc$certification)) {
    certification <- tryCatch(
      metasurvey::RecipeCertification$from_list(doc$certification),
      error = function(e) NULL
    )
  }
  user_info <- NULL
  if (!is.null(doc$user_info)) {
    user_info <- tryCatch(
      metasurvey::RecipeUser$from_list(doc$user_info),
      error = function(e) NULL
    )
  }
  cached_doc <- NULL
  if (!is.null(doc$doc)) {
    cached_doc <- list(
      input_variables = as.character(unlist(doc$doc$input_variables)),
      output_variables = as.character(unlist(doc$doc$output_variables)),
      pipeline = doc$doc$pipeline %||% list()
    )
  }
  tryCatch(
    metasurvey::Recipe$new(
      name = doc$name %||% "Unnamed",
      user = doc$user %||% "Unknown",
      edition = doc$svy_edition %||% doc$edition %||% "Unknown",
      survey_type = doc$svy_type %||% doc$survey_type %||% "Unknown",
      default_engine = "data.table",
      depends_on = doc$depends_on %||% list(),
      description = doc$description %||% "",
      steps = as.list(doc$steps %||% list()),
      id = doc[["_id"]] %||% doc$id %||% paste0("r_", sample.int(1e6, 1)),
      doi = doc$doi, topic = doc$topic,
      cached_doc = cached_doc, categories = categories,
      downloads = as.integer(doc$downloads %||% 0),
      certification = certification, user_info = user_info,
      version = doc$version %||% "1.0.0"
    ),
    error = function(e) NULL
  )
}

mongo_increment_downloads <- function(recipe_id) {
  # Try mongo, ignore failures
  tryCatch(
    mongo_request("updateOne", "recipes", list(
      filter = list("_id" = recipe_id),
      update = list("$inc" = list(downloads = 1L))
    )),
    error = function(e) NULL
  )
}

mongo_fetch_user_recipes <- function(user_email) {
  mongo_fetch_recipes(filter = list(user = user_email))
}

# ── Example recipes for local/demo mode ──

example_recipes <- function() {
  iecon <- metasurvey::RecipeUser$new(name = "Instituto de Economia", user_type = "institution", verified = TRUE)
  member_maria <- metasurvey::RecipeUser$new(name = "Maria Garcia", user_type = "institutional_member", email = "maria@iecon.edu.uy", institution = iecon)
  user_juan <- metasurvey::RecipeUser$new(name = "Juan Perez", user_type = "individual", email = "juan@example.com")
  user_ana <- metasurvey::RecipeUser$new(name = "Ana Rodriguez", user_type = "individual", email = "ana@fcea.edu.uy", affiliation = "UdelaR")

  labor <- metasurvey::RecipeCategory$new("labor_market", "Labor market indicators")
  income <- metasurvey::RecipeCategory$new("income", "Income distribution and poverty")
  education <- metasurvey::RecipeCategory$new("education", "Education attainment")
  health <- metasurvey::RecipeCategory$new("health", "Health outcomes")
  demographics <- metasurvey::RecipeCategory$new("demographics", "Population demographics")
  housing <- metasurvey::RecipeCategory$new("housing", "Housing conditions")

  list(
    # 1. Official - Labor Market
    metasurvey::Recipe$new(
      name = "Indicadores del Mercado Laboral",
      edition = "2023", survey_type = "ech",
      default_engine = "data.table", depends_on = list("POBPCOAC", "PE7", "PE7_1"),
      user = "maria@iecon.edu.uy",
      description = "Construye tasas de actividad, empleo y desempleo a partir de la ECH. Incluye clasificacion de condicion de actividad economica segun definiciones del INE Uruguay.",
      steps = list(), id = "example_001", doi = "10.5281/zenodo.12345", topic = "labor_market",
      categories = list(labor, income),
      downloads = 1847L,
      certification = metasurvey::RecipeCertification$new("official", certified_by = iecon),
      user_info = member_maria,
      version = "2.1.0",
      cached_doc = list(
        input_variables = c("POBPCOAC", "PE7", "PE7_1", "PT4", "pesoano"),
        output_variables = c("condicion_actividad", "tasa_actividad", "tasa_empleo", "tasa_desempleo"),
        pipeline = list(
          list(index = 1, type = "recode", outputs = "condicion_actividad", inputs = c("POBPCOAC"), inferred_type = "categorical", comment = "Clasificacion PEA/PEI"),
          list(index = 2, type = "compute", outputs = "horas_trabajadas", inputs = c("PE7", "PE7_1"), inferred_type = "numeric", comment = "Total horas semanales"),
          list(index = 3, type = "compute", outputs = c("tasa_actividad", "tasa_empleo", "tasa_desempleo"), inputs = c("condicion_actividad", "pesoano"), inferred_type = "numeric", comment = "Tasas principales")
        )
      )
    ),

    # 2. Official - Income
    metasurvey::Recipe$new(
      name = "Distribucion del Ingreso y Pobreza",
      edition = "2023", survey_type = "ech",
      default_engine = "data.table", depends_on = list("HT11", "HT13", "YSVL"),
      user = "maria@iecon.edu.uy",
      description = "Calcula ingreso per capita del hogar, deciles de ingreso y clasificacion de linea de pobreza e indigencia segun metodologia INE 2006.",
      steps = list(), id = "example_002", doi = "10.5281/zenodo.12346", topic = "income",
      categories = list(income),
      downloads = 2103L,
      certification = metasurvey::RecipeCertification$new("official", certified_by = iecon),
      user_info = member_maria,
      version = "3.0.1",
      cached_doc = list(
        input_variables = c("HT11", "HT13", "YSVL", "HT19", "pesoano", "nper"),
        output_variables = c("ingreso_pc", "decil_ingreso", "pobreza", "indigencia"),
        pipeline = list(
          list(index = 1, type = "compute", outputs = "ingreso_pc", inputs = c("HT11", "nper"), inferred_type = "numeric", comment = "Ingreso per capita del hogar"),
          list(index = 2, type = "compute", outputs = "decil_ingreso", inputs = c("ingreso_pc", "pesoano"), inferred_type = "numeric", comment = "Deciles ponderados"),
          list(index = 3, type = "recode", outputs = "pobreza", inputs = c("ingreso_pc"), inferred_type = "categorical", comment = "Linea de pobreza INE 2006"),
          list(index = 4, type = "recode", outputs = "indigencia", inputs = c("ingreso_pc"), inferred_type = "categorical", comment = "Linea de indigencia INE 2006")
        )
      )
    ),

    # 3. Reviewed - Education
    metasurvey::Recipe$new(
      name = "Nivel Educativo y Asistencia",
      edition = "2023", survey_type = "ech",
      default_engine = "data.table", depends_on = list("E27", "E197_1", "E51_7"),
      user = "ana@fcea.edu.uy",
      description = "Construye indicadores de nivel educativo alcanzado (primaria, secundaria, terciaria), anos de educacion y asistencia actual al sistema educativo.",
      steps = list(), id = "example_003", topic = "education",
      categories = list(education),
      downloads = 634L,
      certification = metasurvey::RecipeCertification$new("reviewed", certified_by = member_maria),
      user_info = user_ana,
      version = "1.2.0",
      cached_doc = list(
        input_variables = c("E27", "E197_1", "E51_7", "E51_8", "E49"),
        output_variables = c("nivel_educativo", "anos_educacion", "asiste_educacion"),
        pipeline = list(
          list(index = 1, type = "recode", outputs = "nivel_educativo", inputs = c("E27", "E197_1"), inferred_type = "categorical", comment = "Primaria/Secundaria/Terciaria/Posgrado"),
          list(index = 2, type = "compute", outputs = "anos_educacion", inputs = c("E27", "E51_7", "E51_8"), inferred_type = "numeric", comment = "Anos totales aprobados"),
          list(index = 3, type = "recode", outputs = "asiste_educacion", inputs = c("E49"), inferred_type = "categorical", comment = "Asiste actualmente SI/NO")
        )
      )
    ),

    # 4. Community - Health
    metasurvey::Recipe$new(
      name = "Cobertura de Salud",
      edition = "2023", survey_type = "ech",
      default_engine = "data.table", depends_on = list("S8", "S10"),
      user = "juan@example.com",
      description = "Indicadores de tipo de cobertura de salud (ASSE, mutualista, privado), y si tiene derechos vigentes en alguna institucion.",
      steps = list(), id = "example_004", topic = "health",
      categories = list(health),
      downloads = 312L,
      user_info = user_juan,
      version = "1.0.0",
      cached_doc = list(
        input_variables = c("S8", "S10", "S11"),
        output_variables = c("tipo_cobertura", "tiene_derechos"),
        pipeline = list(
          list(index = 1, type = "recode", outputs = "tipo_cobertura", inputs = c("S8", "S10"), inferred_type = "categorical", comment = "ASSE/Mutualista/Privado/Otro"),
          list(index = 2, type = "recode", outputs = "tiene_derechos", inputs = c("S11"), inferred_type = "categorical", comment = "Derechos vigentes SI/NO")
        )
      )
    ),

    # 5. Official - Demographics panel
    metasurvey::Recipe$new(
      name = "Estructura Demografica",
      edition = "2022-2023", survey_type = "ech",
      default_engine = "data.table", depends_on = list("E26", "E27"),
      user = "maria@iecon.edu.uy",
      description = "Variables demograficas basicas: grupos de edad, sexo, region, area geografica. Fundamental como insumo para cualquier analisis sociodemografico.",
      steps = list(), id = "example_005", topic = "demographics",
      categories = list(demographics),
      downloads = 3210L,
      certification = metasurvey::RecipeCertification$new("official", certified_by = iecon),
      user_info = member_maria,
      version = "4.0.0",
      cached_doc = list(
        input_variables = c("E26", "E27", "region_3", "AREA", "dpto"),
        output_variables = c("grupo_edad", "tramo_edad", "sexo_label", "region", "area"),
        pipeline = list(
          list(index = 1, type = "recode", outputs = "grupo_edad", inputs = c("E26"), inferred_type = "categorical", comment = "0-14/15-24/25-64/65+"),
          list(index = 2, type = "recode", outputs = "tramo_edad", inputs = c("E26"), inferred_type = "categorical", comment = "Quinquenales"),
          list(index = 3, type = "recode", outputs = "sexo_label", inputs = c("E27"), inferred_type = "categorical", comment = "Hombre/Mujer"),
          list(index = 4, type = "recode", outputs = "region", inputs = c("region_3"), inferred_type = "categorical", comment = "Montevideo/Interior/Rural"),
          list(index = 5, type = "recode", outputs = "area", inputs = c("AREA"), inferred_type = "categorical", comment = "Urbano/Rural")
        )
      )
    ),

    # 6. Community - Housing
    metasurvey::Recipe$new(
      name = "Condiciones de Vivienda",
      edition = "2023", survey_type = "ech",
      default_engine = "data.table", depends_on = list("D8", "D9"),
      user = "juan@example.com",
      description = "Caracteristicas de la vivienda: tipo (casa, apartamento, etc.), materiales predominantes, hacinamiento, acceso a servicios basicos (agua, saneamiento).",
      steps = list(), id = "example_006", topic = "housing",
      categories = list(housing),
      downloads = 189L,
      user_info = user_juan,
      version = "1.0.0",
      cached_doc = list(
        input_variables = c("D8", "D9", "D10", "D16", "D18", "nper", "D21"),
        output_variables = c("tipo_vivienda", "material_techo", "hacinamiento", "saneamiento"),
        pipeline = list(
          list(index = 1, type = "recode", outputs = "tipo_vivienda", inputs = c("D8"), inferred_type = "categorical", comment = "Casa/Apartamento/Otro"),
          list(index = 2, type = "recode", outputs = "material_techo", inputs = c("D10"), inferred_type = "categorical", comment = "Material predominante"),
          list(index = 3, type = "compute", outputs = "hacinamiento", inputs = c("D9", "nper"), inferred_type = "numeric", comment = "Personas por dormitorio"),
          list(index = 4, type = "recode", outputs = "saneamiento", inputs = c("D21"), inferred_type = "categorical", comment = "Red/Pozo/Otro")
        )
      )
    ),

    # 7. Reviewed - EAII
    metasurvey::Recipe$new(
      name = "Indicadores de Innovacion Empresarial",
      edition = "2019-2021", survey_type = "eaii",
      default_engine = "data.table", depends_on = list("p3_1", "p4_1"),
      user = "ana@fcea.edu.uy",
      description = "Procesamiento de la Encuesta de Actividades de Innovacion en la Industria. Construye indicadores de innovacion en producto, proceso, organizacion y comercializacion.",
      steps = list(), id = "example_007", topic = "labor_market",
      categories = list(labor),
      downloads = 456L,
      certification = metasurvey::RecipeCertification$new("reviewed", certified_by = member_maria),
      user_info = user_ana,
      version = "1.1.0",
      cached_doc = list(
        input_variables = c("p3_1", "p3_2", "p4_1", "p4_2", "p5_1", "tamano", "sector_ciiu"),
        output_variables = c("innova_producto", "innova_proceso", "innova_organizacion", "tipo_innovador"),
        pipeline = list(
          list(index = 1, type = "recode", outputs = "innova_producto", inputs = c("p3_1", "p3_2"), inferred_type = "categorical", comment = "Innovacion en producto SI/NO"),
          list(index = 2, type = "recode", outputs = "innova_proceso", inputs = c("p4_1", "p4_2"), inferred_type = "categorical", comment = "Innovacion en proceso SI/NO"),
          list(index = 3, type = "recode", outputs = "innova_organizacion", inputs = c("p5_1"), inferred_type = "categorical", comment = "Innovacion organizacional SI/NO"),
          list(index = 4, type = "recode", outputs = "tipo_innovador", inputs = c("innova_producto", "innova_proceso", "innova_organizacion"), inferred_type = "categorical", comment = "TPP/Amplio/No innovador")
        )
      )
    ),

    # 8. Community - Panel
    metasurvey::Recipe$new(
      name = "Transiciones Laborales (Panel)",
      edition = "2020-2023", survey_type = "ech",
      default_engine = "data.table", depends_on = list("POBPCOAC"),
      user = "juan@example.com",
      description = "Analisis de transiciones entre estados laborales (ocupado, desocupado, inactivo) usando datos de panel rotativo de la ECH.",
      steps = list(), id = "example_008", topic = "labor_market",
      categories = list(labor, demographics),
      downloads = 98L,
      user_info = user_juan,
      version = "0.9.0",
      cached_doc = list(
        input_variables = c("POBPCOAC", "POBPCOAC_t1", "E26", "pesoano"),
        output_variables = c("estado_t0", "estado_t1", "transicion"),
        pipeline = list(
          list(index = 1, type = "recode", outputs = "estado_t0", inputs = c("POBPCOAC"), inferred_type = "categorical", comment = "Estado laboral periodo 0"),
          list(index = 2, type = "recode", outputs = "estado_t1", inputs = c("POBPCOAC_t1"), inferred_type = "categorical", comment = "Estado laboral periodo 1"),
          list(index = 3, type = "compute", outputs = "transicion", inputs = c("estado_t0", "estado_t1"), inferred_type = "categorical", comment = "Matriz de transicion")
        )
      )
    )
  )
}

# ── Workflow operations ──

mongo_fetch_workflows <- function(filter = list()) {
  result <- mongo_request("find", "workflows", list(filter = filter))

  if (isTRUE(result$ok)) {
    docs <- result$documents %||% list()
    workflows <- lapply(docs, function(doc) {
      tryCatch(metasurvey::workflow_from_list(doc), error = function(e) NULL)
    })
    workflows <- Filter(Negate(is.null), workflows)
    if (length(workflows) > 0) return(workflows)
  }

  # Fallback: example workflows for local/demo mode
  example_workflows()
}

mongo_fetch_workflows_for_recipe <- function(recipe_id) {
  mongo_fetch_workflows(filter = list(recipe_ids = recipe_id))
}

mongo_increment_workflow_downloads <- function(workflow_id) {
  tryCatch(
    mongo_request("updateOne", "workflows", list(
      filter = list("_id" = workflow_id),
      update = list("$inc" = list(downloads = 1L))
    )),
    error = function(e) NULL
  )
}

# ── Example workflows for local/demo mode ──

example_workflows <- function() {
  iecon <- metasurvey::RecipeUser$new(name = "Instituto de Economia", user_type = "institution", verified = TRUE)
  member_maria <- metasurvey::RecipeUser$new(name = "Maria Garcia", user_type = "institutional_member", email = "maria@iecon.edu.uy", institution = iecon)
  user_juan <- metasurvey::RecipeUser$new(name = "Juan Perez", user_type = "individual", email = "juan@example.com")
  user_ana <- metasurvey::RecipeUser$new(name = "Ana Rodriguez", user_type = "individual", email = "ana@fcea.edu.uy", affiliation = "UdelaR")

  list(
    # 1. Labor Market Analysis
    metasurvey::RecipeWorkflow$new(
      id = "wf_001",
      name = "Analisis del Mercado Laboral",
      description = "Estimaciones principales del mercado de trabajo: tasas de actividad, empleo y desempleo, horas trabajadas y brechas de genero.",
      user = "maria@iecon.edu.uy",
      user_info = member_maria,
      survey_type = "ech",
      edition = "2023",
      estimation_type = c("annual", "quarterly"),
      recipe_ids = c("example_001", "example_005"),
      calls = list(
        "svymean(~tasa_actividad, design, na.rm = TRUE)",
        "svymean(~tasa_empleo, design, na.rm = TRUE)",
        "svymean(~tasa_desempleo, design, na.rm = TRUE)",
        "svymean(~horas_trabajadas, design, na.rm = TRUE)",
        "svyby(~tasa_empleo, ~sexo_label, design, svymean, na.rm = TRUE)"
      ),
      call_metadata = list(
        list(type = "svymean", formula = "~tasa_actividad", description = "Proporcion de PEA sobre PET"),
        list(type = "svymean", formula = "~tasa_empleo", description = "Proporcion de ocupados sobre PET"),
        list(type = "svymean", formula = "~tasa_desempleo", description = "Proporcion de desocupados sobre PEA"),
        list(type = "svymean", formula = "~horas_trabajadas", description = "Promedio de horas semanales"),
        list(type = "svyby", formula = "~tasa_empleo", by = "~sexo_label", description = "Tasa de empleo por sexo")
      ),
      downloads = 892L,
      certification = metasurvey::RecipeCertification$new("official", certified_by = iecon),
      version = "2.0.0"
    ),

    # 2. Income Distribution
    metasurvey::RecipeWorkflow$new(
      id = "wf_002",
      name = "Distribucion de Ingresos",
      description = "Estimaciones de ingreso per capita, pobreza e indigencia con desagregacion regional.",
      user = "maria@iecon.edu.uy",
      user_info = member_maria,
      survey_type = "ech",
      edition = "2023",
      estimation_type = "annual",
      recipe_ids = c("example_002"),
      calls = list(
        "svymean(~ingreso_pc, design, na.rm = TRUE)",
        "svytotal(~ingreso_pc, design, na.rm = TRUE)",
        "svymean(~pobreza, design, na.rm = TRUE)",
        "svymean(~indigencia, design, na.rm = TRUE)",
        "svyby(~ingreso_pc, ~region, design, svymean, na.rm = TRUE)"
      ),
      call_metadata = list(
        list(type = "svymean", formula = "~ingreso_pc", description = "Ingreso promedio per capita"),
        list(type = "svytotal", formula = "~ingreso_pc", description = "Masa total de ingresos"),
        list(type = "svymean", formula = "~pobreza", description = "Tasa de pobreza"),
        list(type = "svymean", formula = "~indigencia", description = "Tasa de indigencia"),
        list(type = "svyby", formula = "~ingreso_pc", by = "~region", description = "Ingreso por region")
      ),
      downloads = 1205L,
      certification = metasurvey::RecipeCertification$new("official", certified_by = iecon),
      version = "3.0.0"
    ),

    # 3. Education Profile
    metasurvey::RecipeWorkflow$new(
      id = "wf_003",
      name = "Perfil Educativo Nacional",
      description = "Nivel educativo, anos de educacion y tasas de asistencia con perspectiva demografica.",
      user = "ana@fcea.edu.uy",
      user_info = user_ana,
      survey_type = "ech",
      edition = "2023",
      estimation_type = "annual",
      recipe_ids = c("example_003", "example_005"),
      calls = list(
        "svymean(~anos_educacion, design, na.rm = TRUE)",
        "svymean(~nivel_educativo, design, na.rm = TRUE)",
        "svymean(~asiste_educacion, design, na.rm = TRUE)",
        "svyby(~anos_educacion, ~sexo_label, design, svymean, na.rm = TRUE)"
      ),
      call_metadata = list(
        list(type = "svymean", formula = "~anos_educacion", description = "Anos promedio de educacion"),
        list(type = "svymean", formula = "~nivel_educativo", description = "Distribucion por nivel educativo"),
        list(type = "svymean", formula = "~asiste_educacion", description = "Tasa de asistencia"),
        list(type = "svyby", formula = "~anos_educacion", by = "~sexo_label", description = "Educacion por sexo")
      ),
      downloads = 341L,
      certification = metasurvey::RecipeCertification$new("reviewed", certified_by = member_maria),
      version = "1.2.0"
    ),

    # 4. Health and Demographics Panorama
    metasurvey::RecipeWorkflow$new(
      id = "wf_004",
      name = "Panorama Demografico y Salud",
      description = "Estructura poblacional, cobertura de salud y derechos vigentes.",
      user = "juan@example.com",
      user_info = user_juan,
      survey_type = "ech",
      edition = "2023",
      estimation_type = "annual",
      recipe_ids = c("example_004", "example_005"),
      calls = list(
        "svymean(~tipo_cobertura, design, na.rm = TRUE)",
        "svymean(~tiene_derechos, design, na.rm = TRUE)",
        "svyby(~grupo_edad, ~sexo_label, design, svymean, na.rm = TRUE)",
        "svytotal(~1, design)"
      ),
      call_metadata = list(
        list(type = "svymean", formula = "~tipo_cobertura", description = "Distribucion por cobertura de salud"),
        list(type = "svymean", formula = "~tiene_derechos", description = "Tasa con derechos vigentes"),
        list(type = "svyby", formula = "~grupo_edad", by = "~sexo_label", description = "Piramide poblacional"),
        list(type = "svytotal", formula = "~1", description = "Poblacion total estimada")
      ),
      downloads = 178L,
      version = "1.0.0"
    ),

    # 5. Housing Conditions
    metasurvey::RecipeWorkflow$new(
      id = "wf_005",
      name = "Condiciones Habitacionales",
      description = "Hacinamiento, tipo de vivienda y saneamiento con desagregacion territorial.",
      user = "juan@example.com",
      user_info = user_juan,
      survey_type = "ech",
      edition = "2023",
      estimation_type = "annual",
      recipe_ids = c("example_006", "example_005"),
      calls = list(
        "svymean(~hacinamiento, design, na.rm = TRUE)",
        "svymean(~tipo_vivienda, design, na.rm = TRUE)",
        "svyby(~saneamiento, ~area, design, svymean, na.rm = TRUE)"
      ),
      call_metadata = list(
        list(type = "svymean", formula = "~hacinamiento", description = "Hacinamiento promedio"),
        list(type = "svymean", formula = "~tipo_vivienda", description = "Distribucion tipo de vivienda"),
        list(type = "svyby", formula = "~saneamiento", by = "~area", description = "Saneamiento por area")
      ),
      downloads = 95L,
      version = "1.0.0"
    ),

    # 6. Innovation and Transitions
    metasurvey::RecipeWorkflow$new(
      id = "wf_006",
      name = "Innovacion y Transiciones Laborales",
      description = "Indicadores de innovacion empresarial y analisis de transiciones entre estados laborales.",
      user = "ana@fcea.edu.uy",
      user_info = user_ana,
      survey_type = "eaii",
      edition = "2019-2021",
      estimation_type = "annual",
      recipe_ids = c("example_007", "example_008"),
      calls = list(
        "svymean(~innova_producto, design, na.rm = TRUE)",
        "svymean(~innova_proceso, design, na.rm = TRUE)",
        "svyby(~tipo_innovador, ~tamano, design, svymean, na.rm = TRUE)",
        "svyby(~estado_t1, ~estado_t0, design, svymean, na.rm = TRUE)"
      ),
      call_metadata = list(
        list(type = "svymean", formula = "~innova_producto", description = "Tasa de innovacion en producto"),
        list(type = "svymean", formula = "~innova_proceso", description = "Tasa de innovacion en proceso"),
        list(type = "svyby", formula = "~tipo_innovador", by = "~tamano", description = "Tipo innovador por tamano"),
        list(type = "svyby", formula = "~estado_t1", by = "~estado_t0", description = "Matriz de transicion laboral")
      ),
      downloads = 267L,
      certification = metasurvey::RecipeCertification$new("reviewed", certified_by = member_maria),
      version = "1.0.0"
    )
  )
}
