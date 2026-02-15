#!/usr/bin/env Rscript
# ══════════════════════════════════════════════════════════════════════════════
# seed_ech_recipes.R
# Seed MongoDB Atlas with recipes and workflows based on the {ech} package
# (https://github.com/calcita/ech)
#
# Usage:
#   Rscript inst/scripts/seed_ech_recipes.R
#
# Requires: metasurvey, jsonlite, httr, digest
# ══════════════════════════════════════════════════════════════════════════════

library(metasurvey)
library(jsonlite)
library(httr)
library(digest)

# ── Atlas Data API config ─────────────────────────────────────────────────────
ATLAS_BASE_URL <- Sys.getenv(
  "METASURVEY_ATLAS_URL",
  "https://data.mongodb-api.com/app/data-vonssxi/endpoint/data/v1/action/"
)
ATLAS_API_KEY <- Sys.getenv("METASURVEY_API_KEY", "")
DATABASE      <- "metasurvey"
CLUSTER       <- "Cluster0"

if (!nzchar(ATLAS_API_KEY)) {
  message("WARNING: METASURVEY_API_KEY not set. Will attempt anonymous auth.")
}

# ── API helpers ───────────────────────────────────────────────────────────────

get_token <- function() {
  url <- "https://services.cloud.mongodb.com/api/client/v2.0/app/data-vonssxi/auth/providers/anon-user/login"
  resp <- tryCatch(POST(url, timeout(10)), error = function(e) NULL)
  if (is.null(resp) || resp$status_code != 200) return(NULL)
  content(resp)$access_token
}

api_headers <- function() {
  if (nzchar(ATLAS_API_KEY)) {
    return(c(
      "Content-Type" = "application/json",
      "Access-Control-Request-Headers" = "*",
      "apiKey" = ATLAS_API_KEY
    ))
  }
  token <- get_token()
  if (is.null(token)) stop("Cannot authenticate with Atlas. Set METASURVEY_API_KEY.")
  c(
    "Content-Type" = "application/json",
    "Access-Control-Request-Headers" = "*",
    "Authorization" = paste("Bearer", token)
  )
}

api_request <- function(action, collection, body) {
  url <- paste0(ATLAS_BASE_URL, action)
  payload <- c(
    list(collection = collection, database = DATABASE, dataSource = CLUSTER),
    body
  )
  resp <- POST(
    url,
    body = toJSON(payload, auto_unbox = TRUE, null = "null"),
    add_headers(.headers = api_headers()),
    encode = "raw",
    timeout(15)
  )
  if (resp$status_code >= 400) {
    msg <- content(resp, "text", encoding = "UTF-8")
    stop(sprintf("[%d] %s → %s", resp$status_code, action, msg))
  }
  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  parsed
}

insert_one <- function(collection, doc) {
  api_request("insertOne", collection, list(document = doc))
}

insert_many <- function(collection, docs) {
  api_request("insertMany", collection, list(documents = docs))
}

delete_many <- function(collection, filter = list()) {
  api_request("deleteMany", collection, list(filter = filter))
}

# ── Authors ───────────────────────────────────────────────────────────────────
# {ech} package authors
author_gabriela <- RecipeUser$new(
  name = "Gabriela Mathieu",
  user_type = "individual",
  email = "calcita@gmx.li",
  url = "https://orcid.org/0000-0003-3965-9024"
)

author_richard <- RecipeUser$new(
  name = "Richard Detomasi",
  user_type = "individual",
  email = "richard.detomasi@gmail.com",
  url = "https://orcid.org/0000-0002-6725-0261"
)

ine_institution <- RecipeUser$new(
  name = "Instituto Nacional de Estadistica (INE)",
  user_type = "institution",
  verified = TRUE,
  url = "https://www.gub.uy/instituto-nacional-estadistica"
)

# Categories
labor    <- RecipeCategory$new("labor_market", "Labor market indicators")
income   <- RecipeCategory$new("income", "Income distribution and poverty")
education <- RecipeCategory$new("education", "Education attainment and enrollment")
housing  <- RecipeCategory$new("housing", "Housing conditions and deprivation")
demographics <- RecipeCategory$new("demographics", "Population demographics")
poverty_cat <- RecipeCategory$new("poverty", "Poverty and inequality measurement")

community_cert <- RecipeCertification$new(level = "community")

ech_pkg_ref <- "Based on {ech} R package (https://github.com/calcita/ech) by Mathieu & Detomasi. ECH - Encuesta Continua de Hogares, INE Uruguay."

# ══════════════════════════════════════════════════════════════════════════════
# RECIPES
# ══════════════════════════════════════════════════════════════════════════════

recipes <- list(

  # ── 1. Employment ─────────────────────────────────────────────────────────
  Recipe$new(
    name = "Indicadores de Empleo (ech::employment)",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("pobpcoac"),
    user = "calcita@gmx.li",
    description = paste(
      "Calcula indicadores de condicion de actividad economica:",
      "PEA (poblacion economicamente activa), PET (poblacion en edad de trabajar),",
      "PO (poblacion ocupada), PD (poblacion desocupada).",
      ech_pkg_ref
    ),
    steps = list(
      "step_recode(svy, pea = ech::employment(pobpcoac)$pea)",
      "step_recode(svy, pet = ech::employment(pobpcoac)$pet)",
      "step_recode(svy, po = ech::employment(pobpcoac)$po)",
      "step_recode(svy, pd = ech::employment(pobpcoac)$pd)"
    ),
    id = "ech_employment_001",
    topic = "labor_market",
    categories = list(labor),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("pobpcoac"),
      output_variables = c("pea", "pet", "po", "pd"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("pea"), inputs = list("pobpcoac"), inferred_type = "categorical", comment = "Poblacion economicamente activa (PEA)"),
        list(index = 2L, type = "recode", outputs = list("pet"), inputs = list("pobpcoac"), inferred_type = "categorical", comment = "Poblacion en edad de trabajar (PET)"),
        list(index = 3L, type = "recode", outputs = list("po"), inputs = list("pobpcoac"), inferred_type = "categorical", comment = "Poblacion ocupada"),
        list(index = 4L, type = "recode", outputs = list("pd"), inputs = list("pobpcoac"), inferred_type = "categorical", comment = "Poblacion desocupada")
      )
    )
  ),

  # ── 2. Subempleo ──────────────────────────────────────────────────────────
  Recipe$new(
    name = "Subempleo y Restricciones (ech::underemployment)",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("pobpcoac", "f85", "f98", "f101", "f102", "f103", "f104", "f82"),
    user = "calcita@gmx.li",
    description = paste(
      "Identifica subempleo (trabaja <40hs y desea trabajar mas) y restricciones",
      "al empleo basadas en aportes jubilatorios y subempleo.",
      ech_pkg_ref
    ),
    steps = list(
      "step_recode(svy, underemployment = ech::underemployment(...))",
      "step_recode(svy, employment_restrictions = ech::employment_restrictions(...))"
    ),
    id = "ech_underemployment_002",
    topic = "labor_market",
    categories = list(labor),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("pobpcoac", "f85", "f98", "f101", "f102", "f103", "f104", "f82"),
      output_variables = c("underemployment", "employment_restrictions"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("underemployment"), inputs = list("pobpcoac", "f85", "f98", "f101", "f102", "f103", "f104"), inferred_type = "categorical", comment = "Subempleado: trabaja <40hs, desea y puede trabajar mas"),
        list(index = 2L, type = "recode", outputs = list("employment_restrictions"), inputs = list("f82", "underemployment"), inferred_type = "categorical", comment = "Restricciones: aportes jubilatorios + subempleo")
      )
    )
  ),

  # ── 3. Rama de actividad CIIU ─────────────────────────────────────────────
  Recipe$new(
    name = "Rama de Actividad CIIU Rev.4 (ech::branch_ciiu)",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("f72_2"),
    user = "calcita@gmx.li",
    description = paste(
      "Clasifica la rama de actividad economica segun CIIU Rev.4 en 18 categorias,",
      "con agrupamientos opcionales.",
      ech_pkg_ref
    ),
    steps = list(
      "step_recode(svy, branch_ciiu = ech::branch_ciiu(f72_2))"
    ),
    id = "ech_branch_ciiu_003",
    topic = "labor_market",
    categories = list(labor),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("f72_2"),
      output_variables = c("branch_ciiu", "branch_group_ciiu"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("branch_ciiu", "branch_group_ciiu"), inputs = list("f72_2"), inferred_type = "categorical", comment = "18 ramas CIIU Rev.4 + agrupamiento")
      )
    )
  ),

  # ── 4. Ingreso a precios constantes ───────────────────────────────────────
  Recipe$new(
    name = "Ingreso a Precios Constantes (ech::income_constant_prices)",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("ht11", "ht13", "ht19", "mes", "anio", "dpto"),
    user = "calcita@gmx.li",
    description = paste(
      "Deflacta el ingreso del hogar usando IPC o IPAB. Calcula ingreso per capita",
      "a precios constantes, con y sin valor locativo.",
      ech_pkg_ref
    ),
    steps = list(
      "step_compute(svy, y_pc = ech::income_constant_prices(...)$y_pc)",
      "step_compute(svy, y_pc_d = ech::income_constant_prices(...)$y_pc_d)"
    ),
    id = "ech_income_prices_004",
    topic = "income",
    categories = list(income),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("ht11", "ht13", "ht19", "mes", "anio", "dpto"),
      output_variables = c("y_pc", "y_pc_d", "rv_d", "y_wrv_pc_d"),
      pipeline = list(
        list(index = 1L, type = "compute", outputs = list("y_pc"), inputs = list("ht11", "ht19"), inferred_type = "numeric", comment = "Ingreso per capita a precios corrientes"),
        list(index = 2L, type = "compute", outputs = list("y_pc_d"), inputs = list("y_pc", "mes", "anio", "dpto"), inferred_type = "numeric", comment = "Ingreso per capita deflactado (IPC/IPAB)"),
        list(index = 3L, type = "compute", outputs = list("rv_d"), inputs = list("ht13", "mes", "anio"), inferred_type = "numeric", comment = "Valor locativo deflactado"),
        list(index = 4L, type = "compute", outputs = list("y_wrv_pc_d"), inputs = list("y_pc_d", "rv_d", "ht19"), inferred_type = "numeric", comment = "Ingreso sin valor locativo per capita deflactado")
      )
    )
  ),

  # ── 5. Quintiles/Deciles de ingreso ───────────────────────────────────────
  Recipe$new(
    name = "Quintiles y Deciles de Ingreso (ech::income_quantiles)",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("y_pc_d", "pesoano"),
    user = "calcita@gmx.li",
    description = paste(
      "Asigna quintiles y deciles de ingreso ponderados por peso muestral.",
      ech_pkg_ref
    ),
    steps = list(
      "step_compute(svy, quintil = ech::income_quantiles(., quantile=5)$quintil)",
      "step_compute(svy, decil = ech::income_quantiles(., quantile=10)$decil)"
    ),
    id = "ech_quantiles_005",
    topic = "income",
    categories = list(income),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("y_pc_d", "pesoano"),
      output_variables = c("quintil", "decil"),
      pipeline = list(
        list(index = 1L, type = "compute", outputs = list("quintil"), inputs = list("y_pc_d", "pesoano"), inferred_type = "numeric", comment = "Quintil de ingreso ponderado"),
        list(index = 2L, type = "compute", outputs = list("decil"), inputs = list("y_pc_d", "pesoano"), inferred_type = "numeric", comment = "Decil de ingreso ponderado")
      )
    )
  ),

  # ── 6. Ingreso laboral ────────────────────────────────────────────────────
  Recipe$new(
    name = "Ingreso Laboral per Capita y por Hora (ech::labor_income)",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("numero", "pobpcoac", "g126_1", "g127_3", "g128_1", "g129_2",
                       "g130_1", "g131_1", "g133_1", "f85", "pt4", "mes", "anio", "dpto"),
    user = "richard.detomasi@gmail.com",
    description = paste(
      "Calcula ingreso laboral del trabajo principal, secundario y cuenta propia.",
      "Agrega a nivel hogar y per capita. Tambien calcula ingreso por hora deflactado.",
      ech_pkg_ref
    ),
    steps = list(
      "step_compute(svy, labor_income = ech::labor_income_per_capita(...)$labor_income)",
      "step_compute(svy, labor_income_h_percapita = ech::labor_income_per_capita(...)$labor_income_h_percapita)",
      "step_compute(svy, total_income_per_hour = ech::labor_income_per_hour(...)$total_income_per_hour)"
    ),
    id = "ech_labor_income_006",
    topic = "income",
    categories = list(income, labor),
    downloads = 0L,
    certification = community_cert,
    user_info = author_richard,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("numero", "pobpcoac", "g126_1", "g127_3", "g128_1", "g129_2",
                           "g130_1", "g131_1", "g133_1", "f85", "pt4", "mes", "anio", "dpto"),
      output_variables = c("labor_income", "labor_income_h_percapita", "total_income_per_hour"),
      pipeline = list(
        list(index = 1L, type = "compute", outputs = list("labor_income"), inputs = list("g126_1", "g127_3", "g128_1", "pobpcoac"), inferred_type = "numeric", comment = "Ingreso laboral individual (principal + secundario + cuenta propia)"),
        list(index = 2L, type = "compute", outputs = list("labor_income_h_percapita"), inputs = list("labor_income", "numero"), inferred_type = "numeric", comment = "Ingreso laboral del hogar per capita"),
        list(index = 3L, type = "compute", outputs = list("total_income_per_hour"), inputs = list("f85", "pt4", "mes", "anio", "dpto"), inferred_type = "numeric", comment = "Ingreso por hora deflactado")
      )
    )
  ),

  # ── 7. Pobreza e Indigencia ───────────────────────────────────────────────
  Recipe$new(
    name = "Pobreza e Indigencia (ech::poverty)",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("region_4", "dpto", "ht11", "ht19", "numero", "mes", "anio"),
    user = "calcita@gmx.li",
    description = paste(
      "Determina si el hogar es pobre o indigente comparando ingreso per capita",
      "con lineas de pobreza e indigencia regionalizadas (canasta INE).",
      ech_pkg_ref
    ),
    steps = list(
      "step_compute(svy, poverty_line = ech::poverty(...)$poverty_line)",
      "step_recode(svy, poor = ech::poverty(...)$poor)",
      "step_recode(svy, indigent = ech::poverty(...)$indigent)"
    ),
    id = "ech_poverty_007",
    topic = "income",
    categories = list(poverty_cat, income),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("region_4", "dpto", "ht11", "ht19", "numero", "mes", "anio"),
      output_variables = c("poverty_line", "indigency_line", "poor", "indigent"),
      pipeline = list(
        list(index = 1L, type = "compute", outputs = list("poverty_line", "indigency_line"), inputs = list("region_4", "dpto", "mes", "anio"), inferred_type = "numeric", comment = "Lineas de pobreza e indigencia (canasta basica regional)"),
        list(index = 2L, type = "recode", outputs = list("poor"), inputs = list("ht11", "ht19", "poverty_line"), inferred_type = "categorical", comment = "Hogar pobre: ingreso pc < linea pobreza"),
        list(index = 3L, type = "recode", outputs = list("indigent"), inputs = list("ht11", "ht19", "indigency_line"), inferred_type = "categorical", comment = "Hogar indigente: ingreso pc < linea indigencia")
      )
    )
  ),

  # ── 8. NBI ────────────────────────────────────────────────────────────────
  Recipe$new(
    name = "Necesidades Basicas Insatisfechas (ech::unsatisfied_basic_needs)",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("c2", "c3", "c4", "d9", "d11", "d12", "d13", "d14",
                       "d16", "d18", "d19", "d21_1", "d260", "ht19", "e27", "e238", "anio"),
    user = "calcita@gmx.li",
    description = paste(
      "Calcula indicadores de NBI: vivienda, agua, saneamiento, electricidad,",
      "confort y educacion. Clasifica hogares por cantidad de NBI.",
      ech_pkg_ref
    ),
    steps = list(
      "step_recode(svy, UBN_housing = ech::unsatisfied_basic_needs(...)$UBN_housing)",
      "step_recode(svy, UBN_q = ech::unsatisfied_basic_needs(...)$UBN_q)",
      "step_recode(svy, UBN = ech::unsatisfied_basic_needs(...)$UBN)"
    ),
    id = "ech_nbi_008",
    topic = "housing",
    categories = list(poverty_cat, housing),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("c2", "c3", "c4", "d9", "d11", "d12", "d13", "d14",
                           "d16", "d18", "d19", "d21_1", "d260", "ht19", "e27", "e238", "anio"),
      output_variables = c("UBN_housing", "UBN_water", "UBN_sewerage", "UBN_electricity",
                            "UBN_confort", "UBN_education", "UBN_q", "UBN"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("UBN_housing"), inputs = list("c2", "c3", "c4", "d9"), inferred_type = "categorical", comment = "NBI Vivienda: materiales y hacinamiento"),
        list(index = 2L, type = "recode", outputs = list("UBN_water", "UBN_sewerage", "UBN_electricity"), inputs = list("d11", "d12", "d13", "d14", "d16", "d18", "d19"), inferred_type = "categorical", comment = "NBI Servicios basicos: agua, saneamiento, electricidad"),
        list(index = 3L, type = "recode", outputs = list("UBN_confort"), inputs = list("d260"), inferred_type = "categorical", comment = "NBI Confort: bienes del hogar"),
        list(index = 4L, type = "recode", outputs = list("UBN_education"), inputs = list("e27", "e238", "anio"), inferred_type = "categorical", comment = "NBI Educacion: asistencia y rezago"),
        list(index = 5L, type = "compute", outputs = list("UBN_q"), inputs = list("UBN_housing", "UBN_water", "UBN_sewerage", "UBN_electricity", "UBN_confort", "UBN_education"), inferred_type = "numeric", comment = "Cantidad de NBI del hogar"),
        list(index = 6L, type = "recode", outputs = list("UBN"), inputs = list("UBN_q"), inferred_type = "categorical", comment = "Al menos 1 NBI: SI/NO")
      )
    )
  ),

  # ── 9. Pobreza integrada ──────────────────────────────────────────────────
  Recipe$new(
    name = "Medida Integrada de Pobreza (ech::integrated_poverty_measure)",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("pobre06", "UBN_q"),
    user = "calcita@gmx.li",
    description = paste(
      "Combina pobreza por ingresos y NBI en una medida integrada:",
      "pobreza cronica, reciente, inercial o no pobre.",
      ech_pkg_ref
    ),
    steps = list(
      "step_recode(svy, integrated_poverty_measure = ech::integrated_poverty_measure(pobre06, UBN_q))"
    ),
    id = "ech_integrated_poverty_009",
    topic = "income",
    categories = list(poverty_cat),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("pobre06", "UBN_q"),
      output_variables = c("integrated_poverty_measure"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("integrated_poverty_measure"), inputs = list("pobre06", "UBN_q"), inferred_type = "categorical", comment = "Cronica/Reciente/Inercial/No pobre")
      )
    )
  ),

  # ── 10. Educacion ─────────────────────────────────────────────────────────
  Recipe$new(
    name = "Indicadores Educativos (ech::education)",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("e27", "e49", "e51_2", "e51_3", "e51_4", "e51_5", "e51_6",
                       "e51_7", "e51_7_1", "e51_8", "e51_9", "e51_10", "e51_11",
                       "e193", "e197", "e197_1", "e201", "e212", "e215", "e218",
                       "e221", "e224", "anio"),
    user = "calcita@gmx.li",
    description = paste(
      "Calcula: asistencia escolar, anos de escolaridad, nivel educativo maximo",
      "alcanzado y completitud de ciclos (primaria, secundaria baja/alta, terciaria).",
      ech_pkg_ref
    ),
    steps = list(
      "step_recode(svy, school_enrollment = ech::enrolled_school(...))",
      "step_compute(svy, years_schooling = ech::years_of_schooling(...))",
      "step_recode(svy, level_education = ech::level_education(...))",
      "step_recode(svy, primary_completion = ech::level_completion(...)$primary)"
    ),
    id = "ech_education_010",
    topic = "education",
    categories = list(education),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("e27", "e49", "e51_2", "e51_3", "e51_4", "e51_5", "e51_6",
                           "e51_7", "e51_7_1", "e51_8", "e51_9", "e51_10", "e51_11",
                           "e193", "e197", "e197_1", "e201", "e212", "e215", "e218", "e221", "e224", "anio"),
      output_variables = c("school_enrollment", "years_schooling", "level_education",
                            "primary_completion", "lower_secondary_completion",
                            "upper_secondary_completion", "tertiary_completion"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("school_enrollment"), inputs = list("e27", "e193", "e197", "e201", "e212", "e215", "e218", "e221", "e224"), inferred_type = "categorical", comment = "Asiste a algun nivel educativo: SI/NO"),
        list(index = 2L, type = "compute", outputs = list("years_schooling"), inputs = list("e193", "e49", "e51_2", "e51_3", "e51_4", "e51_5", "e51_6", "e51_7", "e51_8", "anio"), inferred_type = "numeric", comment = "Anos de escolaridad formal acumulados"),
        list(index = 3L, type = "recode", outputs = list("level_education"), inputs = list("e49", "e51_2", "e51_3", "e51_4", "e51_7", "e193"), inferred_type = "categorical", comment = "Nivel: sin instruccion/primaria/secundaria/magisterio/universidad"),
        list(index = 4L, type = "recode", outputs = list("primary_completion", "lower_secondary_completion", "upper_secondary_completion", "tertiary_completion"), inputs = list("e197", "e197_1", "e201", "e51_4", "e51_7", "e51_8", "e51_9", "e212", "e215", "e218", "e221"), inferred_type = "categorical", comment = "Completitud de cada ciclo educativo")
      )
    )
  ),

  # ── 11. Vivienda ──────────────────────────────────────────────────────────
  Recipe$new(
    name = "Condiciones Habitacionales (ech::housing)",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("c2", "c3", "c4", "c5_1", "c5_2", "c5_3", "c5_4", "c5_5",
                       "c5_6", "c5_7", "c5_8", "c5_9", "c5_10", "c5_11", "c5_12",
                       "d8_1", "d9", "d10", "d11", "d12", "d13", "d16", "d18", "d19",
                       "ht19", "quintil", "region_4"),
    user = "richard.detomasi@gmail.com",
    description = paste(
      "Construye indicadores de condiciones de vivienda: tipo tenencia,",
      "situacion estructural, condiciones materiales, hacinamiento",
      "y privacion habitacional (10 indicadores).",
      ech_pkg_ref
    ),
    steps = list(
      "step_recode(svy, housing_tenure = ech::housing_tenure(d8_1))",
      "step_recode(svy, housing_situation = ech::housing_situation(...))",
      "step_recode(svy, housing_conditions = ech::housing_conditions(c2, c3, c4))",
      "step_compute(svy, overcrowding = ech::overcrowding(ht19, d10))",
      "step_recode(svy, housing_deprivation = ech::housing_deprivation(...))"
    ),
    id = "ech_housing_011",
    topic = "housing",
    categories = list(housing),
    downloads = 0L,
    certification = community_cert,
    user_info = author_richard,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("c2", "c3", "c4", "c5_1", "c5_2", "c5_3", "c5_4", "c5_5",
                           "c5_6", "c5_7", "c5_8", "c5_9", "c5_10", "c5_11", "c5_12",
                           "d8_1", "d9", "d10", "d11", "d12", "d13", "d16", "d18", "d19",
                           "ht19", "quintil", "region_4"),
      output_variables = c("housing_tenure", "housing_situation", "housing_conditions",
                            "overcrowding", "housing_deprivation"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("housing_tenure"), inputs = list("d8_1"), inferred_type = "categorical", comment = "Tipo tenencia: propietario/inquilino/ocupante/etc."),
        list(index = 2L, type = "recode", outputs = list("housing_situation"), inputs = list("c5_1", "c5_2", "c5_3", "c5_4", "c5_5", "c5_6"), inferred_type = "categorical", comment = "Situacion estructural: 4 categorias de defectos"),
        list(index = 3L, type = "recode", outputs = list("housing_conditions"), inputs = list("c2", "c3", "c4"), inferred_type = "categorical", comment = "Calidad materiales: paredes, techo, piso"),
        list(index = 4L, type = "compute", outputs = list("overcrowding"), inputs = list("ht19", "d10"), inferred_type = "numeric", comment = "Personas por dormitorio"),
        list(index = 5L, type = "recode", outputs = list("housing_deprivation"), inputs = list("overcrowding", "d9", "d11", "d12", "d16", "d18", "d19", "quintil", "region_4"), inferred_type = "categorical", comment = "Privacion habitacional: 10 indicadores compuestos")
      )
    )
  ),

  # ── 12. Tipo de hogar ─────────────────────────────────────────────────────
  Recipe$new(
    name = "Tipo de Hogar (ech::household_type)",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("numero", "e26", "e27", "e30"),
    user = "calcita@gmx.li",
    description = paste(
      "Clasifica hogares en 7 tipos: unipersonal, pareja sin hijos,",
      "monoparental (jefa/jefe), biparental, extendido, compuesto.",
      ech_pkg_ref
    ),
    steps = list(
      "step_recode(svy, household_type = ech::household_type(numero, e26, e27, e30))"
    ),
    id = "ech_household_type_012",
    topic = "demographics",
    categories = list(demographics),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("numero", "e26", "e27", "e30"),
      output_variables = c("household_type"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("household_type"), inputs = list("numero", "e26", "e27", "e30"), inferred_type = "categorical", comment = "7 tipos: unipersonal/pareja/monoparental/biparental/extendido/compuesto")
      )
    )
  ),

  # ── 13. Grupos de edad ────────────────────────────────────────────────────
  Recipe$new(
    name = "Grupos de Edad (ech::age_groups)",
    edition = "2023", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("e27"),
    user = "calcita@gmx.li",
    description = paste(
      "Crea grupos etarios configurables. Por defecto: 0-13, 14-17, 18-24,",
      "25-29, 30-44, 45-59, 60+.",
      ech_pkg_ref
    ),
    steps = list(
      "step_recode(svy, age_groups = ech::age_groups(., e27=e27))"
    ),
    id = "ech_age_groups_013",
    topic = "demographics",
    categories = list(demographics),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("e27"),
      output_variables = c("age_groups"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("age_groups"), inputs = list("e27"), inferred_type = "categorical", comment = "Grupos etarios: 0-13/14-17/18-24/25-29/30-44/45-59/60+")
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# WORKFLOWS
# ══════════════════════════════════════════════════════════════════════════════

workflows <- list(

  # ── WF1. Mercado Laboral ──────────────────────────────────────────────────
  RecipeWorkflow$new(
    id = "ech_wf_labor",
    name = "Mercado Laboral ECH (ech)",
    description = paste(
      "Estimaciones basicas del mercado de trabajo usando variables del paquete {ech}:",
      "tasas de actividad, empleo, desempleo, subempleo y brechas de genero."
    ),
    user = "calcita@gmx.li",
    user_info = author_gabriela,
    survey_type = "ech", edition = "2023",
    estimation_type = c("annual"),
    recipe_ids = c("ech_employment_001", "ech_underemployment_002", "ech_branch_ciiu_003", "ech_age_groups_013"),
    calls = list(
      "svymean(~pea, design, na.rm=TRUE)",
      "svymean(~po, design, na.rm=TRUE)",
      "svymean(~pd, design, na.rm=TRUE)",
      "svymean(~underemployment, design, na.rm=TRUE)",
      "svyby(~po, ~age_groups, design, svymean, na.rm=TRUE)",
      "svyby(~po, ~branch_ciiu, design, svymean, na.rm=TRUE)"
    ),
    call_metadata = list(
      list(type = "svymean", formula = "~pea", description = "Tasa de actividad"),
      list(type = "svymean", formula = "~po", description = "Tasa de empleo"),
      list(type = "svymean", formula = "~pd", description = "Tasa de desempleo"),
      list(type = "svymean", formula = "~underemployment", description = "Tasa de subempleo"),
      list(type = "svyby", formula = "~po", by = "~age_groups", description = "Empleo por grupo de edad"),
      list(type = "svyby", formula = "~po", by = "~branch_ciiu", description = "Empleo por rama CIIU")
    ),
    downloads = 0L,
    certification = community_cert,
    version = "1.0.0"
  ),

  # ── WF2. Pobreza y Desigualdad ────────────────────────────────────────────
  RecipeWorkflow$new(
    id = "ech_wf_poverty",
    name = "Pobreza y Desigualdad ECH (ech)",
    description = paste(
      "Estimaciones de pobreza, indigencia, NBI y distribucion del ingreso.",
      "Incluye Gini, QSR y medida integrada de pobreza."
    ),
    user = "calcita@gmx.li",
    user_info = author_gabriela,
    survey_type = "ech", edition = "2023",
    estimation_type = c("annual"),
    recipe_ids = c("ech_income_prices_004", "ech_quantiles_005", "ech_poverty_007",
                    "ech_nbi_008", "ech_integrated_poverty_009"),
    calls = list(
      "svymean(~poor, design, na.rm=TRUE)",
      "svymean(~indigent, design, na.rm=TRUE)",
      "svymean(~UBN, design, na.rm=TRUE)",
      "svymean(~y_pc_d, design, na.rm=TRUE)",
      "svyby(~poor, ~quintil, design, svymean, na.rm=TRUE)",
      "svymean(~integrated_poverty_measure, design, na.rm=TRUE)"
    ),
    call_metadata = list(
      list(type = "svymean", formula = "~poor", description = "Tasa de pobreza"),
      list(type = "svymean", formula = "~indigent", description = "Tasa de indigencia"),
      list(type = "svymean", formula = "~UBN", description = "Porcentaje hogares con al menos 1 NBI"),
      list(type = "svymean", formula = "~y_pc_d", description = "Ingreso per capita promedio (constante)"),
      list(type = "svyby", formula = "~poor", by = "~quintil", description = "Pobreza por quintil de ingreso"),
      list(type = "svymean", formula = "~integrated_poverty_measure", description = "Medida integrada: cronica/reciente/inercial")
    ),
    downloads = 0L,
    certification = community_cert,
    version = "1.0.0"
  ),

  # ── WF3. Educacion ────────────────────────────────────────────────────────
  RecipeWorkflow$new(
    id = "ech_wf_education",
    name = "Perfil Educativo ECH (ech)",
    description = paste(
      "Estimaciones de nivel educativo, anos de escolaridad, asistencia",
      "y completitud de ciclos, con desagregacion por sexo y quintil."
    ),
    user = "calcita@gmx.li",
    user_info = author_gabriela,
    survey_type = "ech", edition = "2023",
    estimation_type = c("annual"),
    recipe_ids = c("ech_education_010", "ech_quantiles_005", "ech_age_groups_013"),
    calls = list(
      "svymean(~years_schooling, design, na.rm=TRUE)",
      "svymean(~level_education, design, na.rm=TRUE)",
      "svymean(~school_enrollment, design, na.rm=TRUE)",
      "svyby(~years_schooling, ~quintil, design, svymean, na.rm=TRUE)",
      "svymean(~primary_completion, design, na.rm=TRUE)"
    ),
    call_metadata = list(
      list(type = "svymean", formula = "~years_schooling", description = "Anos de escolaridad promedio"),
      list(type = "svymean", formula = "~level_education", description = "Distribucion por nivel educativo"),
      list(type = "svymean", formula = "~school_enrollment", description = "Tasa de asistencia escolar"),
      list(type = "svyby", formula = "~years_schooling", by = "~quintil", description = "Escolaridad por quintil de ingreso"),
      list(type = "svymean", formula = "~primary_completion", description = "Tasa de completitud primaria")
    ),
    downloads = 0L,
    certification = community_cert,
    version = "1.0.0"
  ),

  # ── WF4. Vivienda ─────────────────────────────────────────────────────────
  RecipeWorkflow$new(
    id = "ech_wf_housing",
    name = "Condiciones Habitacionales ECH (ech)",
    description = paste(
      "Estimaciones de condiciones de vivienda: tenencia, hacinamiento,",
      "privacion habitacional y calidad de materiales."
    ),
    user = "richard.detomasi@gmail.com",
    user_info = author_richard,
    survey_type = "ech", edition = "2023",
    estimation_type = c("annual"),
    recipe_ids = c("ech_housing_011", "ech_quantiles_005"),
    calls = list(
      "svymean(~housing_tenure, design, na.rm=TRUE)",
      "svymean(~overcrowding, design, na.rm=TRUE)",
      "svymean(~housing_deprivation, design, na.rm=TRUE)",
      "svyby(~housing_deprivation, ~quintil, design, svymean, na.rm=TRUE)"
    ),
    call_metadata = list(
      list(type = "svymean", formula = "~housing_tenure", description = "Distribucion tipo tenencia"),
      list(type = "svymean", formula = "~overcrowding", description = "Hacinamiento promedio"),
      list(type = "svymean", formula = "~housing_deprivation", description = "Tasa de privacion habitacional"),
      list(type = "svyby", formula = "~housing_deprivation", by = "~quintil", description = "Privacion por quintil de ingreso")
    ),
    downloads = 0L,
    certification = community_cert,
    version = "1.0.0"
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# SEED USERS
# ══════════════════════════════════════════════════════════════════════════════

seed_users <- list(
  list(
    name = "Gabriela Mathieu",
    email = "calcita@gmx.li",
    password_hash = digest("ech_package_2024", algo = "sha256", serialize = FALSE),
    user_type = "individual",
    institution = NULL,
    url = "https://orcid.org/0000-0003-3965-9024",
    verified = FALSE,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  ),
  list(
    name = "Richard Detomasi",
    email = "richard.detomasi@gmail.com",
    password_hash = digest("ech_package_2024", algo = "sha256", serialize = FALSE),
    user_type = "individual",
    institution = NULL,
    url = "https://orcid.org/0000-0002-6725-0261",
    verified = FALSE,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# SERIALIZATION
# ══════════════════════════════════════════════════════════════════════════════

serialize_recipe <- function(r) {
  doc_info <- r$doc()
  list(
    id            = r$id,
    name          = r$name,
    user          = r$user,
    svy_type      = r$survey_type,
    edition       = r$edition,
    description   = r$description,
    topic         = r$topic,
    doi           = r$doi,
    version       = r$version,
    downloads     = r$downloads,
    steps         = r$steps,
    depends_on    = as.list(unlist(r$depends_on)),
    categories    = lapply(r$categories, function(c) c$to_list()),
    certification = r$certification$to_list(),
    user_info     = if (!is.null(r$user_info)) r$user_info$to_list() else NULL,
    doc = list(
      input_variables  = as.list(doc_info$input_variables),
      output_variables = as.list(doc_info$output_variables),
      pipeline         = doc_info$pipeline
    ),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  )
}

serialize_workflow <- function(wf) {
  wf$to_list()
}

# ══════════════════════════════════════════════════════════════════════════════
# MIGRATION
# ══════════════════════════════════════════════════════════════════════════════

cat("═══════════════════════════════════════════════════════════\n")
cat("  metasurvey MongoDB Atlas Seed — {ech} package recipes\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Dry-run mode: just export JSON
dry_run <- !nzchar(ATLAS_API_KEY) && identical(Sys.getenv("METASURVEY_SEED_MODE"), "")

if (dry_run) {
  cat("No ATLAS_API_KEY found. Running in DRY-RUN mode (JSON export only).\n\n")

  output_dir <- "inst/seed-data"
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Users
  writeLines(
    toJSON(seed_users, auto_unbox = TRUE, null = "null", pretty = TRUE),
    file.path(output_dir, "users.json")
  )
  cat(sprintf("  [users]     %d documents -> %s/users.json\n", length(seed_users), output_dir))

  # Recipes
  recipe_docs <- lapply(recipes, serialize_recipe)
  writeLines(
    toJSON(recipe_docs, auto_unbox = TRUE, null = "null", pretty = TRUE),
    file.path(output_dir, "recipes.json")
  )
  cat(sprintf("  [recipes]   %d documents -> %s/recipes.json\n", length(recipe_docs), output_dir))

  # Workflows
  wf_docs <- lapply(workflows, serialize_workflow)
  writeLines(
    toJSON(wf_docs, auto_unbox = TRUE, null = "null", pretty = TRUE),
    file.path(output_dir, "workflows.json")
  )
  cat(sprintf("  [workflows] %d documents -> %s/workflows.json\n", length(wf_docs), output_dir))

  cat("\nDry-run complete. To push to Atlas:\n")
  cat("  export METASURVEY_API_KEY='your-atlas-data-api-key'\n")
  cat("  Rscript inst/scripts/seed_ech_recipes.R\n")

} else {
  cat("Pushing to MongoDB Atlas...\n\n")

  # 1. Users
  cat("[users] Clearing existing seed users...\n")
  for (u in seed_users) {
    tryCatch(
      delete_many("users", list(email = u$email)),
      error = function(e) message("  skip delete: ", e$message)
    )
  }
  cat("[users] Inserting ", length(seed_users), " users...\n")
  for (u in seed_users) {
    tryCatch({
      insert_one("users", u)
      cat(sprintf("  + %s (%s)\n", u$name, u$email))
    }, error = function(e) message("  ERROR: ", e$message))
  }

  # 2. Recipes
  cat("\n[recipes] Clearing existing ech recipes...\n")
  for (r in recipes) {
    tryCatch(
      delete_many("recipes", list(id = r$id)),
      error = function(e) message("  skip delete: ", e$message)
    )
  }
  cat("[recipes] Inserting ", length(recipes), " recipes...\n")
  for (r in recipes) {
    doc <- serialize_recipe(r)
    tryCatch({
      insert_one("recipes", doc)
      cat(sprintf("  + %s [%s]\n", doc$name, doc$id))
    }, error = function(e) message("  ERROR: ", e$message))
  }

  # 3. Workflows
  cat("\n[workflows] Clearing existing ech workflows...\n")
  for (wf in workflows) {
    tryCatch(
      delete_many("workflows", list(id = wf$id)),
      error = function(e) message("  skip delete: ", e$message)
    )
  }
  cat("[workflows] Inserting ", length(workflows), " workflows...\n")
  for (wf in workflows) {
    doc <- serialize_workflow(wf)
    tryCatch({
      insert_one("workflows", doc)
      cat(sprintf("  + %s [%s]\n", doc$name, doc$id))
    }, error = function(e) message("  ERROR: ", e$message))
  }

  cat("\nSeed complete!\n")
}

cat("\n")
cat(sprintf("Summary: %d users, %d recipes, %d workflows\n",
            length(seed_users), length(recipes), length(workflows)))
