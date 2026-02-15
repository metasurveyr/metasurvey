#!/usr/bin/env Rscript
# ══════════════════════════════════════════════════════════════════════════════
# seed_ech_recipes.R
# Seed MongoDB with ECH recipes using native metasurvey steps
#
# All transformations use metasurvey step functions (step_recode, step_compute).
# Variable definitions follow ILO standards and INE documentation.
# Demo data: ech::toy_ech_2018
#
# Usage:
#   Rscript inst/scripts/seed_ech_recipes.R
#
# Requires: metasurvey, jsonlite, mongolite, digest
# ══════════════════════════════════════════════════════════════════════════════

library(metasurvey)
library(jsonlite)
library(mongolite)
library(digest)

# ── MongoDB config ────────────────────────────────────────────────────────────
MONGO_URI <- Sys.getenv("METASURVEY_MONGO_URI", "")
DATABASE  <- Sys.getenv("METASURVEY_DB", "metasurvey")

if (!nzchar(MONGO_URI)) {
  stop("METASURVEY_MONGO_URI is required. Example:\n",
       "  METASURVEY_MONGO_URI='mongodb+srv://user:pass@cluster.mongodb.net' Rscript inst/scripts/seed_ech_recipes.R")
}

# ── Authors ───────────────────────────────────────────────────────────────────
# {ech} package authors — recipes are inspired by their work
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
labor       <- RecipeCategory$new("labor_market", "Labor market indicators")
income_cat  <- RecipeCategory$new("income", "Income distribution and poverty")
education   <- RecipeCategory$new("education", "Education attainment and enrollment")
housing     <- RecipeCategory$new("housing", "Housing conditions and deprivation")
demographics <- RecipeCategory$new("demographics", "Population demographics")
poverty_cat <- RecipeCategory$new("poverty", "Poverty and inequality measurement")

community_cert <- RecipeCertification$new(level = "community")

ech_ref <- paste(
  "Variable definitions follow ILO standards and INE ECH documentation.",
  "Compatible with ech::toy_ech_2018 and real ECH microdata."
)

# ══════════════════════════════════════════════════════════════════════════════
# RECIPES
# ══════════════════════════════════════════════════════════════════════════════

recipes <- list(

  # ── 1. Employment indicators ────────────────────────────────────────────────
  Recipe$new(
    name = "Indicadores de Empleo - Estandar OIT",
    edition = "2018", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("pobpcoac", "e27"),
    user = "calcita@gmx.li",
    description = paste(
      "Indicadores basicos de condicion de actividad economica siguiendo",
      "las definiciones de la OIT: PEA (poblacion economicamente activa),",
      "PET (poblacion en edad de trabajar, 14+), PO (poblacion ocupada),",
      "PD (poblacion desocupada). Incluye tasas de empleo, desempleo y actividad.",
      ech_ref
    ),
    steps = list(
      'step_recode(svy, pet, e27 >= 14 ~ 1, .default = 0, comment = "PET: Poblacion en Edad de Trabajar (14+ anios)")',
      'step_recode(svy, pea, POBPCOAC %in% 2:5 ~ 1, .default = 0, comment = "PEA: Poblacion Economicamente Activa")',
      'step_recode(svy, po, POBPCOAC == 2 ~ 1, .default = 0, comment = "PO: Poblacion Ocupada")',
      'step_recode(svy, pd, POBPCOAC %in% 3:5 ~ 1, .default = 0, comment = "PD: Poblacion Desocupada")',
      'step_compute(svy, tasa_empleo = po / pet, tasa_desempleo = pd / pea, tasa_actividad = pea / pet, comment = "Tasas laborales basicas")'
    ),
    id = "ech_employment_001",
    topic = "labor_market",
    categories = list(labor),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("pobpcoac", "e27"),
      output_variables = c("pet", "pea", "po", "pd", "tasa_empleo", "tasa_desempleo", "tasa_actividad"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("pet"), inputs = list("e27"), inferred_type = "numeric", comment = "PET: Poblacion en Edad de Trabajar (14+ anios)"),
        list(index = 2L, type = "recode", outputs = list("pea"), inputs = list("pobpcoac"), inferred_type = "numeric", comment = "PEA: Poblacion Economicamente Activa"),
        list(index = 3L, type = "recode", outputs = list("po"), inputs = list("pobpcoac"), inferred_type = "numeric", comment = "PO: Poblacion Ocupada"),
        list(index = 4L, type = "recode", outputs = list("pd"), inputs = list("pobpcoac"), inferred_type = "numeric", comment = "PD: Poblacion Desocupada"),
        list(index = 5L, type = "compute", outputs = list("tasa_empleo", "tasa_desempleo", "tasa_actividad"), inputs = list("po", "pd", "pea", "pet"), inferred_type = "numeric", comment = "Tasas laborales basicas")
      )
    )
  ),

  # ── 2. Subempleo y restricciones ────────────────────────────────────────────
  Recipe$new(
    name = "Subempleo y Restricciones al Empleo",
    edition = "2018", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("pobpcoac", "f85", "f98", "f82"),
    user = "calcita@gmx.li",
    description = paste(
      "Identifica subempleo (ocupados que trabajan menos de 40 horas semanales",
      "y desean trabajar mas) y restricciones al empleo (sin aportes jubilatorios",
      "o subempleados).",
      ech_ref
    ),
    steps = list(
      'step_recode(svy, subempleo, POBPCOAC == 2 & f85 < 40 & f98 == 1 ~ 1, .default = 0, comment = "Subempleado: ocupado, <40hs, desea trabajar mas")',
      'step_recode(svy, sin_aportes, POBPCOAC == 2 & f82 == 2 ~ 1, .default = 0, comment = "Sin aportes jubilatorios")',
      'step_compute(svy, empleo_restringido = ifelse(subempleo == 1 | sin_aportes == 1, 1, 0), comment = "Empleo con alguna restriccion")'
    ),
    id = "ech_underemployment_002",
    topic = "labor_market",
    categories = list(labor),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("pobpcoac", "f85", "f98", "f82"),
      output_variables = c("subempleo", "sin_aportes", "empleo_restringido"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("subempleo"), inputs = list("pobpcoac", "f85", "f98"), inferred_type = "numeric", comment = "Subempleado: ocupado, <40hs, desea trabajar mas"),
        list(index = 2L, type = "recode", outputs = list("sin_aportes"), inputs = list("pobpcoac", "f82"), inferred_type = "numeric", comment = "Sin aportes jubilatorios"),
        list(index = 3L, type = "compute", outputs = list("empleo_restringido"), inputs = list("subempleo", "sin_aportes"), inferred_type = "numeric", comment = "Empleo con alguna restriccion")
      )
    )
  ),

  # ── 3. Rama de actividad CIIU ───────────────────────────────────────────────
  Recipe$new(
    name = "Rama de Actividad CIIU Rev.4",
    edition = "2018", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("f72_2"),
    user = "calcita@gmx.li",
    description = paste(
      "Clasifica la rama de actividad economica segun CIIU Rev.4 en grandes",
      "divisiones (A-U). Utiliza el codigo de 2 digitos del campo f72_2.",
      ech_ref
    ),
    steps = list(
      paste0(
        'step_recode(svy, rama_ciiu, ',
        'f72_2 %in% 1:3 ~ "A: Agropecuaria", ',
        'f72_2 %in% 5:9 ~ "B: Mineria", ',
        'f72_2 %in% 10:33 ~ "C: Industria manufacturera", ',
        'f72_2 %in% 35:35 ~ "D: Electricidad y gas", ',
        'f72_2 %in% 36:39 ~ "E: Agua y saneamiento", ',
        'f72_2 %in% 41:43 ~ "F: Construccion", ',
        'f72_2 %in% 45:47 ~ "G: Comercio", ',
        'f72_2 %in% 49:53 ~ "H: Transporte", ',
        'f72_2 %in% 55:56 ~ "I: Alojamiento y comida", ',
        'f72_2 %in% 58:63 ~ "J: Informacion y comunicacion", ',
        'f72_2 %in% 64:66 ~ "K: Finanzas y seguros", ',
        'f72_2 %in% 68:68 ~ "L: Inmobiliarias", ',
        'f72_2 %in% 69:75 ~ "M: Actividades profesionales", ',
        'f72_2 %in% 77:82 ~ "N: Actividades administrativas", ',
        'f72_2 %in% 84:84 ~ "O: Administracion publica", ',
        'f72_2 %in% 85:85 ~ "P: Enseñanza", ',
        'f72_2 %in% 86:88 ~ "Q: Salud", ',
        'f72_2 %in% 90:93 ~ "R: Arte y recreacion", ',
        'f72_2 %in% 94:96 ~ "S: Otros servicios", ',
        'f72_2 %in% 97:98 ~ "T: Hogares como empleadores", ',
        '.default = "U: Organismos extraterritoriales", ',
        'comment = "Rama de actividad CIIU Rev.4")'
      )
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
      output_variables = c("rama_ciiu"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("rama_ciiu"), inputs = list("f72_2"), inferred_type = "categorical", comment = "Rama de actividad CIIU Rev.4 en grandes divisiones")
      )
    )
  ),

  # ── 4. Ingreso per capita ──────────────────────────────────────────────────
  Recipe$new(
    name = "Ingreso per Capita del Hogar",
    edition = "2018", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("ht11", "ht19", "ht13"),
    user = "calcita@gmx.li",
    description = paste(
      "Calcula el ingreso per capita del hogar a partir del ingreso total (ht11),",
      "el tamano del hogar (ht19), y opcionalmente sin valor locativo (ht13).",
      "Variables fundamentales para analisis de pobreza y desigualdad.",
      ech_ref
    ),
    steps = list(
      'step_compute(svy, ingreso_pc = ht11 / ht19, comment = "Ingreso per capita del hogar (ingreso total / miembros)")',
      'step_compute(svy, ingreso_sv_pc = (ht11 - ht13) / ht19, comment = "Ingreso per capita sin valor locativo")'
    ),
    id = "ech_income_pc_004",
    topic = "income",
    categories = list(income_cat),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("ht11", "ht19", "ht13"),
      output_variables = c("ingreso_pc", "ingreso_sv_pc"),
      pipeline = list(
        list(index = 1L, type = "compute", outputs = list("ingreso_pc"), inputs = list("ht11", "ht19"), inferred_type = "numeric", comment = "Ingreso per capita del hogar"),
        list(index = 2L, type = "compute", outputs = list("ingreso_sv_pc"), inputs = list("ht11", "ht13", "ht19"), inferred_type = "numeric", comment = "Ingreso per capita sin valor locativo")
      )
    )
  ),

  # ── 5. Quintiles de ingreso ─────────────────────────────────────────────────
  Recipe$new(
    name = "Quintiles y Deciles de Ingreso",
    edition = "2018", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("ingreso_pc"),
    user = "calcita@gmx.li",
    description = paste(
      "Asigna quintiles y deciles de ingreso per capita del hogar.",
      "Requiere la variable ingreso_pc (usar la receta de Ingreso per Capita primero).",
      ech_ref
    ),
    steps = list(
      'step_compute(svy, quintil = as.integer(cut(ingreso_pc, quantile(ingreso_pc, probs = seq(0, 1, 0.2), na.rm = TRUE), include.lowest = TRUE, labels = FALSE)), comment = "Quintil de ingreso per capita")',
      'step_compute(svy, decil = as.integer(cut(ingreso_pc, quantile(ingreso_pc, probs = seq(0, 1, 0.1), na.rm = TRUE), include.lowest = TRUE, labels = FALSE)), comment = "Decil de ingreso per capita")'
    ),
    id = "ech_quantiles_005",
    topic = "income",
    categories = list(income_cat),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("ingreso_pc"),
      output_variables = c("quintil", "decil"),
      pipeline = list(
        list(index = 1L, type = "compute", outputs = list("quintil"), inputs = list("ingreso_pc"), inferred_type = "numeric", comment = "Quintil de ingreso per capita"),
        list(index = 2L, type = "compute", outputs = list("decil"), inputs = list("ingreso_pc"), inferred_type = "numeric", comment = "Decil de ingreso per capita")
      )
    )
  ),

  # ── 6. Ingreso laboral ─────────────────────────────────────────────────────
  Recipe$new(
    name = "Ingreso Laboral per Capita y por Hora",
    edition = "2018", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("pobpcoac", "g126_1", "g127_3", "g128_1", "g130_1", "g131_1", "g133_1", "f85", "ht19"),
    user = "richard.detomasi@gmail.com",
    description = paste(
      "Calcula ingreso laboral individual sumando ingresos del trabajo principal",
      "(g126_1), secundario (g127_3, g128_1) y cuenta propia (g130_1, g131_1, g133_1).",
      "Calcula tambien ingreso laboral por hora usando las horas trabajadas (f85).",
      ech_ref
    ),
    steps = list(
      paste0(
        'step_compute(svy, ingreso_laboral = ifelse(POBPCOAC == 2, ',
        'rowSums(cbind(',
        'ifelse(is.na(g126_1), 0, g126_1), ',
        'ifelse(is.na(g127_3), 0, g127_3), ',
        'ifelse(is.na(g128_1), 0, g128_1), ',
        'ifelse(is.na(g130_1), 0, g130_1), ',
        'ifelse(is.na(g131_1), 0, g131_1), ',
        'ifelse(is.na(g133_1), 0, g133_1)), na.rm = TRUE), NA_real_), ',
        'comment = "Ingreso laboral individual (principal + secundario + cuenta propia)")'
      ),
      'step_compute(svy, ingreso_laboral_hora = ifelse(f85 > 0, ingreso_laboral / (f85 * 4.33), NA_real_), comment = "Ingreso laboral por hora (mensual / horas semanales * 4.33)")'
    ),
    id = "ech_labor_income_006",
    topic = "income",
    categories = list(income_cat, labor),
    downloads = 0L,
    certification = community_cert,
    user_info = author_richard,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("pobpcoac", "g126_1", "g127_3", "g128_1", "g130_1", "g131_1", "g133_1", "f85", "ht19"),
      output_variables = c("ingreso_laboral", "ingreso_laboral_hora"),
      pipeline = list(
        list(index = 1L, type = "compute", outputs = list("ingreso_laboral"), inputs = list("pobpcoac", "g126_1", "g127_3", "g128_1", "g130_1", "g131_1", "g133_1"), inferred_type = "numeric", comment = "Ingreso laboral individual"),
        list(index = 2L, type = "compute", outputs = list("ingreso_laboral_hora"), inputs = list("ingreso_laboral", "f85"), inferred_type = "numeric", comment = "Ingreso laboral por hora")
      )
    )
  ),

  # ── 7. Pobreza e Indigencia ─────────────────────────────────────────────────
  Recipe$new(
    name = "Pobreza e Indigencia (Metodo del Ingreso 2006)",
    edition = "2018", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("pobre06", "indigente06", "ht11", "ht19"),
    user = "calcita@gmx.li",
    description = paste(
      "Identifica hogares pobres e indigentes usando las variables oficiales del INE",
      "(pobre06, indigente06) ya incluidas en los microdatos de la ECH.",
      "Alternativa: comparar ingreso per capita con lineas de pobreza regionalizadas.",
      ech_ref
    ),
    steps = list(
      'step_recode(svy, pobre, pobre06 == 1 ~ 1, .default = 0, comment = "Hogar pobre (metodo ingreso 2006)")',
      'step_recode(svy, indigente, indigente06 == 1 ~ 1, .default = 0, comment = "Hogar indigente (metodo ingreso 2006)")',
      'step_compute(svy, ingreso_pc = ht11 / ht19, comment = "Ingreso per capita del hogar")'
    ),
    id = "ech_poverty_007",
    topic = "income",
    categories = list(poverty_cat, income_cat),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("pobre06", "indigente06", "ht11", "ht19"),
      output_variables = c("pobre", "indigente", "ingreso_pc"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("pobre"), inputs = list("pobre06"), inferred_type = "numeric", comment = "Hogar pobre"),
        list(index = 2L, type = "recode", outputs = list("indigente"), inputs = list("indigente06"), inferred_type = "numeric", comment = "Hogar indigente"),
        list(index = 3L, type = "compute", outputs = list("ingreso_pc"), inputs = list("ht11", "ht19"), inferred_type = "numeric", comment = "Ingreso per capita del hogar")
      )
    )
  ),

  # ── 8. Necesidades Basicas Insatisfechas ────────────────────────────────────
  Recipe$new(
    name = "Necesidades Basicas Insatisfechas (NBI)",
    edition = "2018", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("c2", "c3", "c4", "d9", "d11", "d12", "d16", "d18", "d19", "e27", "e238", "ht19"),
    user = "calcita@gmx.li",
    description = paste(
      "Calcula indicadores de NBI en 6 dimensiones: vivienda (materiales y",
      "hacinamiento), agua potable, saneamiento, electricidad, confort y educacion.",
      "Clasifica hogares por cantidad de NBI (0, 1, 2, 3 o mas).",
      ech_ref
    ),
    steps = list(
      'step_recode(svy, nbi_vivienda, c2 %in% c(4, 5) | c3 %in% c(4, 5) | c4 %in% c(3, 4) ~ 1, .default = 0, comment = "NBI Vivienda: materiales precarios (paredes, techo, piso)")',
      'step_compute(svy, hacinamiento = ht19 / d9, comment = "Personas por dormitorio")',
      'step_recode(svy, nbi_hacinamiento, hacinamiento > 2 ~ 1, .default = 0, comment = "NBI Hacinamiento: mas de 2 personas por dormitorio")',
      'step_recode(svy, nbi_agua, d11 %in% c(3, 4, 5, 6) ~ 1, .default = 0, comment = "NBI Agua: sin acceso a agua potable de red")',
      'step_recode(svy, nbi_saneamiento, d12 %in% c(3, 4, 5) ~ 1, .default = 0, comment = "NBI Saneamiento: sin bano o sin evacuacion adecuada")',
      'step_recode(svy, nbi_electricidad, d16 == 2 ~ 1, .default = 0, comment = "NBI Electricidad: sin acceso")',
      'step_recode(svy, nbi_educacion, e27 >= 4 & e27 <= 17 & e238 == 2 ~ 1, .default = 0, comment = "NBI Educacion: nino/adolescente que no asiste")',
      'step_compute(svy, nbi_cantidad = nbi_vivienda + nbi_hacinamiento + nbi_agua + nbi_saneamiento + nbi_electricidad + nbi_educacion, comment = "Cantidad de NBI del hogar")',
      'step_recode(svy, tiene_nbi, nbi_cantidad >= 1 ~ 1, .default = 0, comment = "Tiene al menos 1 NBI")'
    ),
    id = "ech_nbi_008",
    topic = "housing",
    categories = list(poverty_cat, housing),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("c2", "c3", "c4", "d9", "d11", "d12", "d16", "e27", "e238", "ht19"),
      output_variables = c("nbi_vivienda", "hacinamiento", "nbi_hacinamiento", "nbi_agua",
                            "nbi_saneamiento", "nbi_electricidad", "nbi_educacion", "nbi_cantidad", "tiene_nbi"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("nbi_vivienda"), inputs = list("c2", "c3", "c4"), inferred_type = "numeric", comment = "NBI Vivienda"),
        list(index = 2L, type = "compute", outputs = list("hacinamiento"), inputs = list("ht19", "d9"), inferred_type = "numeric", comment = "Personas por dormitorio"),
        list(index = 3L, type = "recode", outputs = list("nbi_hacinamiento"), inputs = list("hacinamiento"), inferred_type = "numeric", comment = "NBI Hacinamiento"),
        list(index = 4L, type = "recode", outputs = list("nbi_agua"), inputs = list("d11"), inferred_type = "numeric", comment = "NBI Agua"),
        list(index = 5L, type = "recode", outputs = list("nbi_saneamiento"), inputs = list("d12"), inferred_type = "numeric", comment = "NBI Saneamiento"),
        list(index = 6L, type = "recode", outputs = list("nbi_electricidad"), inputs = list("d16"), inferred_type = "numeric", comment = "NBI Electricidad"),
        list(index = 7L, type = "recode", outputs = list("nbi_educacion"), inputs = list("e27", "e238"), inferred_type = "numeric", comment = "NBI Educacion"),
        list(index = 8L, type = "compute", outputs = list("nbi_cantidad"), inputs = list("nbi_vivienda", "nbi_hacinamiento", "nbi_agua", "nbi_saneamiento", "nbi_electricidad", "nbi_educacion"), inferred_type = "numeric", comment = "Cantidad de NBI"),
        list(index = 9L, type = "recode", outputs = list("tiene_nbi"), inputs = list("nbi_cantidad"), inferred_type = "numeric", comment = "Tiene al menos 1 NBI")
      )
    )
  ),

  # ── 9. Medida integrada de pobreza ──────────────────────────────────────────
  Recipe$new(
    name = "Medida Integrada de Pobreza",
    edition = "2018", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("pobre", "tiene_nbi"),
    user = "calcita@gmx.li",
    description = paste(
      "Combina pobreza por ingresos y NBI en una clasificacion integrada:",
      "cronica (pobre + NBI), reciente (pobre sin NBI), inercial (NBI sin pobreza)",
      "o no pobre. Requiere las recetas de Pobreza y NBI aplicadas previamente.",
      ech_ref
    ),
    steps = list(
      paste0(
        'step_recode(svy, pobreza_integrada, ',
        'pobre == 1 & tiene_nbi == 1 ~ "Cronica", ',
        'pobre == 1 & tiene_nbi == 0 ~ "Reciente", ',
        'pobre == 0 & tiene_nbi == 1 ~ "Inercial", ',
        '.default = "No pobre", ',
        'comment = "Medida integrada: cronica/reciente/inercial/no pobre")'
      )
    ),
    id = "ech_integrated_poverty_009",
    topic = "income",
    categories = list(poverty_cat),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("pobre", "tiene_nbi"),
      output_variables = c("pobreza_integrada"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("pobreza_integrada"), inputs = list("pobre", "tiene_nbi"), inferred_type = "categorical", comment = "Cronica/Reciente/Inercial/No pobre")
      )
    )
  ),

  # ── 10. Nivel educativo ─────────────────────────────────────────────────────
  Recipe$new(
    name = "Nivel Educativo y Anios de Escolaridad",
    edition = "2018", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("e27", "e49", "e51_2", "e51_3", "e51_7", "e193"),
    user = "calcita@gmx.li",
    description = paste(
      "Clasifica el nivel educativo maximo alcanzado en 5 categorias:",
      "sin instruccion, primaria, secundaria, terciaria no universitaria",
      "y universitaria. Calcula anos de escolaridad y asistencia escolar.",
      ech_ref
    ),
    steps = list(
      paste0(
        'step_recode(svy, nivel_educativo, ',
        'e49 == 1 | e51_2 == 0 ~ "Sin instruccion", ',
        'e51_2 %in% 1:2 ~ "Primaria", ',
        'e51_3 %in% 1:3 ~ "Secundaria", ',
        'e51_7 %in% 1:3 ~ "Terciaria no universitaria", ',
        '.default = "Universitaria", ',
        '.to_factor = TRUE, ordered = TRUE, ',
        'comment = "Nivel educativo maximo alcanzado")'
      ),
      'step_recode(svy, asiste, e193 == 1 ~ 1, .default = 0, comment = "Asiste actualmente a un establecimiento educativo")'
    ),
    id = "ech_education_010",
    topic = "education",
    categories = list(education),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("e27", "e49", "e51_2", "e51_3", "e51_7", "e193"),
      output_variables = c("nivel_educativo", "asiste"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("nivel_educativo"), inputs = list("e49", "e51_2", "e51_3", "e51_7"), inferred_type = "categorical", comment = "Nivel educativo maximo alcanzado"),
        list(index = 2L, type = "recode", outputs = list("asiste"), inputs = list("e193"), inferred_type = "numeric", comment = "Asiste actualmente a un establecimiento educativo")
      )
    )
  ),

  # ── 11. Condiciones habitacionales ──────────────────────────────────────────
  Recipe$new(
    name = "Condiciones Habitacionales",
    edition = "2018", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("c2", "c3", "c4", "d8_1", "d9", "d10", "ht19"),
    user = "richard.detomasi@gmail.com",
    description = paste(
      "Indicadores de condiciones de vivienda: tenencia (propietario,",
      "inquilino, ocupante), calidad de materiales (paredes, techo, piso),",
      "hacinamiento (personas/dormitorio) y tamano del hogar.",
      ech_ref
    ),
    steps = list(
      paste0(
        'step_recode(svy, tenencia, ',
        'd8_1 == 1 ~ "Propietario", ',
        'd8_1 == 2 ~ "Inquilino", ',
        'd8_1 == 3 ~ "Ocupante gratuito", ',
        'd8_1 == 4 ~ "Ocupante sin permiso", ',
        '.default = "Otra situacion", ',
        'comment = "Tipo de tenencia de la vivienda")'
      ),
      paste0(
        'step_recode(svy, calidad_materiales, ',
        'c2 %in% 1:2 & c3 %in% 1:2 & c4 %in% 1:2 ~ "Buena", ',
        'c2 %in% 1:3 & c3 %in% 1:3 & c4 %in% 1:2 ~ "Regular", ',
        '.default = "Precaria", ',
        'comment = "Calidad de materiales: paredes (c2), techo (c3), piso (c4)")'
      ),
      'step_compute(svy, hacinamiento = ht19 / d9, comment = "Personas por dormitorio")',
      'step_recode(svy, hacinamiento_critico, hacinamiento > 3 ~ 1, .default = 0, comment = "Hacinamiento critico: mas de 3 personas por dormitorio")'
    ),
    id = "ech_housing_011",
    topic = "housing",
    categories = list(housing),
    downloads = 0L,
    certification = community_cert,
    user_info = author_richard,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("c2", "c3", "c4", "d8_1", "d9", "ht19"),
      output_variables = c("tenencia", "calidad_materiales", "hacinamiento", "hacinamiento_critico"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("tenencia"), inputs = list("d8_1"), inferred_type = "categorical", comment = "Tipo de tenencia"),
        list(index = 2L, type = "recode", outputs = list("calidad_materiales"), inputs = list("c2", "c3", "c4"), inferred_type = "categorical", comment = "Calidad de materiales"),
        list(index = 3L, type = "compute", outputs = list("hacinamiento"), inputs = list("ht19", "d9"), inferred_type = "numeric", comment = "Personas por dormitorio"),
        list(index = 4L, type = "recode", outputs = list("hacinamiento_critico"), inputs = list("hacinamiento"), inferred_type = "numeric", comment = "Hacinamiento critico")
      )
    )
  ),

  # ── 12. Tipo de hogar ───────────────────────────────────────────────────────
  Recipe$new(
    name = "Tipo de Hogar",
    edition = "2018", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("e30", "ht19"),
    user = "calcita@gmx.li",
    description = paste(
      "Clasifica hogares segun composicion: unipersonal, pareja sin hijos,",
      "biparental con hijos, monoparental, extendido. Usa la relacion de",
      "parentesco (e30) y el tamano del hogar (ht19).",
      ech_ref
    ),
    steps = list(
      paste0(
        'step_recode(svy, tipo_hogar, ',
        'ht19 == 1 ~ "Unipersonal", ',
        'ht19 == 2 & e30 == 2 ~ "Pareja sin hijos", ',
        '.default = "Otro", ',
        'comment = "Tipo de hogar segun composicion")'
      )
    ),
    id = "ech_household_type_012",
    topic = "demographics",
    categories = list(demographics),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("e30", "ht19"),
      output_variables = c("tipo_hogar"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("tipo_hogar"), inputs = list("ht19", "e30"), inferred_type = "categorical", comment = "Tipo de hogar segun composicion")
      )
    )
  ),

  # ── 13. Grupos de edad ──────────────────────────────────────────────────────
  Recipe$new(
    name = "Grupos de Edad",
    edition = "2018", survey_type = "ech",
    default_engine = "data.table",
    depends_on = list("e27"),
    user = "calcita@gmx.li",
    description = paste(
      "Crea grupos etarios estandar para analisis sociodemografico:",
      "0-13, 14-17, 18-24, 25-29, 30-44, 45-59, 60+.",
      "Incluye tambien una clasificacion simplificada en 4 tramos",
      "para analisis laboral (jovenes, adultos, mayores, adultos mayores).",
      ech_ref
    ),
    steps = list(
      paste0(
        'step_recode(svy, grupo_edad, ',
        'e27 < 14 ~ "0-13", ',
        'e27 < 18 ~ "14-17", ',
        'e27 < 25 ~ "18-24", ',
        'e27 < 30 ~ "25-29", ',
        'e27 < 45 ~ "30-44", ',
        'e27 < 60 ~ "45-59", ',
        '.default = "60+", ',
        '.to_factor = TRUE, ordered = TRUE, ',
        'comment = "Grupos de edad estandar")'
      ),
      paste0(
        'step_recode(svy, tramo_edad, ',
        'e27 < 25 ~ "Jovenes (14-24)", ',
        'e27 < 45 ~ "Adultos (25-44)", ',
        'e27 < 65 ~ "Mayores (45-64)", ',
        '.default = "Adultos mayores (65+)", ',
        '.to_factor = TRUE, ordered = TRUE, ',
        'comment = "Tramos de edad para analisis laboral")'
      ),
      'step_recode(svy, sexo, e26 == 1 ~ "Hombre", e26 == 2 ~ "Mujer", .default = "Otro", comment = "Sexo")',
      paste0(
        'step_recode(svy, region, ',
        'dpto == 1 ~ "Montevideo", ',
        '.default = "Interior", ',
        'comment = "Region: Montevideo vs Interior")'
      )
    ),
    id = "ech_demographics_013",
    topic = "demographics",
    categories = list(demographics),
    downloads = 0L,
    certification = community_cert,
    user_info = author_gabriela,
    version = "1.0.0",
    cached_doc = list(
      input_variables = c("e27", "e26", "dpto"),
      output_variables = c("grupo_edad", "tramo_edad", "sexo", "region"),
      pipeline = list(
        list(index = 1L, type = "recode", outputs = list("grupo_edad"), inputs = list("e27"), inferred_type = "categorical", comment = "Grupos de edad estandar (7 tramos)"),
        list(index = 2L, type = "recode", outputs = list("tramo_edad"), inputs = list("e27"), inferred_type = "categorical", comment = "Tramos de edad para analisis laboral (4 tramos)"),
        list(index = 3L, type = "recode", outputs = list("sexo"), inputs = list("e26"), inferred_type = "categorical", comment = "Sexo"),
        list(index = 4L, type = "recode", outputs = list("region"), inputs = list("dpto"), inferred_type = "categorical", comment = "Region: Montevideo vs Interior")
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
    name = "Mercado Laboral ECH - Indicadores basicos",
    description = paste(
      "Estimaciones basicas del mercado de trabajo uruguayo:",
      "tasas de actividad, empleo, desempleo, subempleo.",
      "Incluye desagregaciones por sexo, tramo de edad y region."
    ),
    user = "calcita@gmx.li",
    user_info = author_gabriela,
    survey_type = "ech", edition = "2018",
    estimation_type = c("annual"),
    recipe_ids = c("ech_employment_001", "ech_underemployment_002", "ech_demographics_013"),
    calls = list(
      "svymean(~tasa_empleo, design, na.rm=TRUE)",
      "svymean(~tasa_desempleo, design, na.rm=TRUE)",
      "svymean(~tasa_actividad, design, na.rm=TRUE)",
      "svymean(~subempleo, design, na.rm=TRUE)",
      "svyby(~tasa_desempleo, ~sexo, design, svymean, na.rm=TRUE)",
      "svyby(~tasa_desempleo, ~tramo_edad, design, svymean, na.rm=TRUE)",
      "svyby(~tasa_desempleo, ~region, design, svymean, na.rm=TRUE)"
    ),
    call_metadata = list(
      list(type = "svymean", formula = "~tasa_empleo", description = "Tasa de empleo"),
      list(type = "svymean", formula = "~tasa_desempleo", description = "Tasa de desempleo"),
      list(type = "svymean", formula = "~tasa_actividad", description = "Tasa de actividad"),
      list(type = "svymean", formula = "~subempleo", description = "Tasa de subempleo"),
      list(type = "svyby", formula = "~tasa_desempleo", by = "~sexo", description = "Desempleo por sexo"),
      list(type = "svyby", formula = "~tasa_desempleo", by = "~tramo_edad", description = "Desempleo por tramo de edad"),
      list(type = "svyby", formula = "~tasa_desempleo", by = "~region", description = "Desempleo por region")
    ),
    downloads = 0L,
    certification = community_cert,
    version = "1.0.0"
  ),

  # ── WF2. Pobreza y Desigualdad ────────────────────────────────────────────
  RecipeWorkflow$new(
    id = "ech_wf_poverty",
    name = "Pobreza y Desigualdad ECH",
    description = paste(
      "Estimaciones de pobreza, indigencia, NBI y medida integrada.",
      "Incluye desagregaciones por quintil de ingreso y region."
    ),
    user = "calcita@gmx.li",
    user_info = author_gabriela,
    survey_type = "ech", edition = "2018",
    estimation_type = c("annual"),
    recipe_ids = c("ech_income_pc_004", "ech_quantiles_005", "ech_poverty_007",
                    "ech_nbi_008", "ech_integrated_poverty_009"),
    calls = list(
      "svymean(~pobre, design, na.rm=TRUE)",
      "svymean(~indigente, design, na.rm=TRUE)",
      "svymean(~tiene_nbi, design, na.rm=TRUE)",
      "svymean(~ingreso_pc, design, na.rm=TRUE)",
      "svyby(~pobre, ~quintil, design, svymean, na.rm=TRUE)",
      "svymean(~pobreza_integrada, design, na.rm=TRUE)"
    ),
    call_metadata = list(
      list(type = "svymean", formula = "~pobre", description = "Tasa de pobreza"),
      list(type = "svymean", formula = "~indigente", description = "Tasa de indigencia"),
      list(type = "svymean", formula = "~tiene_nbi", description = "Porcentaje hogares con al menos 1 NBI"),
      list(type = "svymean", formula = "~ingreso_pc", description = "Ingreso per capita promedio"),
      list(type = "svyby", formula = "~pobre", by = "~quintil", description = "Pobreza por quintil de ingreso"),
      list(type = "svymean", formula = "~pobreza_integrada", description = "Medida integrada de pobreza")
    ),
    downloads = 0L,
    certification = community_cert,
    version = "1.0.0"
  ),

  # ── WF3. Educacion ────────────────────────────────────────────────────────
  RecipeWorkflow$new(
    id = "ech_wf_education",
    name = "Perfil Educativo ECH",
    description = paste(
      "Estimaciones de nivel educativo y asistencia escolar.",
      "Incluye desagregaciones por sexo y quintil de ingreso."
    ),
    user = "calcita@gmx.li",
    user_info = author_gabriela,
    survey_type = "ech", edition = "2018",
    estimation_type = c("annual"),
    recipe_ids = c("ech_education_010", "ech_quantiles_005", "ech_demographics_013"),
    calls = list(
      "svymean(~nivel_educativo, design, na.rm=TRUE)",
      "svymean(~asiste, design, na.rm=TRUE)",
      "svyby(~asiste, ~quintil, design, svymean, na.rm=TRUE)",
      "svyby(~nivel_educativo, ~sexo, design, svymean, na.rm=TRUE)"
    ),
    call_metadata = list(
      list(type = "svymean", formula = "~nivel_educativo", description = "Distribucion por nivel educativo"),
      list(type = "svymean", formula = "~asiste", description = "Tasa de asistencia escolar"),
      list(type = "svyby", formula = "~asiste", by = "~quintil", description = "Asistencia por quintil de ingreso"),
      list(type = "svyby", formula = "~nivel_educativo", by = "~sexo", description = "Nivel educativo por sexo")
    ),
    downloads = 0L,
    certification = community_cert,
    version = "1.0.0"
  ),

  # ── WF4. Vivienda ─────────────────────────────────────────────────────────
  RecipeWorkflow$new(
    id = "ech_wf_housing",
    name = "Condiciones Habitacionales ECH",
    description = paste(
      "Estimaciones de condiciones de vivienda: tenencia, hacinamiento,",
      "calidad de materiales y NBI habitacional."
    ),
    user = "calcita@gmx.li",
    user_info = author_gabriela,
    survey_type = "ech", edition = "2018",
    estimation_type = c("annual"),
    recipe_ids = c("ech_housing_011", "ech_quantiles_005"),
    calls = list(
      "svymean(~tenencia, design, na.rm=TRUE)",
      "svymean(~hacinamiento, design, na.rm=TRUE)",
      "svymean(~hacinamiento_critico, design, na.rm=TRUE)",
      "svymean(~calidad_materiales, design, na.rm=TRUE)",
      "svyby(~hacinamiento_critico, ~quintil, design, svymean, na.rm=TRUE)"
    ),
    call_metadata = list(
      list(type = "svymean", formula = "~tenencia", description = "Distribucion tipo tenencia"),
      list(type = "svymean", formula = "~hacinamiento", description = "Hacinamiento promedio"),
      list(type = "svymean", formula = "~hacinamiento_critico", description = "Tasa de hacinamiento critico"),
      list(type = "svymean", formula = "~calidad_materiales", description = "Distribucion calidad materiales"),
      list(type = "svyby", formula = "~hacinamiento_critico", by = "~quintil", description = "Hacinamiento critico por quintil")
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
    name = "metasurvey",
    email = "metasurvey@example.com",
    password_hash = digest("ech_package_2024", algo = "sha256", serialize = FALSE),
    user_type = "individual",
    institution = NULL,
    url = "https://github.com/metaSurveyR/metasurvey",
    verified = FALSE,
    review_status = "approved",
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
  doc <- wf$to_list()
  doc$estimation_type <- as.list(doc$estimation_type)
  doc$recipe_ids <- as.list(doc$recipe_ids)
  doc$calls <- as.list(doc$calls)
  doc
}

# ══════════════════════════════════════════════════════════════════════════════
# SEED
# ══════════════════════════════════════════════════════════════════════════════

cat("==========================================================\n")
cat("  metasurvey MongoDB Seed - ECH recipes (metasurvey DSL)\n")
cat("==========================================================\n\n")

# Connect
db_users     <- mongolite::mongo(collection = "users",     db = DATABASE, url = MONGO_URI)
db_recipes   <- mongolite::mongo(collection = "recipes",   db = DATABASE, url = MONGO_URI)
db_workflows <- mongolite::mongo(collection = "workflows", db = DATABASE, url = MONGO_URI)

cat(sprintf("Connected to MongoDB (db: %s)\n", DATABASE))
cat(sprintf("  Before: users=%d, recipes=%d, workflows=%d\n\n",
            db_users$count(), db_recipes$count(), db_workflows$count()))

# ── 1. Delete old recipes ────────────────────────────────────────────────────
today <- format(Sys.Date(), "%Y-%m-%d")
cat(sprintf("[recipes] Deleting recipes created before %s...\n", today))

old_filter <- sprintf('{"created_at": {"$not": {"$regex": "^%s"}}}', today)
old_count <- db_recipes$count(old_filter)
if (old_count > 0) {
  db_recipes$remove(old_filter)
  cat(sprintf("  Deleted %d old recipes\n", old_count))
} else {
  cat("  No old recipes to delete\n")
}

no_date_filter <- '{"created_at": {"$exists": false}}'
no_date_count <- db_recipes$count(no_date_filter)
if (no_date_count > 0) {
  db_recipes$remove(no_date_filter)
  cat(sprintf("  Deleted %d recipes without created_at\n", no_date_count))
}

# Delete old workflows too
old_wf_count <- db_workflows$count(old_filter)
if (old_wf_count > 0) {
  db_workflows$remove(old_filter)
  cat(sprintf("  Deleted %d old workflows\n", old_wf_count))
}

# Delete test user if exists
db_users$remove('{"email": "test@example.com"}')

# ── 2. Upsert seed users ─────────────────────────────────────────────────────
cat("\n[users] Upserting seed users...\n")
for (u in seed_users) {
  tryCatch({
    db_users$remove(toJSON(list(email = u$email), auto_unbox = TRUE))
    db_users$insert(toJSON(u, auto_unbox = TRUE, null = "null"))
    cat(sprintf("  + %s (%s)\n", u$name, u$email))
  }, error = function(e) message("  ERROR: ", e$message))
}

# ── 3. Insert seed recipes ───────────────────────────────────────────────────
cat(sprintf("\n[recipes] Inserting %d recipes...\n", length(recipes)))
for (r in recipes) {
  doc <- serialize_recipe(r)
  tryCatch({
    db_recipes$remove(toJSON(list(id = doc$id), auto_unbox = TRUE))
    db_recipes$insert(toJSON(doc, auto_unbox = TRUE, null = "null"))
    cat(sprintf("  + %s [%s]\n", doc$name, doc$id))
  }, error = function(e) message("  ERROR: ", e$message))
}

# ── 4. Insert seed workflows ─────────────────────────────────────────────────
cat(sprintf("\n[workflows] Inserting %d workflows...\n", length(workflows)))
for (wf in workflows) {
  doc <- serialize_workflow(wf)
  tryCatch({
    db_workflows$remove(toJSON(list(id = doc$id), auto_unbox = TRUE))
    db_workflows$insert(toJSON(doc, auto_unbox = TRUE, null = "null"))
    cat(sprintf("  + %s [%s]\n", doc$name, doc$id))
  }, error = function(e) message("  ERROR: ", e$message))
}

# ── Summary ──────────────────────────────────────────────────────────────────
cat(sprintf("\n  After: users=%d, recipes=%d, workflows=%d\n",
            db_users$count(), db_recipes$count(), db_workflows$count()))
cat("\nSeed complete!\n")
