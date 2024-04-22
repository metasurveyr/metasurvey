## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(metasurvey)

set_engine(
  "data.table"
)

svy_example <- load_survey(
  svy_type = "eaii",
  svy_edition = "2019-2021",
  svy_weight = "w_trans",
  input = load_survey_example("2019-2021.csv"),
  dec = ","
)

# as.data.frame(svy_example)
# as.tibble(svy_example)


set_use_copy(TRUE)

new_svy <- svy_example %>%
  step_recode(
    new_var = "realiza_innovacion",
    B1_1_1 == 1 ~ "Realiza innovación",
    B1_2_1 == 1 ~ "Realiza innovación",
    B1_3_1 == 1 ~ "Realiza innovación",
    B1_4_1 == 1 ~ "Realiza innovación",
    B1_5_1 == 1 ~ "Realiza innovación",
    B1_6_1 == 1 ~ "Realiza innovación",
    B1_7_1 == 1 ~ "Realiza innovación",
    B1_8_1 == 1 ~ "Realiza innovación",
    B1_9_1 == 1 ~ "Realiza innovación",
    .default = "No realiza innovación"
  ) %>%
  step_recode(
    new_var = "sector",
    data.table::between(Division, 10, 33) ~ "Industria",
    data.table::between(Division, 34, 99) ~ "Servicios",
    Division == "C1" ~ "Industria",
    Division == "C2" ~ "Servicios",
    Division == "E1" ~ "Servicios"
  ) %>%
  metasurvey::step_recode(
    new_var = "tipo_actividad",
    B1_1_1 == 1 ~ "I + D Interna",
    B1_2_1 == 1 ~ "I + D Externa",
    B1_3_1 == 1 ~ "Bienes de Capital",
    B1_4_1 == 1 ~ "Software",
    B1_5_1 == 1 ~ "Propiedad Intelectual",
    B1_6_1 == 1 ~ "Ingeniería",
    B1_7_1 == 1 ~ "Capacitación",
    B1_8_1 == 1 ~ "Marketing",
    B1_9_1 == 1 ~ "Gestión",
    .default = "Otra"
  ) %>%
  metasurvey::step_recode(
    new_var = "tipo_innovacion",
    E1_1_1 == 1 ~ "Producto",
    E1_2_1 == 1 ~ "Proceso",
    .default = "Otra"
  ) %>%
  metasurvey::step_compute(
    subsector = Division
  ) %>%
  metasurvey::step_recode(
    new_var = "cant_traba_tramo",
    data.table::between(IG_4_1_3, 0, 4) ~ "1",
    data.table::between(IG_4_1_3, 5, 19) ~ "2",
    data.table::between(IG_4_1_3, 20, 99) ~ "3",
    IG_4_1_3 > 99 ~ "4"
  ) %>%
  metasurvey::step_recode(
    new_var = "ingreso_vta_pesos",
    data.table::between(IG_5_1_1_3, 0, 9942787) ~ "1",
    data.table::between(IG_5_1_1_3, 9942788, 49713934) ~ "2", # nolint
    data.table::between(IG_5_1_1_3, 49713935, 372854507) ~ "3", # nolint
    IG_5_1_1_3 > 372854507 ~ "4"
  ) %>%
  metasurvey::step_recode(
    new_var = "tamanio",
    cant_traba_tramo == "1" & ingreso_vta_pesos == "1" ~ "Pequenias",
    cant_traba_tramo == "2" & ingreso_vta_pesos == "2" ~ "Pequenias",
    cant_traba_tramo == "2" & ingreso_vta_pesos == "1" ~ "Pequenias",
    cant_traba_tramo == "1" & ingreso_vta_pesos == "2" ~ "Pequenias",
    cant_traba_tramo == "3" & ingreso_vta_pesos == "3" ~ "Medianas",
    cant_traba_tramo == "3" & ingreso_vta_pesos == "2" ~ "Medianas",
    cant_traba_tramo == "3" & ingreso_vta_pesos == "1" ~ "Medianas",
    cant_traba_tramo == "1" & ingreso_vta_pesos == "3" ~ "Medianas",
    cant_traba_tramo == "2" & ingreso_vta_pesos == "3" ~ "Medianas",
    cant_traba_tramo == "4" & ingreso_vta_pesos == "4" ~ "Grandes",
    cant_traba_tramo == "4" & ingreso_vta_pesos == "3" ~ "Grandes",
    cant_traba_tramo == "4" & ingreso_vta_pesos == "2" ~ "Grandes",
    cant_traba_tramo == "4" & ingreso_vta_pesos == "1" ~ "Grandes",
    cant_traba_tramo == "1" & ingreso_vta_pesos == "4" ~ "Grandes",
    cant_traba_tramo == "2" & ingreso_vta_pesos == "4" ~ "Grandes",
    cant_traba_tramo == "3" & ingreso_vta_pesos == "4" ~ "Grandes"
  ) %>%
  metasurvey::step_compute(
    subsector = Division
  )

get_metadata(new_svy)

# Ver steps

steps <- get_steps(new_svy)

## -----------------------------------------------------------------------------
eph2022_3 <- load_survey(
  path = load_survey_example("eph2022_3.csv"),
  svy_type = "eph",
  svy_edition = "2022_3",
  svy_weight = "PONDERA"
) %>%
  metasurvey::step_recode(
    "pea",
    ESTADO %in% 1:2 ~ 1,
    .default = 0
  ) %>%
  metasurvey::step_recode(
    "pet",
    ESTADO != 4 ~ 1,
    .default = 0
  ) %>%
  metasurvey::step_recode(
    "po",
    ESTADO == 1 ~ 1,
    .default = 0
  ) %>%
  metasurvey::step_recode(
    "pd",
    ESTADO == 2 ~ 1,
    .default = 0
  )

