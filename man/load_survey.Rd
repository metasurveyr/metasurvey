% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_survey.R
\name{load_survey}
\alias{load_survey}
\title{Read survey files from different formats and create a Survey object}
\usage{
load_survey(
  path = NULL,
  svy_type = NULL,
  svy_edition = NULL,
  svy_weight = NULL,
  svy_psu = NULL,
  ...,
  bake = FALSE,
  recipes = NULL
)
}
\arguments{
\item{path}{Survey file path, file can be in different formats, csv, xtsx, dta, sav and rds}

\item{svy_type}{String with the survey type, supported types; "ech" (Encuensta Continua de Hogares, Uruguay), "eph" ( Encuesta Permanente de Hogares, Argentina), "eai" (Encuesta de Actividades de Innovación, Uruguay)}

\item{svy_edition}{String with survey edition information, support different time patterns: "YYYYMM"/"MMYYYY" (year- month), "YYYY" (year),  ("YYYY-YYYY") date range}

\item{svy_weight}{List with survey weight information specifing periodicity and  the name of the weight variable. Recomended to use the helper function add_weight().}

\item{svy_psu}{Primary sampling unit}

\item{...}{Further arguments to be passed to  load_survey}

\item{bake}{Logical inicating if a recipes is processed when the data are loaded.}

\item{recipes}{object, using helper function get_recipes()}
}
\value{
Survey object
}
\description{
Read survey files from different formats and create a Survey object
}
\examples{
ech_2022 <- load_survey(
  metasurvey::load_survey_example(
    "ech",
    "ech_2022"
  ),
  svy_edition = "2022",
  svy_type = "ech",
  svy_weight = add_weight(annual = "w_ano"),
  recipes = get_recipe(
    "ech",
    "2022"
  )
)
# Example of loading a survey file
survey <- load_survey(
  path = "path/to/survey.csv",
  svy_type = "ech",
  svy_edition = "2023",
  svy_weight = add_weight(annual = "w_ano"),
  bake = TRUE
)
print(survey)
}
\keyword{preprocessing}
