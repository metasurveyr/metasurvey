% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Recipes.R
\name{get_recipe}
\alias{get_recipe}
\title{Get Recipe}
\usage{
get_recipe(
  svy_type = NULL,
  svy_edition = NULL,
  topic = NULL,
  allowMultiple = TRUE
)
}
\arguments{
\item{svy_type}{A character string specifying the survey type.}

\item{svy_edition}{A character string specifying the survey edition.}

\item{topic}{A character string specifying the topic of the recipe.}

\item{allowMultiple}{A logical value indicating whether to allow multiple recipes.}
}
\value{
A Recipe object or a list of Recipe objects.
}
\description{
Retrieves a Recipe object from the API based on specified criteria.
}
\details{
This function queries the API to retrieve recipes matching the specified criteria.
}
\examples{
# Example of retrieving a Recipe object from the API
recipe_obj <- get_recipe(svy_type = "eaii", svy_edition = "2019-2021")
print(recipe_obj)
}
\keyword{utils}
